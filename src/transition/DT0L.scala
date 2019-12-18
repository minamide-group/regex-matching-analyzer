package matching.transition

import matching.Witness
import matching.tool.{Analysis, Debug}

class DT0L[A,Q](
  states: Set[Q],
  morphs: Map[A, Map[Q,Seq[Q]]]
) {
  type Pump = (Q,Seq[A],Q)

  def rename(): (DT0L[A,Int], Map[Q,Int]) = {
    val renameMap = states.zipWithIndex.toMap
    val renamedStates = states.map(renameMap)
    val renamedMorphs = morphs.mapValues(
      _.map{case (q,qs) => renameMap(q) -> qs.map(renameMap)}
    )

    (new DT0L(renamedStates, renamedMorphs), renameMap)
  }

  def calcGrowthRate(initials: Set[Q]): (Option[Int], Witness[A], Option[Q]) = {
    def toLabeledGraph(initials: Set[Q]): LabeledGraph[Q,A] = {
      val labeledEdges = morphs.flatMap{ case (a,morph) =>
        morph.flatMap{ case (q,qs) =>
          Analysis.checkInterrupted("preparing for calculate growth rate")
          qs.map((q,a,_))
        }
      }.toSeq

      val graph = new LabeledGraph(labeledEdges)
      graph.reachablePartFrom(initials)
    }

    val graph = toLabeledGraph(initials)

    def calcGrowthRate(): (Option[Int], Seq[Pump]) = {
      val scs = graph.calcStrongComponents()
      val scMap = (for (sc <- scs; q <- sc) yield q -> sc).toMap
      val scEdges = graph.edges.map{ case (q1,q2) =>
        (scMap(q1),scMap(q2))
      }.filter{case (sc1,sc2) => sc1 != sc2}.distinct
      val scGraph = new Graph(scs, scEdges)

      Debug.info("graph info") (
        ("number of nodes", graph.nodes.size),
        ("number of edges", graph.labeledEdges.size),
        ("number of strong components", scs.size)
      )

      val alpha = morphs.keys.toSeq
      val reachableMapScGraph = scGraph.reachableMapDAG()
      val reachableMapScGraphRev = scGraph.reverse().reachableMapDAG()
      val scPairLabeledAdj = graph.labeledEdges.groupBy{ case (q1,a,q2) =>
        Analysis.checkInterrupted("preparing for calculate growth rate")
        (scMap(q1),scMap(q2))
      }.mapValues(
        _.groupBy(_._2).mapValues(
          _.map{case (q1,_,q2) => (q1,q2)}
        ).withDefaultValue(Seq())
      ).withDefaultValue(Map().withDefaultValue(Seq()))

      val hasSelfLoop = graph.labeledEdges.collect{case (q1,a,q2) if q1 == q2 => q1}.toSet
      def isAtom(sc: Set[Q]): Boolean = {
        sc.size == 1 && !hasSelfLoop(sc.head)
      }

      def checkEDA(): Option[Pump] = {
        def checkEDA(sc: Set[Q]): Option[Pump] = {
          val labeledAdjSc = scPairLabeledAdj((sc,sc))

          def constructG2(sc: Set[Q]): LabeledGraph[(Q,Q),A] = {
            val e2 = for (
              (a,es) <- labeledAdjSc.toSeq;
              (p1,q1) <- es;
              (p2,q2) <- es
            ) yield {
              Analysis.checkInterrupted("construct G2")
              ((p1,p2), a, (q1,q2))
            }

            new LabeledGraph(e2)
          }

          labeledAdjSc.find{case (_,es) => es.length != es.distinct.length} match {
            case Some((a,es)) =>
              val (q1,q2) = es.diff(es.distinct).head
              val back = graph.getPath(q2,q1).get
              Some((q1, a +: back, q1))
            case None =>
              val g2 = constructG2(sc)
              g2.calcStrongComponents().find(
                _.exists{case (q1,q2) => q1 == q2}
              ).flatMap(_.find{
                case (q1,q2) => q1 != q2
              }.map{ case (q1,q2) =>
                val path1 = g2.getPath((q1,q1),(q1,q2)).get
                val path2 = g2.getPath((q1,q2),(q1,q1)).get
                (q1, path1 ++ path2, q1)
              })
          }
        }

        scGraph.nodes.filterNot(isAtom).toStream.map(checkEDA).find(_.isDefined).map(_.get)
      }

      def calcDegree(): (Int, Seq[Pump]) = {
        def checkIDA(start: Set[Q], end: Set[Q]): Option[Pump] = {
          def constructG3(): LabeledGraph[(Q,Q,Q),A] = {
            val labeledAdjStart = scPairLabeledAdj((start,start))
            val passage = reachableMapScGraph(start) & reachableMapScGraphRev(end)
            val labeledAdjPassage = for (sc1 <- passage; sc2 <- passage) yield scPairLabeledAdj((sc1,sc2))
            val labeledAdjEnd = scPairLabeledAdj((end,end))

            val e3 = for (
              a <- alpha;
              (p1,q1) <- labeledAdjStart(a);
              (p2,q2) <- labeledAdjPassage.flatMap(_(a));
              (p3,q3) <- labeledAdjEnd(a)
            ) yield {
              Analysis.checkInterrupted("construct G3")
              ((p1,p2,p3), a, (q1,q2,q3))
            }

            new LabeledGraph(e3)
          }

          val g3 = constructG3()
          val e3WithBack = g3.labeledEdges.map{
            case (v1,a,v2) => (v1,Some(a),v2)
          } ++ (for (
            p <- start;
            q <- end
          ) yield {
            Analysis.checkInterrupted("construct G3")
            ((p,q,q), None, (p,p,q))
          })
          val g3WithBack = new LabeledGraph(e3WithBack)
          g3WithBack.calcStrongComponents().toStream.map{ sc =>
            sc.collect{
              case (p1,p2,p3) if p2 == p3 && p1 != p2 => (p1,p2)
            }.find{
              case (p,q) => sc.exists{case (q1,q2,q3) => q1 == p && q2 == p && q3 == q}
            }.map{
              case (p,q) => (p,g3.getPath((p,p,q),(p,q,q)).get,q)
            }
          }.find(_.isDefined).map(_.get)
        }

        var result = Map[Set[Q], (Int,Seq[Pump])]()
        def calcDegree(sc: Set[Q]): (Int,Seq[Pump]) = {
          result.get(sc) match {
            case Some(res) => res
            case None =>
              val children = scGraph.adj(sc)
              val maxChild @ (maxDegree,_) = ((-1,Seq()) +: children.map(calcDegree)).maxBy(_._1)
              val degreePumpSc = if (maxChild._1 == -1) {
                (if (isAtom(sc)) -1 else 0, Seq())
              } else {
                if (isAtom(sc)) {
                  maxChild
                } else {
                  reachableMapScGraph(sc).filter( end =>
                    end != sc &&
                    !isAtom(end) &&
                    result(end)._1 == maxDegree
                  ).toStream.map{ end =>
                    (end,checkIDA(sc,end))
                  }.find(_._2.isDefined) match {
                    case Some((end,Some(pump))) => (maxDegree+1, pump +: result(end)._2)
                    case _ => maxChild
                  }
                }
              }

              result += sc -> degreePumpSc
              degreePumpSc
          }
        }

        scGraph.nodes.map(calcDegree).maxBy(_._1)
      }

      checkEDA() match {
        case Some(pump) =>
          (None, Seq(pump))
        case None =>
          val (degree, pumps) = calcDegree()
          (Some(degree), pumps)
      }
    }

    def generateWitness(ps: Seq[Pump]): Witness[A] = {
      var separators = IndexedSeq[Seq[A]]()
      var pumps = IndexedSeq[Seq[A]]()
      if (ps.nonEmpty) {
        separators :+= graph.getPath(initials,ps.head._1).get
        pumps :+= ps.head._2
        if (ps.length > 1) {
          ps.sliding(2).foreach{ case Seq((_,_,q12),(q21,as,_)) =>
            separators :+= graph.getPath(q12,q21).get
            pumps :+= as
          }
        }
      }

      Witness(separators, pumps)
    }

    val (growthRate, pumps) = calcGrowthRate()
    (growthRate, generateWitness(pumps), pumps.lastOption.map(_._3))
  }

  def calcGrowthRate(initial: Q): (Option[Int], Witness[A], Option[Q]) = {
    calcGrowthRate(Set(initial))
  }
}

class PairDT0L[A,R,P](
  states: Set[(R,P)],
  morphs: Map[A, Map[(R,P),Seq[(R,P)]]]
) {
  type Q = (R,P)
  type Pump = (Q,Seq[A],Q)

  def calcGrowthRate(initials: Set[Q]): (Option[Int], Witness[A], Option[Q]) = {
    def toLabeledGraph(initials: Set[Q]): LabeledGraph[Q,A] = {
      val labeledEdges = morphs.flatMap{ case (a,morph) =>
        morph.flatMap{ case (q,qs) =>
          Analysis.checkInterrupted("preparing for calculate growth rate")
          qs.map((q,a,_))
        }
      }.toSeq

      val graph = new LabeledGraph(labeledEdges)
      graph.reachablePartFrom(initials)
    }

    val graph = toLabeledGraph(initials)

    def calcGrowthRate(): (Option[Int], Seq[Pump]) = {
      val scs = graph.calcStrongComponents()
      val scMap = (for (sc <- scs; q <- sc) yield q -> sc).toMap
      val scEdges = graph.edges.map{ case (q1,q2) =>
        (scMap(q1),scMap(q2))
      }.filter{case (sc1,sc2) => sc1 != sc2}.distinct
      val scGraph = new Graph(scs, scEdges)

      Debug.info("graph info") (
        ("number of nodes", graph.nodes.size),
        ("number of edges", graph.labeledEdges.size),
        ("number of strong components", scs.size)
      )

      val alpha = morphs.keys.toSeq
      val reachableMapScGraph = scGraph.reachableMapDAG()
      val reachableMapScGraphRev = scGraph.reverse().reachableMapDAG()
      val scPairLabeledAdj = graph.labeledEdges.groupBy{ case (q1,a,q2) =>
        Analysis.checkInterrupted("preparing for calculate growth rate")
        (scMap(q1),scMap(q2))
      }.mapValues(
        _.groupBy(_._2).mapValues(
          _.map{case (q1,_,q2) => (q1,q2)}
        ).withDefaultValue(Seq())
      ).withDefaultValue(Map().withDefaultValue(Seq()))
      val secondComponentMap = scs.map(sc => sc -> sc.map(_._2)).toMap

      val hasSelfLoop = graph.labeledEdges.collect{case (q1,a,q2) if q1 == q2 => q1}.toSet
      def isAtom(sc: Set[Q]): Boolean = {
        sc.size == 1 && !hasSelfLoop(sc.head)
      }

      def checkEDA(): Option[Pump] = {
        def checkEDA(sc: Set[Q]): Option[Pump] = {
          val labeledAdjSc = scPairLabeledAdj((sc,sc))

          def constructG2(sc: Set[Q]): LabeledGraph[((R,R),P),A] = {
            val e2 = for (
              (a,es) <- labeledAdjSc.toSeq;
              ((r11,p11),(r12,p12)) <- es;
              ((r21,p21),(r22,p22)) <- es;
              if p11 == p21 && p12 == p22
            ) yield {
              Analysis.checkInterrupted("construct G2")
              (((r11,r21), p11), a, ((r12,r22),p12))
            }

            new LabeledGraph(e2)
          }

          labeledAdjSc.find{case (_,es) => es.length != es.distinct.length} match {
            case Some((a,es)) =>
              val (q1,q2) = es.diff(es.distinct).head
              val back = graph.getPath(q2,q1).get
              Some((q1, a +: back, q1))
            case None =>
              val g2 = constructG2(sc)
              g2.calcStrongComponents().find( sc =>
                sc.exists{case ((r1,r2),_) => r1 == r2} &&
                sc.exists{case ((r1,r2),_) => r1 != r2}
              ).map{ case sc =>
                val q1 @ ((r1,r2),p) = sc.find{case ((r1,r2),p) => r1 != r2}.get
                val q2 @ (_,p0) = sc.find{case ((r3,r4),_) => r3 == r1 && r4 == r1}.get
                val path1 = g2.getPath(q2,q1).get
                val path2 = g2.getPath(q1,q2).get
                ((r1,p0), path1 ++ path2, (r1,p0))
              }
          }
        }

        scGraph.nodes.filterNot(isAtom).toStream.map(checkEDA).find(_.isDefined).map(_.get)
      }

      def calcDegree(): (Int, Seq[Pump]) = {
        def checkIDA(start: Set[Q], end: Set[Q]): Option[Pump] = {
          def constructG3(): LabeledGraph[((R,R,R),P),A] = {
            val labeledAdjStart = scPairLabeledAdj((start,start))
            val passage = reachableMapScGraph(start) & reachableMapScGraphRev(end)
            val labeledAdjPassage = for (sc1 <- passage; sc2 <- passage) yield scPairLabeledAdj((sc1,sc2))
            val labeledAdjEnd = scPairLabeledAdj((end,end))

            val e3 = for (
              a <- alpha;
              ((r11,p11),(r12,p12)) <- labeledAdjStart(a);
              ((r21,p21),(r22,p22)) <- labeledAdjPassage.flatMap(_(a));
              ((r31,p31),(r32,p32)) <- labeledAdjEnd(a);
              if p11 == p21 && p21 == p31 && p12 == p22 && p22 == p32
            ) yield {
              Analysis.checkInterrupted("construct G3")
              (((r11,r21,r31),p11), a, ((r12,r22,r32), p12))
            }

            new LabeledGraph(e3)
          }

          val g3 = constructG3()
          val e3WithBack = g3.labeledEdges.map{
            case (v1,a,v2) => (v1,Some(a),v2)
          } ++ (for (
            (r1,p1) <- start;
            (r2,p2) <- end;
            if p1 == p2
          ) yield {
            Analysis.checkInterrupted("construct G3")
            (((r1,r2,r2),p1), None, ((r1,r1,r2),p1))
          })
          val g3WithBack = new LabeledGraph(e3WithBack)
          g3WithBack.calcStrongComponents().toStream.map{ sc =>
            sc.collect{
              case ((r1,r2,r3),p) if r1 == r2 && r2 != r3 => (r1,r3,p)
            }.find{
              case (r11,r12,p1) => sc.exists{
                case ((r21,r22,r23),p2) => r21 == r11 && r22 == r12 && r23 == r12 && p1 == p2
              }
            }.map{
              case (r11,r12,p1) => ((r11,p1),g3.getPath(((r11,r11,r12),p1),((r11,r12,r12),p1)).get,(r12,p1))
            }
          }.find(_.isDefined).map(_.get)
        }

        var result = Map[Set[Q], (Int,Seq[Pump])]()
        def calcDegree(sc: Set[Q]): (Int,Seq[Pump]) = {
          result.get(sc) match {
            case Some(res) => res
            case None =>
              val children = scGraph.adj(sc)
              val maxChild @ (maxDegree,_) = ((-1,Seq()) +: children.map(calcDegree)).maxBy(_._1)
              val degreePumpSc = if (maxChild._1 == -1) {
                (if (isAtom(sc)) -1 else 0, Seq())
              } else {
                if (isAtom(sc)) {
                  maxChild
                } else {
                  reachableMapScGraph(sc).filter( end =>
                    end != sc &&
                    !isAtom(end) &&
                    result(end)._1 == maxDegree &&
                    (secondComponentMap(sc) & secondComponentMap(end)).nonEmpty
                  ).toStream.map(end => (end,checkIDA(sc,end))).find(_._2.isDefined) match {
                    case Some((end,Some(pump))) => (maxDegree+1, pump +: result(end)._2)
                    case _ => maxChild
                  }
                }
              }

              result += sc -> degreePumpSc
              degreePumpSc
          }
        }

        scGraph.nodes.map(calcDegree).maxBy(_._1)
      }

      checkEDA() match {
        case Some(pump) =>
          (None, Seq(pump))
        case None =>
          val (degree, pumps) = calcDegree()
          (Some(degree), pumps)
      }
    }

    def generateWitness(ps: Seq[Pump]): Witness[A] = {
      var separators = IndexedSeq[Seq[A]]()
      var pumps = IndexedSeq[Seq[A]]()
      if (ps.nonEmpty) {
        separators :+= graph.getPath(initials,ps.head._1).get
        pumps :+= ps.head._2
        if (ps.length > 1) {
          ps.sliding(2).foreach{ case Seq((_,_,q12),(q21,as,_)) =>
            separators :+= graph.getPath(q12,q21).get
            pumps :+= as
          }
        }
      }

      Witness(separators, pumps)
    }

    val (growthRate, pumps) = calcGrowthRate()
    (growthRate, generateWitness(pumps), pumps.lastOption.map(_._3))
  }

  def calcGrowthRate(initial: Q): (Option[Int], Witness[A], Option[Q]) = {
    calcGrowthRate(Set(initial))
  }
}

class IndexedDT0L[A,Q,P](
  states: Set[Q],
  indices: Set[P],
  indexedMorphs: Map[(P,P), Map[A, Map[Q,Seq[Q]]]]
) {
  def calcGrowthRate(initials: Set[(Q,P)], lookaheadDFA: DFA[P,A]): (Option[Int], Witness[A], Option[P]) = {
    def toDT0L(): DT0L[(A,P),(Q,P)] = {
      val statesDT0L = for (state <- states; index <- indices) yield (state, index)

      val morphs = indexedMorphs.flatMap{ case ((p1,p2),morphs) =>
        morphs.map{ case (a,morph) =>
          (a,p2) -> morph.map{ case (b,bs) =>
             (b,p1) -> bs.map((_,p2))
          }
        }
      }

      new DT0L(statesDT0L, morphs)
    }

    def toPairDT0L(): PairDT0L[(A,P),Q,P] = {
      val statesDT0L = for (state <- states; index <- indices) yield (state, index)

      val morphs = indexedMorphs.flatMap{ case ((p1,p2),morphs) =>
        morphs.map{ case (a,morph) =>
          (a,p2) -> morph.map{ case (b,bs) =>
             (b,p1) -> bs.map((_,p2))
          }
        }
      }

      new PairDT0L(statesDT0L, morphs)
    }

    def convertWitness(w: Witness[(A,P)]): Witness[A] = {
      Witness(w.separators.map(_.map(_._1)), w.pumps.map(_.map(_._1)))
    }

    val pairDT0L = Debug.time("indexed DT0L -> DT0L") {
      toPairDT0L()
    }

    val (growthRate, witness, last) = Debug.time("calculate growth rate") {
      pairDT0L.calcGrowthRate(initials)
    }

    (growthRate, convertWitness(witness), last.map(_._2))


    // val dt0l = Debug.time("indexed DT0L -> DT0L") {
    //   toDT0L()
    // }
    //
    // val (renamedDT0L, renameMap) = dt0l.rename()
    //
    // val (growthRate, witness, last) = Debug.time("calculate growth rate") {
    //   renamedDT0L.calcGrowthRate(initials.map(renameMap))
    // }
    //
    // (growthRate, convertWitness(witness), last.map{ last =>
    //   renameMap.find{case (_,v) => v == last}.get._1._2
    // })
  }
}
