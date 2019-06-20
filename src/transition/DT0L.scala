package matching.transition

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
              a <- morphs.keys.toSeq;
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
                    end != sc && !isAtom(end) && result(end)._1 == maxDegree
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

    def convertWitness(w: Witness[(A,P)]): Witness[A] = {
      Witness(w.separators.map(_.map(_._1)), w.pumps.map(_.map(_._1)))
    }

    val dt0l = Debug.time("indexed DT0L -> DT0L") {
      toDT0L()
    }

    val (growthRate, witness, last) = Debug.time("calculate growth rate") {
      dt0l.calcGrowthRate(initials)
    }

    (growthRate, convertWitness(witness), last.map(_._2))

    // val (renamedDT0L, renameMap) = dt0l.rename()
    //
    // val (growthRate, witness, last) = Debug.time("calculate growth rate") {
    //   renamedDT0L.calcGrowthRate(initials.map(renameMap))
    // }
    //
    // (growthRate, convertWitness(witness), last.map{ last =>
    //   renameMap.find{case (k,v) => v == last}.get._1._2
    // })
  }
}
