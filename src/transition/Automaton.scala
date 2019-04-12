package matching.transition

import scala.collection.mutable.Stack
import matching.tool.Analysis

class NFA[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val delta: Seq[(Q,A,Q)],
  val initialStates: Set[Q],
  val finalStates: Set[Q]
) extends LabeledGraph[Q,A](states, delta) {
  type Pump = (Q,Seq[A],Q)

  lazy val reachableMapScsGraph = scsGraph.reachableMapDAG()
  lazy val reachableMapScsGraphRev = scsGraph.reverse().reachableMapDAG()
  lazy val scPairLabeledAdj = delta.groupBy{ case (q1,a,q2) =>
    Analysis.checkInterrupted("preparing for calculate ambiguity")
    (scsGraph.nodes.find(_.contains(q1)).get, scsGraph.nodes.find(_.contains(q2)).get)
  }.mapValues(
    _.groupBy(_._2).mapValues(
      _.map{case (v1,_,v2) => (v1,v2)}
    ).withDefaultValue(Seq())
  ).withDefaultValue(Map().withDefaultValue(Seq()))

  override def reverse(): NFA[Q,A] = {
    new NFA(states, sigma, delta.map{case (q1,a,q2) => (q2,a,q1)}, finalStates, initialStates)
  }

  def reachablePart(): NFA[Q,A] = {
    val reachableStates = initialStates.flatMap(reachableFrom)
    new NFA(
      reachableStates,
      sigma,
      delta.filter{case (q1,a,q2) => reachableStates(q1) && reachableStates(q2)},
      initialStates.filter(reachableStates),
      finalStates.filter(reachableStates)
    )
  }

  def toDFA(): DFA[Set[Q],A] = {
    val newInitial = initialStates
    var newStates = Set(initialStates)
    val stack = Stack(newInitial)
    var newDelta = Map[(Set[Q],A),Set[Q]]()

    while (stack.nonEmpty) {
      Analysis.checkInterrupted("constructing DFA")
      val qs = stack.pop
      sigma.foreach{ a =>
        val next = qs.flatMap(labeledAdj(_)(a))
        newDelta += (qs,a) -> next
        if (!newStates.contains(next)) {
          newStates += next
          stack.push(next)
        }
      }
    }

    val newFinal = newStates.filter(qs => (qs & finalStates).nonEmpty)

    new DFA(newStates, sigma, newDelta, newInitial, newFinal)
  }

  def hasLoop(): Boolean = {
    edges.exists{case (q1,q2) => q1 == q2} || scsGraph.nodes.size != states.size
  }

  private def calcAmbiguity(checkIDA: (Set[Q],Set[Q]) => Option[Pump]): (Option[Int], Seq[Pump]) = {
    def checkEDA(): Option[Pump] = {
      def checkEDA(sc: Set[Q]): Option[Pump] = {
        Analysis.checkInterrupted("checking EDA")
        val labeledAdjSc = scPairLabeledAdj((sc,sc))

        def constructG2(sc: Set[Q]): LabeledGraph[(Q,Q),A] = {
          val g2Edges = labeledAdjSc.toSeq.flatMap{ case (a,es) =>
            for ((p1,q1) <- es; (p2,q2) <- es) yield ((p1,p2), a, (q1,q2))
          }
          new LabeledGraph(g2Edges)
        }

        labeledAdjSc.find{case (_,es) => es.length != es.distinct.length} match {
          case Some((a,es)) =>
            val (q1,q2) = es.diff(es.distinct).head
            val back = getPath(q2,q1).get
            Some((q1, a +: back, q1))
          case None =>
            val g2 = constructG2(sc)
            g2.calcStrongComponents().find(
              _.exists{case (q1,q2) => q1 == q2}
            ) match {
              case Some(g2sc) => g2sc.find{
                case (q1,q2) => q1 != q2
              }.map{ case (q1,q2) =>
                val path1 = g2.getPath((q1,q1),(q1,q2)).get
                val path2 = g2.getPath((q1,q2),(q1,q1)).get
                (q1, path1 ++ path2, q1)
              }
              case None => None
            }
        }
      }

      scsGraph.nodes.toStream.map(checkEDA).find(_.isDefined).map(_.get)
    }

    val hasSelfLoop = delta.collect{case (q1,a,q2) if q1 == q2 => q1}.toSet
    def canSkip(sc: Set[Q]): Boolean = {
      sc.size == 1 && !hasSelfLoop(sc.head)
    }

    def calcDegree(): (Int, Seq[Pump])  = {
      var result = Map[Set[Q], (Int,Seq[Pump])]()
      def calcDegree(sc: Set[Q]): (Int,Seq[Pump]) = {
        Analysis.checkInterrupted("calculating degree")
        result.get(sc) match {
          case Some(res) => res
          case None =>
            val children = scsGraph.adj(sc)
            val degreePumpSc = if (children.isEmpty) {
              (0, Seq())
            } else {
              val maxChild @ (maxDegree, _) = children.map(calcDegree).maxBy(_._1)
              if (canSkip(sc)) {
                maxChild
              } else {
                reachableMapScsGraph(sc).filter( end =>
                  end != sc && !canSkip(end) && result(end)._1 == maxDegree
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

      scsGraph.nodes.map(calcDegree).maxBy(_._1)
    }

    checkEDA match {
      case Some(pump) =>
        (None, Seq(pump))
      case None =>
        val (degree, pumps) = calcDegree()
        (Some(degree), pumps)
    }
  }

  def calcAmbiguity(): (Option[Int], Witness[A]) = {
    def checkIDA(start: Set[Q], end: Set[Q]): Option[Pump] = {
      def constructG3(): LabeledGraph[(Q,Q,Q),A] = {
        val labeledAdjStart = scPairLabeledAdj((start,start))
        val passage = reachableMapScsGraph(start) & reachableMapScsGraphRev(end)
        val labeledAdjPassage = for (sc1 <- passage; sc2 <- passage) yield scPairLabeledAdj((sc1,sc2))
        val labeledAdjEnd = scPairLabeledAdj((end,end))
        val e3 = (
          (for (
            a <- sigma.toSeq;
            (p1,q1) <- labeledAdjStart(a);
            (p2,q2) <- labeledAdjPassage.flatMap(_(a));
            (p3,q3) <- labeledAdjEnd(a)
          ) yield ((p1,p2,p3), a, (q1,q2,q3)))
        )

        new LabeledGraph(e3)
      }

      val g3 = constructG3()
      val e3WithBack = g3.labeledEdges.map{
        case (v1,a,v2) => (v1,Some(a),v2)
      } ++ (for (
        p <- start;
        q <- end
      ) yield ((p,q,q), None, (p,p,q)))
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

    def generateWitness(ps: Seq[Pump]): Witness[A] = {
      var separators = IndexedSeq[Seq[A]]()
      var pumps = IndexedSeq[Seq[A]]()
      if (ps.nonEmpty) {
        separators :+= getPath(initialStates,ps.head._1).get
        pumps :+= ps.head._2
        if (ps.length > 1) {
          ps.sliding(2).foreach{ case Seq((_,_,q12),(q21,as,_)) =>
            separators :+= getPath(q12,q21).get
            pumps :+= as
          }
        }
        separators :+= Seq()
      }

      Witness(separators, pumps)
    }

    val (ambiguity, pumps) = calcAmbiguity(checkIDA)
    (ambiguity, generateWitness(pumps))
  }

  def calcAmbiguityWithTransition[R,P](ladfa: DFA[P,A])(implicit ev1: Q <:< (R,P), ev2: (R,P) <:< Q): (Option[Int], Witness[A]) = {
    val scPairLabeledAdjRP = scPairLabeledAdj.map{ case (scp, m) =>
      scp -> m.map{ case (a,es) =>
        a -> es.map{ case (v1,v2) =>
          (v1,v2): ((R,P),(R,P))
        }
      }.withDefaultValue(Seq())
    }.withDefaultValue(Map().withDefaultValue(Seq()))
    val laSet = scsGraph.nodes.map(sc => sc -> sc.map(_._2)).toMap

    def checkIDA(start: Set[Q], end: Set[Q]): Option[Pump] = {
      val startRP = start.map(v1 => v1: (R,P))
      val endRP = end.map(v1 => v1: (R,P))

      def constructG4(): LabeledGraph[((R,R,R),P), A] = {
        val labeledAdjStart = scPairLabeledAdjRP((start,start))
        val passage = reachableMapScsGraph(start) & reachableMapScsGraphRev(end)
        val labeledAdjPassage = for (sc1 <- passage; sc2 <- passage) yield scPairLabeledAdjRP((sc1,sc2))
        val labeledAdjEnd = scPairLabeledAdjRP((end,end))
        val e4 = (
          (for (
            a <- sigma.toSeq;
            ((r11,p11),(r12,p12)) <- labeledAdjStart(a);
            ((r21,p21),(r22,p22)) <- labeledAdjPassage.flatMap(_(a));
            ((r31,p31),(r32,p32)) <- labeledAdjEnd(a)
            if p11 == p21 && p21 == p31 && p12 == p22 && p22 == p32
          ) yield (((r11,r21,r31),p11), a, ((r12,r22,r32),p12)))

        )

        new LabeledGraph(e4)
      }

      if ((laSet(start) & laSet(end)).isEmpty) {
        None
      } else {
        val g4 = constructG4()
        val e4WithBack = g4.labeledEdges.map{
          case (v1,a,v2) => (v1,Some(a),v2)
        } ++
        (for (
          (r1,p1) <- startRP;
          (r2,p2) <- endRP
          if r1 != r2 && p1 == p2
        ) yield (((r1,r2,r2),p1), None, ((r1,r1,r2),p1)))
        val g4WithBack = new LabeledGraph(e4WithBack)

        g4WithBack.calcStrongComponents().toStream.map{ sc =>
          sc.collect{
            case ((r11,r12,r13),p1) if r12 == r13 && r11 != r12 => (r11,r12,p1)
          }.find{
            case (r1,r2,p) => sc.exists{ case ((r21,r22,r23),p2) =>
              r21 == r1 && r22 == r1 && r23 == r2 && p2 == p
            }
          }.map{
            case (r1,r2,p) => ((r1,p): Q, g4.getPath(((r1,r1,r2),p),((r1,r2,r2),p)).get, (r2,p): Q)
          }
        }.find(_.isDefined).map(_.get)
      }
    }

    def generateWitness(ps: Seq[Pump]): Witness[A] = {
      var separators = IndexedSeq[Seq[A]]()
      var pumps = IndexedSeq[Seq[A]]()
      if (ps.nonEmpty) {
        separators :+= getPath(initialStates,ps.head._1).get
        pumps :+= ps.head._2
        if (ps.length > 1) {
          ps.sliding(2).foreach{ case Seq((_,_,q12),(q21,as,_)) =>
            separators :+= getPath(q12,q21).get
            pumps :+= as
          }
        }
        separators :+= ladfa.getPath(ladfa.initialState, ps.last._3._2).get.reverse
      }

      Witness(separators, pumps)
    }

    val (ambiguity, pumps) = calcAmbiguity(checkIDA)
    (ambiguity, generateWitness(pumps))
  }
}


class DFA[Q,A](
  states: Set[Q],
  sigma: Set[A],
  val deltaDet: Map[(Q,A),Q],
  val initialState: Q,
  finalStates: Set[Q]
) extends NFA[Q,A](
  states,
  sigma,
  deltaDet.map{case ((v1,a),v2) => (v1,a,v2)}.toSeq,
  Set(initialState),
  finalStates
)
