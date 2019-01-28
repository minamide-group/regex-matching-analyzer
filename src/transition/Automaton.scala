package matching.transition

import scala.collection.mutable.Stack
import matching.tool.{File, Debug}

class NFA[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val delta: Seq[(Q,A,Q)],
  val initialStates: Set[Q],
  val finalStates: Set[Q]
) extends Graph[Q](states.toSeq, delta.map{case (v1,_,v2) => (v1,v2)}.toSeq) {
  lazy val scsGraph = {
    val scs = calcStrongComponents()
    val scsEdges = edges.map{ case (v1,v2) =>
      (scs.find(_.contains(v1)).get, scs.find(_.contains(v2)).get)
    }.filter{case (v1,v2) => v1 != v2}.distinct
    new Graph(scs.toSeq, scsEdges)
  }

  override def reverse(): NFA[Q,A] = {
    new NFA(states, sigma, delta.map{case (q1,a,q2) => (q2,a,q1)}, finalStates, initialStates)
  }

  override protected def visualizeNodes(file: File, renameMap: Map[Q,Int]) {
    file.writeln("\"initial\" [", 1)
    file.writeln("label = \"\",", 2)
    file.writeln("shape = none,", 2)
    file.writeln("fixedsize = true,", 2)
    file.writeln("width = 0,", 2)
    file.writeln("height = 0", 2)
    file.writeln("];", 1)

    states.foreach{ state =>
      if (finalStates.contains(state)) {
        file.writeln(s""""${renameMap(state)}" [label = "${state}", shape = doublecircle];""", 1)
      } else {
        file.writeln(s""""${renameMap(state)}" [label = "${state}"];""", 1)
      }
    }
  }

  override protected def visualizeEdges(file: File, renameMap: Map[Q,Int]) {
    initialStates.foreach{ initialState =>
      file.writeln(s""""initial" -> "${renameMap(initialState)}";""", 1)
    }

    delta.foreach{ case (q1,a,q2) =>
      file.writeln(s""""${renameMap(q1)}" -> "${renameMap(q2)}" [label = "${a}"];""", 1)
    }
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
    var deltaMap = Map[(Q,A),Set[Q]]().withDefaultValue(Set())
    delta.foreach{case (q1,a,q2) => deltaMap += (q1,a) -> (deltaMap((q1,a)) + q2)}
    def deltaHat(qs: Set[Q], a: A): Set[Q] = qs.flatMap(q => deltaMap((q,a)))

    val newInitial = initialStates
    var newStates = Set(initialStates)
    val stack = Stack(newInitial)
    var newDelta = Map[(Set[Q],A),Set[Q]]()

    while (stack.nonEmpty) {
      val qs = stack.pop
      sigma.foreach{ a =>
        val next = deltaHat(qs,a)
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

  private def calcAmbiguity(shouldCheck: (Set[Q], Set[Q]) => Boolean): Option[Int] = {
    Debug.info("NFA info") (
      ("number of states", nodes.size),
      ("number of transitions", delta.size),
      ("number of alphabets", sigma.size),
      ("number of strong components", scsGraph.nodes.size)
    )

    val scPair2Label2Edges = delta.groupBy{ case (q1,a,q2) =>
      (scsGraph.nodes.find(_.contains(q1)).get, scsGraph.nodes.find(_.contains(q2)).get)
    }.mapValues(
      _.groupBy(_._2).mapValues(
        _.map{case (v1,_,v2) => (v1,v2)}
      ).withDefaultValue(Seq())
    ).withDefaultValue(Map().withDefaultValue(Seq()))

    def isEDA(): Boolean = {
      def isEDA(sc: Set[Q]): Boolean = {
        val label2EdgesSc = scPair2Label2Edges((sc,sc))

        def constructG2(sc: Set[Q]): Graph[(Q,Q)] = {
          val g2Edges = label2EdgesSc.toSeq.flatMap{ case (a,es) =>
            for ((p1,q1) <- es; (p2,q2) <- es) yield ((p1,p2), (q1,q2))
          }.distinct
          new Graph(g2Edges)
        }

        label2EdgesSc.exists{case (_,es) => es.length != es.distinct.length} ||
        (constructG2(sc).calcStrongComponents().find(_.exists{case (v1,v2) => v1 == v2}) match {
          case Some(g2sc) => g2sc.exists{case (v1,v2) => v1 != v2}
          case None => false
        })
      }

      scsGraph.nodes.exists(isEDA)
    }

    def calcDegree(): Int = {
      val scsGraphRev = scsGraph.reverse()

      var degree = Map[Set[Q], Int]()
      def calcDegree(sc: Set[Q]): Int = {
        def isIDA(start: Set[Q], passage: Set[Set[Q]], end: Set[Q]): Boolean = {
          def constructG4(): Graph[(Q,Q,Q)] = {
            val label2EdgesStart = scPair2Label2Edges((start,start))
            val label2EdgesPassage = for (sc1 <- passage; sc2 <- passage) yield scPair2Label2Edges((sc1,sc2))
            val label2EdgesEnd = scPair2Label2Edges((end,end))
            val e4 = (
              (for (
                a <- sigma.toSeq;
                (p1,q1) <- label2EdgesStart(a);
                (p2,q2) <- label2EdgesPassage.flatMap(_(a));
                (p3,q3) <- label2EdgesEnd(a)
              ) yield ((p1,p2,p3),(q1,q2,q3))) ++
              start.flatMap(p =>
                end.collect{case q if p != q => ((p,q,q),(p,p,q))}
              )
            ).distinct

            new Graph(e4)
          }

          shouldCheck(start,end) && {
            val g4 = Debug.time("construct G4", false) {
              constructG4()
            }

            g4.calcStrongComponents().exists{sc =>
              sc.collect{
                case (p1,p2,p3) if p2 == p3 && p1 != p2 => (p1,p2)
              }.exists{
                case (p,q) => sc.exists{case (q1,q2,q3) => q1 == p && q2 == p && q3 == q}
              }
            }
          }
        }

        degree.get(sc) match {
          case Some(d) => d
          case None =>
            val children = scsGraph.adj(sc)
            val degreeSc = if (children.isEmpty) 0
            else {
              val maxDegree = children.map(calcDegree).max
              val reachableFromSc = scsGraph.reachableFrom(sc)
              val maxDegreeScs = reachableFromSc.filter(
                degree.get(_) == Some(maxDegree)
              )
              if (maxDegreeScs.exists{ maxDegreeSc =>
                val passage = reachableFromSc & scsGraphRev.reachableFrom(maxDegreeSc)
                isIDA(sc, passage, maxDegreeSc)
              }) maxDegree + 1 else maxDegree
            }

            degree += sc -> degreeSc
            degreeSc
        }
      }

      (0 +: scsGraphRev.nodes.filter(scsGraphRev.adj(_).isEmpty).map(calcDegree)).max
    }

    if (isEDA()) None else Some(calcDegree())
  }

  def calcAmbiguity(): Option[Int] = {
    calcAmbiguity((_,_) => true)
  }

  def calcAmbiguityWithTransition[R,P]()(implicit ev: Q <:< (R,P)): Option[Int] = {
    val laSet = scsGraph.nodes.map(sc => sc -> sc.map(_._2)).toMap
    calcAmbiguity((start,end) => (laSet(start) & laSet(end)).nonEmpty)
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
