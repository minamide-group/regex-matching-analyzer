package matching.transition

import scala.collection.mutable.Stack
import matching.tool.Debug

class NFA[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val delta: Set[(Q,A,Q)],
  val initialStates: Set[Q],
  val finalStates: Set[Q]
) extends Graph[Q](states.toSeq, delta.map{case (v1,_,v2) => (v1,v2)}.toSeq) {
  var deltaMap = Map[(Q,A),Set[Q]]().withDefaultValue(Set())
  delta.foreach{case (q1,a,q2) => deltaMap += (q1,a) -> (deltaMap((q1,a)) + q2)}
  def deltaHat(qs: Set[Q], a: A): Set[Q] = qs.flatMap(q => deltaMap((q,a)))
  def deltaHat(qs: Set[Q], as: Seq[A]): Set[Q] = as.foldLeft(qs)(deltaHat(_,_))

  override def reverse(): NFA[Q,A] = {
    new NFA(states, sigma, delta.map{case (q1,a,q2) => (q2,a,q1)}, finalStates, initialStates)
  }

  def toDFA(): DFA[Set[Q],A] = {
    val newInitial = initialStates
    var newStates = Set(newInitial)
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

  def calcAmbiguity(): Option[Int] = {
    val label2Edges = delta.groupBy(_._2).mapValues(_.map{case (v1,_,v2) => (v1,v2)})

    val scs = calcStrongComponents()
    val scsEdges = edges.map{ case (v1,v2) =>
      (scs.find(_.contains(v1)).get, scs.find(_.contains(v2)).get)
    }.filter{case (v1,v2) => v1 != v2}
    val scsGraph = new Graph(scs.toSeq, scsEdges)

    def isEDA(): Boolean = {
      def constructG2(sc: Set[Q]): Graph[(Q,Q)] = {
        val label2EdgesSc = label2Edges.mapValues(_.filter{ case (v1,v2) =>
          sc.contains(v1) && sc.contains(v2)
        })
        val g2Edges = label2EdgesSc.toSeq.flatMap{ case (a,es) =>
          for ((p1,q1) <- es; (p2,q2) <- es) yield ((p1,p2), (q1,q2))
        }.distinct
        new Graph(g2Edges)
      }

      scsGraph.nodes.exists{ sc =>
        constructG2(sc).calcStrongComponents().find(_.exists{case (v1,v2) => v1 == v2}) match {
          case Some(g2sc) => g2sc.exists{case (v1,v2) => v1 != v2}
          case None => false
        }
      }
    }

    def calcDegree(): Int = {
      val scsGraphRev = scsGraph.reverse()

      var degree = Map[Set[Q], Int]()
      def calcDegree(sc: Set[Q]): Int = {
        def isIDA(start: Set[Q], passage: Set[Q], end: Set[Q]): Boolean = {
          def constructG4(): Graph[(Q,Q,Q)] = {
            val label2EdgesStart = label2Edges.mapValues(_.filter{
              case (v1,v2) => start.contains(v1) && start.contains(v2)
            })
            val label2EdgesPassage = label2Edges.mapValues(_.filter{
              case (v1,v2) => passage.contains(v1) && passage.contains(v2)
            })
            val label2EdgesEnd = label2Edges.mapValues(_.filter{
              case (v1,v2) => end.contains(v1) && end.contains(v2)
            })
            val e4 = (
              (for (
                (a,e2s) <- label2EdgesPassage.toSeq;
                (p1,q1) <- label2EdgesStart(a);
                (p2,q2) <- e2s;
                (p3,q3) <- label2EdgesEnd(a)
              ) yield ((p1,p2,p3),(q1,q2,q3))) ++
              start.flatMap(p =>
                end.collect{case q if p != q => ((p,q,q),(p,p,q))}
              )
            ).distinct

            new Graph(e4)
          }

          val g4 = Debug.time("construct G4") {
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
                val passage = (reachableFromSc & scsGraphRev.reachableFrom(maxDegreeSc)).flatten
                isIDA(sc, passage, maxDegreeSc)
              }) maxDegree + 1 else maxDegree
            }

            degree += sc -> degreeSc
            degreeSc
        }
      }

      scsGraphRev.nodes.filter(scsGraphRev.adj(_).isEmpty).map(calcDegree).max
    }

    if (isEDA()) None else Some(calcDegree())
  }
}

class DFA[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val delta: Map[(Q,A),Q],
  val initialState: Q,
  val finalStates: Set[Q]
) {
  def deltaHat(q: Q, as: Seq[A]): Q = as.foldLeft(q)((q,a) => delta((q,a)))
}
