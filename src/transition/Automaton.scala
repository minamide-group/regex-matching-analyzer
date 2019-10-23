package matching.transition

import collection.mutable.Stack
import matching.tool.Analysis

class NFA[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val delta: Seq[(Q,A,Q)],
  val initialStates: Set[Q],
  val finalStates: Set[Q]
) extends LabeledGraph[Q,A](states, delta) {
  override def reverse(): NFA[Q,A] = {
    new NFA(states, sigma, delta.map{case (q1,a,q2) => (q2,a,q1)}, finalStates, initialStates)
  }

  lazy val deltaSet = delta.groupBy{case (q1,a,_) => (q1,a)}.mapValues(
    _.map(_._3)
  ).withDefaultValue(Seq[Q]())

  def deltaHat(qs: Set[Q], a: A): Set[Q] = {
    qs.flatMap(q => deltaSet((q,a)))
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
  deltaDet.map{ case ((v1,a),v2) =>
    Analysis.checkInterrupted("construct DFA")
    (v1,a,v2)
  }.toSeq,
  Set(initialState),
  finalStates
)
