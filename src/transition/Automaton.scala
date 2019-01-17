package transition

import scala.collection.mutable.Stack

class NFA[Q,A](
  states: Set[Q],
  val sigma: Set[A],
  val delta: Set[(Q,A,Q)],
  val initialStates: Set[Q],
  val finalStates: Set[Q]
) {
  var deltaMap = Map[(Q,A),Set[Q]]().withDefaultValue(Set())
  delta.foreach{case (q1,a,q2) => deltaMap += (q1,a) -> (deltaMap((q1,a)) + q2)}
  def deltaHat(qs: Set[Q], a: A): Set[Q] = qs.flatMap(q => deltaMap((q,a)))
  def deltaHat(qs: Set[Q], as: Seq[A]): Set[Q] = as.foldLeft(qs)(deltaHat(_,_))

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
