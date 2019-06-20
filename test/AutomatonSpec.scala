package matching.transition

import org.scalatest._

class AutomatonSpec extends FlatSpec with Matchers {
  "reverse" should "construct reverse NFA" in {
      val nfa = new NFA[Int,Char](
        Set(1,2,3,4),
        Set('a','b'),
        Seq(
          (1,'a',1),
          (1,'a',2),
          (2,'b',1),
          (2,'a',3),
          (3,'b',4),
          (4,'b',4)
        ),
        Set(1),
        Set(3,4)
      )

      val nfaRev = nfa.reverse()
      nfaRev.states should be (nfa.states)
      nfaRev.sigma should be (nfa.sigma)
      nfaRev.delta should contain only (
        (1,'a',1),
        (2,'a',1),
        (1,'b',2),
        (3,'a',2),
        (4,'b',3),
        (4,'b',4)
      )
      nfaRev.initialStates should be (nfa.finalStates)
      nfaRev.finalStates should be (nfa.initialStates)
  }

  "toDFA" should "construct subset DFA" in {
    val nfa = new NFA[Int,Char](
      Set(1,2,3,4,5),
      Set('a','b'),
      Seq(
        (1,'a',3),
        (1,'a',4),
        (2,'b',2),
        (2,'b',3),
        (3,'a',5),
        (3,'b',5),
        (4,'b',5),
        (5,'a',2),
        (5,'a',4),
        (5,'b',5)
      ),
      Set(1,2),
      Set(4,5)
    )

    val dfa = nfa.toDFA()
    dfa.states should be (
      Set(Set(1,2),Set(3,4),Set(5),Set(2,4),Set(),Set(2,3,5),Set(2,4,5),Set(2,3))
    )
    dfa.sigma should be (nfa.sigma)
    dfa.delta should contain only (
      (Set(1,2),'a',Set(3,4)),
      (Set(1,2),'b',Set(2,3)),
      (Set(3,4),'a',Set(5)),
      (Set(3,4),'b',Set(5)),
      (Set(5),'a',Set(2,4)),
      (Set(5),'b',Set(5)),
      (Set(2,4),'a',Set()),
      (Set(2,4),'b',Set(2,3,5)),
      (Set(),'a',Set()),
      (Set(),'b',Set()),
      (Set(2,3,5),'a',Set(2,4,5)),
      (Set(2,3,5),'b',Set(2,3,5)),
      (Set(2,4,5),'a',Set(2,4)),
      (Set(2,4,5),'b',Set(2,3,5)),
      (Set(2,3),'a',Set(5)),
      (Set(2,3),'b',Set(2,3,5))
    )
    dfa.initialState should be (Set(1,2))
    dfa.finalStates should be (
      Set(Set(3,4),Set(5),Set(2,4),Set(2,3,5),Set(2,4,5))
    )
  }
}
