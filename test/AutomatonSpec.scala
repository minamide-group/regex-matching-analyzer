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

  "reachablePart" should "construct NFA which only contains reachable states from initial states" in {
    val nfa = new NFA[Int,Char](
      Set(1,2,3,4,5,6,7),
      Set('a','b'),
      Seq(
        (1,'a',2),
        (1,'a',3),
        (2,'b',3),
        (2,'a',4),
        (3,'b',6),
        (4,'a',4),
        (5,'b',7)
      ),
      Set(1),
      Set(6,7)
    )

    val reachablePartNFA = nfa.reachablePart()
    reachablePartNFA.states should be (Set(1,2,3,4,6))
    reachablePartNFA.sigma should be (nfa.sigma)
    reachablePartNFA.delta should contain only (
      (1,'a',2),
      (1,'a',3),
      (2,'b',3),
      (2,'a',4),
      (3,'b',6),
      (4,'a',4),
    )
    reachablePartNFA.initialStates should be (Set(1))
    reachablePartNFA.finalStates should be (Set(6))
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


  "hasLoop" should "decide whether it has loop or not" in {
    val nfa1 = new NFA[Int,Char](
      Set(1,2,3),
      Set('a','b'),
      Seq(
        (1,'a',2),
        (2,'b',2),
        (2,'a',3)
      ),
      Set(1),
      Set(3)
    )

    val nfa2 = new NFA[Int,Char](
      Set(1,2,3),
      Set('a','b'),
      Seq(
        (1,'a',2),
        (2,'b',3),
        (3,'a',1)
      ),
      Set(1),
      Set(3)
    )

    val nfa3 = new NFA[Int,Char](
      Set(1,2,3,4),
      Set('a','b'),
      Seq(
        (1,'a',2),
        (1,'b',3),
        (2,'a',3),
        (2,'a',4),
        (3,'b',4)
      ),
      Set(1),
      Set(3,4)
    )

    nfa1.hasLoop() should be (true)
    nfa2.hasLoop() should be (true)
    nfa3.hasLoop() should be (false)
  }


  "calcAmbiguity" should "decide finitely ambiguous graph" in {
    val g1 = new NFA(
      Set(1,2),
      Set('a','b'),
      Seq(
        (1,'a',1),
        (1,'b',2),
        (2,'a',2)
      ),
      Set(1),
      Set(2)
    )

    val g2 = new NFA(
      Set(1,2,3,4),
      Set('a','b','c'),
      Seq(
        (1,'a',2),
        (2,'b',3),
        (3,'c',4)
      ),
      Set(1),
      Set(4)
    )

    val g3 = new NFA(
      Set(1,2,3,4),
      Set('a','b','c'),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'b',1),
        (2,'b',3),
        (2,'b',3),
        (3,'c',4),
        (4,'b',3)
      ),
      Set(1),
      Set(4)
    )

    g1.calcAmbiguity() should be (Some(0))
    g2.calcAmbiguity() should be (Some(0))
    g3.calcAmbiguity() should be (Some(0))
  }

  it should "decide polynomially ambiguous graph" in {
    val g11 = new NFA(
      Set(1,2),
      Set('a'),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'a',2)
      ),
      Set(1),
      Set(2)
    )

    val g12 = new NFA(
      Set(1,2,3,4),
      Set('a','b'),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'b',1),
        (2,'b',3),
        (3,'a',4),
        (4,'b',3)
      ),
      Set(1),
      Set(4)
    )

    val g21 = new NFA(
      Set(1,2,3,4),
      Set('a','b','c'),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'a',2),
        (2,'b',3),
        (3,'c',3),
        (3,'c',4),
        (4,'c',4)
      ),
      Set(1),
      Set(4)
    )

    val g22 = new NFA(
      Set(1,2,3,4,5,6),
      Set('a','x','y','z'),
      Seq(
        (1,'x',2),
        (2,'y',1),
        (2,'a',2),
        (2,'a',3),
        (3,'a',3),
        (2,'y',4),
        (4,'x',5),
        (5,'y',4),
        (4,'z',4),
        (4,'z',6),
        (6,'z',6)
      ),
      Set(1),
      Set(6)
    )

    val g3 = new NFA(
      Set(1,2,3,4),
      Set('a','b','c'),
      Seq(
        (1,'a',1),
        (1,'a',2),
        (2,'a',2),
        (2,'b',2),
        (2,'b',3),
        (3,'b',3),
        (3,'c',3),
        (3,'c',4),
        (4,'c',4)
      ),
      Set(1),
      Set(4)
    )

    g11.calcAmbiguity() should be (Some(1))
    g12.calcAmbiguity() should be (Some(1))
    g21.calcAmbiguity() should be (Some(2))
    g22.calcAmbiguity() should be (Some(2))
    g3.calcAmbiguity() should be (Some(3))
  }

  it should "decide exponentially ambiguous graph" in {
    val g1 = new NFA(
      Set(1,2,3),
      Set('a'),
      Seq(
        (1,'a',2),
        (1,'a',3),
        (2,'a',1),
        (3,'a',1)
      ),
      Set(1),
      Set(1)
    )

    val g2 = new NFA(
      Set(1,2,3),
      Set('a','b','c'),
      Seq(
        (1,'a',1),
        (1,'b',2),
        (1,'a',3),
        (2,'c',1),
        (3,'b',3),
        (3,'c',1)
      ),
      Set(1),
      Set(1)
    )

    val g3 = new NFA(
      Set(1,2,3,4,5,6),
      Set('a','b','c','d','e'),
      Seq(
        (1,'a',2),
        (2,'b',3),
        (3,'c',2),
        (2,'b',4),
        (4,'c',5),
        (5,'b',3),
        (2,'d',6),
        (6,'e',6)
      ),
      Set(1),
      Set(6)
    )

    val g4 = new NFA(
      Set(1,2,3),
      Set('a','b'),
      Seq(
        (1,'a',2),
        (2,'b',3),
        (2,'b',3),
        (3,'a',2)
      ),
      Set(1),
      Set(3)
    )

    val g5 = new NFA(
      Set(1,2,3),
      Set('a','b','c'),
      Seq(
        (1,'a',2),
        (2,'b',3),
        (3,'c',3),
        (3,'c',3)
      ),
      Set(1),
      Set(3)
    )

    g1.calcAmbiguity() should be (None)
    g2.calcAmbiguity() should be (None)
    g3.calcAmbiguity() should be (None)
    g4.calcAmbiguity() should be (None)
    g5.calcAmbiguity() should be (None)
  }
}
