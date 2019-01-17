import org.scalatest._

import transition._

class AutomatonSpec extends FlatSpec with Matchers {
  "deltaHat in NFA" should "calculate transition" in {
    val nfa = new NFA[Int,Char](
      Set(1,2,3,4),
      Set('a','b'),
      Set(
        (1,'a',2),
        (1,'a',3),
        (2,'b',3),
        (3,'a',3),
        (3,'b',4),
        (4,'b',4)
      ),
      Set(1),
      Set(4)
    )

    nfa.deltaHat(Set(1),"ab".toSeq) should contain only (3,4)
    nfa.deltaHat(Set(2,3),"bab".toSeq) should contain only (4)
  }

  "toDFA" should "construct subset DFA" in {
    val nfa = new NFA[Int,Char](
      Set(1,2,3,4,5),
      Set('a','b'),
      Set(
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
    dfa.states should contain only (
      Set(1,2),Set(3,4),Set(5),Set(2,4),Set(),Set(2,3,5),Set(2,4,5),Set(2,3)
    )
    dfa.sigma should be (nfa.sigma)
    dfa.delta should contain only (
      (Set(1,2),'a') -> Set(3,4),
      (Set(1,2),'b') -> Set(2,3),
      (Set(3,4),'a') -> Set(5),
      (Set(3,4),'b') -> Set(5),
      (Set(5),'a') -> Set(2,4),
      (Set(5),'b') -> Set(5),
      (Set(2,4),'a') -> Set(),
      (Set(2,4),'b') -> Set(2,3,5),
      (Set(),'a') -> Set(),
      (Set(),'b') -> Set(),
      (Set(2,3,5),'a') -> Set(2,4,5),
      (Set(2,3,5),'b') -> Set(2,3,5),
      (Set(2,4,5),'a') -> Set(2,4),
      (Set(2,4,5),'b') -> Set(2,3,5),
      (Set(2,3),'a') -> Set(5),
      (Set(2,3),'b') -> Set(2,3,5)
    )
    dfa.initialState should be (nfa.initialStates)
    dfa.finalStates should contain only (
      Set(3,4),Set(5),Set(2,4),Set(2,3,5),Set(2,4,5)
    )
  }

  "deltaHat in DFA" should "calculate transition" in {
    val dfa = new DFA[Int,Char](
      Set(1,2,3),
      Set('a','b'),
      Map(
        (1,'a') -> 2,
        (1,'b') -> 1,
        (2,'a') -> 2,
        (2,'b') -> 3,
        (3,'a') -> 2,
        (3,'b') -> 3,
      ),
      1,
      Set(3)
    )

    dfa.deltaHat(1,"abab".toSeq) should be (3)
    dfa.deltaHat(2,"baa".toSeq) should be (2)
  }
}
