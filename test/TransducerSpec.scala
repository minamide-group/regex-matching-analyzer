package matching.transition

import org.scalatest._
import matching.monad._

class TransducerSpec extends FlatSpec with Matchers {
  "toNFA" should "construct NFA" in {
    val t = new DetTransducer[Int,Char](
      Set(1,2,3),
      Set('a','b'),
      1,
      Map(
        (1, Some('a')) -> Or(Leaf(1),Leaf(2)),
        (1, Some('b')) -> Or(Leaf(1),Fail),
        (1, None) -> Fail,
        (2, Some('a')) -> Or(Leaf(2),Or(Fail,Leaf(3))),
        (2, Some('b')) -> Or(Or(Leaf(1),Leaf(1)), Success),
        (2, None) -> Or(Fail,Fail),
        (3, Some('a')) -> Fail,
        (3, Some('b')) -> Or(Leaf(1),Or(Leaf(2),Leaf(3))),
        (3, None) -> Or(Success,Fail)
      )
    )

    val nfa = t.toNFA()

    nfa.states should be (Set(Some(1),Some(2),Some(3),None))
    nfa.sigma should be (Set('a','b'))
    nfa.initialStates should be (Set(Some(1)))
    nfa.finalStates should be (Set(Some(3),None))
    nfa.delta should contain theSameElementsAs (Seq(
      (Some(1),'a',Some(1)),
      (Some(1),'a',Some(2)),
      (Some(1),'b',Some(1)),
      (Some(2),'a',Some(2)),
      (Some(2),'a',Some(3)),
      (Some(2),'b',Some(1)),
      (Some(2),'b',Some(1)),
      (Some(2),'b',None),
      (Some(3),'b',Some(1)),
      (Some(3),'b',Some(2)),
      (Some(3),'b',Some(3)),
      (None,'a',None),
      (None,'b',None)
    ))
  }
}
