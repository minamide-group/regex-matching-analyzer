package matching.monad

import org.scalatest._

import DMonad._
import DTree._
import DTree.DTreeMonad._

class DTreeSpec extends FlatSpec with Matchers {
  ">>=l" should "calculate left bind" in {
    val t: DTree[Char,Int] = DOr(DLeaf(1), DAssert(DOr(DFail(), DLeaf('a')), DLeaf(2)))
    (t `>>=l` (c => DOr(DLeaf(c.toString), DLeaf(c.toUpper.toString)): DTree[String, String])) should be (
      DOr(DLeaf(1), DAssert(DOr(DFail(), DOr(DLeaf("a"), DLeaf("A"))), DLeaf(2))): DTree[String, Int]
    )
  }

  ">>=r" should "calculate right bind" in {
    val t: DTree[Char,Int] = DOr(DLeaf(1), DAssert(DOr(DFail(), DLeaf('a')), DLeaf(2)))
    (t `>>=r` (i => DAssertNot(DLeaf('b'), DLeaf(i*2L)): DTree[Char, Long])) should be (
      DOr(
        DAssertNot(DLeaf('b'), DLeaf(2L)),
        DAssert(DOr(DFail(), DLeaf('a')), DAssertNot(DLeaf('b'), DLeaf(4L)))
      ): DTree[Char, Long]
    )
  }

  "leaves" should "collect leaves from the given tree" in {
    val t: DTree[Int,Int] = DOr(DLeaf(1), DOr(
      DAssert(DOr(DFail(), DSuccess()), DOr(DLeaf(2), DSuccess())),
      DAssertNot(DSuccess(), DLeaf(3)))
    )
    leaves(t) should be (Set(1,2,3))
  }

  "eval" should "evaluate the given tree" in {
    val t: DTree[Int,Int] = DOr(DLeaf(10), DAssert(DAssertNot(DFail(), DLeaf(7)), DLeaf(3)))
    eval(t)(_ < 10) should be (true)
    eval(t)(_ < 5) should be (false)
  }
}
