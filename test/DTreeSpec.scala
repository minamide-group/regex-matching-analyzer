package matching.monad

import org.scalatest._

import EPAMonad._
import DTree._

class DTreeSpec extends FlatSpec with Matchers {
  "bind" should "calculate bind" in {
    val t: DTree[Char,Int] = DOr(DLeaf(1), DAssert(DOr(DFail, DLeaf('a')), DLeaf(2)))
    (t >>= (c => DOr(DLeaf(c), DLeaf(c.toUpper)), i => DAssert(DLeaf('z'), DLeaf(i*2)))) should be (
      DOr(DAssert(DLeaf('z'), DLeaf(2)), DAssert(DOr(DFail, DOr(DLeaf('a'), DLeaf('A'))), DAssert(DLeaf('z'), DLeaf(4))))
    )
  }

  "toSet" should "convert the given tree to set" in {
    val t: DTree[Nothing,Int] = DOr(DLeaf(1), DOr(
      DAssert(DOr(DFail, DSuccess), DOr(DLeaf(2), DSuccess)),
      DAssertNot(DSuccess, DLeaf(3)))
    )
    DTreeMonad.toSet(t) should be (Set(Some(1),Some(2),None))
  }

  "eval" should "evaluate the given tree" in {
    val t1: DTree[Nothing,Nothing] = DOr(DSuccess, DAssert(DSuccess, DSuccess))
    DTreeMonad.eval(t1) should be (true)

    val t2: DTree[Nothing,Nothing] = DOr(DFail, DAssertNot(DOr(DFail, DSuccess), DSuccess))
    DTreeMonad.eval(t2) should be (false)
  }
}
