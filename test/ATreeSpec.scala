package matching.monad

import org.scalatest._

import AMonad._
import ATree._
import ATree.ATreeMonad._

class ATreeSpec extends FlatSpec with Matchers {
  ">>=l" should "calculate left bind" in {
    val t: ATree[Char,Int] = AOr(ALeaf(1), AAssert(AOr(AFail(), ALeaf('a')), ALeaf(2)))
    (t `>>=l` (c => AOr(ALeaf(c.toString), ALeaf(c.toUpper.toString)): ATree[String, String])) should be (
      AOr(ALeaf(1), AAssert(AOr(AFail(), AOr(ALeaf("a"), ALeaf("A"))), ALeaf(2))): ATree[String, Int]
    )
  }

  ">>=r" should "calculate right bind" in {
    val t: ATree[Char,Int] = AOr(ALeaf(1), AAssert(AOr(AFail(), ALeaf('a')), ALeaf(2)))
    (t `>>=r` (i => AAssertNot(ALeaf('b'), ALeaf(i*2L)): ATree[Char, Long])) should be (
      AOr(
        AAssertNot(ALeaf('b'), ALeaf(2L)),
        AAssert(AOr(AFail(), ALeaf('a')), AAssertNot(ALeaf('b'), ALeaf(4L)))
      ): ATree[Char, Long]
    )
  }

  "leaves" should "collect leaves from the given tree" in {
    val t: ATree[Int,Int] = AOr(ALeaf(1), AOr(
      AAssert(AOr(AFail(), ASuccess()), AOr(ALeaf(2), ASuccess())),
      AAssertNot(ASuccess(), ALeaf(3)))
    )
    leaves(t) should be (Seq(1,2,3))
  }

  "eval" should "evaluate the given tree" in {
    val t: ATree[Int,Int] = AOr(ALeaf(10), AAssert(AAssertNot(AFail(), ALeaf(7)), ALeaf(3)))
    eval(t)(_ < 10) should be (true)
    eval(t)(_ < 5) should be (false)
  }
}
