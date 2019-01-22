package matching.monad

import org.scalatest._

import Monad._

class TreeSpec extends FlatSpec with Matchers {
  "bind" should "calculate bind" in {
    val t: Tree[Int] = Or(Leaf(1), Or(Fail, Leaf(2)))
    (t >>= ((i: Int) => Or(Leaf(i), Leaf(i*2)))) should be (
      Or(Or(Leaf(1), Leaf(2)), Or(Fail, Or(Leaf(2), Leaf(4))))
    )
  }

  "flat" should "calculate list of leaf from left to right" in {
    val t: Tree[Int] = Or(Leaf(1), Or(Or(Leaf(2), Fail), Leaf(3)))
    t.flat should be (Seq(1,2,3))
  }

  "cuts" should "calculate cut trees" in {
    val t1: Tree[Int] = Or(Leaf(1), Or(Leaf(2), Leaf(3)))
    t1.cuts should contain theSameElementsAs Seq(
      Or(Leaf(1), Or(Leaf(2), Leaf(3))),
      Or(Leaf(1), Lft(Leaf(2))),
      Lft(Leaf(1))
    )

    val t2: Tree[Int] = Or(Fail, Or(Leaf(1), Leaf(2)))
    t2.cuts should contain theSameElementsAs Seq(
      Or(Fail, Or(Leaf(1), Leaf(2))),
      Or(Fail, Lft(Leaf(1)))
    )

    val t3: Tree[Int] = Or(Or(Leaf(1), Leaf(2)), Fail)
    t3.cuts should contain theSameElementsAs Seq(
      Lft(Or(Leaf(1), Leaf(2))),
      Lft(Lft(Leaf(1)))
    )

    val t4: Tree[Int] = Or(Fail, Or(Fail, Fail))
    t4.cuts should be (empty)

    val t5: Tree[Int] = Or(Or(Leaf(1), Fail), Or(Or(Fail, Leaf(2)), Leaf(3)))
    t5.cuts should contain theSameElementsAs Seq(
      Or(Or(Leaf(1), Fail), Or(Or(Fail, Leaf(2)), Leaf(3))),
      Or(Or(Leaf(1), Fail), Lft(Or(Fail, Leaf(2)))),
      Lft(Lft(Leaf(1)))
    )
  }
}
