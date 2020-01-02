package matching.monad

import org.scalatest._
import StateT._
import DMonad._

class StateTSpec extends FlatSpec with Matchers {
  "update" should "read current state" in {
    val t = StateTDTreeMonad.update[Int](identity) `>>=r` (b => StateTDTreeMonad[Int,String](if(b) "t" else "f"))
    t(true) should be (DLeaf(("t", true)))
  }

  it should "update state" in {
    val t = StateTDTreeMonad.update[Int](_ => false) `>>=r` (_ => StateTDTreeMonad[Int,String]("bb"))
    t(true) should be (DLeaf(("bb", false)))
  }
}
