package matching.monad

import org.scalatest._
import StateT._
import AMonad._

class StateTSpec extends FlatSpec with Matchers {
  "update" should "read current state" in {
    val t = StateTATreeMonad.update[Int](identity) `>>=r` (b => StateTATreeMonad[Int,String](if(b) "t" else "f"))
    t(true) should be (ALeaf(("t", true)))
  }

  it should "update state" in {
    val t = StateTATreeMonad.update[Int](_ => false) `>>=r` (_ => StateTATreeMonad[Int,String]("bb"))
    t(true) should be (ALeaf(("bb", false)))
  }
}
