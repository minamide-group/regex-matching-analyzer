package matching.monad

import org.scalatest._
import StateT._
import DMonad._

class StateTSpec extends FlatSpec with Matchers {
  "update" should "read current state" in {
    val t = StateTDTreeMonad.update[Int](identity) `>>=r` ((s: String) => StateTDTreeMonad[Int,String](s * 2))
    t("aa") should be (DLeaf(("aaaa", "aa")))
  }

  it should "update state" in {
    val t = StateTDTreeMonad.update[Int](_ * 2) `>>=r` ((s: String) => StateTDTreeMonad[Int,String]("bb"))
    t("aa") should be (DLeaf(("bb", "aaaa")))
  }
}
