package matching.monad

import org.scalatest._
import StateT._
import DMonad._

class StateTSpec extends FlatSpec with Matchers {
  "update" should "read current state" in {
    val t = StateTDTreeMonad.update[Int](identity) `>>=r` (s => StateTDTreeMonad[Int,String](s.map(_.get).mkString * 2))
    t(Vector(Some('a'), Some('b'))) should be (DLeaf(("abab", Vector(Some('a'), Some('b')))))
  }

  it should "update state" in {
    val t = StateTDTreeMonad.update[Int](_ :+ None) `>>=r` (_ => StateTDTreeMonad[Int,String]("bb"))
    t(Vector(Some('a'), Some('b'))) should be (DLeaf(("bb", Vector(Some('a'), Some('b'), None))))
  }
}
