package matching.monad

import EPAMonad._

sealed trait DTree[+A,+B]
case class DLeaf[A,B](b: B) extends DTree[A,B]
case object DSuccess extends DTree[Nothing,Nothing]
case object DFail extends DTree[Nothing,Nothing]
case class DOr[A,B](l: DTree[A,B], r: DTree[A,B]) extends DTree[A,B]
case class DAssert[A,B](l: DTree[A,A], r: DTree[A,B]) extends DTree[A,B]
case class DAssertNot[A,B](l: DTree[A,A], r: DTree[A,B]) extends DTree[A,B]
case class DLft[A,B](l: DTree[A,B]) extends DTree[A,B]

object DTree {
  implicit object DTreeMonad extends EPAMonad[DTree] {
    def unit[A,B](b: B) = DLeaf(b)
    def bind[A,B,C,D](m: DTree[A,B], f: A => DTree[C,C], g: B => DTree[C,D]) = {
      m match {
        case DLeaf(b) => g(b)
        case DSuccess => DSuccess
        case DFail => DFail
        case DOr(l,r) => DOr(l >>= (f,g), r >>= (f,g))
        case DAssert(l,r) => DAssert(l >>= (f,f), r >>= (f,g))
        case DAssertNot(l,r) => DAssertNot(l >>= (f,f), r >>= (f,g))
        case DLft(_) => throw new Exception("bind operetor is undefined for DLft.")
      }
    }

    def success[A,B] = DSuccess
    def fail[A,B] = DFail
    def plus[A,B](m1: DTree[A,B], m2: DTree[A,B]) = DOr(m1,m2)
    def assert[A, B](m1: DTree[A,A],m2: DTree[A,B]) = DAssert(m1,m2)
    def assertNot[A, B](m1: DTree[A,A],m2: DTree[A,B]) = DAssertNot(m1,m2)

    def toSet[A](m: DTree[Nothing,A]) = {
      m match {
        case DLeaf(a) => Set(Some(a))
        case DSuccess => Set(None)
        case DFail => Set()
        case DOr(l,r) => toSet(l) | toSet(r)
        case DAssert(l,r) => if (eval(l)) toSet(r) else Set()
        case DAssertNot(l,r) => if (!eval(l)) toSet(r) else Set()
        case DLft(_) => throw new Exception("toSet operetor is undefined for DLft.")
      }
    }

    def eval(m: DTree[Nothing,Nothing]) = {
      m match {
        case DLeaf(_) => throw new Exception("eval operetor is undefined for DLeaf.")
        case DSuccess => true
        case DFail => false
        case DOr(l,r) => eval(l) || eval(r)
        case DAssert(l,r) => eval(l) && eval(r)
        case DAssertNot(l,r) => !eval(l) && eval(r)
        case DLft(_) => throw new Exception("eval operetor is undefined for DLft.")
      }
    }
  }
}
