package matching.monad

import DMonad._


sealed trait DTree[A,B]
case class DLeaf[A,B](b: B) extends DTree[A,B]
case class DSuccess[A,B]() extends DTree[A,B]
case class DFail[A,B]() extends DTree[A,B]
case class DOr[A,B](l: DTree[A,B], r: DTree[A,B]) extends DTree[A,B]
case class DAssert[A,B](l: DTree[A,A], r: DTree[A,B]) extends DTree[A,B]
case class DAssertNot[A,B](l: DTree[A,A], r: DTree[A,B]) extends DTree[A,B]
case class DLft[A,B](l: DTree[A,B]) extends DTree[A,B]

object DTree {
  implicit object DTreeMonad extends DMonad[DTree] with Evaluable[DTree] {
    def unit[A,B](b: B) = DLeaf(b)
    def bindl[A,B,C](m: DTree[A,B], f: A => DTree[C,C]) = {
      m match {
        case DLeaf(b) => DLeaf(b)
        case DSuccess() => DSuccess()
        case DFail() => DFail()
        case DOr(l,r) => DOr(l `>>=l` f, r `>>=l` f)
        case DAssert(l,r) => DAssert(l >>= f, r `>>=l` f)
        case DAssertNot(l,r) => DAssertNot(l >>= f, r `>>=l` f)
        case DLft(_) => throw new Exception("bind operetor is undefined for DLft.")
      }
    }

    def bindr[A,B,C](m: DTree[A,B], f: B => DTree[A,C]) = {
      m match {
        case DLeaf(b) => f(b)
        case DSuccess() => DSuccess()
        case DFail() => DFail()
        case DOr(l,r) => DOr(l `>>=r` f, r `>>=r` f)
        case DAssert(l,r) => DAssert(l, r `>>=r` f)
        case DAssertNot(l,r) => DAssertNot(l, r `>>=r` f)
        case DLft(_) => throw new Exception("bind operetor is undefined for DLft.")
      }
    }

    def success[A,B] = DSuccess()
    def fail[A,B] = DFail()
    def plus[A,B](m1: DTree[A,B], m2: DTree[A,B]) = DOr(m1,m2)
    def assert[A, B](m1: DTree[A,A],m2: DTree[A,B]) = DAssert(m1,m2)
    def assertNot[A, B](m1: DTree[A,A],m2: DTree[A,B]) = DAssertNot(m1,m2)

    def eval[A](m: DTree[A,A])(v: A => Boolean) = {
      m match {
        case DLeaf(a) => v(a)
        case DSuccess() => true
        case DFail() => false
        case DOr(l,r) => eval(l)(v) || eval(r)(v)
        case DAssert(l,r) => eval(l)(v) && eval(r)(v)
        case DAssertNot(l,r) => !eval(l)(v) && eval(r)(v)
        case DLft(_) => throw new Exception("eval operetor is undefined for DLft.")
      }
    }

    def evalr[A](m: DTree[Nothing,A])(v: A => Boolean) = {
      m match {
        case DLeaf(a) => v(a)
        case DSuccess() => true
        case DFail() => false
        case DOr(l,r) => evalr(l)(v) || evalr(r)(v)
        case DAssert(l,r) => eval(l)(identity) && evalr(r)(v)
        case DAssertNot(l,r) => !eval(l)(identity) && evalr(r)(v)
        case DLft(_) => throw new Exception("eval operetor is undefined for DLft.")
      }
    }

    def leaves[A](m: DTree[A,A]) = {
      m match {
        case DLeaf(a) => Seq(a)
        case DSuccess() | DFail() => Seq()
        case DOr(l,r) => leaves(l) ++ leaves(r)
        case DAssert(l,r) => leaves(l) ++ leaves(r)
        case DAssertNot(l,r) => leaves(l) ++ leaves(r)
        case DLft(l) => leaves(l)
      }
    }
  }

  def prune[Q](t: DTree[Q,Q], qs: Set[Q] = Set[Q]()): DTree[Q,Q] = {
    t match {
      case DOr(l,r) => if (DTreeMonad.eval(l)(qs)) DLft(prune(l,qs)) else DOr(l,prune(r,qs))
      case DAssert(l,r) => if (!DTreeMonad.eval(l)(qs)) DLft(prune(l,qs)) else DAssert(l,prune(r,qs))
      case DAssertNot(l,r) => if (DTreeMonad.eval(l)(qs)) DLft(prune(l,qs)) else DAssertNot(l,prune(r,qs))
      case _ => t
    }
  }
}
