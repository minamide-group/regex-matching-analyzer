package matching.monad

import AMonad._

sealed trait ATree[A,B]
case class ALeaf[A,B](b: B) extends ATree[A,B]
case class ASuccess[A,B]() extends ATree[A,B]
case class AFail[A,B]() extends ATree[A,B]
case class AFail1[A,B](t: ATree[A,B]) extends ATree[A,B]
case class AOr[A,B](l: ATree[A,B], r: ATree[A,B]) extends ATree[A,B]
case class AAssert[A,B](l: ATree[A,A], r: ATree[A,B]) extends ATree[A,B]
case class AAssertNot[A,B](l: ATree[A,A], r: ATree[A,B]) extends ATree[A,B]
case class ALft[A,B](l: ATree[A,B]) extends ATree[A,B]

object ATree {
  implicit object ATreeMonad extends AMonad[ATree] with Evaluable[ATree] {
    def unit[A,B](b: B) = ALeaf(b)
    def bindl[A,B,C](m: ATree[A,B], f: A => ATree[C,C]) = {
      m match {
        case ALeaf(b) => ALeaf(b)
        case ASuccess() => ASuccess()
        case AFail() => AFail()
        case AFail1(t) => AFail1(t `>>=l` f)
        case AOr(l,r) => AOr(l `>>=l` f, r `>>=l` f)
        case AAssert(l,r) => AAssert(l >>= f, r `>>=l` f)
        case AAssertNot(l,r) => AAssertNot(l >>= f, r `>>=l` f)
        case ALft(_) => throw new Exception("bind operetor is undefined for ALft.")
      }
    }

    def bindr[A,B,C](m: ATree[A,B], f: B => ATree[A,C]) = {
      m match {
        case ALeaf(b) => f(b)
        case ASuccess() => ASuccess()
        case AFail() => AFail()
        case AFail1(t) => AFail1(t `>>=r` f)
        case AOr(l,r) => AOr(l `>>=r` f, r `>>=r` f)
        case AAssert(l,r) => AAssert(l, r `>>=r` f)
        case AAssertNot(l,r) => AAssertNot(l, r `>>=r` f)
        case ALft(_) => throw new Exception("bind operetor is undefined for ALft.")
      }
    }

    def success[A,B] = ASuccess()
    def fail[A,B] = AFail()
    def fail[A,B](m: ATree[A,B]) = AFail1(m)
    def plus[A,B](m1: ATree[A,B], m2: ATree[A,B]) = AOr(m1,m2)
    def assert[A, B](m1: ATree[A,A],m2: ATree[A,B]) = AAssert(m1,m2)
    def assertNot[A, B](m1: ATree[A,A],m2: ATree[A,B]) = AAssertNot(m1,m2)

    def eval[A](m: ATree[A,A])(v: A => Boolean) = {
      m match {
        case ALeaf(a) => v(a)
        case ASuccess() => true
        case AFail() => false
        case AFail1(_) => false
        case AOr(l,r) => eval(l)(v) || eval(r)(v)
        case AAssert(l,r) => eval(l)(v) && eval(r)(v)
        case AAssertNot(l,r) => !eval(l)(v) && eval(r)(v)
        case ALft(_) => throw new Exception("eval operetor is undefined for ALft.")
      }
    }

    def evalr[A](m: ATree[Nothing,A])(v: A => Boolean) = {
      m match {
        case ALeaf(a) => v(a)
        case ASuccess() => true
        case AFail() => false
        case AFail1(_) => false
        case AOr(l,r) => evalr(l)(v) || evalr(r)(v)
        case AAssert(l,r) => eval(l)(identity) && evalr(r)(v)
        case AAssertNot(l,r) => !eval(l)(identity) && evalr(r)(v)
        case ALft(_) => throw new Exception("eval operetor is undefined for ALft.")
      }
    }

    def leaves[A](m: ATree[A,A]) = {
      m match {
        case ALeaf(a) => Seq(a)
        case ASuccess() | AFail() => Seq()
        case AFail1(t) => leaves(t)
        case AOr(l,r) => leaves(l) ++ leaves(r)
        case AAssert(l,r) => leaves(l) ++ leaves(r)
        case AAssertNot(l,r) => leaves(l) ++ leaves(r)
        case ALft(l) => leaves(l)
      }
    }
  }

  def prune[Q](t: ATree[Q,Q], qs: Set[Q] = Set[Q]()): ATree[Q,Q] = {
    t match {
      case AOr(l,r) => if (ATreeMonad.eval(l)(qs)) ALft(prune(l,qs)) else AOr(l,prune(r,qs))
      case AFail1(t) => prune(t,qs)
      case AAssert(l,r) => if (!ATreeMonad.eval(l)(qs)) ALft(prune(l,qs)) else AAssert(l,prune(r,qs))
      case AAssertNot(l,r) => if (ATreeMonad.eval(l)(qs)) ALft(prune(l,qs)) else AAssertNot(l,prune(r,qs))
      case _ => t
    }
  }
}
