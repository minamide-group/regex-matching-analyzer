package matching.monad

import AMonad._
import ATree._

trait StateOperatable[M[_,_], S] {
  def update[A](f: S => S): M[A,S]
}

object StateT {
  type StateT[S,M[_,_],A,B] = S => M[(A,S),(B,S)]
  trait StateTBooleanATree[A,B] extends StateT[Boolean, ATree, A, B]

  implicit object StateTATreeMonad extends AMonad[StateTBooleanATree] with StateOperatable[StateTBooleanATree, Boolean] {
    def unit[A,B](b: B) = s => ATreeMonad((b,s))
    def bindl[A,B,C](m: StateTBooleanATree[A,B], f: A => StateTBooleanATree[C,C])
      = s => m(s) `>>=l` {case (a,s) => f(a)(s)}
    def bindr[A,B,C](m: StateTBooleanATree[A,B], f: B => StateTBooleanATree[A,C])
      = s => m(s) `>>=r` {case (b,s) => f(b)(s)}
    def success[A,B] = _ => ATreeMonad.success
    def fail[A,B] = _ => ATreeMonad.fail
    def fail[A,B](m: StateTBooleanATree[A,B]) = s => ATreeMonad.fail(m(s))
    def plus[A,B](m1: StateTBooleanATree[A,B], m2: StateTBooleanATree[A,B])
      = s => m1(s) ++ m2(s)
    def assert[A,B](m1: StateTBooleanATree[A,A], m2: StateTBooleanATree[A,B])
      = s => ATreeMonad.assert(m1(s), m2(s))
    def assertNot[A,B](m1: StateTBooleanATree[A,A], m2: StateTBooleanATree[A,B])
      = s => ATreeMonad.assertNot(m1(s), m2(s))

    def update[A](f: Boolean => Boolean)
      = s => ATreeMonad((s, f(s)))
  }
}
