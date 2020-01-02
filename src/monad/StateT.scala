package matching.monad

import DMonad._
import DTree._

trait StateOperatable[M[_,_], S] {
  def update[A](f: S => S): M[A,S]
}

object StateT {
  type StateT[S,M[_,_],A,B] = S => M[(A,S),(B,S)]
  trait StateTBooleanDTree[A,B] extends StateT[Boolean, DTree, A, B]

  implicit object StateTDTreeMonad extends DMonad[StateTBooleanDTree] with StateOperatable[StateTBooleanDTree, Boolean] {
    def unit[A,B](b: B) = s => DTreeMonad((b,s))
    def bindl[A,B,C](m: StateTBooleanDTree[A,B], f: A => StateTBooleanDTree[C,C])
      = s => m(s) `>>=l` {case (a,s) => f(a)(s)}
    def bindr[A,B,C](m: StateTBooleanDTree[A,B], f: B => StateTBooleanDTree[A,C])
      = s => m(s) `>>=r` {case (b,s) => f(b)(s)}
    def success[A,B] = _ => DTreeMonad.success
    def fail[A,B] = _ => DTreeMonad.fail
    def fail[A,B](m: StateTBooleanDTree[A,B]) = s => DTreeMonad.fail(m(s))
    def plus[A,B](m1: StateTBooleanDTree[A,B], m2: StateTBooleanDTree[A,B])
      = s => m1(s) ++ m2(s)
    def assert[A,B](m1: StateTBooleanDTree[A,A], m2: StateTBooleanDTree[A,B])
      = s => DTreeMonad.assert(m1(s), m2(s))
    def assertNot[A,B](m1: StateTBooleanDTree[A,A], m2: StateTBooleanDTree[A,B])
      = s => DTreeMonad.assertNot(m1(s), m2(s))

    def update[A](f: Boolean => Boolean)
      = s => DTreeMonad((s, f(s)))
  }
}
