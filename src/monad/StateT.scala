package matching.monad

import DMonad._
import DTree._

trait StateOperatable[M[_,_], S] {
  def update[A](f: S => S): M[A,S]
}

object StateT {
  type OptString = Vector[Option[Char]]
  type StateT[S,M[_,_],A,B] = S => M[(A,S),(B,S)]
  trait StateTStringDTree[A,B] extends StateT[OptString, DTree, A, B]

  implicit object StateTDTreeMonad extends DMonad[StateTStringDTree] with StateOperatable[StateTStringDTree, OptString] {
    def unit[A,B](b: B) = s => DTreeMonad((b,s))
    def bindl[A,B,C](m: StateTStringDTree[A,B], f: A => StateTStringDTree[C,C])
      = s => m(s) `>>=l` {case (a,s) => f(a)(s)}
    def bindr[A,B,C](m: StateTStringDTree[A,B], f: B => StateTStringDTree[A,C])
      = s => m(s) `>>=r` {case (b,s) => f(b)(s)}
    def success[A,B] = _ => DTreeMonad.success
    def fail[A,B] = _ => DTreeMonad.fail
    def fail[A,B](m: StateTStringDTree[A,B]) = s => DTreeMonad.fail(m(s))
    def plus[A,B](m1: StateTStringDTree[A,B], m2: StateTStringDTree[A,B])
      = s => m1(s) ++ m2(s)
    def assert[A,B](m1: StateTStringDTree[A,A], m2: StateTStringDTree[A,B])
      = s => DTreeMonad.assert(m1(s), m2(s))
    def assertNot[A,B](m1: StateTStringDTree[A,A], m2: StateTStringDTree[A,B])
      = s => DTreeMonad.assertNot(m1(s), m2(s))

    def update[A](f: OptString => OptString)
      = s => DTreeMonad((s, f(s)))
  }
}
