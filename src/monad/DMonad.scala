package matching.monad

trait DMonad[M[_,_]] {
  def unit[A,B](b: B): M[A,B]
  def bindl[A,B,C](m: M[A,B], f: A => M[C,C]): M[C,B]
  def bindr[A,B,C](m: M[A,B], f: B => M[A,C]): M[A,C]
  def success[A,B]: M[A,B]
  def fail[A,B]: M[A,B]
  def plus[A,B](m1: M[A,B], m2: M[A,B]): M[A,B]
  def assert[A,B](m1: M[A,A], m2: M[A,B]): M[A,B]
  def assertNot[A,B](m1: M[A,A], m2: M[A,B]): M[A,B]

  final def apply[A,B](b: B) = unit[A,B](b)
}

object DMonad {
  implicit class DMonadOp[M[_,_],A,B](self: M[A,B])(implicit m: DMonad[M]) {
    def `>>=l`[C](f: A => M[C,C]) = m.bindl(self, f)
    def `>>=r`[C](f: B => M[A,C]) = m.bindr(self, f)
    def ++(m2: M[A,B]) = m.plus(self,m2)
  }

  implicit class DMonadAbbrev[M[_,_],A](self: M[A,A])(implicit m: DMonad[M]) {
    def >>=[B](f: A => M[B,B]) = self `>>=l` f `>>=r` f
  }
}
