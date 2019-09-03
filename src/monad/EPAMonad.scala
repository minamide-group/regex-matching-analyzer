package matching.monad

trait EPAMonad[M[_,_]] {
  def unit[A,B](b: B): M[A,B]
  def bind[A,B,C,D](m: M[A,B], f: A => M[C,C], g: B => M[C,D]): M[C,D]
  def success[A,B]: M[A,B]
  def fail[A,B]: M[A,B]
  def plus[A,B](m1: M[A,B], m2: M[A,B]): M[A,B]
  def assert[A,B](m1: M[A,A], m2: M[A,B]): M[A,B]
  def assertNot[A,B](m1: M[A,A], m2: M[A,B]): M[A,B]

  def toSet[A](m: M[Nothing,A]): Set[Option[A]] // None: Top
  def eval(m: M[Nothing,Nothing]): Boolean

  final def apply[B](b: B) = unit(b)
}

object EPAMonad {
  implicit class EPAMonadOp[M[_,_],A,B](self: M[A,B])(implicit m: EPAMonad[M]) {
    def >>=[C,D](f: A => M[C,C], g: B => M[C,D]) = m.bind(self, f, g)
    def ++(m2: M[A,B]) = m.plus(self,m2)
  }
}
