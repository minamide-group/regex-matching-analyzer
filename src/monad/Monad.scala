package matching.monad

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
  def fail[A]: M[A]
  def concat[A](m1: M[A], m2: M[A]): M[A]
  def flat[A](m: M[A]): Seq[A]
  def cuts[A](m: M[A]): Seq[M[A]]

  final def apply[A](a: A) = unit(a)
}

object Monad {
  implicit class MonadOp[M[_],A](self: M[A])(implicit m: Monad[M]) {
    def >>=[B](f: A => M[B]) = m.bind(self,f)
    def ++(m2: M[A]) = m.concat(self,m2)
    def flat = m.flat(self)
    def cuts = m.cuts(self)
  }

  implicit object ListMonad extends Monad[List] {
    def unit[A](a: A) = List(a)
    def bind[A,B](m: List[A], f: A => List[B]) = m.flatMap(f)
    def fail[A] = Nil
    def concat[A](m1: List[A], m2: List[A]) = m1 ++ m2
    def flat[A](m: List[A]) = m
    def cuts[A](m: List[A]) = m.inits.toList.init
  }
}
