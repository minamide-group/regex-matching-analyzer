package matching.monad

trait Evaluable[M[_,_]] {
  def eval[A](m: M[A,A])(assignment: A => Boolean): Boolean
  def evalr[A](m: M[Nothing,A])(v: A => Boolean): Boolean

  def leaves[A](m: M[A,A]): Seq[A]
}
