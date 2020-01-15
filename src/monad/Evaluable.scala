package matching.monad

trait Evaluable[M[_,_]] {
  def eval[A](m: M[A,A])(assignment: A => Boolean): Boolean
}
