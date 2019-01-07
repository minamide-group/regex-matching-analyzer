package monad

import Monad._

sealed trait Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case class Or[A](l: Tree[A], r: Tree[A]) extends Tree[A]
case object Fail extends Tree[Nothing]

object Tree {
  implicit object TreeMonad extends Monad[Tree] {
    def unit[A](a: A) = Leaf(a)
    def bind[A,B](m: Tree[A], f: A => Tree[B]) = {
      m match {
        case Leaf(a) => f(a)
        case Or(l,r) => Or(l >>= f, r >>= f)
        case Fail => Fail
      }
    }
    def fail[A] = Fail
    def concat[A](m1: Tree[A], m2: Tree[A]) = Or(m1,m2)
  }
}
