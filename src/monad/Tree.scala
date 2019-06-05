package matching.monad

import Monad._

sealed trait Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case object Success extends Tree[Nothing]
case object Fail extends Tree[Nothing]
case class Or[A](l: Tree[A], r: Tree[A]) extends Tree[A]

sealed trait CutTree[+A] extends Tree[A]
case class Lft[A](l: Tree[A]) extends CutTree[A]

object Tree {
  implicit object TreeMonad extends Monad[Tree] {
    def unit[A](a: A) = Leaf(a)
    def bind[A,B](m: Tree[A], f: A => Tree[B]) = {
      m match {
        case Leaf(a) => f(a)
        case Success => Success
        case Fail => Fail
        case Or(l,r) => Or(l >>= f, r >>= f)
        case Lft(l) => Lft(l >>= f)
      }
    }
    def fail[A] = Fail
    def concat[A](m1: Tree[A], m2: Tree[A]) = Or(m1,m2)
  }

  def flat[Q](m: Tree[Q]): Seq[Q] = {
    m match {
      case Leaf(q) => Seq(q)
      case Success | Fail => Seq()
      case Or(l,r) => flat(l) ++ flat(r)
      case Lft(l) => flat(l)
    }
  }

  def hasSuccess[Q](t: Tree[Q], qs: Set[Q] = Set[Q]()): Boolean = {
    t match {
      case Leaf(q) => qs.contains(q)
      case Success => true
      case Fail => false
      case Or(l,r) => hasSuccess(l, qs) || hasSuccess(r, qs)
      case Lft(l) => hasSuccess(l, qs)
    }
  }

  def cut[Q](t: Tree[Q], qs: Set[Q] = Set[Q]()): Tree[Q] = {
    t match {
      case Or(l,r) => if (hasSuccess(l,qs)) Lft(cut(l,qs)) else Or(l,cut(r,qs))
      case _ => t
    }
  }
}
