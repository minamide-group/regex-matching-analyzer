package matching.monad

import Monad._

sealed trait Tree[+A]
case class Leaf[A](a: A) extends Tree[A]
case class Or[A](l: Tree[A], r: Tree[A]) extends Tree[A]
case class Lft[A](l: Tree[A]) extends Tree[A]
case object Fail extends Tree[Nothing]

object Tree {
  implicit object TreeMonad extends Monad[Tree] {
    def unit[A](a: A) = Leaf(a)
    def bind[A,B](m: Tree[A], f: A => Tree[B]) = {
      m match {
        case Leaf(a) => f(a)
        case Or(l,r) => Or(l >>= f, r >>= f)
        case Lft(l) => Lft(l >>= f)
        case Fail => Fail
      }
    }
    def fail[A] = Fail
    def concat[A](m1: Tree[A], m2: Tree[A]) = Or(m1,m2)
    def flat[A](m: Tree[A]) = {
      m match {
        case Leaf(a) => Seq(a)
        case Or(l,r) => l.flat ++ r.flat
        case Lft(l) => l.flat
        case Fail => Seq()
      }
    }
    def cuts[A](m: Tree[A]) = {
      def cutsAll(m: Tree[A]): Seq[Tree[A]] = {
        m match {
          case Or(l,r) => l.cuts.map(Lft(_)) ++ r.cuts.map(Or(l,_))
          case Lft(l) => l.cuts.map(Lft(_))
          case _ => Seq(m)
        }
      }
      def isValidCut(m: Tree[A]): Boolean = {
        m match {
          case Leaf(a) => true
          case Or(l,r) => isValidCut(r)
          case Lft(l) => isValidCut(l)
          case Fail => false
        }
      }

      cutsAll(m).filter(isValidCut)
    }
  }
}
