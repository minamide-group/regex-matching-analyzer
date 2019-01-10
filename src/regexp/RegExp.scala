package regexp

import monad._
import Monad._

trait RegExp[A] {
  def derive[M[_]](a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = RegExp.derive[M,A](this,a)
}

case class ElemExp[A](a: A) extends RegExp[A] {
  override def toString(): String = a.toString
}
case class EmptyExp[A]() extends RegExp[A] {
  override def toString(): String = "∅"
}
case class EpsExp[A]() extends RegExp[A] {
  override def toString(): String = "ε"
}
case class ConcatExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A] {
  override def toString(): String = s"(${r1}${r2})"
}
case class AltExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A] {
  override def toString(): String = s"(${r1}|${r2})"
}
case class StarExp[A](r: RegExp[A]) extends RegExp[A] {
  override def toString(): String = s"(${r})*"
}

object RegExp {
  def derive[M[_],A](r: RegExp[A], a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = {
    r match {
      case ElemExp(b) => if (a == b) m(Some(EpsExp())) else m.fail
      case EmptyExp() => m(Some(EmptyExp()))
      case EpsExp() => m(None)
      case ConcatExp(r1,r2) =>
        r1.derive[M](a) >>= {
          case Some(r) => m(Some(ConcatExp(r,r2)))
          case None => r2.derive[M](a)
        }
      case AltExp(r1,r2) =>
        r1.derive[M](a) ++ r2.derive[M](a)
      case StarExp(r) =>
        (r.derive[M](a) >>= {
          case Some(r1) => m(Some(ConcatExp(r1,StarExp(r))))
          case None => m(None)
        }: M[Option[RegExp[A]]]) ++ m(None)
    }
  }
}
