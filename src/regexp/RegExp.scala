package regexp

import scala.collection.mutable.Stack
import monad._
import Monad._
import transition.Morph._


trait RegExp[A] {
  override def toString(): String = RegExp.toString(this)
  def derive[M[_]](a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = RegExp.derive[M,A](this,a)
  def calcMorphs(): Seq[Morph[RegExp[A]]] = RegExp.calcMorphs(this)
  def calcGrowthRate(): Option[Int] = morphs2Graph(rename(calcMorphs())).calcAmbiguity()
}

case class ElemExp[A](a: A) extends RegExp[A]
case class EmptyExp[A]() extends RegExp[A]
case class EpsExp[A]() extends RegExp[A]
case class ConcatExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
case class AltExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
case class StarExp[A](r: RegExp[A]) extends RegExp[A]


object RegExp {
  def toString[A](r: RegExp[A]): String = {
    r match {
      case ElemExp(a) => a.toString
      case EmptyExp() => "∅"
      case EpsExp() => "ε"
      case ConcatExp(r1,r2) => s"(${r1}${r2})"
      case AltExp(r1,r2) => s"(${r1}|${r2})"
      case StarExp(r) => s"(${r})*"
    }
  }

  def derive[M[_],A](r: RegExp[A], a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = {
    r match {
      case ElemExp(b) => if (a == b) m(Some(EpsExp())) else m.fail
      case EmptyExp() => m(Some(EmptyExp()))
      case EpsExp() => m(None)
      case ConcatExp(r1,r2) =>
        r1.derive[M](a) >>= {
          case Some(EpsExp()) => m(Some(r2))
          case Some(r) => m(Some(ConcatExp(r,r2)))
          case None => r2.derive[M](a)
        }
      case AltExp(r1,r2) =>
        r1.derive[M](a) ++ r2.derive[M](a)
      case StarExp(r) =>
        (r.derive[M](a) >>= {
          case Some(EpsExp()) => m(Some(StarExp(r)))
          case Some(r1) => m(Some(ConcatExp(r1,StarExp(r))))
          case None => m(None)
        }: M[Option[RegExp[A]]]) ++ m(None)
    }
  }

  def calcMorphs[A](r: RegExp[A]): Seq[Morph[RegExp[A]]] = {
    def getElems(r: RegExp[A]): Set[A] = {
      r match {
        case ElemExp(a) => Set(a)
        case EmptyExp() | EpsExp() => Set()
        case ConcatExp(r1,r2) => getElems(r1) | getElems(r2)
        case AltExp(r1,r2) => getElems(r1) | getElems(r2)
        case StarExp(r) => getElems(r)
      }
    }

    val elems = getElems(r)
    var regExps = Set(r)
    val stack = Stack(r)
    var morphs = elems.map(_ -> Map[RegExp[A], Seq[RegExp[A]]]()).toMap
    while (stack.nonEmpty) {
      val r = stack.pop
      elems.foreach{ e =>
        val rs = r.derive[List](e).flatten
        morphs += (e -> (morphs(e) + (r -> r.derive[List](e).flatten)))
        val rsNew = rs.filterNot(regExps.contains)
        regExps |= rsNew.toSet
        stack.pushAll(rsNew)
      }
    }

    morphs.values.toList
  }
}
