package matching.regexp

import scala.collection.mutable.Stack
import matching.monad._
import matching.monad.Monad._
import matching.transition._
import matching.tool.Debug._

sealed trait RegExp[A] {
  override def toString(): String = RegExp.toString(this)
  def derive[M[_]](a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = RegExp.derive[M,A](this,a)
  def calcGrowthRate(): Option[Int] = RegExp.constructMorphs[List,A](this).toNFA().calcAmbiguity()
  def calcBtrGrowthRate(): Option[Int] = RegExp.constructBtrMorphs[List,A](this).toIndexedMorphs().toNFA().calcAmbiguity()
}

case class ElemExp[A](a: A) extends RegExp[A]
case class EmptyExp[A]() extends RegExp[A]
case class EpsExp[A]() extends RegExp[A]
case class ConcatExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
case class AltExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
case class StarExp[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]
case class PlusExp[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]
case class OptionExp[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]
case class DotExp[A]() extends RegExp[A]
case class CharClassExp(cs: Seq[CharClassElem], positive: Boolean) extends RegExp[Char] {
  val charSet = cs.flatMap(_.toCharSet()).toSet
}
case class RepeatExp[A](r: RegExp[A], min: Option[Int], max: Option[Int], greedy: Boolean) extends RegExp[A] {
  (min, max) match {
    case (Some(min),Some(max)) =>
      if (min <= 0 || max <= 0) {
        throw new Exception(s"illegal repeat expression: min and max must be positive")
      } else if (min > max) {
        throw new Exception(s"illegal repeat expression: ${min} is larger than ${max}")
      }
    case (None,None) =>
      throw new Exception("illegal repeat expression: either min or max must be specified")
    case _ =>
  }
}


object RegExp {
  def toString[A](r: RegExp[A]): String = {
    r match {
      case ElemExp(a) => a.toString
      case EmptyExp() => "∅"
      case EpsExp() => "ε"
      case ConcatExp(r1,r2) => s"(${r1}${r2})"
      case AltExp(r1,r2) => s"(${r1}|${r2})"
      case StarExp(r,greedy) => s"(${r})*${if (greedy) "" else "?"}"
      case PlusExp(r,greedy) => s"(${r})+${if (greedy) "" else "?"}"
      case OptionExp(r,greedy) => s"(${r})?${if (greedy) "" else "?"}"
      case DotExp() => "."
      case CharClassExp(es,positive) => s"[${if (positive) "" else "^"}${es.mkString}]"
      case RepeatExp(r,min,max,greedy) =>
        if (min == max) s"${r}{${min.get}}${if (greedy) "" else "?"}"
        else s"${r}{${min.getOrElse("")},${max.getOrElse("")}}${if (greedy) "" else "?"}"
    }
  }

  def derive[M[_],A](r: RegExp[A], a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = {
    def optConcatExp(r1: RegExp[A], r2: RegExp[A]): RegExp[A] = {
      r1 match {
        case EpsExp() => r2
        case _ => ConcatExp(r1,r2)
      }
    }

    def decrease(r: RepeatExp[A]): RegExp[A] = {
      def decrease(x: Option[Int]): Option[Int] = {
        x match {
          case Some(1) => None
          case Some(n) => Some(n-1)
          case None => None
        }
      }

      val RepeatExp(r1,min,max,greedy) = r
      (decrease(min),decrease(max)) match {
        case (None,None) => StarExp(r1,greedy)
        case (min,max) => RepeatExp(r1,min,max,greedy)
      }
    }

    r match {
      case ElemExp(b) => if (a == b) m(Some(EpsExp())) else m.fail
      case EmptyExp() => m.fail
      case EpsExp() => m(None)
      case ConcatExp(r1,r2) =>
        r1.derive[M](a) >>= {
          case Some(r) => m(Some(optConcatExp(r,r2)))
          case None => r2.derive[M](a)
        }
      case AltExp(r1,r2) =>
        r1.derive[M](a) ++ r2.derive[M](a)
      case StarExp(r,greedy) =>
        if (greedy) {
          (r.derive[M](a) >>= {
            case Some(r1) => m(Some(optConcatExp(r1,StarExp(r,true))))
            case None => m(None)
          }: M[Option[RegExp[A]]]) ++ m(None)
        } else {
          (m(None): M[Option[RegExp[A]]]) ++ r.derive[M](a) >>= {
            case Some(r1) => m(Some(optConcatExp(r1,StarExp(r,false))))
            case None => m(None)
          }
        }
      case PlusExp(r,greedy) =>
        if (greedy) {
          r.derive[M](a) >>= {
            case Some(r1) => m(Some(optConcatExp(r1,StarExp(r,true))))
            case None => StarExp(r,true).derive[M](a)
          }
        } else {
          r.derive[M](a) >>= {
            case Some(EpsExp()) => m(Some(StarExp(r,false)))
            case Some(r1) => m(Some(ConcatExp(r1,StarExp(r,false))))
            case None => StarExp(r,false).derive[M](a)
          }
        }
      case OptionExp(r,greedy) =>
        if (greedy) {
          r.derive[M](a) ++ m(None)
        } else {
          (m(None): M[Option[RegExp[A]]]) ++ r.derive[M](a)
        }
      case DotExp() => m(Some(EpsExp()))
      case r @ CharClassExp(_,positive) => if (r.charSet.contains(a) ^ !positive) m(Some(EpsExp())) else m.fail
      case r @ RepeatExp(r1,min,max,greedy) =>
        val rd = r1.derive[M](a) >>= {
          case Some(r1) => m(Some(optConcatExp(r1,decrease(r))))
          case None => decrease(r).derive[M](a)
        }: M[Option[RegExp[A]]]
        if (min.isDefined) rd
        else if (greedy) {
          rd ++ m(None)
        } else {
          (m(None): M[Option[RegExp[A]]]) ++ rd
        }
    }
  }

  def constructMorphs[M[_],A](r: RegExp[A])(implicit m: Monad[M]): IndexedMorphs[A,RegExp[A],M] = {
    def getElems(r: RegExp[A]): Set[A] = {
      r match {
        case ElemExp(a) => Set(a)
        case EmptyExp() | EpsExp() | DotExp() => Set()
        case ConcatExp(r1,r2) => getElems(r1) | getElems(r2)
        case AltExp(r1,r2) => getElems(r1) | getElems(r2)
        case StarExp(r,_) => getElems(r)
        case PlusExp(r,_) => getElems(r)
        case OptionExp(r,_) => getElems(r)
        case r @ CharClassExp(_,_) => r.charSet
        case RepeatExp(r,_,_,_) => getElems(r)
      }
    }

    def nullable(r: RegExp[A]): Boolean = {
      r match {
        case EpsExp() | StarExp(_,_) | OptionExp(_,_) => true
        case ElemExp(_) | EmptyExp() | DotExp() | CharClassExp(_,_) => false
        case ConcatExp(r1,r2) => nullable(r1) && nullable(r2)
        case AltExp(r1,r2) => nullable(r1) || nullable(r2)
        case PlusExp(r,_) => nullable(r)
        case RepeatExp(r,min,max,_) => min.isEmpty || nullable(r)
      }
    }

    val elems = getElems(r)
    var regExps = Set(r)
    val stack = Stack(r)
    var morphs = elems.map(_ -> Map[RegExp[A], M[RegExp[A]]]()).toMap
    while (stack.nonEmpty) {
      val r = stack.pop
      elems.foreach{ e =>
        val rd: M[RegExp[A]] = r.derive[M](e) >>= {
          case Some(r) => m(r)
          case None => m.fail
        }
        morphs += (e -> (morphs(e) + (r -> rd)))
        val rdNew = rd.flat.filterNot(regExps.contains)
        regExps |= rdNew.toSet
        stack.pushAll(rdNew)
      }
    }

    new IndexedMorphs(morphs, Set(r), regExps.filter(nullable))
  }

  def constructBtrMorphs[M[_],A](r: RegExp[A])(implicit m: Monad[M]): IndexedMorphsWithTransition[Set[RegExp[A]],A,RegExp[A],M] = {
    val indexedMorphs = constructMorphs[M,A](r)
    val ladfa = indexedMorphs.toNFA().reverse().toDFA()
    val morphCuts = indexedMorphs.morphs.mapValues(_.mapValues{rd =>
        (rd, ladfa.states.filter(state => rd.flat.forall(!state.contains(_)))) +:
        rd.cuts.map(rdCut => (rdCut, ladfa.states.filter{ state =>
        val rdCutFail :+ rdCutSuccess = rdCut.flat
        rdCutFail.forall(!state.contains(_)) && state.contains(rdCutSuccess)
      }))
    })

    var edge2Sigma = (for (p1 <- ladfa.states; p2 <- ladfa.states) yield (p1,p2) -> Set[A]()).toMap
    ladfa.delta.foreach{case ((q1,a),q2) => edge2Sigma += (q1,q2) -> (edge2Sigma((q1,q2)) + a)}

    val btrMorphs = edge2Sigma.map{ case ((p1,p2),as) =>
      (p2,p1) -> as.map( a =>
        a -> morphCuts(a).map{ case (r,rds) =>
          r -> rds.find{case (rd,ps) => ps.contains(p2)}.get._1
        }
      ).toMap
    }.filter(_._2.nonEmpty)

    new IndexedMorphsWithTransition(btrMorphs, indexedMorphs.initials, indexedMorphs.finals, ladfa.finalStates, Set(ladfa.initialState))
  }
}


sealed trait CharClassElem {
  override def toString(): String = CharClassElem.toString(this)
  def toCharSet(): Set[Char] = CharClassElem.toCharSet(this)
}

case class SingleCharExp(c: Char) extends CharClassElem
case class RangeExp(start: Char, end: Char) extends CharClassElem


object CharClassElem {
  def toString(e: CharClassElem): String = {
    e match {
      case SingleCharExp(c) => c.toString
      case RangeExp(start, end) => s"${start}-${end}"
    }
  }

  def toCharSet(e: CharClassElem): Set[Char] = {
    e match {
      case SingleCharExp(c) => Set(c)
      case RangeExp(start, end) => (start to end).toSet
    }
  }
}
