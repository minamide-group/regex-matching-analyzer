package matching.regexp

import scala.collection.mutable.Stack
import matching.monad._
import matching.monad.Monad._
import matching.transition._
import matching.tool.{Analysis, Debug}

sealed trait RegExp[A] {
  override def toString(): String = RegExp.toString(this)
  def derive[M[_]](a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = RegExp.derive[M,A](this,a)
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
case class CharClassExp(cs: Seq[CharClassElem], positive: Boolean) extends RegExp[Char] {
  val charSet = cs.flatMap(_.charSet).toSet
}
case class MetaCharExp(c: Char) extends RegExp[Char] with CharClassElem {
  val charSet = c match {
    case 's' => Set(' ', '\t', '\n', '\r')
    case 't' => Set('\t')
    case 'n' => Set('\n')
    case 'r' => Set('\r')
    case 'w' => ('a' to 'z').toSet | ('A' to 'Z').toSet | ('0' to '9').toSet + '_'
    case 'd' => ('0' to '9').toSet
    case _ => throw new Exception(s"illegal meta character")
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
      case MetaCharExp(c) => s"\\${c}"
      case RepeatExp(r,min,max,greedy) =>
        if (min == max) s"${r}{${min.get}}${if (greedy) "" else "?"}"
        else s"${r}{${min.getOrElse("")},${max.getOrElse("")}}${if (greedy) "" else "?"}"
    }
  }

  def optConcatExp[A](r1: RegExp[A], r2: RegExp[A]): RegExp[A] = {
    (r1,r2) match {
      case (EpsExp(),_) => r2
      case (_,EpsExp()) => r1
      case (_,RepeatExp(`r1`,min,max,greedy)) =>
        RepeatExp(r1,min.orElse(Some(0)).map(_+1),max.map(_+1),greedy)
      case _ => ConcatExp(r1,r2)
    }
  }

  def derive[M[_],A](r: RegExp[A], a: A)(implicit m: Monad[M]): M[Option[RegExp[A]]] = {
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
      case r @ RepeatExp(r1,min,max,greedy) =>
        def decrease(r: RepeatExp[A]): RegExp[A] = {
          val RepeatExp(r1,min,max,greedy) = r
          (min.map(_-1),max.map(_-1)) match {
            case (min,Some(0)) => EpsExp()
            case (Some(0),None) => StarExp(r1,greedy)
            case (Some(0),max) => RepeatExp(r1,None,max,greedy)
            case (min,max) => RepeatExp(r1,min,max,greedy)
          }
        }

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
      case r @ CharClassExp(_,positive) =>
        if (r.charSet.contains(a) ^ !positive) m(Some(EpsExp())) else m.fail
      case r @ MetaCharExp(_) =>
        if (r.charSet.contains(a)) m(Some(EpsExp())) else m.fail
    }
  }

  def constructMorphs[M[_],A](r: RegExp[A], sigma: Set[A])(implicit m: Monad[M]): IndexedMorphs[A,RegExp[A],M] = {
    def nullable(r: RegExp[A]): Boolean = {
      r match {
        case EpsExp() | StarExp(_,_) | OptionExp(_,_) => true
        case ElemExp(_) | EmptyExp() | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => false
        case ConcatExp(r1,r2) => nullable(r1) && nullable(r2)
        case AltExp(r1,r2) => nullable(r1) || nullable(r2)
        case PlusExp(r,_) => nullable(r)
        case RepeatExp(r,min,max,_) => min.isEmpty || nullable(r)
      }
    }

    var regExps = Set(r)
    val stack = Stack(r)
    var morphs = sigma.map(_ -> Map[RegExp[A], M[RegExp[A]]]()).toMap
    while (stack.nonEmpty) {
      Analysis.checkInterrupted("constructing morphisms")
      val r = stack.pop
      sigma.foreach{ e =>
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

  def calcGrowthRate[A](r: RegExp[A], sigma: Set[A]): Option[Int] = {
    val indexedMorphs = constructMorphs[List,A](r, sigma).rename()
    val nfa = indexedMorphs.toNFA().reachablePart()
    if (!nfa.hasLoop()) Some(0)
    else {
      val ambiguity = Debug.time("calculate ambiguity") {
        nfa.calcAmbiguity()
      }
      ambiguity.map(_+1)
    }
  }

  def calcBtrGrowthRate[A](r: RegExp[A], sigma: Set[A]): Option[Int] = {
    val indexedMorphs = Debug.time("consruct IndexedMorphs") {
      constructMorphs[List,A](r, sigma).rename()
    }
    val indexedMorphsWithTransition = Debug.time("consruct IndexedMorphsWithTransition") {
      indexedMorphs.toIndexedMorphsWithTransition().rename()
    }
    val productIndexedMorphs = Debug.time("consruct product IndexedMorphs") {
      indexedMorphsWithTransition.toIndexedMorphs()
    }
    val nfa = Debug.time("consruct NFA") {
      productIndexedMorphs.toNFA().reachablePart()
    }
    if (!nfa.hasLoop()) Some(0)
    else {
      val ambiguity = Debug.time("calculate ambiguity") {
        nfa.calcAmbiguityWithTransition()
      }
      ambiguity.map(_+1)
    }
  }
}


sealed trait CharClassElem {
  val charSet: Set[Char]
  override def toString(): String = CharClassElem.toString(this)
}

case class SingleCharExp(c: Char) extends CharClassElem {
  val charSet = Set(c)
}
case class RangeExp(start: Char, end: Char) extends CharClassElem {
  val charSet = (start to end).toSet
}


object CharClassElem {
  def toString(e: CharClassElem): String = {
    e match {
      case SingleCharExp(c) => c.toString
      case RangeExp(start, end) => s"${start}-${end}"
      case MetaCharExp(c) => s"\\${c}"
    }
  }
}
