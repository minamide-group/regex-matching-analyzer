package matching.regexp

import collection.mutable.Stack
import matching.Witness
import matching.monad._
import matching.monad.DMonad._
import matching.monad.DTree._
import matching.monad.StateT._
import matching.transition._
import matching.tool.{Analysis, Debug, IO}

trait RegExp[A] {
  override def toString(): String = RegExp.toString(this)
  def derive[M[_,_]](a: A)(implicit deriver: RegExpDeriver[M])
    : M[Option[RegExp[A]], Option[RegExp[A]]] = deriver.derive(this,Some(a))
  def derive[M[_,_]](a: Option[A])(implicit deriver: RegExpDeriver[M])
    : M[Option[RegExp[A]], Option[RegExp[A]]] = deriver.derive(this,a)
  def deriveEOL[M[_,_]]()(implicit deriver: RegExpDeriver[M])
    : M[Unit, Unit] = deriver.deriveEOL(this)
}

case class ElemExp[A](a: A) extends RegExp[A]
case class EmptyExp[A]() extends RegExp[A]
case class EpsExp[A]() extends RegExp[A]
case class ConcatExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
case class AltExp[A](r1: RegExp[A], r2: RegExp[A]) extends RegExp[A]
case class StarExp[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]
case class PlusExp[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]
case class OptionExp[A](r: RegExp[A], greedy: Boolean) extends RegExp[A]
case class RepeatExp[A](r: RegExp[A], var min: Option[Int], var max: Option[Int], greedy: Boolean) extends RegExp[A]
object RepeatExp {
  def apply[A](r: RegExp[A], min: Option[Int], max: Option[Int], greedy: Boolean): RegExp[A] = {
    def validate() {
      (min, max) match {
        case (Some(min),Some(max)) =>
          if (min < 0 || max < 0) {
            throw RegExp.InvalidRegExpException(s"invalid repeat expression: min and max must be positive")
          } else if (min > max) {
            throw RegExp.InvalidRegExpException(s"invalid repeat expression: ${min} is larger than ${max}")
          }
        case (Some(min),None) =>
          if (min < 0) {
            throw RegExp.InvalidRegExpException(s"invalid repeat expression: min and max must be positive")
          }
        case (None,Some(max)) =>
          if (max < 0) {
            throw RegExp.InvalidRegExpException(s"invalid repeat expression: min and max must be positive")
          }
        case (None,None) =>
          throw RegExp.InvalidRegExpException("invalid repeat expression: either min or max must be specified")
      }
    }

    validate()

    (min,max) match {
      case (min,Some(0)) => EpsExp()
      case (Some(0),None) => StarExp(r,greedy)
      case (Some(0),max) => new RepeatExp(r,None,max,greedy)
      case _ => new RepeatExp(r,min,max,greedy)
    }
  }
}
case class GroupExp[A](r: RegExp[A], id: Int, name: Option[String]) extends RegExp[A]
case class BackReferenceExp[A](n: Int, name: Option[String]) extends RegExp[A]
case class StartAnchorExp[A]() extends RegExp[A]
case class EndAnchorExp[A]() extends RegExp[A]
case class LookaheadExp[A](r: RegExp[A], positive: Boolean) extends RegExp[A]
case class LookbehindExp[A](r: RegExp[A], positive: Boolean) extends RegExp[A]
case class IfExp[A](cond: RegExp[A], rt: RegExp[A], rf: RegExp[A]) extends RegExp[A]
case class DotExp[A]() extends RegExp[A]
case class CharClassExp(es: Seq[CharClassElem], positive: Boolean) extends RegExp[Char]
case class MetaCharExp(c: Char) extends RegExp[Char] with CharClassElem {
  val negetiveChar = Set('D', 'H', 'S', 'V', 'W')

  val charSet = c match {
    case 'd' | 'D' => ('0' to '9').toSet
    case 'h' | 'H' => Set('\u0009')
    case 'R' => Set('\r', '\n')
    case 's' | 'S' => Set(' ', '\t', '\n', '\r', '\f')
    case 'v' | 'V' => Set('\u000B')
    case 'w' | 'W' => ('a' to 'z').toSet | ('A' to 'Z').toSet | ('0' to '9').toSet + '_'

    case _ => throw RegExp.InvalidRegExpException(s"invalid meta character: \\${c}")
  }

  val negative = negetiveChar(c)
}

case class FailEpsExp[A]() extends RegExp[A]

object RegExp {
  case class InvalidRegExpException(message: String) extends Exception(message: String)

  class PCREOptions(s: String = "") {
    var ignoreCase = false
    var dotAll = false
    var ungreedy = false

    s.foreach{
      case 'i' => ignoreCase = true
      case 's' => dotAll = true
      case 'U' => ungreedy = true
      case c => throw RegExp.InvalidRegExpException(s"unsupported PCRE option: ${c}")
    }
  }

  def toString[A](r: RegExp[A]): String = {
    r match {
      case ElemExp(a) => IO.escape(a)
      case EmptyExp() => "∅"
      case EpsExp() => "ε"
      case ConcatExp(r1,r2) =>
        val reg1 = r1 match {
          case AltExp(_,_) => s"(?:${r1})"
          case _ => r1.toString
        }
        val reg2 = r2 match {
          case ConcatExp(_,_) | AltExp(_,_) => s"(?:${r2})"
          case _ => r2.toString
        }
        s"${reg1}${reg2}"
      case AltExp(r1,r2) => r2 match {
        case AltExp(_,_) => s"${r1}|(?:${r2})"
        case _ => s"${r1}|${r2}"
      }
      case StarExp(r,greedy) => r match {
        case ConcatExp(_,_) | AltExp(_,_) |
          StarExp(_,_) | PlusExp(_,_) | OptionExp(_,_) | RepeatExp(_,_,_,_) =>
          s"(?:${r})*${if (greedy) "" else "?"}"
        case _ => s"${r}*${if (greedy) "" else "?"}"
      }
      case PlusExp(r,greedy) => r match {
        case ConcatExp(_,_) | AltExp(_,_) |
          StarExp(_,_) | PlusExp(_,_) | OptionExp(_,_) | RepeatExp(_,_,_,_) =>
          s"(?:${r})+${if (greedy) "" else "?"}"
        case _ => s"${r}+${if (greedy) "" else "?"}"
      }
      case OptionExp(r,greedy) => r match {
        case ConcatExp(_,_) | AltExp(_,_) |
          StarExp(_,_) | PlusExp(_,_) | OptionExp(_,_) | RepeatExp(_,_,_,_)
          => s"(?:${r})?${if (greedy) "" else "?"}"
        case _ => s"${r}?${if (greedy) "" else "?"}"
      }
      case DotExp() => "."
      case RepeatExp(r,min,max,greedy) =>
        val reg = r match {
          case ConcatExp(_,_) | AltExp(_,_) |
            StarExp(_,_) | PlusExp(_,_) | OptionExp(_,_) | RepeatExp(_,_,_,_) => s"(?:${r})"
          case _ => r.toString
        }
        val rep = if (min == max) {
          s"{${min.get}}${if (greedy) "" else "?"}"
        } else {
          s"{${min.getOrElse("")},${max.getOrElse("")}}${if (greedy) "" else "?"}"
        }
        s"${reg}${rep}"
      case CharClassExp(es,positive) => s"[${if (positive) "" else "^"}${es.mkString}]"
      case MetaCharExp(c) => s"\\${c}"
      case GroupExp(r,_,name) => name match {
        case Some(name) => s"(?<${name}>${r})"
        case None => s"(${r})"
      }
      case StartAnchorExp() => "^"
      case EndAnchorExp() => "$"
      case BackReferenceExp(n, name) => name match {
        case Some(name) => s"(?P=${name})"
        case None => s"\\${n}"
      }
      case LookaheadExp(r,positive) => s"(?${if (positive) "=" else "!"}${r})"
      case LookbehindExp(r,positive) => s"(?<${if (positive) "=" else "!"}${r})"
      case IfExp(cond,rt,rf) => s"(?(${cond})${rt}|${rf})"
      case FailEpsExp() => "\u25C7"
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

  def modifyRegExp[A](r: RegExp[A]): (RegExp[A], Boolean, Int) = {
    var approximated = false

    def recursiveApply(r: RegExp[A], f: RegExp[A] => RegExp[A]): RegExp[A] = {
      r match {
        case ConcatExp(r1,r2) => ConcatExp(f(r1),f(r2))
        case AltExp(r1,r2) => AltExp(f(r1),f(r2))
        case StarExp(r,greedy) => StarExp(f(r),greedy)
        case PlusExp(r,greedy) => PlusExp(f(r),greedy)
        case OptionExp(r,greedy) => OptionExp(f(r),greedy)
        case RepeatExp(r,min,max,greedy) => RepeatExp(f(r),min,max,greedy)
        case GroupExp(r,id,name) => GroupExp(f(r),id,name)
        case LookaheadExp(r,positive) => LookaheadExp(f(r),positive)
        case LookbehindExp(r,positive) => LookbehindExp(f(r),positive)
        case _ => r
      }
    }

    def checkSupported(r: RegExp[A], inLookAhaed: Boolean = false): Unit = {
      r match {
        case ConcatExp(r1,r2) => checkSupported(r1, inLookAhaed); checkSupported(r2, inLookAhaed)
        case AltExp(r1,r2) => checkSupported(r1, inLookAhaed); checkSupported(r2, inLookAhaed)
        case StarExp(r,_) => checkSupported(r, inLookAhaed)
        case PlusExp(r,_) => checkSupported(r, inLookAhaed)
        case OptionExp(r,_) => checkSupported(r, inLookAhaed)
        case RepeatExp(r,_,_,_) => checkSupported(r, inLookAhaed)
        case GroupExp(r,_,_) => checkSupported(r, inLookAhaed)
        case BackReferenceExp(_,_) => if (inLookAhaed) {
          throw RegExp.InvalidRegExpException(
            s"back reference in lookahead is unsupported.")
          }
        case LookaheadExp(r,_) => checkSupported(r, true)
        case LookbehindExp(r,_) => if (inLookAhaed) {
            throw RegExp.InvalidRegExpException(
              s"lookbehind in lookahead is unsupported.")
          } else {
            checkSupported(r, inLookAhaed)
          }
        case IfExp(_,_,_) => throw RegExp.InvalidRegExpException(
            s"conditional expression is unsupported.")
        case _ => // NOP
      }
    }

    def isStartAnchorHead(r: RegExp[A]): Boolean = {
      r match {
        case StartAnchorExp() => true
        case ConcatExp(r1,_) => isStartAnchorHead(r1)
        case AltExp(r1,r2) => isStartAnchorHead(r1) && isStartAnchorHead(r2)
        case PlusExp(r,_) => isStartAnchorHead(r)
        case RepeatExp(r,min,_,_) if min.isDefined => isStartAnchorHead(r)
        case GroupExp(r,_,_) => isStartAnchorHead(r)
        case _ => false
      }
    }

    def getLookbehindMaxLength(r: RegExp[A]): Option[Int] = {
      def getMaxLength(r: RegExp[A]): Option[Int] = {
        r match {
          case ElemExp(_) | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => Some(1)
          case ConcatExp(r1, r2) =>
            for (n1 <- getMaxLength(r1); n2 <- getMaxLength(r2)) yield n1 + n2
          case AltExp(r1, r2) =>
            for (n1 <- getMaxLength(r1); n2 <- getMaxLength(r2)) yield n1.max(n2)
          case StarExp(_,_) | PlusExp(_,_) => None
          case OptionExp(r,_) => getMaxLength(r)
          case RepeatExp(r,_,max,_) =>
            if (max.isDefined) getMaxLength(r).map(_*max.get) else None
          case GroupExp(r,_,_) => getMaxLength(r)
          case StartAnchorExp() => None
          case _ => Some(0)
        }
      }

      r match {
        case LookbehindExp(r,_) => getMaxLength(r)
        case ConcatExp(r1, r2) =>
          for (n1 <- getLookbehindMaxLength(r1); n2 <- getLookbehindMaxLength(r2)) yield n1.max(n2)
        case AltExp(r1, r2) =>
          for (n1 <- getLookbehindMaxLength(r1); n2 <- getLookbehindMaxLength(r2)) yield n1.max(n2)
        case StarExp(r,_) => getLookbehindMaxLength(r)
        case PlusExp(r,_) => getLookbehindMaxLength(r)
        case OptionExp(r,_) => getLookbehindMaxLength(r)
        case RepeatExp(r,_,_,_) => getLookbehindMaxLength(r)
        case GroupExp(r,_,_) => getLookbehindMaxLength(r)
        case _ => Some(0)
      }
    }

    def getGroupMap(r: RegExp[A]): Map[Int, RegExp[A]] = {
      r match {
        case GroupExp(r,n,_) => Map(n -> r) ++ getGroupMap(r)
        case ConcatExp(r1, r2) => getGroupMap(r1) ++ getGroupMap(r2)
        case AltExp(r1, r2) => getGroupMap(r1) ++ getGroupMap(r2)
        case StarExp(r,_) => getGroupMap(r)
        case PlusExp(r,_) => getGroupMap(r)
        case OptionExp(r,_) => getGroupMap(r)
        case RepeatExp(r,_,_,_) => getGroupMap(r)
        case LookaheadExp(r,positive) if positive => getGroupMap(r)
        case LookbehindExp(r,positive) if positive => getGroupMap(r)
        case _ => Map()
      }
    }

    def collectBackReferences(r: RegExp[A]): Set[Int] = {
      r match {
        case ConcatExp(r1,r2) => collectBackReferences(r1) | collectBackReferences(r2)
        case AltExp(r1,r2) => collectBackReferences(r1) | collectBackReferences(r2)
        case StarExp(r,_) => collectBackReferences(r)
        case PlusExp(r,_) => collectBackReferences(r)
        case OptionExp(r,_) => collectBackReferences(r)
        case RepeatExp(r,_,_,_) => collectBackReferences(r)
        case GroupExp(r,_,_) => collectBackReferences(r)
        case BackReferenceExp(n,_) => Set(n)
        case _ => Set()
      }
    }

    def replaceLookbehind(r: RegExp[A]): RegExp[A] = {
      r match {
        case LookbehindExp(_,_) =>
          approximated = true
          EpsExp()
          case _ => recursiveApply(r,replaceLookbehind)
      }
    }

    def replace(r: RegExp[A], groupMap: Map[Int, RegExp[A]]): RegExp[A] = {
      def replace(r: RegExp[A]): RegExp[A] = {
        def removeAssert(r: RegExp[A]): RegExp[A] = {
          r match {
            case LookaheadExp(_,_) | LookbehindExp(_,_) | StartAnchorExp() | EndAnchorExp() => FailEpsExp()
            case GroupExp(r,_,_) => r
            case _ => recursiveApply(r,removeAssert)
          }
        }

        r match {
          case GroupExp(r,_,_) => replace(r)
          case LookbehindExp(_,_) =>
            approximated = true
            FailEpsExp()
          case BackReferenceExp(n,_) =>
            approximated = true
            ConcatExp(replace(removeAssert(groupMap(n))), FailEpsExp())
          case _ => recursiveApply(r,replace)
        }
      }

      replace(r)
    }

    checkSupported(r)
    val maxLength = getLookbehindMaxLength(r) match {
      case Some(n) => n
      case None => throw RegExp.InvalidRegExpException(
        s"lookbehind with unbounded matching length is unsupported.")
    }

    // simulates suffix match
    val r1 = if (isStartAnchorHead(r)) r else ConcatExp(StarExp(DotExp(), false), r)

    // check dependencies of back references
    val groupMap = getGroupMap(r1).withDefaultValue(EmptyExp())
    var dependencyMap = groupMap.mapValues(collectBackReferences)

    while (dependencyMap.nonEmpty) {
      val newDependencyMap = dependencyMap.filter(_._2.nonEmpty)
      if (dependencyMap.keySet == newDependencyMap.keySet) {
        throw RegExp.InvalidRegExpException(
          s"back reference with cycle is unsupported.")
      } else {
        val newKeys = newDependencyMap.keySet
        dependencyMap = newDependencyMap.mapValues(_.filter(newKeys))
      }
    }

    // approximate lookbehind/back reference
    val r2 = replace(r1, groupMap)

    (r2, approximated, maxLength)
  }

  def constructTransducer(
    r: RegExp[Char], maxLength: Int, options: PCREOptions = new PCREOptions()
  ): DetTransducer[(RegExp[Char], OptString), Option[Char]] = {
    def getElems(r: RegExp[Char]): Set[Char] = {
      r match {
        case ElemExp(a) => if (options.ignoreCase && a.isLetter) Set(a.toLower) else Set(a)
        case ConcatExp(r1,r2) => getElems(r1) | getElems(r2)
        case AltExp(r1,r2) => getElems(r1) | getElems(r2)
        case StarExp(r,_) => getElems(r)
        case PlusExp(r,_) => getElems(r)
        case OptionExp(r,_) => getElems(r)
        case DotExp() => if (options.dotAll) Set() else Set('\n')
        case RepeatExp(r,_,_,_) => getElems(r)
        case GroupExp(r,_,_) => getElems(r)
        case LookaheadExp(r,_) => getElems(r)
        case LookbehindExp(r,_) => getElems(r)
        case r @ CharClassExp(es,positive) =>
          val s = es.flatMap(_.charSet).toSet
          if (options.ignoreCase) {
            s.map{
              case a if a.isLetter => a.toLower
              case a => a
            }
          } else s
        case r @ MetaCharExp(c) =>
          val s = r.charSet
          if (options.ignoreCase) {
            s.map{
              case a if a.isLetter => a.toLower
              case a => a
            }
          } else s
        case _ => Set()
      }
    }

    def cutState(s: OptString): OptString = {
      if (maxLength == 0) { // only start anchor
        if (s.isEmpty) Vector() else Vector(None)
      } else {
        s.takeRight(maxLength)
      }
    }

    val sigma = getElems(r).map(Option(_)) + None // None: character which does not appear in the given expression
    val initialState: (RegExp[Char], OptString) = (r, Vector())
    var states = Set(initialState)
    val stack = Stack(initialState)
    var delta = Map[
      ((RegExp[Char], OptString), Option[Option[Char]]),
      DTree[(RegExp[Char], OptString), (RegExp[Char], OptString)]
    ]()
    implicit val deriver = new RegExpDeriver[StateTStringDTree](options)

    while (stack.nonEmpty) {
      Analysis.checkInterrupted("regular expression -> transducer")
      val s @ (r,u) = stack.pop
      sigma.foreach{ a =>
        val t = (r.derive(a) >>= {
          case Some(r) => StateTDTreeMonad[RegExp[Char], RegExp[Char]](r)
          case None => StateTDTreeMonad.success[RegExp[Char], RegExp[Char]] // simulates prefix match
        })(u) >>= {
          case (r,s) => DTreeMonad[(RegExp[Char], OptString), (RegExp[Char], OptString)]((r, cutState(s)))
        }
        delta += (s,Some(a)) -> t
        val newExps = DTreeMonad.leaves(t).filterNot(states.contains)
        states |= newExps.toSet
        stack.pushAll(newExps)
      }
      val t = (r.deriveEOL >>= (_ => StateTDTreeMonad.success[RegExp[Char], RegExp[Char]]))(u)
      delta += (s,None) -> t
    }

    new DetTransducer(states, sigma, initialState, delta)
  }

  def calcTimeComplexity(
    r: RegExp[Char],
    options: PCREOptions = new PCREOptions(),
    method: Option[BacktrackMethod]
  ): (Option[Int], Witness[Char], Boolean, Int) // (degree, witness, approximated?, size of transducer)
  = {
    def convertWitness(w: Witness[Option[Char]]): Witness[Char] = {
      val charForNone = '\u25AE'
      Witness(w.separators.map(_.map(_.getOrElse(charForNone))), w.pumps.map(_.map(_.getOrElse(charForNone))))
    }

    val (rm, approximated, maxLength) = modifyRegExp(r)

    val transducer = Debug.time("regular expression -> transducer") {
      constructTransducer(rm, maxLength, options)
    }.rename()

    val (growthRate, witness) = method match {
      case Some(method) => transducer.calcGrowthRateBacktrack(method)
      case None => transducer.calcGrowthRate()
    }
    (growthRate, convertWitness(witness), approximated, transducer.deltaDet.size)
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
      case SingleCharExp(c) => IO.escape(c)
      case RangeExp(start, end) => s"${IO.escape(start)}-${IO.escape(end)}"
      case MetaCharExp(c) => s"\\${c}"
    }
  }
}
