package matching.regexp

import matching.monad._
import matching.monad.DMonad._
import RegExp._

class RegExpDeriver[M[_,_]](option: PCREOption = new PCREOption())(implicit m: DMonad[M]) {
  def derive[A](r: RegExp[A], u: List[Option[A]], a: Option[A]): M[Option[RegExp[A]], Option[RegExp[A]]] = {
    def consume(r: RegExp[A], a: Option[A]): M[Option[RegExp[A]], Option[RegExp[A]]] = {
      def accept(a: Option[A]): Boolean = {
        def acceptCharClass(r: CharClassElem, a: Option[Char]): Boolean = {
          r match {
            case SingleCharExp(c) => a match {
              case Some(a) =>
                if (option.ignoreCase && a.isLetter) {
                  a.toLower == c || a.toUpper == c
                } else a == c
              case None => false
            }
            case RangeExp(start, end) => a match {
              case Some(a) =>
                if (option.ignoreCase && a.isLetter) {
                  r.charSet.contains(a.toLower) || r.charSet.contains(a.toUpper)
                } else r.charSet.contains(a)
              case None => false
            }
            case r @ MetaCharExp(_) => a match {
              case Some(a) =>
                if (option.ignoreCase && a.isLetter) {
                  (r.charSet.contains(a.toLower) || r.charSet.contains(a.toUpper)) ^ r.negative
                } else r.charSet.contains(a) ^ r.negative
              case None => r.negative
            }
          }
        }

        r match {
          case ElemExp(b) => a match {
            case Some(a: Char) =>
              if (option.ignoreCase && a.isLetter) {
                a.toLower == b || a.toUpper == b
              } else {
                a == b
              }
            case Some(a) => a == b
            case None => false
          }
          case DotExp() => a match {
            case Some(a) => option.dotAll || a != '\n'
            case None => true
          }
          case CharClassExp(es,positive) => es.exists(acceptCharClass(_,a)) ^ !positive
          case r @ MetaCharExp(_) => acceptCharClass(r,a)
          case _ => throw new Exception(s"accept unsupported expression: ${r}")
        }
      }

      if (accept(a)) m(Some(EpsExp())) else m.fail
    }

    r match {
      case ElemExp(_) | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => consume(r,a)
      case EmptyExp() => m.fail
      case EpsExp() => m(None)
      case ConcatExp(r1,r2) =>
        derive(r1,u,a) `>>=r` {
          case Some(r) => m(Some(optConcatExp(r,r2)))
          case None => derive(r2,u,a)
        }
      case AltExp(r1,r2) =>
        derive(r1,u,a) ++ derive(r2,u,a)
      case StarExp(r1,greedy) =>
        val rd: M[Option[RegExp[A]], Option[RegExp[A]]] = derive(r1,u,a) `>>=r` {
          case Some(r2) => m(Some(optConcatExp(r2,r)))
          case None => m(None)
        }
        if (greedy ^ option.ungreedy) {
          rd ++ m(None)
        } else {
          (m(None): M[Option[RegExp[A]], Option[RegExp[A]]]) ++ rd
        }
      case PlusExp(r,greedy) =>
        val rStar = StarExp(r,greedy)
        derive(r,u,a) `>>=r` {
          case Some(r1) => m(Some(optConcatExp(r1,rStar)))
          case None => derive(rStar,u,a)
        }
      case OptionExp(r,greedy) =>
        val dr = derive(r,u,a)
        if (greedy ^ option.ungreedy) {
          dr ++ m(None)
        } else {
          (m(None): M[Option[RegExp[A]], Option[RegExp[A]]]) ++ dr
        }
      case RepeatExp(r,min,max,greedy) =>
        val rDec = RepeatExp(r,min.map(_-1),max.map(_-1),greedy)
        val rd: M[Option[RegExp[A]], Option[RegExp[A]]] = derive(r,u,a) `>>=r` {
          case Some(r1) => m(Some(optConcatExp(r1,rDec)))
          case None => derive(rDec,u,a)
        }
        if (min.isDefined) {
          rd
        } else if (greedy ^ option.ungreedy) {
          rd ++ m(None)
        } else {
          (m(None): M[Option[RegExp[A]], Option[RegExp[A]]]) ++ rd
        }
      case GroupExp(r,_,_) => derive(r,u,a)
      case StartAnchorExp() => if (u.isEmpty) m(None) else m.fail
      case EndAnchorExp() => m.fail
      case LookaheadExp(r,positive) =>
        val rd = derive(r,u,a)
        if (positive) m.assert(rd, m(None)) else m.assertNot(rd, m(None))
      case LookbehindExp(_,_) => throw new Exception(s"lookbehind is unsupported.")
      case BackReferenceExp(_,_) => throw new Exception(s"back reference is unsupported.")
      case IfExp(_,_,_) => throw new Exception(s"conditional expression is unsupported.")
    }
  }

  def deriveEOL[A](r: RegExp[A], u: List[Option[A]]): M[Unit, Unit] = {
    r match {
      case ElemExp(_) | EmptyExp() | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => m.fail
      case EpsExp() => m(())
      case ConcatExp(r1,r2) => deriveEOL(r1,u) `>>=r` (_ => deriveEOL(r2,u))
      case AltExp(r1,r2) => deriveEOL(r1,u) ++ deriveEOL(r2,u)
      case StarExp(r,greedy) =>
        val rd = deriveEOL(r,u)
        if (greedy ^ option.ungreedy) {
          rd ++ m(())
        } else {
          m(()) ++ rd
        }
      case PlusExp(r,greedy) =>
        val rStar = StarExp(r,greedy)
        deriveEOL(r,u) `>>=r` (_ => deriveEOL(rStar,u))
      case OptionExp(r,greedy) =>
        val dr = deriveEOL(r,u)
        if (greedy ^ option.ungreedy) {
          dr ++ m(())
        } else {
          m(()) ++ dr
        }
      case RepeatExp(r,min,max,greedy) =>
        val rDec = RepeatExp(r,min.map(_-1),max.map(_-1),greedy)
        val rd = deriveEOL(r,u) `>>=r` (_ => deriveEOL(rDec,u))
        if (min.isDefined) {
          rd
        } else if (greedy ^ option.ungreedy) {
          rd ++ m(())
        } else {
          m(()) ++ rd
        }
      case GroupExp(r,_,_) => deriveEOL(r,u)
      case StartAnchorExp() => if (u.isEmpty) m(()) else m.fail
      case EndAnchorExp() => m(())
      case LookaheadExp(r,positive) =>
        val rd = deriveEOL(r,u)
        if (positive) m.assert(rd, m(())) else m.assertNot(rd, m(()))
      case LookbehindExp(_,_) => throw new Exception(s"lookbehind is unsupported.")
      case BackReferenceExp(_,_) => throw new Exception(s"back reference is unsupported.")
      case IfExp(_,_,_) => throw new Exception(s"conditional expression is unsupported.")
    }
  }
}
