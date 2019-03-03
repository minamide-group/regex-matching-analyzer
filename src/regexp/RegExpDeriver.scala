package matching.regexp

import matching.monad._
import matching.monad.Monad._
import matching.tool.Analysis
import RegExp.optConcatExp

class RegExpDeriver[M[_]](options: Seq[Char] = Seq())(implicit m: Monad[M]) {
  class DeriveOption() {
    var ignoreCase = false
    var dotAll = false
    var ungreedy = false
  }
  val option = new DeriveOption()

  options.foreach{
    case 'i' => option.ignoreCase = true
    case 's' => option.dotAll = true
    case 'U' => option.ungreedy = true
    case c => throw new Exception(s"illegal option: ${c}")
  }

  def derive[A](r: RegExp[A], a: Option[A]): M[Option[RegExp[A]]] = {
    def consume(r: RegExp[Char], a: Option[Char]): M[Option[RegExp[Char]]] = {
      def accept(a: Option[Char]): Boolean = {
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
            case Some(a) =>
              if (option.ignoreCase && a.isLetter) a.toLower == b || a.toUpper == b
              else a == b
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

    Analysis.checkInterrupted("calculating derivative")
    r match {
      case ElemExp(_) | DotExp() | CharClassExp(_,_) | MetaCharExp(_) => consume(r,a)
      case EmptyExp() => m.fail
      case EpsExp() => m(None)
      case ConcatExp(r1,r2) =>
        derive(r1,a) >>= {
          case Some(r) => m(Some(optConcatExp(r,r2)))
          case None => derive(r2,a)
        }
      case AltExp(r1,r2) =>
        derive(r1,a) ++ derive(r2,a)
      case StarExp(r1,greedy) =>
        val rd: M[Option[RegExp[A]]] = derive(r1,a) >>= {
          case Some(r2) => m(Some(optConcatExp(r2,r)))
          case None => m(None)
        }
        if (greedy ^ option.ungreedy) rd ++ m(None) else (m(None): M[Option[RegExp[A]]]) ++ rd
      case PlusExp(r,greedy) =>
        val rStar = StarExp(r,greedy)
        derive(r,a) >>= {
          case Some(r1) => m(Some(optConcatExp(r1,rStar)))
          case None => derive(rStar,a)
        }
      case OptionExp(r,greedy) =>
        val dr = derive(r,a)
        if (greedy ^ option.ungreedy) dr ++ m(None) else (m(None): M[Option[RegExp[A]]]) ++ dr
      case RepeatExp(r1,min,max,greedy) =>
        val rDec = RepeatExp(r1,min.map(_-1),max.map(_-1),greedy)
        val rd: M[Option[RegExp[A]]] = derive(r1,a) >>= {
          case Some(r2) => m(Some(optConcatExp(r2,rDec)))
          case None => derive(rDec,a)
        }
        if (min.isDefined) rd
        else if (greedy ^ option.ungreedy) rd ++ m(None) else (m(None): M[Option[RegExp[A]]]) ++ rd
      case _ => throw new Exception(s"derive unsupported expression: ${r}")
    }
  }
}
