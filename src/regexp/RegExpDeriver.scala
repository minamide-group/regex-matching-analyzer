package matching.regexp

import matching.monad._
import matching.monad.Monad._
import matching.tool.Analysis
import RegExp.optConcatExp

class RegExpDeriver[M[_]](implicit m: Monad[M]) {
  def derive[A](r: RegExp[A], a: A): M[Option[RegExp[A]]] = {
    Analysis.checkInterrupted("calculating derivative")
    r match {
      case ElemExp(b) => if (a == b) m(Some(EpsExp())) else m.fail
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
        if (greedy) rd ++ m(None) else (m(None): M[Option[RegExp[A]]]) ++ rd
      case PlusExp(r,greedy) =>
        val rStar = StarExp(r,greedy)
        derive(r,a) >>= {
          case Some(r1) => m(Some(optConcatExp(r1,rStar)))
          case None => derive(rStar,a)
        }
      case OptionExp(r,greedy) =>
        val dr = derive(r,a)
        if (greedy) dr ++ m(None) else (m(None): M[Option[RegExp[A]]]) ++ dr
      case DotExp() => if (a != '\n') m(Some(EpsExp())) else m.fail
      case RepeatExp(r1,min,max,greedy) =>
        val rDec = RepeatExp(r1,min.map(_-1),max.map(_-1),greedy)
        val rd: M[Option[RegExp[A]]] = derive(r1,a) >>= {
          case Some(r2) => m(Some(optConcatExp(r2,rDec)))
          case None => derive(rDec,a)
        }
        if (min.isDefined) rd
        else if (greedy) rd ++ m(None) else (m(None): M[Option[RegExp[A]]]) ++ rd
      case r @ CharClassExp(_,_) =>
        if (r.accept(a)) m(Some(EpsExp())) else m.fail
      case r @ MetaCharExp(_) =>
        if (r.accept(a)) m(Some(EpsExp())) else m.fail
      case _ => throw new Exception(s"derive unsupported expression: ${r}")
    }
  }
}
