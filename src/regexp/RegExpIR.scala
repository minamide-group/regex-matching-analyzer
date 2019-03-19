package matching.regexp

object RegExpIR {
  sealed trait RegExpIR[A] extends RegExp[A] {
    override def toString(): String = RegExpIR.toString(this)
  }
  case class NumExpIR(s: String) extends RegExpIR[Char]
  case class GroupExpIR[A](r: RegExp[A], name: Option[String]) extends RegExpIR[A]
  case class NamedBackReferenceExpIR[A](name: String) extends RegExpIR[A]

  def toString[A](r: RegExpIR[A]): String = {
    r match {
      case NumExpIR(s) => s"\\${s}"
      case GroupExpIR(r, name) => name match {
        case Some(name) => s"(?<${name}>${r})"
        case None => s"(${r})"
      }
      case NamedBackReferenceExpIR(name) => s"\k<${name}>"
    }
  }

  implicit class IRtoRegExp(r: RegExp[Char]) {
    def recursiveApply(r: RegExp[Char], f: RegExp[Char] => RegExp[Char]): RegExp[Char] = {
      r match {
        case ConcatExp(r1,r2) => ConcatExp(f(r1),f(r2))
        case AltExp(r1,r2) => AltExp(f(r1),f(r2))
        case StarExp(r,greedy) => StarExp(f(r),greedy)
        case PlusExp(r,greedy) => PlusExp(f(r),greedy)
        case OptionExp(r,greedy) => OptionExp(f(r),greedy)
        case RepeatExp(r,min,max,greedy) => RepeatExp(f(r),min,max,greedy)
        case GroupExp(r,id,name) => GroupExp(f(r),id,name)
        case LookAheadExp(r,positive) => LookAheadExp(f(r),positive)
        case LookBehindExp(r,positive) => LookBehindExp(f(r),positive)
        case IfExp(cond,rt,rf) => IfExp(f(cond),f(rt),f(rf))
        case GroupExpIR(r,name) => GroupExpIR(f(r),name)
        case _ => r
      }
    }

    def toRegExp: RegExp[Char] = {
      var captureGroups = 0
      var groupNameMap = Map[String, Int]()

      def convertGroup(r: RegExp[Char]): RegExp[Char] = {
        r match {
          case GroupExpIR(r,name) =>
            captureGroups += 1
            val id = captureGroups
            name match {
              case Some(s) =>
                if (!groupNameMap.contains(s)) groupNameMap += s -> id
                else throw RegExp.InvalidRegExpException(s"duplicated group name: ${s}")
              case None =>
            }
            GroupExp(convertGroup(r), id, name)
          case _ => recursiveApply(r,convertGroup)
        }
      }

      def convert(r: RegExp[Char]): RegExp[Char] = {
        r match {
          case NumExpIR(s) =>
            if (s.head == '0' || captureGroups.max(9) < s.toInt) {
              val (octalPart, elemsPart) = s.take(3).span(_ < '8')
              (elemsPart + s.drop(3)).foldLeft(ElemExp(Integer.parseInt(s"0${octalPart}", 8).toChar): RegExp[Char])(
                (r,c) => ConcatExp(r, ElemExp(c))
              )
            } else {
              if (s.toInt <= captureGroups) BackReferenceExp(s.toInt)
              else throw RegExp.InvalidRegExpException(s"undefined group: ${s}")
            }
          case NamedBackReferenceExpIR(name) =>
            groupNameMap.get(name) match {
              case Some(id) => BackReferenceExp(id)
              case None => throw RegExp.InvalidRegExpException(s"undefined group name: ${name}")
            }
          case _ => recursiveApply(r,convert)
        }
      }

      convert(convertGroup(r))
    }
  }
}
