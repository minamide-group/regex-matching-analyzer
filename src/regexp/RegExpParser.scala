package regexp

import scala.util.parsing.combinator._

object RegExpParser extends RegexParsers {
  def apply(s: String): RegExp[Char] = {
    parseAll(exp, s) match {
      case Success(r,_) => r
      case NoSuccess(msg,next) => throw new Exception(s"${msg} at ${next.pos.line}:${next.pos.column}")
    }
  }

  val char = """[^\s∅ε.|*+?()\[\]\\]""".r
  val esc = """\\[stn∅ε.|*+?()\[\]\\]""".r
  val charInCharClass = """[^\s\[\]\-\\]""".r
  val escInCharClass = """\\[stn^\[\]\-\\]""".r

  def toChar(s: String): Char = {
    if (s.head != '\\') s.head
    else s.tail match {
      case "s" => ' '
      case "t" => '\t'
      case "n" => '\n'
      case c => c.head
    }
  }

  def exp: Parser[RegExp[Char]] = rep1sep(term,"|") ^^ {_.reduce(AltExp(_,_))}
  def term: Parser[RegExp[Char]] = rep1(factor) ^^ {_.reduce(ConcatExp(_,_))}
  def factor: Parser[RegExp[Char]] = (elem | empty | eps | dot | group | charClassNeg | charClass) ~ rep("[*+?]".r) ^^ {
    case f ~ rs =>
      rs.foldLeft(f)((e,r) => r match {
        case "*" => StarExp(e)
        case "+" => PlusExp(e)
        case "?" => OptionExp(e)
      })
  }
  def elem: Parser[ElemExp[Char]] = (char | esc) ^^ {s => ElemExp(toChar(s))}
  def empty: Parser[EmptyExp[Char]] = "∅" ^^ {_ => EmptyExp()}
  def eps: Parser[EpsExp[Char]] = "ε" ^^ {_ => EpsExp()}
  def dot: Parser[DotExp[Char]] = "." ^^ {_ => DotExp()}
  def group: Parser[RegExp[Char]] = "(" ~> exp <~ ")"
  def charClass: Parser[CharClassExp] = "[" ~> charClassElems <~ "]" ^^ {CharClassExp(_)}
  def charClassNeg: Parser[CharClassExp] = "[^" ~> charClassElems <~ "]" ^^ {CharClassExp(_,true)}
  def charClassElems: Parser[Seq[CharClassElem]] = rep1(range | singleChar)
  def singleChar: Parser[SingleCharExp] = charClassElem ^^ {SingleCharExp}
  def range: Parser[RangeExp] = ((charClassElem <~ "-") ~ charClassElem) ^^ {
    case start ~ end => RangeExp(start, end)
  }
  def charClassElem: Parser[Char] = (charInCharClass | escInCharClass) ^^ {toChar}
}
