package regexp

import scala.util.parsing.combinator._

object RegExpParser extends RegexParsers {
  def apply(s: String): RegExp[Char] = {
    parseAll(exp, s) match {
      case Success(r,_) => r
      case NoSuccess(msg, _) => throw new Exception(msg)
    }
  }

  val char = """[^\s∅ε|*()\\]""".r
  val esc = """\\[stn∅ε|*()\\]""".r

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
  def factor: Parser[RegExp[Char]] = (elem | empty | eps | group) ~ rep("*") ^^ { case f ~ rs =>
    rs.foldLeft(f)((e,r) => StarExp(e))
  }
  def elem: Parser[ElemExp[Char]] = (char | esc) ^^ {s => ElemExp(toChar(s))}
  def empty: Parser[RegExp[Char]] = "∅" ^^ {_ => EmptyExp()}
  def eps: Parser[RegExp[Char]] = "ε" ^^ {_ => EpsExp()}
  def group: Parser[RegExp[Char]] = "(" ~> exp <~ ")"
}
