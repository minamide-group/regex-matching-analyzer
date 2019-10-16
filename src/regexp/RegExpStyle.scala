package matching.regexp

sealed trait RegExpStyle
case object Raw extends RegExpStyle
case object PCRE extends RegExpStyle
