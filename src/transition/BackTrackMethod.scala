package matching.transition

sealed trait BacktrackMethod
case object Lookahead extends BacktrackMethod
case object EnsureFail extends BacktrackMethod
case object Nondeterminism extends BacktrackMethod
