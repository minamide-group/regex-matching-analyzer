package matching.transition

sealed trait BackTrackMethod
case object LookAhead extends BackTrackMethod
case object EnsureFail extends BackTrackMethod
case object Nondeterminism extends BackTrackMethod
