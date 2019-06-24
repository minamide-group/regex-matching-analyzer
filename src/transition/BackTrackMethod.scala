package matching.transition

sealed trait BacktrackMethod
case object Lookahead extends BacktrackMethod
case object SubsetPrune extends BacktrackMethod
case object Nondeterminism extends BacktrackMethod
