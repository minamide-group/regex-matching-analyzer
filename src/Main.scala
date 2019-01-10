import transition._
import Morph._
import regexp._
import tool.Debug

object Main {
  def main(args: Array[String]) {
    val r = RegExpParser("a*a*")
    println(isIDA(r))
  }

  def isIDA(r: RegExp[_]): Boolean = {
    val morphs = r.calcMorphs
    Debug.info("morphs info")(
      ("|domain|", morphs.head.size),
      ("|morph|", morphs.size)
    )

    val g4 = morphs2Graph(morphs).toG4
    Debug.info("G4 info")(
      ("|V|", g4.nodes.size),
      ("|E|", g4.edges.size)
    )

    val scs = g4.calcStrongComponents()

    scs.exists{sc =>
      sc.collect{
        case (p1,p2,p3) if p2 == p3 && p1 != p2 => (p1,p2)
      }.exists{
        case (p,q) => sc.exists{case (q1,q2,q3) => q1 == p && q2 == p && q3 == q}
      }
    }
  }
}
