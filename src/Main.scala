import nfa._
import graph._
import tool.Debug

import util.Random

object Main {
  type Morph[Q] = Map[Q,Seq[Q]]

  def main(args: Array[String]) {
    def generateRandomMorphs(domain: Seq[Char], n: Int, m: Int): Seq[Morph[Char]] = {
      val rangeGen = Random.alphanumeric.filter(domain.contains).toIterator

      (1 to n).map(_ => domain.map(_ -> rangeGen.take(m).toList).toMap))

    val morphs = generateRandomMorphs('a' to 'j', 3, 5)
    println(isIDA(morphs))
  }

  def isIDA[Q](morphs: Seq[Morph[Q]]): Boolean = {
    def morphs2NFA[Q](morphs: Seq[Morph[Q]]): NFA[Q,Int] = {
      val states = morphs.head.keys.toSeq // assume: the domain of each Map is same
      val sigma = (1 to morphs.size).toSeq
      val delta = morphs.zipWithIndex.flatMap{ case (h,idx) =>
        h.flatMap{case (q,qs) => qs.map((q,idx+1,_))}
      }
      val initialStates = states
      val finalStates = states
      new NFA(states, sigma, delta, initialStates, finalStates)
    }

    def nfa2g4[Q,A](nfa: NFA[Q,A]): Graph[(Q,Q,Q)] = {
      val nodes = nfa.states.flatMap(q1 => nfa.states.flatMap(q2 => nfa.states.map(q3 => (q1,q2,q3))))
      val e3 = Debug.time("construct E3") {
        nfa.sigma.flatMap(a =>
          nfa.states.flatMap{p1 =>
            val qs1 = nfa.delta_set(p1,a)
            nfa.states.flatMap{p2 =>
              val qs2 = nfa.delta_set(p2,a)
              nfa.states.flatMap{p3 =>
                val qs3 = nfa.delta_set(p3,a)
                qs1.flatMap(q1 => qs2.flatMap(q2 => qs3.map(q3 =>
                  ((p1,p2,p3), (q1,q2,q3))
                )))
              }
            }
          }
        )
      }
      val e4 = e3 ++ nfa.states.flatMap(p =>
        nfa.states.collect{case q if p != q => ((p,q,q),(p,p,q))}
      )
      new Graph(nodes, e4.distinct)
    }

    val nfa = morphs2NFA(morphs)
    Debug.info("NFA info")(
      ("|Q|", nfa.states.size),
      ("|Σ|", nfa.sigma.size)
    )

    val g4 = nfa2g4(nfa)
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
