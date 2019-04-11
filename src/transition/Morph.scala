package matching.transition

import matching.monad._
import matching.monad.Monad._
import matching.tool.Analysis

class IndexedMorphs[A,B,M[_]](
  val morphs: Map[A,Map[B,M[B]]],
  val initials: Set[B],
  val finals: Set[B]
)(implicit m: Monad[M]) {
  lazy val indices = morphs.keySet
  lazy val domain = morphs.values.flatMap( morph =>
    morph.keySet ++ morph.values.flatMap(_.flat)
  ).toSet | initials | finals

  def rename(): IndexedMorphs[A,Int,M] = {
    val renameMap = domain.zipWithIndex.toMap
    val newMorphs = morphs.mapValues(_.map{ case (b,bs) =>
      renameMap(b) -> (bs >>= ((b: B) => m(renameMap(b))))
    })
    val newInitials = initials.map(renameMap)
    val newFinals = finals.map(renameMap)
    new IndexedMorphs(newMorphs, newInitials, newFinals)
  }

  def toNFA(): NFA[B,A] = {
    val states = domain
    val sigma = indices
    val delta = morphs.toSeq.flatMap{ case (a,h) =>
      h.flatMap{case (b,bs) => bs.flat.map((b,a,_))}
    }.toSeq

    new NFA(states, sigma, delta, initials, finals)
  }

  def toIndexedMorphsWithTransition(ladfa: DFA[Set[B],A]): IndexedMorphsWithTransition[Set[B],A,B,M] = {
    val morphCuts = morphs.mapValues(_.mapValues{ rd =>
      Analysis.checkInterrupted("constructing indexed morphisms with transition")
      (rd, ladfa.states.filter(state => rd.flat.forall(!state.contains(_)))) +:
      rd.cuts.map(rdCut => (rdCut, ladfa.states.filter{ state =>
        val rdCutFail :+ rdCutSuccess = rdCut.flat
        rdCutFail.forall(!state.contains(_)) && state.contains(rdCutSuccess)
      }))
    })

    val btrMorphs = ladfa.delta.groupBy{case (p1,_,p2) => (p1,p2)}.mapValues(
      _.map(_._2).toSet
    ).map{ case ((p1,p2),as) =>
      Analysis.checkInterrupted("constructing indexed morphisms with transition")
      (p2,p1) -> as.map( a =>
        a -> morphCuts(a).map{ case (r,rds) =>
          r -> rds.find{case (rd,ps) => ps.contains(p1)}.get._1
        }
      ).toMap
    }

    new IndexedMorphsWithTransition(
      btrMorphs,
      initials,
      finals,
      ladfa.states,
      Set(ladfa.initialState)
    )
  }
}


class IndexedMorphsWithTransition[A,B,C,M[_]](
  val morphs: Map[(A,A),Map[B,Map[C,M[C]]]],
  val initials: Set[C],
  val finals: Set[C],
  val transitionInitials: Set[A],
  val transitionFinals: Set[A]
)(implicit m: Monad[M]) {
  def rename(ladfa: DFA[A,B]): (IndexedMorphsWithTransition[Int,B,C,M], DFA[Int,B]) = {
    val renameMap = ladfa.states.zipWithIndex.toMap

    val newMorphs = morphs.map{ case ((a1,a2),indexedMorphs) =>
      (renameMap(a1),renameMap(a2)) -> indexedMorphs
    }
    val newTransitionInitials = transitionInitials.map(renameMap)
    val newTransitionFinals = transitionFinals.map(renameMap)
    val newIndexedMorphsWithTransition = new IndexedMorphsWithTransition(newMorphs, initials, finals, newTransitionInitials, newTransitionFinals)

    val newStates = renameMap.values.toSet
    val newDeltaDet = ladfa.deltaDet.map{case ((a1,b),a2) => (renameMap(a1),b) -> renameMap(a2)}.toMap
    val newInitialState = renameMap(ladfa.initialState)
    val newFinalStates = ladfa.finalStates.map(renameMap)
    val newLadfa = new DFA(newStates, ladfa.sigma, newDeltaDet, newInitialState, newFinalStates)

    (newIndexedMorphsWithTransition, newLadfa)
  }

  def toIndexedMorphs(): IndexedMorphs[B,(C,A),M] = {
    def merge[K,V](mp1: Map[K,V], mp2: Map[K,V], op: (V,V) => V): Map[K,V] = {
      mp2.foldLeft(mp1){ case (mp,(k,v)) =>
        mp + (k -> (mp.get(k) match {
          case Some(v1) => op(v1,v)
          case None => v
        }))
      }
    }

    val newMorphs = morphs.map{ case ((a1,a2),indexedMorphs) =>
      indexedMorphs.mapValues(_.map{ case (c,cs) =>
        (c,a1) -> (cs >>= ((c: C) => m((c,a2))))
      })
    }.foldLeft(Map[B,Map[(C,A),M[(C,A)]]]())((mp1,mp2) => merge(mp1,mp2,
      (mp1: Map[(C,A),M[(C,A)]], mp2: Map[(C,A),M[(C,A)]]) => merge(mp1,mp2,
        (m1: M[(C,A)], m2: M[(C,A)]) => m1 ++ m2
      )
    ))
    val newInitials = for (c <- initials; a <- transitionInitials) yield (c,a)
    val newFinals = for (c <- finals; a <- transitionFinals) yield (c,a)

    new IndexedMorphs(newMorphs, newInitials, newFinals)
  }
}
