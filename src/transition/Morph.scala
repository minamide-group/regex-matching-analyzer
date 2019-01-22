package matching.transition

import matching.monad._
import matching.monad.Monad._

class IndexedMorphs[A,B,M[_]](
  val morphs: Map[A,Map[B,M[B]]],
  val initials: Set[B],
  val finals: Set[B]
)(implicit m: Monad[M]) {
  def rename(): IndexedMorphs[A,Int,M] = {
    val renameMap = morphs.values.flatMap( morph =>
      morph.keySet ++ morph.values.flatMap(_.flat)
    ).toSet.zipWithIndex.toMap
    val newMorphs = morphs.mapValues(_.map{ case (b,bs) =>
      renameMap(b) -> (bs >>= ((b: B) => m(renameMap(b))))
    })
    val newInitials = initials.map(renameMap)
    val newFinals = finals.map(renameMap)
    new IndexedMorphs(newMorphs, newInitials, newFinals)
  }

  def toNFA(): NFA[B,A] = {
    val states = morphs.values.flatMap( morph =>
      morph.keySet ++ morph.values.flatMap(_.flat)
    ).toSet
    val sigma = morphs.keySet
    val delta = morphs.toSeq.flatMap{ case (a,h) =>
      h.flatMap{case (b,bs) => bs.flat.map((b,a,_))}
    }.toSet

    new NFA(states, sigma, delta, initials, finals)
  }
}

class IndexedMorphsWithTransition[A,B,C,M[_]](
  val morphs: Map[(A,A),Map[B,Map[C,M[C]]]],
  val initials: Set[C],
  val finals: Set[C],
  val transitionInitials: Set[A],
  val transitionFinals: Set[A]
)(implicit m: Monad[M]) {
  def rename(): IndexedMorphsWithTransition[Int,B,C,M] = {
    val renameMap = morphs.keySet.flatMap{case (a1,a2) => Set(a1,a2)}.zipWithIndex.toMap
    val newMorphs = morphs.map{ case ((a1,a2),indexedMorphs) =>
      (renameMap(a1),renameMap(a2)) -> indexedMorphs
    }
    val newTransitionInitials = transitionInitials.map(renameMap)
    val newTransitionFinals = transitionFinals.map(renameMap)
    new IndexedMorphsWithTransition(newMorphs, initials, finals, newTransitionInitials, newTransitionFinals)
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
    val newStates = newMorphs.values.flatMap( morph =>
      morph.keySet ++ morph.values.flatMap(_.flat)
    ).toSet
    val newInitials = for (c <- initials; a <- transitionInitials if newStates.contains((c,a))) yield (c,a)
    val newFinals = for (c <- finals; a <- transitionFinals if newStates.contains((c,a))) yield (c,a)

    new IndexedMorphs(newMorphs, newInitials, newFinals)
  }
}
