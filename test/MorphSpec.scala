package matching.transition

import org.scalatest._

class MorphSpec extends FlatSpec with Matchers {
  "constructNFA" should "construct NFA" in {
    val morphs = Map(
      1 -> Map(
        'a' -> "ab",
        'b' -> "bcb",
        'c' -> "c"
      ),
      2 -> Map(
        'a' -> "",
        'b' -> "cd"
      )
    ).mapValues(_.mapValues(_.toList))

    val nfa = new IndexedMorphs(morphs,Set('a'), Set('c')).toNFA()

    nfa.states should be (Set('a','b','c','d'))
    nfa.sigma should be (Set(1,2))
    nfa.delta should contain theSameElementsAs Seq(
      ('a',1,'a'),
      ('a',1,'b'),
      ('b',1,'b'),
      ('b',1,'b'),
      ('b',1,'c'),
      ('c',1,'c'),
      ('b',2,'c'),
      ('b',2,'d')
    )
    nfa.initialStates should be (Set('a'))
    nfa.finalStates should be (Set('c'))
  }

  "toIndexedMorphsWithTransition" should "construct IndexedMorphsWithTransition for backtrack search" in {
    val morphs = Map(
      1 -> Map(
        'a' -> "ab",
        'b' -> "bcb",
        'c' -> "c"
      ),
      2 -> Map(
        'a' -> "",
        'b' -> "cd"
      )
    ).mapValues(_.mapValues(_.toList))

    val indexedMorphsWithTransition = new IndexedMorphs(
      morphs,
      Set('a'),
      Set('c')
    ).toIndexedMorphsWithTransition()

    val m1 = Map(
      1 -> Map(
        'a' -> Seq('a'),
        'b' -> Seq('b'),
        'c' -> Seq('c')
      )
    )
    val m2 = Map(
      1 -> Map(
        'a' -> Seq('a','b'),
        'b' -> Seq('b'),
        'c' -> Seq('c')
      )
    )
    val m3 = Map(
      2 -> Map(
        'a' -> Seq(),
        'b' -> Seq('c')
      )
    )
    val m4 = Map(
      1 -> Map(
        'a' -> Seq('a','b'),
        'b' -> Seq('b','c'),
        'c' -> Seq('c')
      )
    )
    val m5 = Map(
      2 -> Map(
        'a' -> Seq(),
        'b' -> Seq('c','d')
      )
    )
    val m6 = Map(
      1 -> Map(
        'a' -> Seq('a','b'),
        'b' -> Seq('b','c','b'),
        'c' -> Seq('c')
      ),
      2 -> Map(
        'a' -> Seq(),
        'b' -> Seq('c','d')
      )
    )
    indexedMorphsWithTransition.morphs should contain only (
      (Set('a','b'),Set('a','b')) -> m1,
      (Set('a','b'),Set('b')) -> m2,
      (Set('b'),Set('a','b','c')) -> m3,
      (Set('b'),Set('b','c')) -> m3,
      (Set('b'),Set('c')) -> m3,
      (Set('a','b','c'),Set('a','b','c')) -> m1,
      (Set('a','b','c'),Set('b','c')) -> m2,
      (Set('b','c'),Set('c')) -> m4,
      (Set(),Set('a','b')) -> m5,
      (Set(),Set('b')) -> m5,
      (Set(),Set()) -> m6
    )
  }

  "toIndexedMorphs" should "construct product IndexedMorphs" in {
    val morphs = Map(
      (1,1) -> Map(
        10 -> Map(
          'a' -> "ab",
          'b' -> "bb",
          'c' -> "abc"
        ),
        20 -> Map(
          'a' -> "abb",
          'b' -> "ac",
          'c' -> ""
        )
      ),
      (1,2) -> Map(
        10 -> Map(
          'a' -> "cd",
          'b' -> "a"
        )
      )
    ).mapValues(_.mapValues(_.mapValues(_.toList)))

    val indexedMorphs = new IndexedMorphsWithTransition(
      morphs,
      Set('a'),
      Set('c'),
      Set(2)
    ).toIndexedMorphs()
    indexedMorphs.morphs should contain only (
      10 -> Map(
        ('a',1) -> Seq(('a',1),('b',1),('c',2),('d',2)),
        ('b',1) -> Seq(('b',1),('b',1),('a',2)),
        ('c',1) -> Seq(('a',1),('b',1),('c',1))
      ),
      20 -> Map(
        ('a',1) -> Seq(('a',1),('b',1),('b',1)),
        ('b',1) -> Seq(('a',1),('c',1)),
        ('c',1) -> Seq()
      )
    )

    indexedMorphs.initials should be (Set(('a',1),('a',2)))
    indexedMorphs.finals should be (Set(('c',2)))
  }
}
