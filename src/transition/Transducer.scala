package matching.transition

import scala.collection.mutable.{Stack, Queue}
import matching.monad.Tree
import matching.monad.Tree._
import matching.monad._
import matching.monad.Monad._
import matching.tool.{Analysis, Debug}

class NonDetTransducer[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val initialState: Q,
  val delta: Seq[(Q,Option[A],Tree[Q])]
) {
  def rename(): NonDetTransducer[Int,A] = {
    val renameMap = states.zipWithIndex.toMap
    val renamedStates = states.map(renameMap)
    val renamedInitialState = renameMap(initialState)
    val renamedDelta = delta.map{ case (q,a,t) =>
      (renameMap(q), a, t >>= (q => Leaf(renameMap(q))))
    }

    new NonDetTransducer(renamedStates, sigma, renamedInitialState, renamedDelta)
  }

  def determinize(): DetTransducer[(Q,Int), Either[(Q,Int),A]] = {
    def addIndex(t: Tree[Q]): Tree[(Q,Int)] = {
      t match {
        case Leaf(q) => Leaf((q,1))
        case Success => Success
        case Fail => Fail
        case Or(l,r) => Or(addIndex(l), addIndex(r))
        case Lft(l) => Lft(addIndex(l))
      }
    }

    val deltaSet = delta.groupBy{ case (q,a,_) =>
      Analysis.checkInterrupted("construct deterministic transducer")
      (q,a)
    }.mapValues{ ds =>
      Analysis.checkInterrupted("construct deterministic transducer")
      ds.map(_._3)
    }

    val k = deltaSet.values.map(_.length).max
    val statesDet = for (state <- states; i <- 1 to k) yield (state, i)
    val sigmaLeft = for (
      state <- states;
      i <- 1 to k
    ) yield (Left((state, i)): Either[(Q,Int),A])
    val sigmaDet = sigmaLeft | sigma.map(Right(_))
    val deltaDet = deltaSet.flatMap{ case ((q,a), ts) =>
      Analysis.checkInterrupted("construct deterministic transducer")
      ts.padTo(k, ts.last).zip(1 to k).map{ case (t,i) =>
        ((q,i), a.map(Right(_))) -> addIndex(t)
      }
    } ++ (for (
      s @ (q1,i) <- statesDet;
      a @ Left((q2,j)) <- sigmaLeft
    ) yield (s,Some(a)) -> Leaf(if (q1 == q2) (q2,j) else s))

    new DetTransducer(statesDet, sigmaDet, (initialState, 1), deltaDet)
  }

  def calcGrowthRate(): (Option[Int], Witness[A]) = {
    val detTransducer = Debug.time("nondeterministic transducer -> deterministic transducer") {
      determinize()
    }.rename()

    val detTotalTransducer = Debug.time("deterministic transducer -> deterministic total transducer") {
      detTransducer.totalize()
    }.rename()

    val (growthRate, _) = detTotalTransducer.calcGrowthRate()
    (growthRate, Witness.empty)
  }
}

class DetTransducer[Q,A](
  states: Set[Q],
  sigma: Set[A],
  initialState: Q,
  val deltaDet: Map[(Q,Option[A]), Tree[Q]]
) extends NonDetTransducer(
  states,
  sigma,
  initialState,
  deltaDet.map{ case ((q1,a),t) =>
    Analysis.checkInterrupted("construct transducer")
    (q1,a,t)
  }.toSeq,
) {
  override def rename(): DetTransducer[Int,A] = {
    val renameMap = states.zipWithIndex.toMap
    val renamedStates = states.map(renameMap)
    val renamedInitialState = renameMap(initialState)
    val renamedDelta = deltaDet.map{ case ((q,a), t) =>
      (renameMap(q),a) -> (t >>= (q => Leaf(renameMap(q))))
    }

    new DetTransducer(renamedStates, sigma, renamedInitialState, renamedDelta)
  }

  def deltaHat(qs: Set[Q], a: Option[A]): Set[Q] = {
    qs.collect{ case q if deltaDet.isDefinedAt((q,a)) =>
      flat(deltaDet((q,a)))
    }.flatten
  }

  def toNFA(): NFA[Option[Q],A] = {
    val statesNFA = states.map(Option(_)) + None
    var deltaNFA = Seq[(Option[Q],A,Option[Q])]()
    deltaDet.collect{
      case ((q,Some(a)),t) =>
        flat(t).map(q1 => deltaNFA +:= ((Some(q),a,Some(q1))))
        if (hasSuccess(t)) deltaNFA +:= ((Some(q),a,None))
    }
    sigma.foreach(a => deltaNFA +:= ((None,a,None)))
    val initialStates = Set(Some(initialState): Option[Q])
    val finalStates = statesNFA.filter{
      case Some(q) => hasSuccess(deltaDet((q,None)))
      case None => true
    }

    new NFA(statesNFA, sigma, deltaNFA, initialStates, finalStates)
  }

  def totalize(): DetTransducer[(Q,Set[Q]), A] = {
    var decided = Map[Set[Q], Boolean]()

    def isDomainNonEmpty(qs: Set[Q]): Boolean = {
      var visited = Set(qs)
      val queue = Queue(qs)
      var dependency = Seq[(Set[Q],Set[Q])]()

      var hasFailPathNode: Option[Set[Q]] = None

      while (hasFailPathNode.isEmpty && queue.nonEmpty) {
        Analysis.checkInterrupted("deterministic transducer -> deterministic total transducer")
        val qs = queue.dequeue
        decided.get(qs) match {
          case Some(true) => hasFailPathNode = Some(qs)
          case Some(false) => // NOP
          case None =>
            if (qs.forall(q => deltaDet.isDefinedAt((q,None)))) {
              hasFailPathNode = Some(qs)
            } else {
              sigma.map(Some(_)).collect{ case a
                if (qs.forall(q => deltaDet.isDefinedAt((q,a)))) =>
                  val next = deltaHat(qs,a)
                  dependency +:= ((next,qs))
                  if (!visited(next)) {
                    visited += next
                    queue.enqueue(next)
                  }
              }
            }
        }
      }

      val graph = new Graph(dependency)
      hasFailPathNode match {
        case Some(v) =>
          graph.reachableFrom(v).foreach(decided += _ -> true)
          true
        case None =>
          graph.nodes.foreach(decided += _ -> false)
          false
      }
    }

    def addSet(t: Tree[Q], qs: Set[Q]): Tree[(Q,Set[Q])] = {
      t match {
        case Leaf(q) => Leaf((q,qs))
        case Success => Success
        case Fail => Fail
        case Or(l,r) => Or(addSet(l,qs), addSet(r,qs))
        case Lft(l) => Lft(addSet(l,qs))
      }
    }

    val newInitial = (initialState, Set(initialState))
    var newStates = Set(newInitial)
    val stack = Stack(newInitial)
    var newDelta = Map[((Q,Set[Q]),Option[A]), Tree[(Q,Set[Q])]]()
    while (stack.nonEmpty) {
      Analysis.checkInterrupted("constructing deterministic total transducer")
      val p @ (q,qs) = stack.pop
      (sigma.map(Option(_)) + None).foreach{ a =>
        val t = if (deltaDet.isDefinedAt((q,a)) && isDomainNonEmpty(qs)) {
          addSet(deltaDet((q,a)), deltaHat(qs,a))
        } else {
          Fail
        }
        newDelta += (p,a) -> t
        flat(t).foreach( p =>
          if (!newStates.contains(p)) {
            newStates += p
            stack.push(p)
          }
        )
      }
    }

    new DetTransducer(newStates, sigma, newInitial, newDelta)
  }

  override def calcGrowthRate(): (Option[Int], Witness[A]) = {
    def toDT0L(): DT0L[A,Q] = {
      val morphs = sigma.map{ a =>
        Analysis.checkInterrupted("transducer -> DT0L")
        a -> states.map( q =>
          q -> flat(deltaDet((q,Some(a))))
        ).toMap
      }.toMap

      new DT0L(states, morphs)
    }

    val dt0l = Debug.time("transducer -> DT0L") {
      toDT0L()
    }

    val (growthRate, witness, _) = dt0l.calcGrowthRate(initialState)

    witness.separators :+= Seq()

    (growthRate.map(_+1), witness)
  }

  def calcGrowthRateBacktrack(method: BacktrackMethod): (Option[Int], Witness[A]) = {
    def calcBtrGrowthRateLookahead(): (Option[Int], Witness[A]) = {
      def toTransducerWithLA(): TransducerWithLA[Q,A,Set[Option[Q]]] = {
        val lookaheadDFA = toNFA().reverse().toDFA()

        var deltaLA = Map[(Q,Option[(A,Set[Option[Q]])]), Tree[Q]]()
        deltaDet.foreach{
          case ((q,Some(a)),t) =>
            Analysis.checkInterrupted("transducer -> transducer with lookahead")
            lookaheadDFA.states.foreach( p =>
              deltaLA += (q,Some((a,p))) -> cut(deltaDet((q,Some(a))), p.flatten)
            )
          case ((q,None),t) =>
            deltaLA += (q,None) -> cut(deltaDet((q,None)))
        }

        new TransducerWithLA(states, sigma, initialState, deltaLA, lookaheadDFA)
      }

      val transducerWithLA = Debug.time("transducer -> transducer with lookahead") {
        toTransducerWithLA()
      }.rename()

      transducerWithLA.calcGrowthRate()
    }

    def calcBtrGrowthRateEnsureFail(): (Option[Int], Witness[A]) = {
      def toEnsureFailTransducer(): DetTransducer[(Q,Set[Q]),A] = {
        var decided = Map[Set[Q], Boolean]()

        def ensureFail(t: Tree[Q], qs: Option[Set[Q]]): Tree[(Q,Set[Q])] = {
          def hasFailPath(qs: Set[Q]): Boolean = {
            var visited = Set(qs)
            val queue = Queue(qs)
            var dependency = Seq[(Set[Q],Set[Q])]()

            var hasFailPathNode: Option[Set[Q]] = None

            while (hasFailPathNode.isEmpty && queue.nonEmpty) {
              Analysis.checkInterrupted("constructing ensure fail transducer")
              val qs = queue.dequeue
              decided.get(qs) match {
                case Some(true) => hasFailPathNode = Some(qs)
                case Some(false) => // NOP
                case None =>
                  if (qs.forall(q => !hasSuccess(deltaDet((q,None))))) {
                    hasFailPathNode = Some(qs)
                  } else {
                    sigma.map(Some(_)).collect{ case a
                      if (qs.forall(q => !hasSuccess(deltaDet((q,a))))) =>
                        val next = deltaHat(qs,a)
                        dependency +:= ((next,qs))
                        if (!visited(next)) {
                          visited += next
                          queue.enqueue(next)
                        }
                    }
                  }
              }
            }

            val graph = new Graph(dependency)
            hasFailPathNode match {
              case Some(v) =>
                graph.reachableFrom(v).foreach(decided += _ -> true)
                true
              case None =>
                graph.nodes.foreach(decided += _ -> false)
                false
            }
          }

          t match {
            case Leaf(q) => qs match {
              case Some(qs) => if (hasFailPath(qs)) Leaf((q,qs)) else Fail
              case None => Fail
            }
            case Success => Success
            case Fail => Fail
            case Or(l,r) => qs match {
              case Some(_) =>
                Or(ensureFail(l,qs), if (hasSuccess(l)) {
                  ensureFail(r,None)
                } else {
                  ensureFail(r,qs.map(_ ++ flat(l)))
                })
              case None => Or(ensureFail(l,None), ensureFail(r,None))
            }
            case Lft(l) => Lft(ensureFail(l,qs))
          }
        }

        val newInitial = (initialState, Set[Q]())
        var newStates = Set(newInitial)
        val stack = Stack(newInitial)
        var newDelta = Map[((Q,Set[Q]),Option[A]), Tree[(Q,Set[Q])]]()
        while (stack.nonEmpty) {
          Analysis.checkInterrupted("constructing ensure fail transducer")
          val p @ (q,qs) = stack.pop
          (sigma.map(Option(_)) + None).foreach{ a =>
            val t = ensureFail(deltaDet((q,a)), Some(deltaHat(qs,a)))
            newDelta += (p,a) -> t
            flat(t).foreach( p =>
              if (!newStates.contains(p)) {
                newStates += p
                stack.push(p)
              }
            )
          }
        }

        new DetTransducer(newStates, sigma, newInitial, newDelta)
      }

      val newTransducer = Debug.time("transducer -> ensure fail transducer") {
        toEnsureFailTransducer()
      }.rename()

      val (growthRate, _) = newTransducer.calcGrowthRate()
      (growthRate, Witness.empty)
    }

    def calcBtrGrowthRateNondeterminism(): (Option[Int], Witness[A]) = {
      def toNonDetTransducer(): NonDetTransducer[Option[(Q,Boolean)], A] = {
        def cutTrees(t: Tree[Q], b: Boolean): Set[Tree[Option[(Q,Boolean)]]] = {
          t match {
            case Leaf(q) => Set(Leaf(Some((q,b))))
            case Success => if (b) Set(Success) else Set()
            case Fail => if (b) Set() else Set(Fail)
            case Or(l,r) =>
              if (b) {
                cutTrees(l,true).map(Lft(_): Tree[Option[(Q,Boolean)]]) |
                (for (fl <- cutTrees(l,false); fr <- cutTrees(r,true)) yield Or(fl,fr))
              } else {
                for (fl <- cutTrees(l,false); fr <- cutTrees(r,false)) yield Or(fl,fr)
              }
            case Lft(l) => cutTrees(l,b).map(Lft(_))
          }
        }

        val statesNonDet = (for (
          state <- states;
          b <- Seq(true, false)
        ) yield (state, b)).map(Option(_)) + None
        val initialStateNonDet = None
        var delta = Seq[(Option[(Q,Boolean)], Option[A], Tree[Option[(Q,Boolean)]])]()
        deltaDet.foreach{ case ((q,a), t) =>
          Analysis.checkInterrupted("constructing nondeterministic transducer")
          val sts = cutTrees(t, true)
          val fts = cutTrees(t, false)
          sts.foreach(t => delta +:= ((Some((q,true)),a,t)))
          fts.foreach(t => delta +:= ((Some((q,false)),a,t)))
          if (q == initialState) {
            sts.foreach(t => delta +:= ((None,a,t)))
            fts.foreach(t => delta +:= ((None,a,t)))
          }
        }

        new NonDetTransducer(statesNonDet, sigma, initialStateNonDet, delta)
      }

      val nonDetTransducer = Debug.time("transducer -> nondeterministic transducer") {
        toNonDetTransducer()
      }.rename()

      nonDetTransducer.calcGrowthRate()
    }

    method match {
      case Lookahead => calcBtrGrowthRateLookahead()
      case EnsureFail => calcBtrGrowthRateEnsureFail()
      case Nondeterminism => calcBtrGrowthRateNondeterminism()
    }
  }
}


class TransducerWithLA[Q,A,P](
  val states: Set[Q],
  val sigma: Set[A],
  val initialState: Q,
  val delta: Map[(Q,Option[(A,P)]), Tree[Q]],
  val lookaheadDFA: DFA[P,A]
) {
  def rename(): TransducerWithLA[Q,A,Int] = {
    val renameMap = lookaheadDFA.states.zipWithIndex.toMap
    val renamedDelta = delta.map{
      case ((q,Some((a,p))), t) => (q,Some((a,renameMap(p)))) -> t
      case ((q,None), t) => (q,None) -> t
    }

    val renamedDFAStates = lookaheadDFA.states.map(renameMap)
    val renamedDeltaDet = lookaheadDFA.deltaDet.map{
      case ((p1,a),p2) => (renameMap(p1),a) -> renameMap(p2)
    }
    val renamedDFAInitialState = renameMap(lookaheadDFA.initialState)
    val renamedDFAFinalStates = lookaheadDFA.finalStates.map(renameMap)

    val renamedDFA = new DFA(
      renamedDFAStates,
      sigma,
      renamedDeltaDet,
      renamedDFAInitialState,
      renamedDFAFinalStates
    )

    new TransducerWithLA(states, sigma, initialState, renamedDelta, renamedDFA)
  }

  def calcGrowthRate(): (Option[Int], Witness[A]) = {
    def toIndexedDT0L(): IndexedDT0L[A,Q,P] = {
      val pairToTrans = lookaheadDFA.delta.groupBy{
        case (p1,_,p2) => (p1,p2)
      }.mapValues(_.map(_._2)).withDefaultValue(Seq())

      val indexedMorphs = lookaheadDFA.states.flatMap{ p1 =>
        Analysis.checkInterrupted("transducer with lookahead -> indexed DT0L")
        lookaheadDFA.states.map( p2 =>
          (p1,p2) -> pairToTrans((p2,p1)).map( a =>
            a -> states.map( q =>
              q -> flat(delta((q,Some((a,p2)))))
            ).toMap
          ).toMap
        )
      }.toMap

      new IndexedDT0L(states, lookaheadDFA.states, indexedMorphs)
    }

    Debug.info("lookahead DFA info") {
      ("number of states", lookaheadDFA.states.size)
    }

    val indexedDT0L = Debug.time("transducer with lookahead -> indexed DT0L") {
      toIndexedDT0L()
    }

    val (growthRate, witness, last) = indexedDT0L.calcGrowthRate(
      lookaheadDFA.states.map((initialState,_)),
      lookaheadDFA
    )

    if (last.isDefined) {
      witness.separators :+= lookaheadDFA.getPath(lookaheadDFA.initialState, last.get).get.reverse
    }

    (growthRate.map(_+1), witness)
  }
}
