package matching.transition

import scala.collection.mutable.Stack
import matching.monad.Tree
import matching.monad.Tree._
import matching.monad._
import matching.monad.Monad._
import matching.tool.{Analysis, Debug}

class Transducer[Q,A](
  val states: Set[Q],
  val sigma: Set[A],
  val initialState: Q,
  val delta: Map[(Q,Option[A]), Tree[Q]]
) {
  def rename(): Transducer[Int,A] = {
    val renameMap = states.zipWithIndex.toMap
    val renamedStates = states.map(renameMap)
    val renamedInitialState = renameMap(initialState)
    val renamedDelta = delta.map{ case ((q,a), t) =>
      (renameMap(q),a) -> (t >>= (q => Leaf(renameMap(q))))
    }

    new Transducer(renamedStates, sigma, renamedInitialState, renamedDelta)
  }

  def calcGrowthRate(): (Option[Int], Witness[A]) = {
    def toDT0L(): DT0L[A,Q] = {
      val morphs = sigma.map( a =>
        a -> states.map( q =>
          q -> flat(delta((q,Some(a))))
        ).toMap
      ).toMap

      new DT0L(morphs)
    }

    val dt0l = toDT0L()
    val (growthRate, witness, _) = dt0l.calcGrowthRate(initialState)

    witness.separators :+= Seq()

    (growthRate.map(_+1), witness)
  }

  def calcGrowthRateBacktrack(method: BacktrackMethod): (Option[Int], Witness[A]) = {
    def calcBtrGrowthRateLookAhead(): (Option[Int], Witness[A]) = {
      def toTransducerWithLA(): TransducerWithLA[Q,A,Set[Option[Q]]] = {
        def toNFA(): NFA[Option[Q],A] = {
          val statesNFA = states.map(Option(_)) + None
          var deltaNFA = Seq[(Option[Q],A,Option[Q])]()
          delta.collect{
            case ((q,Some(a)),t) =>
              flat(t).map(q1 => deltaNFA +:= ((Some(q),a,Some(q1))))
              if (hasSuccess(t)) deltaNFA +:= ((Some(q),a,None))
          }
          sigma.foreach(a => deltaNFA +:= ((None,a,None)))
          val initialStates = Set(Some(initialState): Option[Q])
          val finalStates = statesNFA.filter{
            case Some(q) => hasSuccess(delta((q,None)))
            case None => true
          }

          new NFA(statesNFA, sigma, deltaNFA, initialStates, finalStates)
        }

        val lookaheadDFA = toNFA().reverse().toDFA()

        var deltaLA = Map[(Q,Option[(A,Set[Option[Q]])]), Tree[Q]]()
        delta.foreach{
          case ((q,Some(a)),t) =>
            Analysis.checkInterrupted("transducer -> transducer with lookahead")
            lookaheadDFA.states.foreach( p =>
              deltaLA += (q,Some((a,p))) -> cut(delta((q,Some(a))), p.flatten)
            )
          case ((q,None),t) =>
            deltaLA += (q,None) -> cut(delta((q,None)))
        }

        new TransducerWithLA(states, sigma, initialState, deltaLA, lookaheadDFA)
      }

      val transducerWithLA = Debug.time("transducer -> transducer with lookahead") {
        toTransducerWithLA().rename()
      }

      transducerWithLA.calcGrowthRate()
    }

    def calcBtrGrowthRateEnsureFail(): (Option[Int], Witness[A]) = {
      def ensureFail(t: Tree[Q], qs: Option[Set[Q]]): Tree[(Q,Set[Q])] = {
        def hasFailPath(qs: Set[Q]): Boolean = {
          ???
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
          val t = ensureFail(delta((q,a)), Some(qs.flatMap(q => flat(delta((q,a))))))
          newDelta += (p,a) -> t
          flat(t).foreach( p =>
            if (!newStates.contains(p)) {
              newStates += p
              stack.push(p)
            }
          )
        }
      }

      val newTransducer = new Transducer(newStates, sigma, newInitial, newDelta)
      newTransducer.calcGrowthRate()
    }

    method match {
      case LookAhead => calcBtrGrowthRateLookAhead()
      case EnsureFail => calcBtrGrowthRateEnsureFail()
      case Nondeterminism => ???
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

      new IndexedDT0L(indexedMorphs)
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
