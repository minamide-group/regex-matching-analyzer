package matching.transition

import scala.collection.mutable.{Queue, Stack}
import matching.tool.Analysis

class Graph[V](
  val nodes: Set[V],
  val edges: Seq[(V,V)]
) {
  def this(edges: Seq[(V,V)]) {
    this(edges.flatMap{case (v1,v2) => Set(v1,v2)}.toSet, edges)
  }

  lazy val adj = edges.groupBy(_._1).mapValues(
    _.map(_._2)
  ).withDefaultValue(Seq())

  def reachableFrom(v: V): Set[V] = {
    var visited = Set[V]()
    val stack = Stack(v)

    while (stack.nonEmpty) {
      val v = stack.pop
      if (!visited(v)) {
        visited += v
        stack.pushAll(adj(v).distinct)
      }
    }

    visited
  }

  /**
   *  require: this graph is DAG.
   */
  def reachableMapDAG(): Map[V,Set[V]] = {
    var m = Map[V,Set[V]]()
    def reachableFromDAG(v: V): Set[V] = {
      m.get(v) match {
        case Some(vs) => vs
        case None =>
          val vs = adj(v).toSet.flatMap(reachableFromDAG) + v
          m += v -> vs
          vs
      }
    }

    nodes.foreach(reachableFromDAG)
    m
  }

  def reverse(): Graph[V] = {
    new Graph(nodes, edges.map{case (v1,v2) => (v2,v1)})
  }

  def calcStrongComponents(): Set[Set[V]] = {
    def dfs(g: Graph[V], order: Seq[V]): Seq[Seq[V]] = {
      var visited = Set[V]()
      def dfs(v: V): Seq[V] = {
        var postRev = Seq[V]()
        val stack = Stack(Seq(v))

        while (stack.nonEmpty) {
          Analysis.checkInterrupted("calculate strong components")
          stack.pop match {
            case vs @ (v +: rest) =>
              if (!visited(v)) {
                visited += v
                stack.push(vs)
                stack.push(g.adj(v).distinct)
              } else {
                stack.push(rest)
              }
            case Nil =>
              if (stack.nonEmpty) {
                val vs = stack.pop
                postRev +:= vs.head
                stack.push(vs.tail)
              }
          }
        }

        postRev
      }

      var postRevs = Seq[Seq[V]]()
      order.foreach(v => if (!visited(v)) postRevs +:= dfs(v))
      postRevs
    }

    dfs(reverse(), dfs(this, nodes.toSeq).flatten).map(_.toSet).toSet
  }
}


class LabeledGraph[V,A](
  nodes: Set[V],
  val labeledEdges: Seq[(V,A,V)]
) extends Graph(nodes, labeledEdges.map{case (v1,_,v2) => (v1,v2)}) {
  def this(labeledEdges: Seq[(V,A,V)]) {
    this(labeledEdges.flatMap{case (v1,_,v2) => Set(v1,v2)}.toSet, labeledEdges)
  }

  lazy val labeledAdj = labeledEdges.groupBy(_._1).mapValues(
    _.groupBy(_._2).mapValues(
      _.map(_._3)
    ).withDefaultValue(Seq())
  ).withDefaultValue(Map().withDefaultValue(Seq[V]()))

  def reachablePartFrom(vs: Set[V]): LabeledGraph[V,A] = {
    val reachableNodes = vs.flatMap(reachableFrom)
    new LabeledGraph(
      reachableNodes,
      labeledEdges.filter{case (q1,a,q2) => reachableNodes(q1) && reachableNodes(q2)}
    )
  }

  def getPath(srcs: Set[V], dest: V): Option[Seq[A]] = {
    var visited = srcs
    val queue = Queue[V]()
    var path = Map[V,Seq[A]]()
    srcs.foreach{ src =>
      path += src -> Seq()
      queue.enqueue(src)
    }
    while (!path.isDefinedAt(dest) && queue.nonEmpty) {
      val v1 = queue.dequeue
      labeledAdj(v1).foreach{ case (a,vs) =>
        vs.distinct.foreach{ v2 =>
          if (!visited(v2)) {
            visited += v2
            queue.enqueue(v2)
            path += v2 -> (path(v1) :+ a)
          }
        }
      }
    }

    path.get(dest)
  }

  def getPath(src: V, dest: V): Option[Seq[A]] = getPath(Set(src), dest)
}
