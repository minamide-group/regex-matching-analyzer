package matching.transition

import scala.collection.mutable.{Stack, Map => MTMap}
import matching.tool.Analysis

class Graph[V](
  val nodes: Seq[V],
  val edges: Seq[(V,V)]
) {
  val adj = MTMap[V, Seq[V]]().withDefaultValue(Seq())
  edges.foreach{e => adj(e._1) +:= e._2}

  def this(edges: Seq[(V,V)]) {
    this(edges.flatMap{case (v1,v2) => Seq(v1,v2)}.distinct, edges)
  }

  def reachableFrom(v: V): Set[V] = {
    var visited = Set[V]()
    val stack = Stack(v)

    while (stack.nonEmpty) {
      val v = stack.pop
      if (!visited(v)) {
        visited += v
        stack.pushAll(adj(v))
      }
    }

    visited
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
          Analysis.checkInterrupted("calculating strong components")
          stack.pop match {
            case vs @ (v +: rest) =>
              if (!visited(v)) {
                visited += v
                stack.push(vs)
                stack.push(g.adj(v).toSeq)
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

    dfs(reverse(), dfs(this, nodes).flatten).map(_.toSet).toSet
  }
}
