package matching.transition

import scala.collection.mutable.{Stack, Map => MTMap}
import matching.tool.{File, Command, Debug}

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

    Debug.time("calculate strong components", false) {
      dfs(reverse(), dfs(this, nodes).flatten).map(_.toSet).toSet
    }
  }

  protected def visualizeNodes(file: File, renameMap: Map[V,Int]) {
    nodes.foreach(v =>
      file.writeln(s""""${v}";""", 1)
    )
    file.writeln()
  }

  protected def visualizeEdges(file: File, renameMap: Map[V,Int]) {
    edges.foreach{ case (v1,v2) =>
      file.writeln(s""""${v1}" -> "${v2}";""", 1)
    }
  }

  def visualize(name: String) {
    val file = File.makeFile(s"${name}.dot")
    val renameMap = nodes.zipWithIndex.toMap

    file.writeln(s"digraph ${name.replace("/","_")} {")

    file.writeln("graph [", 1)
    file.writeln("rankdir = LR", 2)
    file.writeln("];", 1)
    file.writeln()

    file.writeln("node [", 1)
    file.writeln("shape = circle", 2)
    file.writeln("];", 1)
    file.writeln()

    visualizeNodes(file, renameMap)
    file.writeln()

    visualizeEdges(file, renameMap)
    file.writeln()

    file.writeln("}")

    file.close()

    Command.exec(s"dot -T pdf ${name}.dot -o ${name}.pdf")
  }
}
