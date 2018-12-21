package graph

import scala.collection.mutable.{Stack, Map => MTMap}

import tool.{File, Command, Debug}

class Graph[V](
  val nodes: Seq[V],
  val edges: Seq[(V,V)]
) {
  val adj = MTMap[V, Seq[V]]()
  Debug.time("calculate adj") {
    nodes.foreach(adj(_) = Seq[V]())
    edges.foreach{e => adj(e._1) +:= e._2}
  }

  def reverse(): Graph[V] = {
    Debug.time("calculate reverse") {
      new Graph(nodes, edges.map{case (v1,v2) => (v2,v1)})
    }
  }

  def calcStrongComponents(): Seq[Seq[V]] = {
    def dfs(g: Graph[V], order: Seq[V]): Seq[Seq[V]] = {
      var visited = MTMap[V, Boolean]()
      g.nodes.foreach(visited(_) = false)
      def dfs(v: V): Seq[V] = {
        var postRev = Seq[V]()
        val stack = Stack(Seq(v))

        while (stack.nonEmpty) {
          stack.pop match {
            case vs @ (v +: rest) =>
              if (!visited(v)) {
                visited(v) = true
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

    val r = reverse()

    Debug.time("calculate strong components") {
      dfs(r, dfs(this, nodes).flatten)
    }
  }

  def visualize(name: String) = {
    val file = File.makeFile(s"${name}.dot")

    file.writeln(s"digraph ${name.replace("/","_")} {")

    /* graph config */
    file.writeln("graph [", 1)
    file.writeln("rankdir = LR", 2)
    file.writeln("];", 1)
    file.writeln()

    /* node config */
    file.writeln("node [", 1)
    file.writeln("shape = circle", 2)
    file.writeln("];", 1)
    file.writeln()

    /* node */
    nodes.foreach(v =>
      file.writeln(s""""${v}";""", 1)
    )
    file.writeln()

    /* egde */
    edges.foreach{ case (v1,v2) =>
      file.writeln(s""""${v1}" -> "${v2}";""", 1)
    }

    file.writeln("}")
    file.close()

    val _ = Command.exec(s"dot -T pdf ${name}.dot -o ${name}.pdf")
  }
}

class LabeledGraph[V,A](
  nodes: Seq[V],
  edges: Seq[(V,V)],
  val label: Map[(V,V),A]
) extends Graph[V](nodes, edges) {
  
}
