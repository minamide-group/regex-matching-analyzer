package transition

import scala.collection.mutable.{Stack, Map => MTMap}
import tool.{File, Command, Debug}

class Graph[V](
  val nodes: Seq[V],
  val edges: Seq[(V,V)]
) {
  def reverse(): Graph[V] = {
    Debug.time("calculate reverse") {
      new Graph(nodes, edges.map{case (v1,v2) => (v2,v1)})
    }
  }

  def calcStrongComponents(): Seq[Seq[V]] = {
    def dfs(g: Graph[V], order: Seq[V]): Seq[Seq[V]] = {
      val adj = MTMap[V, Seq[V]]()
      Debug.time("calculate adj") {
        g.nodes.foreach(adj(_) = Seq[V]())
        g.edges.foreach{e => adj(e._1) +:= e._2}
      }

      val visited = MTMap[V, Boolean]()
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
                stack.push(adj(v).toSeq)
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

    Debug.time("calculate strong components") {
      dfs(reverse(), dfs(this, nodes).flatten)
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
  val labeledEdges: Seq[(V,A,V)],
) extends Graph[V](nodes, labeledEdges.map{case (v1,_,v2) => (v1,v2)}) {
  def toG4(): Graph[(V,V,V)] = {
    def triplize[E](es: Seq[E]): Seq[(E,E,E)] = {
      es.flatMap(e1 => es.flatMap(e2 => es.map(e3 => (e1,e2,e3))))
    }

    val label2Edges = labeledEdges.groupBy(_._2).mapValues(_.map{case (v1,_,v2) => (v1,v2)})

    val g4nodes = triplize(nodes)
    val e4 = Debug.time("construct E3") {
      (label2Edges.toSeq.flatMap{ case (a,es) =>
        triplize(es).map{case ((p1,q1),(p2,q2),(p3,q3)) => ((p1,p2,p3),(q1,q2,q3))}
      } ++ nodes.flatMap(p =>
        nodes.collect{case q if p != q => ((p,q,q),(p,p,q))}
      )).distinct
    }

    new Graph(g4nodes, e4)
  }
}
