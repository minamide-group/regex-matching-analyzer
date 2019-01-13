package transition

import scala.collection.mutable.{Stack, Map => MTMap}
import tool.{File, Command, Debug}

class Graph[V](
  val nodes: Seq[V],
  val edges: Seq[(V,V)]
) {
  def this(edges: Seq[(V,V)]) {
    this(edges.flatMap{case (v1,v2) => Seq(v1,v2)}.distinct, edges)
  }

  def reverse(): Graph[V] = {
    Debug.time("calculate reverse") {
      new Graph(nodes, edges.map{case (v1,v2) => (v2,v1)})
    }
  }

  def calcStrongComponents(): Set[Set[V]] = {
    def dfs(g: Graph[V], order: Seq[V]): Seq[Seq[V]] = {
      val adj = MTMap[V, Seq[V]]()
      Debug.time("calculate adj") {
        g.nodes.foreach(adj(_) = Seq[V]())
        g.edges.foreach{e => adj(e._1) +:= e._2}
      }

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
      dfs(reverse(), dfs(this, nodes).flatten).map(_.toSet).toSet
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
) extends Graph[V](nodes, labeledEdges.map{case (v1,_,v2) => (v1,v2)}.distinct) {
  val label2Edges = labeledEdges.groupBy(_._2).mapValues(_.map{case (v1,_,v2) => (v1,v2)})

  def calcAmbiguity(): Option[Int] = {
    val scs = calcStrongComponents()
    val scsEdges = edges.map{ case (v1,v2) =>
      (scs.find(_.contains(v1)).get, scs.find(_.contains(v2)).get)
    }.filter{case (v1,v2) => v1 != v2}
    val scsGraph = new Graph(scs.toSeq, scsEdges)

    def isEDA(): Boolean = {
      def constructG2(sc: Set[V]): Graph[(V,V)] = {
        val label2EdgesSc = label2Edges.mapValues(_.filter{ case (v1,v2) =>
          sc.contains(v1) && sc.contains(v2)
        })
        val g2Edges = label2EdgesSc.toSeq.flatMap{ case (a,es) =>
          for ((p1,q1) <- es; (p2,q2) <- es) yield ((p1,p2), (q1,q2))
        }.distinct
        new Graph(g2Edges)
      }

      scsGraph.nodes.exists{ sc =>
        constructG2(sc).calcStrongComponents().find(_.exists{case (v1,v2) => v1 == v2}) match {
          case Some(g2sc) => g2sc.exists{case (v1,v2) => v1 != v2}
          case None => false
        }
      }
    }

    def calcDegree(): Int = {
      def isIDA(): Boolean = {
        def constructG4(): Graph[(V,V,V)] = {
          def triplize[E](es: Seq[E]): Seq[(E,E,E)] = {
            es.flatMap(e1 => es.flatMap(e2 => es.map(e3 => (e1,e2,e3))))
          }

          val g4nodes = triplize(nodes)
          val e4 = (label2Edges.toSeq.flatMap{ case (a,es) =>
              triplize(es).map{case ((p1,q1),(p2,q2),(p3,q3)) => ((p1,p2,p3),(q1,q2,q3))}
            } ++ nodes.flatMap(p =>
              nodes.collect{case q if p != q => ((p,q,q),(p,p,q))}
            )).distinct


          new Graph(g4nodes, e4)
        }

        val g4 = Debug.time("construct G4") {
          constructG4()
        }
        Debug.info("G4 info")(
          ("|V|", g4.nodes.size),
          ("|E|", g4.edges.size)
        )

        val scs = g4.calcStrongComponents()

        scs.exists{sc =>
          sc.collect{
            case (p1,p2,p3) if p2 == p3 && p1 != p2 => (p1,p2)
          }.exists{
            case (p,q) => sc.exists{case (q1,q2,q3) => q1 == p && q2 == p && q3 == q}
          }
        }
      }

      if (isIDA()) 1 else 0
    }

    if (isEDA()) None else Some(calcDegree())
  }
}
