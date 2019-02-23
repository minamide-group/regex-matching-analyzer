package matching.tool

import matching.transition.{Graph, NFA}

object Visualizer {
  def esc(a: Any): String = {
    a.toString.map{
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c => c
    }.mkString
  }

  implicit class GraphVisualizer[V](g: Graph[V]) {
    protected def visualizeNodes(file: File, renameMap: Map[V,Int]) {
      g.nodes.foreach(v =>
        file.writeln(s""""${renameMap(v)}" [label = "${esc(v)}"];""", 1)
      )
      file.writeln()
    }

    protected def visualizeEdges(file: File, renameMap: Map[V,Int]) {
      g.edges.foreach{ case (v1,v2) =>
        file.writeln(s""""${renameMap(v1)}" -> "${renameMap(v2)}";""", 1)
      }
    }

    def visualize(name: String) {
      val file = File.makeFile(s"${name}.dot")
      val renameMap = g.nodes.zipWithIndex.toMap

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

  implicit class NFAVisualizer[Q,A](nfa: NFA[Q,A]) extends GraphVisualizer[Q](nfa) {
    override protected def visualizeNodes(file: File, renameMap: Map[Q,Int]) {
      file.writeln("\"initial\" [", 1)
      file.writeln("label = \"\",", 2)
      file.writeln("shape = none,", 2)
      file.writeln("fixedsize = true,", 2)
      file.writeln("width = 0,", 2)
      file.writeln("height = 0", 2)
      file.writeln("];", 1)

      nfa.states.foreach{ state =>
        if (nfa.finalStates.contains(state)) {
          file.writeln(s""""${renameMap(state)}" [label = "${esc(state)}", shape = doublecircle];""", 1)
        } else {
          file.writeln(s""""${renameMap(state)}" [label = "${esc(state)}"];""", 1)
        }
      }
    }

    override protected def visualizeEdges(file: File, renameMap: Map[Q,Int]) {
      nfa.initialStates.foreach{ initialState =>
        file.writeln(s""""initial" -> "${renameMap(initialState)}";""", 1)
      }

      nfa.delta.foreach{ case (q1,a,q2) =>
        file.writeln(s""""${renameMap(q1)}" -> "${renameMap(q2)}" [label = "${esc(a)}"];""", 1)
      }
    }
  }
}
