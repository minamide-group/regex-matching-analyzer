package nfa

import tool.{File, Command}

class NFA[Q,A](
  val states: Seq[Q],
  val sigma: Seq[A],
  val delta: Seq[(Q,A,Q)],
  val initialStates: Seq[Q],
  val finalStates: Seq[Q]
) {
  var delta_set = states.flatMap(q => sigma.map(a => (q,a) -> Seq[Q]())).toMap
  delta.foreach{case (q1,a,q2) => delta_set += (q1,a) -> (q2 +: delta_set((q1,a)))}

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
    states.foreach(q =>
      file.writeln(s""""${q}";""", 1)
    )
    file.writeln()

    /* egde */
    delta.foreach{ case (q1,a,q2) =>
      file.writeln(s""""${q1}" -> "${q2}" [label = "${a}"];""", 1)
    }

    file.writeln("}")
    file.close()

    val _ = Command.exec(s"dot -T pdf ${name}.dot -o ${name}.pdf")
  }
}
