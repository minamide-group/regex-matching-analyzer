package transition

object Morph {
  type Morph[A] = Map[A,Seq[A]]

  def morphs2Graph[A](morphs: Seq[Morph[A]]): LabeledGraph[A,Int] = {
    val nodes = morphs.map(_.keys).flatten.distinct
    val labeledEdges = morphs.zipWithIndex.flatMap{ case (h,idx) =>
      h.flatMap{case (q,qs) => qs.map((q,idx+1,_))}
    }

    new LabeledGraph(nodes, labeledEdges)
  }
}
