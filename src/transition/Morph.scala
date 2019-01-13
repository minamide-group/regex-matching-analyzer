package transition

object Morph {
  type Morph[A] = Map[A,Seq[A]]

  def morphs2Graph[A](morphs: Seq[Morph[A]]): LabeledGraph[A,Int] = {
    val nodes = morphs.flatMap(_.keys).distinct
    val labeledEdges = morphs.zipWithIndex.flatMap{ case (h,idx) =>
      h.flatMap{case (q,qs) => qs.map((q,idx,_))}
    }

    new LabeledGraph(nodes, labeledEdges)
  }

  def rename[A](morphs: Seq[Morph[A]]): Seq[Morph[Int]] = {
    val renameMap = morphs.flatMap(m => m.keys ++ m.values.flatten).toSet.zipWithIndex.toMap
    morphs.map(morph =>
      morph.map{ case (a,as) =>
        renameMap(a) -> as.map(renameMap)
      }
    )
  }
}
