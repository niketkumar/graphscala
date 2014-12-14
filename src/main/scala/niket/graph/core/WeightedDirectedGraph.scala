package niket.graph.core

/**
 * Created by niket on 13/12/14.
 */
trait WeightedDirectedGraph {
  def weight(s: Node, t: Node): Weight

  def forEachNeighborOf(node: Node)(function: (Node) => Unit): Unit

  def forEach(function: (Node) => Unit): Unit

  def add(s: Node, t: Node, w: Weight): Unit

  def nodesCount: Int
}

private class WeightedDirectedGraphSparse(val n: Int, val m: Int) extends WeightedDirectedGraph {
  private[this] val nodes: Array[List[(Node, Weight)]] = new Array(n)

  override def add(s: Node, t: Node, w: Weight): Unit = {
    if (nodes(s) == null) nodes(s) = List((t, w))
    else nodes(s) = (t, w) :: nodes(s)
  }

  override def nodesCount: Int = n

  override def weight(s: Node, t: Node): Weight = {
    nodes(s).filter(_._1 == t).minBy(_._2)._2
  }

  override def forEach(function: (Node) => Unit): Unit = {
    (0 until nodes.length).foreach(s => function(s))
  }

  override def forEachNeighborOf(node: Node)(function: (Node) => Unit): Unit = {
    val neighbors: List[(Node, Weight)] = nodes(node)
    if (neighbors != null) neighbors.foreach(pair => function(pair._1))
  }
}

private class WeightedDirectedGraphDense(val n: Int, val m: Int) extends WeightedDirectedGraph {
  override def add(s: Node, t: Node, w: Weight): Unit = ???

  override def nodesCount: Int = n

  override def weight(s: Node, t: Node): Weight = ???

  override def forEach(function: (Node) => Unit): Unit = ???

  override def forEachNeighborOf(node: Node)(function: (Node) => Unit): Unit = ???
}

object WeightedDirectedGraph {
  def apply(n: Int, m: Int): WeightedDirectedGraph = {
    //    val someBigNumber = (n * n) / m
    //    if (someBigNumber > 50)
    //      new WeightedDirectedGraphSparse(n, m)
    //    else
    //      new WeightedDirectedGraphDense(n, m)
    new WeightedDirectedGraphSparse(n, m)
  }
}
