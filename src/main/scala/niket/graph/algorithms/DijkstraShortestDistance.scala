package niket.graph.algorithms

import niket.graph.core.{Node, Path, Weight, WeightedDirectedGraph}

import scala.collection.mutable

/**
 * Created by niket on 13/12/14.
 */
trait DijkstraShortestDistance {
  def findAll(graph: WeightedDirectedGraph, pois: Array[Node], maxL: Weight): Array[(Weight, Path)]

  def find(graph: WeightedDirectedGraph, s: Node, maxL: Weight, pois: Array[Node]): Array[(Weight, Path)]
}

private class DijkstraShortestDistanceImpl extends DijkstraShortestDistance {
  type Scanned = Boolean
  type InQ = Boolean

  override def findAll(graph: WeightedDirectedGraph, pois: Array[Node], maxL: Weight): Array[(Weight, Path)] = {
    (for {
      s <- pois
    } yield find(graph, s, maxL, pois)).flatten
  }

  override def find(graph: WeightedDirectedGraph, s: Node, maxL: Weight, pois: Array[Node]): Array[(Weight, Path)] = {
    val infiniteWeight = maxL + 1
    val state = new Array[(Weight, Node, Scanned, InQ)](graph.nodesCount)
    val q: mutable.PriorityQueue[(Weight, Node)] = mutable.PriorityQueue()

    q += ((0, s))
    state(s) = (0, -1, false, true)

    graph.forEach(v => {
      if (v != s) state(v) = (infiniteWeight, -1, false, false)
    })
    while (q.nonEmpty) {
      val (_, u) = q.dequeue()

      val (wu, pu, _, iu) = state(u)
      state(u) = (wu, pu, true, iu)

      graph.forEachNeighborOf(u) { v =>
        val (wv, _, sv, iv) = state(v)
        if (!sv) {
          val alt = wu + graph.weight(u, v)
          if (alt < wv) {
            if (!iv) q += ((alt, v))
            state(v) = (alt, u, sv, true)
          }
        }
      }
    }
    val results = for {
      t <- pois
      if t != s
      (w, path) = extractWeightAndPath(s, t, state, infiniteWeight)
      if path != Nil
    } yield {
      (w, path)
    }
    results
  }

  private def extractWeightAndPath(s: Node, t: Node, state: Array[(Weight, Node, Scanned, InQ)], infinity: Weight): (Weight, Path) = {
    val shortest = state(t)._1
    var w = shortest
    var p = state(t)._2
    var path = List(t)
    while (w < infinity && p != -1) {
      path = p :: path
      w = state(p)._1
      p = state(p)._2
    }
    if (path.head == s) (shortest, path)
    else (-1, Nil)
  }
}

object DijkstraShortestDistance {
  def apply(): DijkstraShortestDistance = {
    new DijkstraShortestDistanceImpl()
  }
}
