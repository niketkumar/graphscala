package niket.graph

import java.io.File

import niket.graph.algorithms.DijkstraShortestDistance
import niket.graph.core.{Node, Path, Weight, WeightedDirectedGraph}

import scala.io.Source

object Main {

  def findPaths(graph: WeightedDirectedGraph, pois: Array[Node], maxL: Int): Array[(Weight, Path)] = {
    print("Searching for paths: ")
    val t1 = java.lang.System.currentTimeMillis()
    val paths = DijkstraShortestDistance().findAll(graph, pois, maxL)
    val t2 = java.lang.System.currentTimeMillis()
    println((t2 - t1) + " ms")
    val count = paths.length
    println(s"$count paths found")
    paths
  }

  def loadPois(poisPath: String): Array[Node] = {
    print(s"Reading POIs ($poisPath): ")
    val t1 = java.lang.System.currentTimeMillis()
    val lines = Source.fromFile(new File(poisPath)).getLines()
    val pois = lines.next() split " " map (_.toInt)
    val t2 = java.lang.System.currentTimeMillis()
    println((t2 - t1) + " ms")
    val poisCount = pois.length
    println(s"Number of POIs: $poisCount")
    pois
  }


  def loadGraph(connectionsPath: String): WeightedDirectedGraph = {
    print(s"Reading connections ($connectionsPath): ")
    val t1 = java.lang.System.currentTimeMillis()
    val lines = Source.fromFile(new File(connectionsPath)).getLines()
    val n = lines.next().toInt
    val m = lines.next().toInt
    val graph = WeightedDirectedGraph(n, m)
    lines.
      map(line => line split " ").
      map(split => split map (_.toInt)).
      foreach(data => graph add(data(0), data(1), data(2)))
    val t2 = java.lang.System.currentTimeMillis()
    println((t2 - t1) + " ms")
    println(s"Nodes: $n")
    println(s"Edges: $m")
    graph
  }

  def savePaths(pathsFilePath: String, paths: Array[(Weight, Path)]): Unit = {
    print("Writing results to file ($pathsFilePath): ")
    val t1 = java.lang.System.currentTimeMillis()
    def printToFile(f: java.io.File)(function: java.io.PrintWriter => Unit) {
      val printWriter = new java.io.PrintWriter(f)
      try {
        function(printWriter)
      } finally {
        printWriter.close()
      }
    }
    printToFile(new File(pathsFilePath)) { printWriter =>
      printWriter.println(paths.length)
      paths.foreach(tup => printWriter.println(tup._1 + " " + tup._2.mkString(" ")))
    }
    val t2 = java.lang.System.currentTimeMillis()
    println((t2 - t1) + " ms")
    println("Done!")
  }

  def main(args: Array[String]): Unit = {
    val (rootFolder, maxL) = if (args.length == 2) (args(0), args(1).toInt) else (".", 20 * 60)

    val graph = loadGraph(rootFolder + "/connections.dat")

    println()

    val pois = loadPois(rootFolder + "/pois.dat")

    println()

    val paths = findPaths(graph, pois, maxL)

    println()

    savePaths(rootFolder + "/paths.dat", paths)

  }
}
