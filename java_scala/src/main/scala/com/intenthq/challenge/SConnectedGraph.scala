package com.intenthq.challenge

import scala.annotation.tailrec

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false
  def run(source: Node, target: Node): Boolean = {
    @tailrec
    def program(currentNode: Node): Boolean = {
      if (currentNode.value == target.value)
        true
      else if (currentNode.edges.isEmpty)
        false
      else {
        //      currentNode.edges.map(program).par.fold(false)(_||_)
        val headNodes = currentNode.edges.head.edges
        val nodesInOtherPaths = currentNode.edges.tail
        program(currentNode.edges.head.copy(edges = headNodes ++ nodesInOtherPaths))
      }
    }

    program(source)
  }
  
}
