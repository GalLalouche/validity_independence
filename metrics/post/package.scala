package metrics

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge

package object post {
  type ClassGraph = Graph[String, DiEdge]
}
