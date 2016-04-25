package stats

import Jama.Matrix
import common.rich.collections.RichArray.{richArray, richSeqArray}

import scala.util.Random

object RichMatrix {
  implicit def richMatrix[T](m: Matrix) = new {
    def toPrint: String = toPrint(_.toString)
    def toPrint(f: Double ⇒ String) = m.getArray.deepSeq.map(_ map f).map(_ mkString("(", ",", ")")).mkString("\n")
    lazy val deepSeq = m.getArray.deepSeq
    def *(other: Matrix) = m times other
    def *(other: Seq[Seq[Double]]) = m times new Matrix(other deepArray)
    def timesCol(other: Seq[Double]): Seq[Double] = (m times new Matrix(other.toArray, other.size))
      .getArray
      .deepSeq
      .transpose
      .apply(0)
    lazy val rows: Seq[Seq[Double]] = m.getArray.deepSeq
    lazy val cols: Seq[Seq[Double]] = m.getArray.deepSeq.transpose
    lazy val dimensions: (Int, Int) = (m.getArray.size, m.getArray()(0).size)
    def update(i: Int, j: Int, d: Double) { m.set(i, j, d) }
    def updateRow(rowIndex: Int, ds: Seq[Double]) = {
			for ((j, jValue) <- 0 until m.getColumnDimension zip ds)
				m.set(rowIndex, j, jValue)
			this
		}

    def apply(i: Int, j: Int): Double = m.get(i, j)
    def mapRows(f: Seq[Double] => Seq[Double]): Matrix =
      new Matrix(rows map f deepArray)

    def mapCells(f: Double => Double) = mapRows(_ map f)

    def log(): Matrix = {
      println(toPrint)
      m
    }

    def +(other: Matrix) = m plus other
  }

  def eye(n: Int): Matrix = {
    val $ = new Matrix(n, n)
    for (i <- 0 until n)
      $(i, i) = 1
    $
  }
  def zeroes(n: Int): Matrix = new Matrix(n, n)

  def random(n: Int): Matrix = {
    val $ = new Matrix(n, n)
    for (i <- 0 until n; j <- 0 until n)
      $(i, j) = Random.nextDouble() * 2 - 1
    $
  }

  def apply(xss: Seq[Seq[Double]]): Matrix = new Matrix(xss.deepArray)
}