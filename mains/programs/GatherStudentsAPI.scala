package mains.programs

import java.io.{File, IOException}

import mains.generators.tables._
import scala.util.Random
import common.rich.path.Directory
import parsers.XlstoCSV
import common.rich.RichAll._

object GatherStudentsAPI {

  case class Line(api: Int, comment: String, chosen: Boolean)

  def main(args: Array[String]) {
    //    val r = new Random
    //    val data = MetricValuesParser
    //      .getData()i.toString
    //      .ignoreMetrics(Configuration.ignoredMetrics);
    ////    MetricValuesParser.writeMetricValuesToDisk(data.filterLatest, Configuration.rawValuesFolder addSubDir "Latest")
    //    MannWhitneyBetweenProjects.apply(data.getAverageValuesPerFile, Configuration.namedNormalizations)
    var i = 0

    val twins = Set(Set(10, 14), Set(4, 12))
    def getName: String = {
      i += 1
			i.toString
    }
    val indexes = (Directory( """D:\Dropbox\Software Design\HW\HW3\Code""") \ "apiIndices.txt")
      .lines
      .map(line => line.split("->").map(_.trim))
      .map(e => e(1).toInt -> e(0))
      .log()
    val codeDirectory: Directory = Directory( """C:\Users\Gal\Desktop\New folder""")
    val outputDirectory: Directory = codeDirectory.addSubDir("csvs")
    outputDirectory.clear()

    for (xslFile <- codeDirectory.dirs.flatMap(_.files.filter(_.extension == "xls"))) {
      try XlstoCSV.toCsv(xslFile, outputDirectory.addFile(xslFile.parent.name + ".csv"))
      catch {
        case e: IOException => throw new Exception(s"Failed at $xslFile", e)
      }
    }

    val feedbackLines = (for (file <- outputDirectory.files) yield {
      assert(file.lines.size == 6, s"failed in $file")
      for ((line, index) <- file.lines.drop(1).map(_ split ",").zipWithIndex) yield
        try new Line(line(1).toDouble.toInt, line.drop(2).mkString(""), index == 0)
        catch {
          case e: Exception => throw new Exception(s"Failed at $file", e)
        }
    }
      ).flatten
    val f = new File("C:/a.csv")
    f.createNewFile()
    f.clear()
    val x = feedbackLines
      .groupBy(_.api)
      .toSeq
      .sortBy(_._1)
      //      .map(e => e._1 -> Math.max(e._2.count(_.chosen), Math.max(5 - e._2.size, 1)))
      //      .map(e => e._1 -> e._2)
      .join(indexes).by(_._1, _._1, (chosen, id) => (id._2, chosen._1, chosen._2))
      .map(e => e._1 :: e._2 :: e._3.map(_.comment).toList)
      .log(_ mkString "\n")
    f.write(x.map(_.map(e => s""""$e"""").mkString(",")).mkString("\n").getBytes)
  }
}
