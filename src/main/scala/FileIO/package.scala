import java.io.{BufferedReader, File, FileReader, FileWriter, IOException, PrintWriter}
import rocFB._

package object FileIO {
  def writeFile(s: (Asignacion, Double), path: String, n: String): Unit = {
    val fichero = new PrintWriter(new File(path + "/" + n))

    fichero.write("" + s._2 + "\n")
    for (e <- s._1) {
      fichero.write("" + e._1 + "," + e._2.size + "\n")
      for (m <- e._2) {
        fichero.write("" + m._1 + "\n")
      }
    }
    fichero.close()
  }

  def readFile(s:String):(Double, Double, Materias, Vector[Estudiante]) = {
    val reader = new BufferedReader(new FileReader(s))
    var line: String = reader.readLine()

    val k = line.toDouble

    line = reader.readLine()

    val M = (for(i <- (0 until k.toInt)) yield{
      val m = line.split(",")
      line = reader.readLine()
      (m(0).toInt,m(1).toInt)
    }).toVector

    val r = line.toDouble
    line = reader.readLine()

    val E = (for(j <- (0 until r.toInt)) yield {
      val e = line.split(",")
      val ej = e(0).toInt
      val sl = e(1).toInt
      line = reader.readLine()

      val s = (for(l <- 0 until sl) yield {
        val m = line.split(",")
        line = reader.readLine()
        (m(0).toInt, m(1).toInt)
      }).toVector

      (ej, s)
    }).toVector

    (k,r,M,E)
  }

  def listFilesInDirectory(directoryPath: String): List[File] = {
    val directory = new File(directoryPath)
    if (directory.exists() && directory.isDirectory) {
      directory.listFiles().toList
    } else {
      Nil
    }
  }

  def writeTest(instancia: (Materias, Asignacion), dir: String): Unit = {
    val k = instancia._1.size
    val r = instancia._2.size
    val fichero = new PrintWriter(new File(dir + "e_" + k + "_" + r + "_" + r + ".roc"))

    fichero.write("" + k + "\n")
    for (m <- instancia._1) {
      fichero.write("" + m._1 + "," + m._2 + "\n")
    }

    fichero.write("" + r + "\n")
    for (e <- instancia._2) {
      fichero.write("" + e._1 + "," + e._2.size + "\n")
      for (m <- e._2) {
        fichero.write("" + m._1 + "," + m._2 + "\n")
      }
    }
    fichero.close()
  }
}
