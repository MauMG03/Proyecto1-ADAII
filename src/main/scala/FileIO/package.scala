import java.io.{BufferedReader, File, FileReader, FileWriter, IOException, PrintWriter}
import rocFB._

package object FileIO {
  def writeFile(s:(Asignacion, Double),n:String): Unit = {
    val fichero = new PrintWriter(new File("C:\\Users\\mauricio.munoz\\Desktop\\Univalle\\Semestre VI\\ADA II\\Proyecto 1\\Proyecto1\\src\\main\\scala\\Archivos\\Salidas\\" + n))

    fichero.write("" + s._2 + "\n")
    for(e <- s._1) {
      fichero.write("" + e._1 + "," + e._2.size + "\n")
      for(m <- e._2){
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
}
