package object Codigo {

  // Definicion de tipos de datos
  type Materias = Vector[(String,Int)] // (Mi, mi)
  type Solicitud = Vector[(String,Int)] // (sjl,pjl)
  type Estudiante = (String,Solicitud) // (ei, msi) o (ei, mai)
  type Asignacion = Vector[Estudiante] // puede ser E (una de las entradas del problema) o A (la salida)


  def gamma(x:Double) : Double = (3*x)-1

  /*
  * instatisfaccion
  * e X a -> d
  * Recibe
   */
  def insatisfaccion(e:Estudiante, a:Estudiante): Double = {
    var sum = 0.0
    for(s <- e._2) {
      if(!a._2.contains(s)) {
        sum += s._2
      }
    }
    (1.0 - (a._2.size.toDouble / e._2.size.toDouble)) * (sum / gamma(e._2.size))
  }

  def instTotal(est:Vector[Estudiante], a:Asignacion): Double = {
    var sum = 0.0
    for(i <- est.indices) {
      sum += insatisfaccion(est(i),a(i))
    }
    sum/est.size.toDouble
  }
}
