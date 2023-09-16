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
  * Recibe una tupla "e" correspondiente a un estudiante y las materias solicitadas por el mismo,
  * tambien recibe "a" correspondiente a una tupla con el mismo estudiante y las materias
  * asignadas para el, y retorna el valor de insatisfaccion del estudiante
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

  /*
    * instTotal
    * est X a -> d
    * Recibe un vector de tuplas de estudiantes y materias solicitadas llamado "est" y recibe
    * "a" que corresponde a un vector de tuplas con los estudiantes y las materias asignadas.
    * Los elementos de ambos vectores corresponden, es decir, el estudiante e1 esta en la misma
    * posicion tanto en est como en a. La salida es la insatisfaccion total del problema.
     */
  def instTotal(est:Vector[Estudiante], a:Asignacion): Double = {
    var sum = 0.0
    for(i <- est.indices) {
      sum += insatisfaccion(est(i),a(i))
    }
    sum/est.size.toDouble
  }
}
