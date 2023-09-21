import scala.collection.mutable

package object Codigo {

  // Definicion de tipos de datos
  type Materias = Vector[(Int,Int)] // (Mi, mi)
  type Solicitud = Vector[(Int,Int)] // (sjl,pjl)
  type Estudiante = (Int,Solicitud) // (ei, msi) o (ei, mai)
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
    for(s <- e._2; if(!a._2.contains(s))) sum += s._2
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
  def instTotal(est:Vector[Estudiante], a:Asignacion,r:Double): Double = {
    var sum = 0.0
    for(j <- est.indices) sum += insatisfaccion(est(j),a(j))
    sum/r
  }

  //-------------------------------------- SOLUCIONES ---------------------------------------

  // ---------------------------------------- FUERZA BRUTA ---------------------------------

  /*
   * genStCombination
   * e -> V
   * Recibe un estudiante y el conjunto de materias solicitadas, y da como respuesta
   * todos los posibles conjuntos de las materias solicitadas. Complejidad O(2^n) con
   * n las materias solicitadas, 0 <= n <= 7
   */
  def genStCombination(e:Estudiante):Vector[Estudiante] = {
    if(e._2.isEmpty) Vector((e._1,Vector()))
    else{
      val aux = genStCombination((e._1,e._2.tail))
      val aux2 = for(a <- aux) yield (e._1, e._2.head +: a._2)
      aux ++ aux2
    }
  }

  /*
   * genCombination
   * E -> V
   * Recibe E, un grupo de estudiantes con las materias solicitadas representado como
   * vector y da como respuesta todas las formas posibles de asignar materias a los
   * estudiantes. Complejidad O(128^r) con r la cantidad de estudiantes.
   */
  def genCombinations(E:Vector[Estudiante]): Vector[Vector[Estudiante]] = {
    if(E.isEmpty) Vector(Vector())
    else{
      val combinations = genStCombination(E.head)
      for(comb <- combinations; a <- genCombinations(E.tail)) yield comb +: a
    }
  }

  /*
   * genCombination
   * mat X a -> Boolean
   * Dado un grupo de materias con sus respectivos cupos y una posible asignacion
   * de materias a los estudiantes, se da como respuesta si la asignacion es
   * factible o no en terminos de cupos. Complejidad O(k + 7^r) donde m es la
   * cantidad de materias y r la cantidad de estudiantes.
   */
  def isFeasible(mat:Materias, a:Asignacion): Boolean = {
    val cupos = mutable.Map[Int,Int]()

    for(m <- mat){
      cupos(m._1) = m._2
    }

    for(e <- a; m <- e._2){
      cupos(m._1) -= 1
    }

    for(c <- cupos){
      if(c._2 < 0) return false
    }

    true
  }

  /*
   * rocFB
   * k X r X M X E -> (A,d)
   * Dado "k" cantidad de materias, "r" cantidad de estudiantes, "M" el grupo de materias
   * con sus respectivos cupos y "E" el grupo de estudiantes con las materias que solicitan
   * da como respuesta una asignacion de materias "a" tal que la insatisfaccion "d" es la menor
   * posible. Complejidad O(128^r)
   */
  def rocFB(k:Double, r:Double, M:Materias, E:Vector[Estudiante]): (Asignacion,Double) = {
    val combinations = genCombinations(E)
    var sol:Asignacion = Vector()
    var cost:Double = Double.MaxValue

    for(comb <- combinations; if(isFeasible(M,comb))) {
      val insatisfaction = instTotal(E,comb,r)
      if(insatisfaction < cost){
        cost = insatisfaction
        sol = comb
      }
    }

    (sol,cost)
  }

  //----------------------------------- PROGRAMACION VORAZ ------------------------------------



  //----------------------------------- PROGRAMACION DINAMICA ------------------------------------
}
