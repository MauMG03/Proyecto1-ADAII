import scala.collection.immutable.Vector
import scala.collection.mutable

package object Codigo {

  // Definicion de tipos de datos
  type Materias = Vector[(Int,Int)] // (Mi, mi)
  type Solicitud = Vector[(Int,Int)] // (sjl,pjl)
  type Estudiante = (Int,Solicitud) // (ei, msi) o (ei, mai)
  type Asignacion = Vector[Estudiante] // puede ser E (una de las entradas del problema) o A (la salida)
  type SolicitudPeso = Vector[(Int, Double)]
  type EstudiantePeso = (Int, SolicitudPeso)


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
    for(m <- mat) cupos(m._1) = m._2
    for(e <- a; m <- e._2) cupos(m._1) -= 1
    for(c <- cupos) if(c._2 < 0) return false
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

  /*
   * calcPeso
   * [E] -> [E]
   * Dada una entrada de "n" estudiantes "E" se calcula el peso de cada materia "m" en la solicitud de un
   * Estudiante como: Prioridad / PrioridadTotal * 100%
   * Complejidad O(2*n*m)
   */

  def calcPeso(e: Vector[Estudiante]): Vector[(Int, Vector[(Int, Double)])] = {
    // Se calcula la suma de los valores de prioridad para cada estudiante
    val sumMap = e.map { case (_, solicitud) =>
      val sum = solicitud.map(_._2).sum.toDouble
      sum
    }

    // Se calcula el porcentaje de prioridad de cada materia, y se retorna el vector de estudiantes modificado
    e.zip(sumMap).map { case ((key, solicitud), sum) =>
      val SolicitudNormalizada = solicitud.map { case (subKey, value) =>
        (subKey, value.toDouble / sum )
      }
      (key, SolicitudNormalizada)
    }
  }

  /*
   * sortEstudiantes
   * [E] x id -> [E]
   * Toma un vector de Estudiantes previamente normalizado por peso, se confirma que el estudiante
   * haya solicitado la materia al comparar el codigo "id" de esta, y se ordena el vector de acuerdo
   * al peso de la materia mediante sortby
   *
   * Complejidad O(n+n*log n)
   */
  def sortEstudiantes(estudiantes: Vector[EstudiantePeso], id: Int): Vector[EstudiantePeso] = {
    val sortedEstudiantes = estudiantes.filter { case (_, solicitud) =>
      solicitud.exists { case (solicitudId, _) => solicitudId == id } //Comprueba que el estudiante haya inscrito la materia
    }.sortBy { case (_, solicitud) =>
      solicitud.find { case (solicitudId, _) => solicitudId == id }.get._2
    }(Ordering[Double].reverse)

    sortedEstudiantes
  }

  /*
   * asignarMateria
   * int x (int, [int]) x [E] -> (int, [int])
   * recibe la cantidad de cupos de una materia, un resultado inicial que contiene el codigo
   * de la materia y un vector inicialmente vacio, y un vector previamente ordenado por peso
   * de estudiantes que solicitaron la materia, y devuleve un vector de la forma
   * (CodigoMateria, (Codigos de los estudiantes a la que se le asigno))
   */
  def asignarMateria(cupos: Int, result: Vector[(Int, Vector[Int])], e: Vector[EstudiantePeso]): Vector[(Int, Vector[Int])] = {
    if (e.isEmpty | cupos == 0) {
      result
    } else {
      val newRes: Vector[(Int, Vector[Int])] = result.updated(0, (result(0)._1, result(0)._2 :+ e.head._1))
      asignarMateria(cupos - 1, newRes, e.tail)
    }
  }

  def rocVP(k:Double, r:Double, m:Materias, e:Vector[Estudiante]): (Double, Asignacion) = {
    val normE = calcPeso(e)
    var result = e
    m.foreach { case(mId, c) =>
      val sorted = sortEstudiantes(normE, mId)
      val init:Vector[(Int,Vector[Int])] = Vector((mId,Vector.empty[Int]))
      val a = asignarMateria(c, init, sorted)
      result = result.map { case (estudiante, asignaciones) =>
        if (!a.head._2.contains(estudiante)) {
          (estudiante, asignaciones.filter { case (key, _) => key != a.head._1 })
        } else {
          (estudiante, asignaciones)
        }
      }

    }
    (instTotal(e, result, r), result)
  }

  //----------------------------------- PROGRAMACION DINAMICA ------------------------------------
}
