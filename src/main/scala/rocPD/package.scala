import rocFB.genStCombination

import scala.collection.{immutable, mutable}

package object rocPD {
  import rocFB.{Estudiante,Materias,Solicitud,Asignacion, insatisfaccion,instTotal,gamma}
  import scala.collection.mutable.{ArrayBuffer,Map}
  import scala.collection.immutable.Map



  // ----------------------- FUNCIONES PARA VECTOR DE CUPOS ---------------------------
  /*
  * Construye un vector donde la posicion i corresponde al valor de I(k-i-1)
  * es decir, (I_(k-1), I_(k-2), ... , I_1, I_0).  Complejidad O(k)
  * */
  def vectorIi(k:Int, M: Materias): Vector[Int] = {
    var mult = 1
    val Ii = (for (i <- 0 until k-1) yield{
      mult *= M(k-i-1)._2 + 1 ;
      mult}
      ).toVector
    1 +: Ii
  }

  /*
  * Obtiene el numero entero que representa la cantidad de cupos que se tiene por
  * materia en determinado valor. Complejidad O(k)
  * */
  def vToN(k:Int, cupos: Vector[Int], Ii: Vector[Int]) = {
    var n = 0
    for (i <- 0 until k) n += cupos(i) * Ii(k-i-1)
    n
  }

  /*
    * Obtiene el vector de cupos a partir de un valor de entero dado.
    * Complejidad O(k)
    * */
  def nToV(k:Int, n: Int, Ii:Vector[Int]): Vector[Int] = {
    var aux = n
    (for(i <- 0 until k) yield {
      val c_i = aux / Ii(k-i-1)
      aux = aux % Ii(k-i-1)
      c_i
    }).toVector
  }

  /*
  * Dado un vector con cantidad de cupos, una asignacion de materias a un estudiante y
  * el grupo de materias, resta en uno a todos los cupos que se haya asignado materia.
  * Complejidad O(k)
  * */
  def reducirCupos(cupos:Vector[Int], A:Estudiante, M:Materias): Vector[Int] = {
    var i=0
    val l = A._2.length
    val sortA = A._2.sortBy(_._1)

    val asignacion = for(k <- M) yield {
      if(i == l) 0
      else if (k._1 == sortA(i)._1) {i += 1; 1}
      else 0
    }

    (for(c <- cupos.indices) yield {
      if(asignacion(c) == 0) cupos(c)
      else cupos(c)-1
    }).toVector
  }

  /*
   * Determina si la cantidad de cupos de una materia es negativo.
   * Complejidad O(k)
   * */
  def verificarCupos(cupos:Vector[Int]): Boolean = {
    for(c <- cupos) if(c < 0) return false
    true
  }

  // ----------------------- SOLUCION PROGRAMACION DINAMICA ---------------------------

  def rocPD(k:Double, r:Double, M:Materias, E:Vector[Estudiante]): (Asignacion,Double) = {
    val Ii = vectorIi(k.toInt,M)
    val cupos = for(m <- M) yield {m._2}
    var i = 0

    /*
    * Funcion para recuperar o calcular valores, en caso de existir Some(value) recuperamos
    * el valor de la matriz, en caso contrario segun la definicion recursiva de la solucion
    * se calcula el valor.
    * Complejidad alrededor de O(cˆk * r * k)
    * */
    def memoizedROC(n: Int, j: Int, matrix: ArrayBuffer[ArrayBuffer[Option[Double]]]): Double = {
      matrix(n)(j) match {
        case Some(value) => value
        case None => {
          var min = Double.MaxValue
          val assignations_j = genStCombination(E(j-1))

          for(a <- assignations_j){
            val actual_quota = nToV(k.toInt,n,Ii)
            val reduced_quota = reducirCupos(actual_quota,a,M)

            if(verificarCupos(reduced_quota)){
              val newN = vToN(k.toInt, reduced_quota, Ii)
              val q = ((memoizedROC(newN, j - 1, matrix) * (j - 1)) + insatisfaccion(E(j-1), a)) * (1 / j.toDouble)
              if (q < min) min = q
            }

          }
          matrix(n)(j) = Some(min)
          min
        }
      }
    }

    /*
    * Funcion para generar la matriz de costos, primero crea la matriz con su respectiva dimension,
    * asigna los valores en los casos base (j=0) y ejecuta la solucion de un determinado subproblema.
    * Complejidad alrededor de O(cˆk * r * k)
    * */
    def costRocPD(n: Int, j: Int): ArrayBuffer[ArrayBuffer[Option[Double]]] = {
      val matrix: ArrayBuffer[ArrayBuffer[Option[Double]]] = ArrayBuffer.fill(n+1, j+1)(None)

      for (i <- 0 to n) matrix(i)(0) = Some(0.0)
      memoizedROC(n, j, matrix)
      matrix
    }

    //Se guarda la matriz para recuperar el valor
    val matrix = costRocPD(vToN(k.toInt,cupos,Ii),r.toInt)
    //val matrix = costRocPD(2,5)
    //for(m <- matrix) println(m)

    //Se imprime el valor del costo de la solucion.
    println(matrix(vToN(k.toInt,cupos,Ii))(r.toInt))

    (Vector(),1.0)
  }
}
