import Codigo.{Estudiante, Materias, Solicitud, insatisfaccion, instTotal, isFeasible}

import scala.collection.mutable

package object VorazROC {

  /*
    * instTotalV2
    * est X a X r -> d
    * Recibe un vector de tuplas de estudiantes y materias solicitadas llamado "est" y recibe
    * "a" que corresponde a un map de los codigos de los estudiantes y las materias asignadas.
    * La salida es la insatisfaccion total del problema.
     */
  def instTotalV2(est: Vector[Estudiante], a: Map[Int, Solicitud], r: Double): Double = {
    var sum = 0.0
    for (e <- est) sum += insatisfaccion(e, (e._1, a(e._1)))
    sum / r
  }

  /*
  * prioridadMateriaEstudiante
  * e X mi -> i
  * Recibe un Estudiante y el codigo de una materia.
  * La salida es la prioridad del estudiante para dicha materia
  * o 0 si el estudiante no pidio la materia.
   */
  def prioridadMateriaEstudiante(e: Estudiante, mi: Int) = {
    var prioridadMateria = 0
    for (m <- e._2) {
      if (m._1 == mi) {
        prioridadMateria = m._2
      }
    }
    prioridadMateria
  }

  /*
  * ordenarEstudiantesPorMateria
  * E X m -> E'
  * Recibe un vector de Estudiante y una materia como tupla.
  * La salida es el vector E ordenado de forma descendente segun la prioridad de cada estudiante por
  * la materia dada, y en caso de empate, ordenado de forma ascendente segun la cantidad de materias
  * solicitadas por cada estudiante.
   */
  def ordenarEstudiantesPorMateria(E: Vector[Estudiante], m:(Int, Int)): Vector[Estudiante] = {
    E.sortBy(e => (-prioridadMateriaEstudiante(e, m._1), e._2.size))
    //E.sortBy(e => (-e._2.size, -prioridadMateriaEstudiante(e, m._1)))
  }

  /*
  * removerMateriaEstudiante
  * e X materia -> (s, m)
  * Recibe un Estudiante y el codigo de una materia.
  * La salida es la solictud de el estudiante sin la materia corresondiente al codigo dado
  * y dicha materia. O, en el caso en que el estudiante no haya solicitado dicha materia, la salida
  * seria la solicitud sin modificaciones y un valor nulo como materia.
  */
  def removerMateriaEstudiante(e:Estudiante, materia:Int) = {
    var m:(Int, Int) = null
    var solicitud = Vector.empty[(Int, Int)]
    for (mi <- e._2){
      if(mi._1 == materia){
        m = mi
      }
      else {
        solicitud = solicitud :+ mi
      }
    }
    (solicitud, m)
  }

  /*
   * rocV
   * k X r X M X E -> (A,d)
   * Dado "k" cantidad de materias, "r" cantidad de estudiantes, "M" el grupo de materias
   * con sus respectivos cupos y "E" el grupo de estudiantes con las materias que solicitan
   * da como respuesta una asignacion de materias "a" tal que la insatisfaccion "d" es la menor
   * posible hallada de forma voraz.
   */
  def rocV(k:Double, r:Double, M:Materias, E:Vector[Estudiante]) = {
    val A = mutable.Map[Int, Solicitud]()
    for (e <- E){
      A += (e._1 -> Vector.empty[(Int, Int)])
    }

    for (m <- M){
      var estsOrdenadosXMateria = ordenarEstudiantesPorMateria(E, m)
      var cupos = m._2
      var stop = r
      while (cupos > 0 && stop > 0){
        val estudiante = estsOrdenadosXMateria(0)
        val (asignacionActualizada, mRemovida) = removerMateriaEstudiante(estudiante, m._1)
        val estudianteActualizado = (estudiante._1, asignacionActualizada)
        estsOrdenadosXMateria = estsOrdenadosXMateria.tail :+ estudianteActualizado

        if (mRemovida != null){
          cupos -= 1
          val asignacionActual = A(estudiante._1)
          A(estudiante._1) = asignacionActual :+ mRemovida
        }
        stop -= 1
      }
    }
    val sol = A.toVector
    (sol, instTotalV2(E, A.toMap, r))
  }


}
