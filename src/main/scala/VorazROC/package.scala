import Codigo.{Estudiante, Materias, Solicitud, insatisfaccion, instTotal, isFeasible}

import scala.collection.mutable

package object VorazROC {

  type TriplaEstudiante = (Int, Int, Int) // (CodigoEst, MateriasSolicitadas, prioridadMateriaN)

  def instTotalV2(est: Vector[Estudiante], a: Map[Int, Solicitud], r: Double): Double = {
    var sum = 0.0
    for (e <- est) sum += insatisfaccion(e, (e._1, a(e._1)))
    sum / r
  }

  def prioridadMateriaEstudiante(e: Estudiante, mi: Int) = {
    var prioridadMateria = 0
    for (m <- e._2) {
      if (m._1 == mi) {
        prioridadMateria = m._2
      }
    }
    prioridadMateria
  }

  def ordenarEstudiantesPorMateria(E: Vector[Estudiante], m:(Int, Int)): Vector[Estudiante] = {
    E.sortBy(e => (-prioridadMateriaEstudiante(e, m._1), e._2.size))
  }

  def removerMateriaEstudiante(e:Estudiante, materia:Int) = {
    var m:(Int, Int) = null
    var asignacion = Vector.empty[(Int, Int)]
    for (mi <- e._2){
      if(mi._1 == materia){
        m = mi
      }
      else {
        asignacion = asignacion :+ mi
      }
    }
    (asignacion, m)
  }

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
    (instTotalV2(E, A.toMap, r), isFeasible(M, sol), sol)
  }


}
