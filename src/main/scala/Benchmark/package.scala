import Codigo.{Asignacion, Estudiante, Materias}
package object Benchmark {
  type AlgoritmoRoc = (Double, Double, Materias, Vector[Estudiante]) => (Asignacion, Double)

  def probarAlgoritmo(a: AlgoritmoRoc)(k:Double, r:Double, M:Materias, E:Vector[Estudiante]): Double = {
    val startTime = System.nanoTime()
    val result = a(k, r, M, E)
    val endTime = System.nanoTime()
    val executionTimeInSeconds = (endTime - startTime) / 1e9 // Convert nanoseconds to seconds
    executionTimeInSeconds
  }

  def tiempoPromedioEjecucion(a: AlgoritmoRoc)(k:Double, r:Double, M:Materias, E:Vector[Estudiante]): Double = {
    (for(i <- 0 until 3) yield probarAlgoritmo(a)(k, r, M, E)).sum / 3.0
  }
}
