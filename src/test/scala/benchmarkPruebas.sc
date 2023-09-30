import FileIO._
import Benchmark._
import Codigo._

// COLOQUE LA DIRECCION DONDE SE ENCUENTRAN SUS ARCHIVOS DE PRUEBA
// Y EL ALGORITMO QUE DESEA PROBAR
val directoryPath = "/home/juan/Universidad/Semestre VI/ADA II/Proyecto1/Proyecto1-ADAII/src/main/scala/Archivos/Entradas/"
val algoritmo:AlgoritmoRoc = rocFB

//DESCOMENTE ESTA SECCION SI DESEA REALIZAR TODAS LA PRUEBAS
// Recuerde que rocFB se desborda

val pruebas = listFilesInDirectory(directoryPath)
for (prueba <- pruebas){
  val nombre = prueba.toString.substring(directoryPath.length)
  val p = readFile(prueba.toString)
  val tiempoEjecucionPromedio = tiempoPromedioEjecucion(algoritmo)(p._1, p._2, p._3, p._4)
  println(nombre +" : " + tiempoEjecucionPromedio)
}

// DESCOMENTE ESTA SECCION PARA REALIZAR UNA PRUEBA UNITARIA
/*
val pruebaUnitaria = "e_3_5_5.roc"
val p = readFile(directoryPath+pruebaUnitaria)
val tiempoEjecucionPromedio = tiempoPromedioEjecucion(algoritmo)(p._1, p._2, p._3, p._4)
println(pruebaUnitaria + " : " + tiempoEjecucionPromedio)

 */
