import Test._
import FileIO._

// Directorio para las pruebas creadas
val dir = "<Escribir la ruta para guardar las pruebas>"
// Crea carpetas para las pruebas segun r y k
val dir_r = dir+"Entradas_r/"
val dir_k = dir+"Entradas_k/"

writeTest(makeTest(5, 7, 3), dir)

// Pruebas segun r
val k = 3
val cupoMaximo = 3
for (r <- 1 to 5){
  writeTest(makeTest(r*10,k,cupoMaximo), dir_r)
}

// Pruebas segun k
val r = 20
for (k <- 3 to 7){
  writeTest(makeTest(r,k,cupoMaximo), dir_k)
}