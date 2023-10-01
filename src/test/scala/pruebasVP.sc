import Codigo._
import FileIO.{readFile, writeFile}
import RocVP._

val archivo = "e_6_100_12.roc"

val e = readFile("D:\\Universidad\\Semestre 5\\ADA2\\Proyecto1-ADAII\\src\\main\\scala\\Archivos\\Entradas\\"+ archivo)
//writeFile(rocFB(e._1,e._2,e._3,e._4), "r" + archivo)
rocVP(e._1, e._2, e._3, e._4)
/*
val M = Vector((1001,3),(1002,4),(1003,2))
val E = Vector(
  (1,Vector((1001,5),(1002,2),(1003,1))),
  (2,Vector((1001,4),(1002,1),(1003,3))),
  (3,Vector((1002,3),(1003,2))),
  (4,Vector((1001,2),(1003,3))),
  (5,Vector((1001,3),(1002,2),(1003,3)))
)

// Una posible asignacion
val A = Vector(
  (1,Vector((1001,5),(1002,2))),
  (2,Vector((1001,4),(1002,1),(1003,3))),
  (3,Vector((1002,3))),
  (4,Vector((1003,3))),
  (5,Vector((1001,3),(1002,2)))
)*/

/*
var sum = 0.0
var num = 1

for(i <- 0 to 4){
  sum += insatisfaccion(E(i),A(i))
  num += 1
  println(sum)
}

sum/num

instTotal(E,A)*/

//Probando algoritmos de combinaciones

//se debe dar 2^k combinaciones
//genStCombination(E(0)._1,E(0)._2)

// La cantidad debe ser equivalente a la multiplicacion de las
// combinaciones de materias asignadas por cada estudiante.
// Para este E particular se tiene 8 * 8 * 4 * 4 * 8 = 8192
//val combinations = genCombinations(E)


/*(for(comb <- combinations; if(isFeasible(M,comb))) yield {
   comb
}).length*/

//isFeasible(M,A)

// Para este algoritmo es irrelevante el uso de r y k.




/*
val sortedE1 = sortEstudiantes(calcPeso(E), 1001)
val sortedE2 = sortEstudiantes(calcPeso(E), 1002)
val sortedE3 = sortEstudiantes(calcPeso(E), 1003)

var init:Vector[(Int,Vector[Int])] = Vector((M.tail.head._1,Vector()))
var res = asignarMateria(M.head._2, init, sortedE2)

var sol = calcAsign(E,M)
instTotal(E,sol,5)*/

