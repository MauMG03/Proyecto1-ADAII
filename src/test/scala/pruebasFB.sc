import rocFB._
import FileIO.{readFile, writeFile}

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
)

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
genStCombination(E(0)._1,E(0)._2)

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
val archivo = "e_3_20_10.roc"

val e = readFile("C:\\Users\\mauricio.munoz\\Desktop\\Univalle\\Semestre VI\\ADA II\\Proyecto 1\\Proyecto1\\src\\main\\scala\\Archivos\\Entradas\\" + archivo)
writeFile(rocFB(e._1,e._2,e._3,e._4), "r" + archivo)
*/
