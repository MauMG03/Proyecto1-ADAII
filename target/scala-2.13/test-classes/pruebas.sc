import Codigo._

val M = Vector((1001,3),(1002,4),(1003,2))
val E = Vector(
  (1,Vector((1001,5),(1002,2),(1003,1))),
  (2,Vector((1001,4),(1002,1),(1003,3))),
  (3,Vector((1002,3),(1003,2))),
  (4,Vector((1001,2),(1003,3))),
  (5,Vector((1001,3),(1002,2),(1003,3)))
)

val A = Vector(
  (1,Vector((1001,5),(1002,2))),
  (2,Vector((1001,4),(1002,1),(1003,3))),
  (3,Vector((1002,3))),
  (4,Vector((1003,3))),
  (5,Vector((1001,3),(1002,2)))
)


var sum = 0.0
var num = 1

/*for(i <- 0 to 4){
  sum += insatisfaccion(E(i),A(i))
  num += 1
  println(sum)
}

sum/num

instTotal(E,A)*/

//genStCombination(E(0)._1,E(0)._2)
//val combinations = genCombinations(E)

/*(for(comb <- combinations; if(isFeasible(M,comb))) yield {
   comb
}).length*/

//isFeasible(M,A)

rocFB(3,5,M,E)