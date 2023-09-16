import Codigo._

val M = Vector(("M1",3),("M2",4),("M3",2))
val E = Vector(
  ("e1",Vector(("M1",5),("M2",2),("M3",1))),
  ("e2",Vector(("M1",4),("M2",1),("M3",3))),
  ("e3",Vector(("M2",3),("M3",2))),
  ("e4",Vector(("M1",2),("M3",3))),
  ("e5",Vector(("M1",3),("M2",2),("M3",3)))
)

val A = Vector(
  ("e1",Vector(("M1",5),("M2",2))),
  ("e2",Vector(("M1",4),("M2",1),("M3",3))),
  ("e3",Vector(("M2",3))),
  ("e4",Vector(("M3",3))),
  ("e5",Vector(("M1",3),("M2",2)))
)


var sum = 0.0
var num = 1

for(i <- 0 to 4){
  sum += insatisfaccion(E(i),A(i))
  num += 1
  println(sum)
}

sum/num

instTotal(E,A)