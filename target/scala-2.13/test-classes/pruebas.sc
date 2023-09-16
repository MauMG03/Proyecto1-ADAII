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

/*
insatisfaccion(E(0),A(0))
insatisfaccion(E(1),A(1))
insatisfaccion(E(2),A(2))
insatisfaccion(E(3),A(3))
insatisfaccion(E(4),A(4))
*/

var sum = 0.0

for(i <- 0 to 4){
  sum += insatisfaccion(E(i),A(i))
  println(sum)
}

sum/5

instTotal(E,A)

val e1 = ("e1",Set(("M1",5),("M2",2),("M3",1)))
val a1 = ("e1",Set(("M1",5),("M2",2)))

//insatisfaccion(e1,a1)