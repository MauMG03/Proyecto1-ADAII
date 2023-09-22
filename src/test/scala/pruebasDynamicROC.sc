import DynamicROC._

val M = Vector((100,1),(101,3),(102,2))
val E = Vector(
  (1000,Vector((100,3),(101,2))),
  (1001,Vector((102,5),(100,1),(101,2))),
  (1002,Vector((100,3),(102,2))),
  (1003,Vector((102,2))),
  (1004,Vector((100,1),(101,4)))
)

val m = rocPD(M.length, E.length, M, E)

for (fila <- m){
  for (colum <- fila){
    print(colum._1 / 5)
    print(" ")
  }
  print('\n')
}

//var myVector = Vector.empty[Int]

//myVector = myVector :+ 1

//myVector



