import Benchmark.tiempoPromedioEjecucion
import FileIO.{readFile, writeFile}
import rocFB.{instTotal, rocFB}
import rocPD._

import scala.collection.mutable

val M = Vector((1001,3),(1002,4),(1003,2))
val E = Vector(
  (1,Vector((1001,5),(1002,2),(1003,1))),
  (2,Vector((1001,4),(1002,1),(1003,3))),
  (3,Vector((1002,3),(1003,2))),
  (4,Vector((1001,2),(1003,3))),
  (5,Vector((1001,3),(1002,2),(1003,3)))
)

//val map = mutable.Map[Int,Int]()
//for(m <- M) map(m._1) = m._2

//val Ii = vectorIi(3,M)

//reducirCupos(Vector(3,4,2),(1,Vector((1001,5),(1003,5))),M)
//rocFB(3,5,M,E)


//instTotal(E, Vector((1,Vector()),(2, Vector((1003, 3))), (3,Vector()),(4, Vector((1003, 3))),(5,Vector())),5)

//rocPD(3,5,M,E)



val archivo = "e_3_5_5.roc"
val e = readFile("C:\\Users\\mauricio.munoz\\Desktop\\Univalle\\Semestre VI\\ADA II\\Proyecto 1\\Proyecto1\\src\\main\\scala\\Archivos\\Entradas\\" + archivo)


val sol = rocPD(e._1,e._2,e._3,e._4)
sol._1
sol._2
//tiempoPromedioEjecucion(rocPD)(e._1,e._2,e._3,e._4)

