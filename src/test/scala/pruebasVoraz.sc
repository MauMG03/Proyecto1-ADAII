import FileIO.{readFile, writeFile}
import VorazROC._

val estudiantes = Vector(
  (8, 2),
  (6, 3),
  (5, 5),
  (10, 2)
)

val M = Vector((1001,3),(1002,4),(1003,2))
val E = Vector(
  (1,Vector((1001,5),(1002,2),(1003,1))),
  (2,Vector((1001,4),(1002,1),(1003,3))),
  (3,Vector((1002,3),(1003,2))),
  (4,Vector((1001,2),(1003,3))),
  (5,Vector((1001,3),(1002,2),(1003,3)))
)

//findMaxArrEstudiantes(estudiantes, 0)

estudiantes.sortBy(estudiantes => (estudiantes._2, estudiantes._1))


val archivo = "e_3_5_5.roc"

val e = readFile("/home/juan/Universidad/Semestre VI/ADA II/Proyecto1/Proyecto1-ADAII/src/main/scala/Archivos/Entradas/" + archivo)

rocV(e._1,e._2,e._3,e._4)

//rocV(M.length, E.length, M, E)