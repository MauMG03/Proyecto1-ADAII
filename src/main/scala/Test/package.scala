import rocFB.{Asignacion, Estudiante, Materias, Solicitud, gamma}

import scala.util.Random

package object Test {
  val rand = new Random()

  def makeM(k:Int, cupoMax:Int):Materias = {
    var M_i = 100
    var M = Vector.empty[(Int, Int)] //{(M_i,m_i), ...}
    for (i <- 0 until  k){
      val m_i = rand.nextInt(cupoMax) + 1 // m_i => [1, cupoMax]
      val m = (M_i, m_i)
      M = M :+ m
      M_i = M_i + 1
    }
    M
  }

  def makeMs_j(M:Materias):Solicitud = {
    var list_sj_l = Vector.empty[Int]
    var list_p_jl = Vector.empty[Int]

    var stop = 7
    for (m <- M){
      if (stop > 0 && math.random() < 0.5){
        list_sj_l = list_sj_l :+ m._1
        stop -= 1
      }
    }
    if (list_sj_l.isEmpty) list_sj_l = list_sj_l :+ M(rand.nextInt(M.size))._1

    var gamma_j = gamma(list_sj_l.size).toInt
    for (m <- 0 until list_sj_l.size-1){
      val p_jl = rand.nextInt(gamma_j+1)
      list_p_jl = list_p_jl :+ p_jl
      gamma_j -= p_jl
    }
    list_p_jl = list_p_jl :+ gamma_j

    list_sj_l.zip(list_p_jl)
  }

  def makeE(M:Materias, r:Int):Asignacion = {
    var E = Vector.empty[Estudiante]
    for (e_j <- 1000 until 1000+r){
      E = E :+ (e_j, makeMs_j(M))
    }
    E
  }

  def makeTest(r:Int, k:Int, cupoMax:Int) = {
    val M = makeM(k, cupoMax)
    val E = makeE(M, r)
    (M,E)
  }
}
