import scala.collection.mutable
import Codigo.{Estudiante, Materias, Solicitud, gamma, insatisfaccion}
import ujson.IndexedValue.False

import scala.util.control.Breaks._

package object DynamicROC {

  type InstEst = (Double, Int, Int) // (Insatidaccion, numeroMateriasdadas, prioridadAcumulada) de un estudiante
  type InstEsts = (Double, Vector[InstEst])

  /*
  * getCuposCombination
  * cupos -> vCupos
  * Recibe un vector de cupos y retorna todas las conbinaciones de dichos cupos
   */
  def getCuposCombination(cupos: Vector[Int]): IndexedSeq[Vector[Int]] = {
    if (cupos.tail.isEmpty) for (c <- 0 to cupos.head) yield Vector(c)
    else {
      for (c <- 0 to cupos.head; a <- getCuposCombination(cupos.tail)) yield c +: a
    }
  }

  def getIi(M: Materias, i: Int): Int = {
    var mult = 1
    val k = M.length
    for (j <- i+1 to k - 1) {
      val mj = M(j)._2
      mult *= mj + 1
    }
    mult
  }

  def vectorCuposToNumero(cupos: Vector[Int], M:Materias) = {
    val k = M.length
    var n = 0
    for (i <- 0 to k-1){
      val ci = cupos(i)
      val Ii = getIi(M, i)
      n += ci * Ii
    }
    n
  }

  def numeroToVectorCupos(n:Int, M:Materias) = {
    def aux(n:Int,Ii:Vector[Int]):Int = {
      if (Ii.tail.isEmpty) n / Ii.head
      else {
        val ci = n % Ii.head
        aux(ci, Ii.tail)
      }
    }
    val Ii = for (i <- 0 until M.length) yield getIi(M, i)
    val vectorCupos = for (i <- 1 to M.length) yield {
      val (left, right) = Ii splitAt i
      aux(n,left.toVector)
    }
    vectorCupos
  }

  def rocPD(k:Int, r:Int, M:Materias, E:Vector[Estudiante]) = {
    val cuposIniciales = for (m <- M) yield m._2
    val cuposCombs = getCuposCombination(cuposIniciales)

    // type InstEsts = (Int, Vector[InstEst])
    // type InstEst = (Int, Int, Int)

    // Caso trivial (0, 0, 0) -> Fila 0
    val instEstTrivial = (1.0, 0, 0):InstEst
    val instEstsTrivial = (r.toDouble, for (e <- E) yield instEstTrivial):InstEsts
    val fila0 = for (e <- E) yield instEstsTrivial
    val matriz = Vector(fila0)


    rocPDMatriz(matriz, cuposCombs, M, E)
  }

  def rocPDMatriz(matriz:Vector[Vector[InstEsts]], cuposCombs:IndexedSeq[Vector[Int]], M:Materias, E:Vector[Estudiante]):Vector[Vector[InstEsts]] = {
    val n = cuposCombs.length
    val m = matriz.length
    if (m == n) matriz
    else {
      val proximaFila = m
      val newFila = calcularFilaMatriz(proximaFila, matriz, M, E):Vector[InstEsts]
      val newMatriz = matriz :+ newFila
      rocPDMatriz(newMatriz, cuposCombs, M, E)
    }
  }

  def calcularFilaMatriz(fila: Int, matriz: Vector[Vector[InstEsts]], M: Materias, E: Vector[Estudiante]): Vector[InstEsts] = {
    var newFila = Vector.empty[InstEsts]
    var actualMatriz = matriz
    for (colum <- 0 until E.length) {
      newFila = newFila :+ calcularMatrizPos(fila, colum, actualMatriz, M, E)
      actualMatriz = matriz :+ newFila
    }
    newFila

  }

  def lookMateriaIn(e:Estudiante, mi:Int) = {
    var Mi = (-1, -1) // -1 significa que la materia no esta
    for (m <- e._2) {
      if (m._1 == mi) {
        Mi = m
      }
    }
    // (materia, encontrada?)
    if (Mi._1 == -1) (Mi, 0)
    else (Mi, 1)

  }

  def auxxx(comb:IndexedSeq[Int], k:Int):Int = {
    if(comb(k) >= 1) k
    else auxxx(comb, k - 1)
  }

  def calcularMatrizPos(i: Int, j:Int, matriz:Vector[Vector[InstEsts]], M:Materias, E:Vector[Estudiante]):InstEsts = {
    val roc_i_j_1 = if (j-1 < 0) Int.MaxValue else matriz(i)(j-1)._1
    val roc_i_j = {
      val comb = numeroToVectorCupos(i, M)
      val Mi = M(auxxx(comb, comb.length - 1))


      val (mEncontrada, encontrada) = lookMateriaIn(E(j), Mi._1)
      var pjl = 0
      if (encontrada == 1) {pjl = mEncontrada._2}

      val roc_i_1_r = matriz(i-1)(E.length - 1)._2
      var intEst_j = roc_i_1_r(j)
      val msj = E(j)._2.length
      val gammaj = gamma(msj)
      val maj = intEst_j._2 + encontrada
      val sumPjl = intEst_j._3 + pjl

      intEst_j = (insatisfaccionEst(maj, sumPjl, gammaj, msj), maj, sumPjl)

      var Fme:Double = 0
      for (e <- 0 until E.length-1){
        if (e == j) Fme += intEst_j._1
        else Fme += roc_i_1_r(e)._1
      }

      val (left, right) = roc_i_1_r splitAt j-1
      val intEsts = (left :+ intEst_j) ++: right.tail

      (Fme, intEsts)

    }
    val rocMin = Math.min(roc_i_j._1, roc_i_j_1)
    if (rocMin == roc_i_j_1){
      if (j >= 1) matriz(i)(j-1)
      else roc_i_j
    }
    else {
      roc_i_j
    }
  }

  def insatisfaccionEst(maj: Double, sumPjl: Double, gammaj: Double, msj: Double): Double = {
    val pMaterias: Double = 1.0 - (maj / msj)
    val pPrioridad: Double = sumPjl / gammaj
    pMaterias * pPrioridad
  }


}
