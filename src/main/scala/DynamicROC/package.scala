

package object DynamicROC {

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

}
