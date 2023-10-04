import rocFB.{Asignacion, Estudiante, Materias, gamma, instTotal}

package object rocV {

  type SolicitudPeso = Vector[(Int, Double)]
  type EstudiantePeso = (Int, SolicitudPeso)

  /*
   * calcPeso
   * [E] -> [E]
   * Dada una entrada de "n" estudiantes "E" se calcula el peso de cada materia "m" en la solicitud de un
   * Estudiante como: Prioridad / PrioridadTotal * 100%
   * Complejidad O(7r) -> O(r)
   */

  def calcPeso(e: Vector[Estudiante]): Vector[(Int, Vector[(Int, Double)])] = {
    // Se calcula el porcentaje de prioridad de cada materia, y se retorna el vector de estudiantes modificado
    e.map { case (e_j, ms_j) =>
      val SolicitudNormalizada = ms_j.map { case (s_jl, p_jl) =>
        (s_jl, p_jl.toDouble / gamma(ms_j.size))
      }
      (e_j, SolicitudNormalizada)
    }
  }

  /*
   * sortEstudiantes
   * [E] x id -> [E]
   * Toma un vector de Estudiantes previamente normalizado por peso E_p, se confirma que el estudiante
   * haya solicitado la materia al comparar el codigo "M_i" de esta, y se ordena el vector de acuerdo
   * al peso de la materia mediante sortby
   *
   * Complejidad O(n+n*log n)
   */
  def sortEstudiantes(E_p: Vector[EstudiantePeso], M_i: Int): Vector[EstudiantePeso] = {
    // E_p es E pero con msp_j en vez de ms_j
    E_p.filter { case (_, msp_j) => //msp_j es ms_j pero con peso en vez de prioridad
      msp_j.exists { case (s_jl, _) => s_jl == M_i } //Comprueba que el estudiante haya inscrito la materia
    }.sortBy { case (_, msp_j) =>
      -msp_j.find { case (s_jl, _) => s_jl == M_i }.get._2
    }
  }

  /*
   * asignarMateria
   * int x (int, [int]) x [E] -> (int, [int])
   * recibe la cantidad de cupos de una materia, un resultado inicial que contiene
   * un vector inicialmente vacio, y un vector previamente ordenado por peso
   * de estudiantes que solicitaron la materia, y devuleve un vector
   * con los codigos de los estudiantes a la que se le asigno la materia.
   */
  def asignarMateria(m_i: Int, A_i: Vector[Int], E_p: Vector[EstudiantePeso]): Vector[Int] = {
    if (E_p.isEmpty | m_i == 0) {
      A_i
    } else {
      val newRes: Vector[Int] = A_i :+ E_p.head._1
      asignarMateria(m_i - 1, newRes, E_p.tail)
    }
  }

  def rocV(k: Double, r: Double, M: Materias, E: Vector[Estudiante]): (Asignacion, Double) = {
    val E_p = calcPeso(E)
    var A = E
    M.foreach { case (m_i, c) => // (M_i, m_i)
      val sorted = sortEstudiantes(E_p, m_i)
      val a = asignarMateria(c, Vector.empty[Int], sorted)
      A = A.map { case (e_j, ma_j) =>
        if (!a.contains(e_j)) {
          (e_j, ma_j.filter { case (key, _) => key != m_i })
        } else {
          (e_j, ma_j)
        }
      }

    }
    (A, instTotal(E, A, r))
  }
}
