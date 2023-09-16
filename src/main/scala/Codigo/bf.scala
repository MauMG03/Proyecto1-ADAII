object Bf {
    def main(args:Array[String]) {

        /*
            * @function: print_arr_as_matrix: Prints a matrix as an array.
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
        */
        def print_arr_as_matrix(arr:Array[Int], cols_dim:Int):Unit = {
            var counter = 0;
            for (i <- 0 until arr.length){
                if (counter == cols_dim) {
                    println();
                    counter = 0;
                }
                print(arr(i));
                print(" ");
                counter += 1;
            }
            println();
         }


        /*
            * @function: min_first_k: Return the min value's index among the first k elements in arr.
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
            * examples:
                    arr = [1.3, 0.5, 4.5, 0.2], k = 2 >> 0.5
        */

        def min_first_k_index(arr:Array[Double], k:Int):Int = {
            if (arr.length == 1) return 0;
            var cur_min = arr(0);
            var cur_min_index = 0;

            var i = 1;

            while (i < k) {
                if (arr(i) < cur_min) {
                    cur_min = arr(i);
                    cur_min_index = i;
                }
                i += 1;            
            }
            
            cur_min_index
        }

        /*
            * @function: print_array: Imprime en consola los valores del arreglo de 1D arr.
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
        */

        def print_array(arr:Array[Int]):Unit = {
            for (i <- 0 until arr.length) {
                print(arr(i));
                print(" ");
            }
            println();
        }


        /*
            * @function: get_matrix_value_arr: Imprime la coordenada (i, j) de la matriz que tiene cols_dim columnas
            representada como un arreglo arr.
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
            * @examples: 
                - i = 1, j = 0, cols_dim = 2, arr = [1, 2, 3, 4] >> 3
        */

        def get_matrix_value_arr(i:Int, j:Int, cols_dim:Int, arr:Array[Int]):Int = {
            arr(i*cols_dim + j);
        }



        /*
            * Inspired on: https://www.geeksforgeeks.org/generate-all-the-binary-strings-of-n-bits/
            * @function: generate_all_combinations: Generate all possible binary combinations of the base_arr array without changing
            those values equal to zero in base_arr
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
            * @examples:
                - base_arr = [5, 4, 3]  >> [[0, 0, 0], [0,0,1], [0,1,0], ... , [1, 1, 1] ]
                - base_arr = [0, 2, 0] >> [[0, 0, 0], [0, 1, 0]]
        */

        def generate_all_combinations(base_arr:Array[Int]):Array[Array[Int]] = {

            val arr_length = base_arr.length;

            var fixed_vals = 0;

            for (w <- 0 to arr_length - 1) if (base_arr(w) == 0) fixed_vals += 1;

            val possible_comb_q = scala.math.pow(2, arr_length-fixed_vals);

            var arr = Array.ofDim[Int](arr_length);

            var matrix_arr = Array.ofDim[Int](possible_comb_q.toInt, arr_length)

            var next_pos = 0;

            def generate_all_combinations_aux(base_arr:Array[Int], arr:Array[Int], i:Int, n:Int):Unit = {
                if (i == n) {
                    matrix_arr(next_pos) = arr.clone();
                    next_pos += 1;
                    return;
                } else {
                    if (base_arr(i) == 0) generate_all_combinations_aux(base_arr, arr, i+1, n);
                    else {
                        arr(i) = 0;
                        generate_all_combinations_aux(base_arr, arr, i+1, n);

                        arr(i) = 1;
                        generate_all_combinations_aux(base_arr, arr, i+1, n);
                    }
                    
                }
            }

            generate_all_combinations_aux(base_arr, arr, 0, arr_length);

            matrix_arr
        }


        /*
            * @function: check_max_quota: Check if the binary-array (matrix rxc) arr accomplishes the max quota condition specified in the 
            max_quota array
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
            * @examples: 
                - arr = Array(1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0), r = 3, c = 5, max_quota = [3, 4, 2]
                 >> true, because sum(1, 1, 0, 0, 1) <= 3, sum(1, 1, 1, 0, 1) <= 4, sum( 0, 0, 1, 1, 0) <= 2

                - arr = Array(1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0), r = 3, c = 5, max_quota = [3, 4, 2]
                 >> false, because sum(1, 1, 0, 1, 1) not <= 3, sum(1, 1, 1, 0, 1) <= 4, sum( 0, 0, 1, 1, 0) <= 2
        */

        def check_max_quota(arr:Array[Int], r:Int, c:Int, max_quota:Array[Int]):Boolean = {
            var counter = 0;
            for (i <- 0 until r) {
                counter = 0;
                for (j <- 0 until c) counter += get_matrix_value_arr(i, j, c, arr);
                if (counter > max_quota(i)) return false;
            }
            true;
        }


        /*
            * @function: calc_general_diss: Calculate the general dissatisfaction for the solution-matrix: assign_matrix (rxc) using
            the priorities specified in priority_matrix (rxc)
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
            * @examples: 
                -
                    priority_matrix = [5, 4, 0, 2, 3, 2, 1, 3, 0, 2, 1, 3, 2, 3, 3],
                    assign_matrix = [1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0],
                    r = 3, c = 5
                    >> 0.09833333333333334

        */

        def calc_general_diss(priority_matrix:Array[Int], assign_matrix: Array[Int], r:Int, c:Int):Double = {
            var diss_arr = Array.ofDim[Double](c);
            
            var total_required = 0.toDouble;
            var total_registered = 0.toDouble;
            var total_rq_not_rg = 0.toDouble; // total required not registered

            var cur_priority_val = 0;
            var cur_assign_val = 0;

            var ind_diss = 0.toDouble;

            for (j <- 0 until c) {

                total_required = 0.toDouble;
                total_registered = 0.toDouble;
                total_rq_not_rg = 0.toDouble;

                for (i <- 0 until r) {
                    cur_priority_val = get_matrix_value_arr(i, j, c, priority_matrix);
                    cur_assign_val = get_matrix_value_arr(i, j, c, assign_matrix);
                    if (cur_priority_val != 0) {
                        total_required += 1;

                        if (cur_assign_val == 0) total_rq_not_rg += cur_priority_val;
                    }

                    total_registered += cur_assign_val;
                }  

                // Calculate individual dissatisfaction for current student
                ind_diss = (1 - (total_registered/total_required))*(total_rq_not_rg/((3*total_required) - 1));

                diss_arr(j) = ind_diss;

            }

            var total_diss = 0.toDouble;

            for (i <- 0 until diss_arr.length ) total_diss += diss_arr(i);  

            total_diss/c;
        }


        /*
            * @function: solve_quota_distribution_bf: Solves the quota distribution problem using  a brute force strategy
            * @author: 2023 Paul Rodrigo Rojas Guerrero <paul.rojas@correounivalle.edu.co> <PaulRodrigoRojasECL@gmail.com>
            * @examples: 
                -
                    priority_matrix = [5, 4, 0, 2, 3, 2, 1, 3, 0, 2, 1, 3, 2, 3, 3],
                    max_quota = [3, 4, 2],
                    r = 3, c = 5
                    >> [1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0]

        */


        def solve_quota_distribution_bf(priority_matrix:Array[Int], max_quota:Array[Int], r:Int, c:Int):Array[Int] = {

            var possible_matrices = generate_all_combinations(priority_matrix);

            var diss_arr = Array.ofDim[Double](possible_matrices.length);

            var k = 0;
            
            var l = 0;

            var feasible = false;

            while (l < diss_arr.length) {
                feasible = check_max_quota(possible_matrices(l), r, c, max_quota );
                if (feasible == true) {
                    diss_arr(k) = calc_general_diss(priority_matrix, possible_matrices(l), r, c);
                    // Note that possible_matrices array is being re-used to save the first k-elements that are already filtered
                    possible_matrices(k) = possible_matrices(l);
                    k += 1;
                }
                l += 1;
            }
            
            var min_index = min_first_k_index(diss_arr, k);

            println(diss_arr(min_index))

            possible_matrices(min_index)

        }

        val b = Array(
            5, 4, 0, 2, 3,
            2, 1, 3, 0, 2,
            1, 3, 2, 3, 3
        )

        val d = solve_quota_distribution_bf(b, Array(3, 4, 2), 3, 5);

        print_arr_as_matrix(d, 5);
        
    }
}
