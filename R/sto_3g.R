# Function to generate STO-3G type orbitals

#' Function to generate STO-3G type orbital functions
#'
#' #' Function to generate Slater-Type Orbital Functions
#'
#'This function takes in the name of an element and a basiss et and generates a function that can return the energy at that radius.
#
#'
#' @param basis
#' @param element
#'
#' @importFrom purrr map_dbl map2_dbl reduce
#'
#' @return
#' @export
#'
#' @examples
#' TRUE
sto_3g_generator <- function(basis, element) {

  basis_data <- load_basis_set(basis, element)

  sto_3g_function  <- function(radius) {
    gto_a <- gto_generator(basis_data$exponents[[1]])
    gto_b <- gto_generator(basis_data$exponents[[2]])
    gto_c <- gto_generator(basis_data$exponents[[3]])

    gto_funs <- list(gto_a, gto_b, gto_c)
    output <- gto_funs |>
      purrr::map_dbl(~ .x(radius)) |>
      purrr::map2_dbl(basis_data$coefficients, ~ .x * .y) |>
      purrr::reduce(`+`)
    output
  }

  return(sto_3g_function)
}
