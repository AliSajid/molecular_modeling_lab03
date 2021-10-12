#' Function to generate STO-2G type orbital functions
#'
#' This function takes in the name of an element and a basis et and generates a function that can return the energy at that radius.
#
#'
#' @param element The name or symbol of the element
#'
#' @importFrom purrr map_dbl map2_dbl reduce
#'
#' @return
#' @export
#'
#' @examples
#' TRUE
sto_2g_generator <- function(element) {

  basis_data <- load_basis_set("sto-2g", element)

  sto_2g_function  <- function(radius) {
    gto_a <- gto_generator(basis_data$exponents[[1]])
    gto_b <- gto_generator(basis_data$exponents[[2]])

    gto_funs <- list(gto_a, gto_b)
    output <- gto_funs |>
      purrr::map_dbl(~ .x(radius)) |>
      purrr::map2_dbl(basis_data$coefficients, ~ .x * .y) |>
      purrr::reduce(`+`)
    output
  }

  return(sto_2g_function)
}
