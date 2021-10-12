#' Function to generate STO-6G type orbital functions
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
sto_6g_generator <- function(element) {

  basis_data <- load_basis_set("sto-6g", element)

  sto_6g_function  <- function(radius) {
    gto_a <- gto_generator(basis_data$exponents[[1]])
    gto_b <- gto_generator(basis_data$exponents[[2]])
    gto_c <- gto_generator(basis_data$exponents[[3]])
    gto_d <- gto_generator(basis_data$exponents[[4]])
    gto_e <- gto_generator(basis_data$exponents[[5]])
    gto_f <- gto_generator(basis_data$exponents[[6]])

    gto_funs <- list(gto_a, gto_b, gto_c, gto_d, gto_e, gto_f)
    output <- gto_funs |>
      purrr::map_dbl(~ .x(radius)) |>
      purrr::map2_dbl(basis_data$coefficients, ~ .x * .y) |>
      purrr::reduce(`+`)
    output
  }

  return(sto_6g_function)
}
