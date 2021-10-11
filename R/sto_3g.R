# Function to generate STO-3G type orbitals

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
