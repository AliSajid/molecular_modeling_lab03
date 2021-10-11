# Function for calculating Slater Type Orbitals

#' Function to generate Slater-Type Orbital Functions
#'
#' This function takes in a constatne "zeta" and returns a function that takes
#' in the radius and gives you the energy at that radius.
#'
#' @param zeta a numeric
#'
#' @return
#' @export
#'
#' @examples
#' TRUE
sto_generator <- function(zeta) {
  sto_function  <- function(radius) {
    z <- zeta ** 3
    e <- exp(1)
    zr <- -zeta * radius
    out <- sqrt(z/pi) * e ** (zr)
    out
  }

  return(sto_function)
}
