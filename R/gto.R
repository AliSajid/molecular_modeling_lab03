# Function for calculating Gaussian Type Orbitals

#' Function to generate Gaussian-Type Orbital Functions
#'
#' This function takes in a constant "alpha" and returns a function that takes
#' in the radius and gives you the energy at that radius.
#'
#' @param alpha a numeric
#'
#' @return
#' @export
#'
#' @examples
#' TRUE
gto_generator <- function(alpha) {
  gto_function  <- function(radius) {
    a <- alpha * 2
    e <- exp(1)
    ar <- -alpha * (radius ** 2)
    out <- (a / pi) ** 0.75 * e ** (ar)
    out
  }

  return(gto_function)
}
