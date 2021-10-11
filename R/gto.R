# Function for calculating Gaussian Type Orbitals

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
