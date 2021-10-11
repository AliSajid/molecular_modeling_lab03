# Function for calculating Slater Type Orbitals

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
