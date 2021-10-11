# Basis set loading function

#' Download basis sets
#'
#' This function takes a name of an element and returns its basis set.
#'
#' @param basis
#' @param element
#'
#' @importFrom httr GET status_code
#' @importFrom stringr str_to_lower str_glue
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map pluck
#' @importFrom tidyr unnesst_wieder, pivot_longer separate
#' @importFrom dplyr mutate
#'
#' @return
#' @export
#'
#' @examples
load_basis_set <- function(basis, element) {
  atomicNumber <- periodic_table |>
    dplyr::filter(
      stringr::str_to_lower(name) == stringr::str_to_lower(element) |
        stringr::str_to_lower(symbol) == stringr::str_to_lower(element)
    ) |>
    dplyr::pull("atomicNumber")


  res <-
    httr::GET(
      stringr::str_glue(
        "https://www.basissetexchange.org/api/basis/{basis}/format/json/?version=1&elements={atomicNumber}"
      )
    )

  if (httr::status_code(res) == 200) {
    data <-
      jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8")) |>
      purrr::pluck("elements") |>
      purrr::map("electron_shells") |>
      purrr::pluck(1) |>
      tidyr::unnest_wider(col = "exponents", names_sep = "_") |>
      tidyr::unnest_wider(col = "coefficients", names_sep = "_") |>
      tidyr::pivot_longer(cols = c(dplyr::starts_with("exponents"), dplyr::starts_with("coefficients"))) |>
      dplyr::mutate(value = as.numeric(value)) |>
      tidyr::separate(name, into = c("name", "index")) |>
      tidyr::pivot_wider()
  }

  data
}
