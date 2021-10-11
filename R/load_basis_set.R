# Basis set loading function

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

  if (status_code(res) == 200) {
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
