## code to prepare `periodic_table` dataset goes here

library(httr)
library(jsonlite)

url <-
  "https://periodic-table-elements-info.herokuapp.com/elements"


elements <- GET(url)

if (status_code(elements) == 200) {
  periodic_table <- fromJSON(content(elements, as = "text", encoding = "UTF-8"))
}

usethis::use_data(periodic_table, overwrite = TRUE)
