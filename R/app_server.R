#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  mod_abund_unmarked_server("abund_unmarked")
  mod_acerca_de_server("acerca_de")
}
