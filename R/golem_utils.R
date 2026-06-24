# golem_utils.R — utilidades internas de golem
# Generado automáticamente por golem

is_it_golem <- function() {
  res <- try(golem::get_golem_options("golem_name"), silent = TRUE)
  !inherits(res, "try-error")
}
