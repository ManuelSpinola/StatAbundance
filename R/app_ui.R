#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatAbundance")
  )

  page_navbar(
    header = shinyjs::useShinyjs(),
    title  = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatAbundance.png", height = "38px"),
      span("StatAbundance", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),

    # \u2500\u2500 M\u00f3dulo 1: N-mixture unmarked (activo) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    nav_panel(
      title = "Abundancia frecuentista (unmarked)",
      icon  = bs_icon("bar-chart-line"),
      mod_abund_unmarked_ui("abund_unmarked")
    ),

    # \u2500\u2500 M\u00f3dulo 2: spAbundance (pr\u00f3ximamente) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    nav_panel(
      title = "Abundancia espacial (spAbundance)",
      icon  = bs_icon("globe-americas"),
      proximamente_ui(
        icono     = "globe-americas",
        titulo    = "Abundancia espacial avanzada (spAbundance)",
        subtitulo = paste0(
          "Modelos de abundancia con estructura espacial expl\u00edcita mediante ",
          "Nearest Neighbour Gaussian Processes (NNGP) e inferencia bayesiana v\u00eda MCMC. ",
          "Soporta modelos de una y m\u00faltiples especies con correlaci\u00f3n espacial ",
          "entre sitios. Dise\u00f1ado para datasets masivos con m\u00e1s de 100\u000 sitios. ",
          "Paquete: spAbundance (Doser et al. 2024)."
        ),
        paquete  = "spAbundance \u2014 Doser et al. (2024)",
        datasets = "Datos de aves de Norteam\u00e9rica (BBS) \u00b7 ejemplos simulados multi-especie"
      )
    ),

    nav_spacer(),

    nav_panel(
      title = "Acerca de",
      icon  = bs_icon("info-circle"),
      mod_acerca_de_ui("acerca_de")
    ),

    nav_item(
      tags$span(class = "text-white-50 small", "StatAbundance v1.0")
    )
  )
}
