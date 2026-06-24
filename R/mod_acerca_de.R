# ============================================================
# mod_acerca_de.R — Información sobre StatAbundance
# StatAbundance · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatAbundance",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatAbundance es la app de modelos de abundancia de sitios de StatSuite, ",
        "desarrollada en el ICOMVIS, Universidad Nacional, Costa Rica. ",
        "Estima la abundancia real de una especie corrigiendo por detectabilidad imperfecta, ",
        "usando modelos N-mixture (Royle 2004) y marcos espaciales avanzados (spAbundance)."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite \u2014 Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),    " \u2014 Dise\u00f1o de estudios y muestreo"),
              tags$li(strong("StatFlow"),      " \u2014 Primeros an\u00e1lisis y visualizaci\u00f3n"),
              tags$li(strong("StatGeo"),       " \u2014 An\u00e1lisis espacial y mapas"),
              tags$li(strong("StatMonitor"),   " \u2014 Monitoreo poblacional"),
              tags$li(strong("StatModels"),    " \u2014 Modelos estad\u00edsticos"),
              tags$li(strong("StatOccu"),      " \u2014 Modelos de ocupaci\u00f3n"),
              tags$li(strong("StatAbundance"), " \u2014 Modelos de abundancia \u2190 aqu\u00ed")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "M\u00f3dulos de StatAbundance"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(
                bs_icon("check-circle-fill", class = "me-1",
                        style = paste0("color:", colores$exito)),
                strong("Abundancia N-mixture"),
                " \u2014 Royle (2004), paquete ", code("unmarked")
              ),
              tags$li(
                bs_icon("hourglass-split", class = "me-1",
                        style = paste0("color:", colores$acento)),
                strong("Abundancia espacial"),
                " \u2014 spAbundance (pr\u00f3ximamente)"
              )
            )
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("book", class = "me-1"), "Referencias clave"),
        card_body(
          tags$ul(
            class = "small text-muted mb-0",
            tags$li(
              "Royle, J.A. (2004). N-mixture models for estimating population size from ",
              "spatially replicated counts. ",
              em("Biometrics"), ", 60(1), 108\u2013115."
            ),
            tags$li(
              class = "mt-1",
              "Fiske, I. & Chandler, R. (2011). unmarked: An R package for fitting ",
              "hierarchical models of wildlife occurrence and abundance. ",
              em("Journal of Statistical Software"), ", 43(10), 1\u201323."
            ),
            tags$li(
              class = "mt-1",
              "Doser, J.W., Finley, A.O., K\u00e9ry, M. & Zipkin, E.F. (2022). ",
              "spOccupancy: An R package for single-species, multi-species, and ",
              "integrated spatial occupancy models. ",
              em("Methods in Ecology and Evolution"), ", 13(8), 1670\u20131678."
            )
          )
        )
      ),

      div(
        class = "alert alert-info small mt-3",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva por ahora
  })
}
