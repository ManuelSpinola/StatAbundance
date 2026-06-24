# ============================================================
# mod_abund_unmarked.R — Modelos N-mixture de abundancia
# StatAbundance · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: modelos N-mixture de una sola especie (Royle 2004)
# Ecosistema: unmarked + tidyverse + ggplot2
# ============================================================

# ── Loaders de datasets desde inst/app/data/ ──────────────

.cargar_aves <- function() {
  df      <- readRDS(app_sys("app/data/datos_aves.rds"))
  cols_y  <- grep("^y\\.", names(df), value = TRUE)
  cols_oc <- grep("^esfuerzo\\.", names(df), value = TRUE)
  cols_si <- setdiff(names(df), c(cols_y, cols_oc))
  list(
    y        = as.matrix(df[, cols_y]),
    cov_sitio = if (length(cols_si) > 0) df[, cols_si, drop = FALSE] else NULL,
    cov_obs  = if (length(cols_oc) > 0)
                 list(esfuerzo = df[, cols_oc, drop = FALSE]) else NULL,
    n_sitios = nrow(df),
    n_ocas   = length(cols_y)
  )
}

.cargar_ranas <- function() {
  df      <- readRDS(app_sys("app/data/datos_ranas.rds"))
  cols_y  <- grep("^y\\.", names(df), value = TRUE)
  cols_oc <- grep("^temp\\.", names(df), value = TRUE)
  cols_si <- setdiff(names(df), c(cols_y, cols_oc))
  list(
    y         = as.matrix(df[, cols_y]),
    cov_sitio = if (length(cols_si) > 0) df[, cols_si, drop = FALSE] else NULL,
    cov_obs   = if (length(cols_oc) > 0)
                  list(temp = df[, cols_oc, drop = FALSE]) else NULL,
    n_sitios  = nrow(df),
    n_ocas    = length(cols_y)
  )
}

.cargar_mallard <- function() {
  df      <- readRDS(app_sys("app/data/datos_mallard.rds"))
  cols_y  <- grep("^y\\.", names(df), value = TRUE)
  cols_dt <- grep("^date\\.", names(df), value = TRUE)
  cols_iv <- grep("^ivel\\.", names(df), value = TRUE)
  cols_si <- setdiff(names(df), c(cols_y, cols_dt, cols_iv))
  oc <- list()
  if (length(cols_dt) > 0) oc$date <- df[, cols_dt, drop = FALSE]
  if (length(cols_iv) > 0) oc$ivel <- df[, cols_iv, drop = FALSE]
  list(
    y         = as.matrix(df[, cols_y]),
    cov_sitio = if (length(cols_si) > 0) df[, cols_si, drop = FALSE] else NULL,
    cov_obs   = if (length(oc) > 0) oc else NULL,
    n_sitios  = nrow(df),
    n_ocas    = length(cols_y)
  )
}

# ── UI ────────────────────────────────────────────────────
mod_abund_unmarked_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("bar-chart-line", class = "me-2"),
        "Modelos de abundancia N-mixture",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Los modelos N-mixture (Royle 2004) estiman la ",
        tags$strong("abundancia verdadera"), " (N) de una especie en cada sitio ",
        "a partir de conteos repetidos, corrigiendo el sesgo causado por la ",
        tags$strong("detectabilidad imperfecta"), " (p < 1)."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "El problema del conteo imperfecto"),
              card_body(
                p(class = "small text-muted mb-3",
                  "Cuando contamos individuos en el campo, solo detectamos una fracci\u00f3n ",
                  "de los que realmente est\u00e1n presentes. El conteo observado (y) ",
                  tags$strong("siempre subestima"), " la abundancia real (N). ",
                  "Ignorar esto sesga la estimaci\u00f3n de la abundancia y sus relaciones ",
                  "con covariables del h\u00e1bitat."
                ),
                div(
                  div(
                    style = "display:flex; gap:8px; align-items:center; margin-bottom:6px;",
                    div(
                      style = "background:#E8F4FB; border-radius:8px; padding:8px 12px;
                               text-align:center; min-width:110px;",
                      bs_icon("people-fill", style = "color:#1170AA; font-size:1.2rem;"),
                      tags$br(),
                      tags$b(class = "small", style = "color:#1170AA", "N real"),
                      p(class = "small text-muted mb-0", "ej. N = 12")
                    ),
                    div(style = "color:#A3ACB9; font-size:1.1rem;", "\u2192"),
                    div(
                      style = "display:flex; flex-direction:column; gap:6px; flex:1;",
                      div(
                        style = "background:#FFF3E0; border-radius:8px; padding:6px 12px;
                                 display:flex; align-items:center; gap:8px;",
                        bs_icon("eye-fill", style = "color:#FC7D0B; font-size:1.1rem;"),
                        div(
                          tags$b(class = "small", style = "color:#FC7D0B", "Detectados"),
                          p(class = "small text-muted mb-0", "y = 7  (p \u2248 0.58)")
                        )
                      ),
                      div(
                        style = "background:#EEF3FA; border-radius:8px; padding:6px 12px;
                                 display:flex; align-items:center; gap:8px;
                                 border: 1px solid #C8D9EC;",
                        bs_icon("eye-slash", style = "color:#5FA2CE; font-size:1.1rem;"),
                        div(
                          tags$b(class = "small", style = "color:#5FA2CE",
                                 "No detectados"),
                          p(class = "small text-muted mb-0", "5 individuos perdidos")
                        )
                      )
                    )
                  )
                )
              )
            ),
            card(
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Estructura jer\u00e1rquica del modelo"),
              card_body(
                p(class = "small text-muted mb-2",
                  "El modelo N-mixture tiene dos niveles estimados simult\u00e1neamente:"
                ),
                div(
                  style = paste0("border-left: 4px solid #1170AA;",
                                 " padding: 8px 12px; margin-bottom:10px;",
                                 " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#1170AA",
                         bs_icon("people-fill", class = "me-1"),
                         "Nivel 1 \u2014 Proceso de abundancia"),
                  p(class = "small mb-1 mt-1",
                    style = "font-family: monospace;", "N[i] ~ Poisson(\u03bb[i])"),
                  p(class = "small text-muted mb-0",
                    "\u03bb puede depender de covariables del sitio.")
                ),
                div(
                  style = paste0("border-left: 4px solid #FC7D0B;",
                                 " padding: 8px 12px;",
                                 " background:#FFF3E0; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#FC7D0B",
                         bs_icon("eye-fill", class = "me-1"),
                         "Nivel 2 \u2014 Proceso de detecci\u00f3n"),
                  p(class = "small mb-1 mt-1",
                    style = "font-family: monospace;",
                    "y[i,j] | N[i] ~ Binomial(N[i], p[i,j])"),
                  p(class = "small text-muted mb-0",
                    "p puede depender de covariables de la ocasi\u00f3n.")
                )
              )
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",
            card(
              card_header(bs_icon("shield-check", class = "me-1"), "Supuestos clave"),
              card_body(
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li(tags$strong("Poblaci\u00f3n cerrada:"),
                    " N[i] no cambia entre ocasiones."),
                  tags$li(tags$strong("Independencia:"),
                    " detecciones independientes entre individuos."),
                  tags$li(tags$strong("Sin falsos positivos:"),
                    " cada detecci\u00f3n es un individuo real."),
                  tags$li(tags$strong("\u2265 2 ocasiones"),
                    " por sitio para estimar p separado de \u03bb.")
                )
              )
            ),
            card(
              card_header(bs_icon("camera", class = "me-1"),
                          "\u00bfCu\u00e1ndo usar modelos N-mixture?"),
              card_body(class = "p-0",
                tags$table(
                  class = "table table-sm table-bordered small mb-0",
                  style = "background: #ffffff;",
                  tags$thead(
                    style = "background:#1170AA; color: #ffffff;",
                    tags$tr(
                      tags$th("Tipo de dato"), tags$th("Organismo"),
                      tags$th("Cov. \u03bb"), tags$th("Cov. p")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td("Puntos de conteo"), tags$td("Aves"),
                      tags$td("Bosque, elev."), tags$td("Viento, hora")
                    ),
                    tags$tr(style = paste0("background:", colores$fondo),
                      tags$td("Transectos"), tags$td("Anf\u00edbios"),
                      tags$td("Humedad, veg."), tags$td("Lluvia, temp.")
                    ),
                    tags$tr(
                      tags$td("C\u00e1maras trampa"), tags$td("Mam\u00edferos"),
                      tags$td("H\u00e1bitat"), tags$td("D\u00edas-trampa")
                    )
                  )
                )
              )
            )
          )
        )
      ), # /PESTAÑA 1

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("lightbulb", class = "me-1"), "Fundamentos"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(bs_icon("calculator", class = "me-1"),
                          "Verosimilitud marginal"),
              card_body(
                p(class = "small text-muted mb-3",
                  "N[i] no es observado directamente; se marginaliza sobre todos los ",
                  "valores posibles de N hasta K:"
                ),
                div(style = paste0("border-left: 4px solid #1170AA;",
                                   " padding: 8px 12px; background:#E8F4FB;",
                                   " border-radius:0 6px 6px 0; margin-bottom:8px;"),
                  tags$b(class = "small", style = "color:#1170AA", "Verosimilitud marginal"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "L(y[i]) = \u03a3\u2099 P(y[i]|N) \u00b7 P(N|\u03bb)")
                ),
                div(style = paste0("border-left: 4px solid #5FA2CE;",
                                   " padding: 8px 12px; background:#EEF3FA;",
                                   " border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#5FA2CE", "Truncamiento K"),
                  p(class = "small text-muted mb-0 mt-1",
                    "K debe ser mayor que el conteo m\u00e1ximo observado. ",
                    "Un valor 3\u20135\u00d7 el m\u00e1ximo suele ser suficiente.")
                )
              )
            ),
            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Covariables: log-link y logit"),
              card_body(
                div(style = paste0("border-left: 4px solid #1170AA;",
                                   " padding: 8px 12px; margin-bottom:8px;",
                                   " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#1170AA",
                         bs_icon("people-fill", class = "me-1"), "Abundancia (\u03bb)"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "log(\u03bb[i]) = \u03b2\u2080 + \u03b2\u2081\u00b7bosque[i]"),
                  p(class = "small text-muted mb-0",
                    "\u03b2\u2081 > 0 \u2192 m\u00e1s bosque = mayor abundancia esperada")
                ),
                div(style = paste0("border-left: 4px solid #FC7D0B;",
                                   " padding: 8px 12px;",
                                   " background:#FFF3E0; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#FC7D0B",
                         bs_icon("eye-fill", class = "me-1"), "Detecci\u00f3n (p)"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "logit(p[i,j]) = \u03b1\u2080 + \u03b1\u2081\u00b7esfuerzo[i,j]"),
                  p(class = "small text-muted mb-0",
                    "\u03b1\u2081 > 0 \u2192 m\u00e1s esfuerzo = mayor detecci\u00f3n")
                )
              )
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",
            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Distribuci\u00f3n de mezcla para N"),
              card_body(
                p(class = "small text-muted mb-3",
                  "En un modelo N-mixture, la verosimilitud combina dos distribuciones: ",
                  "la de ", tags$strong("N dado \u03bb"), " (cu\u00e1ntos individuos hay realmente) ",
                  "y la de ", tags$strong("y dado N y p"), " (cu\u00e1ntos se detectaron). ",
                  "El t\u00e9rmino ", tags$em("mixture"), " hace referencia precisamente a esa ",
                  "mezcla. La opci\u00f3n de distribuci\u00f3n afecta c\u00f3mo se modela N:"
                ),
                div(style = paste0("border-left: 4px solid #1170AA;",
                                   " padding: 8px 12px; background:#E8F4FB;",
                                   " border-radius:0 6px 6px 0; margin-bottom:6px;"),
                  tags$b(class = "small", style = "color:#1170AA", "Poisson"),
                  p(class = "small text-muted mb-0",
                    "Varianza = media. Punto de partida natural. ",
                    "Adecuado cuando la variabilidad en los conteos es moderada y consistente entre sitios.")
                ),
                div(style = paste0("border-left: 4px solid #FC7D0B;",
                                   " padding: 8px 12px; background:#FFF3E0;",
                                   " border-radius:0 6px 6px 0; margin-bottom:6px;"),
                  tags$b(class = "small", style = "color:#FC7D0B", "Binomial negativa"),
                  p(class = "small text-muted mb-0",
                    "Varianza > media (sobredispersi\u00f3n). \u00datil cuando hay sitios con conteos ",
                    "muy altos y muchos con conteos bajos \u2014 m\u00e1s variabilidad de la que ",
                    "Poisson puede explicar.")
                ),
                div(style = paste0("border-left: 4px solid #5FA2CE;",
                                   " padding: 8px 12px; background:#EEF3FA;",
                                   " border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#5FA2CE",
                         "ZIP (Zero-Inflated Poisson)"),
                  p(class = "small text-muted mb-0",
                    "Poisson con exceso estructural de ceros. Apropiado cuando algunos sitios ",
                    "tienen abundancia cero porque la especie ", tags$em("nunca puede estar ah\u00ed"),
                    " \u2014 no solo por baja detectabilidad.")
                )
              )
            ),
            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Selecci\u00f3n de modelos (AIC)"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Par\u00e1metros estimados por m\u00e1xima verosimilitud. ",
                  "Modelos candidatos comparados con AIC:"
                ),
                tags$ul(
                  class = "small text-muted mb-0",
                  style = "padding-left: 1.2rem;",
                  tags$li(tags$code("\u03bb(.) p(.)"), " \u2014 modelo nulo"),
                  tags$li(tags$code("\u03bb(cov) p(.)"), " \u2014 abundancia var\u00eda"),
                  tags$li(tags$code("\u03bb(.) p(cov)"), " \u2014 detecci\u00f3n var\u00eda"),
                  tags$li(tags$code("\u03bb(cov) p(cov)"), " \u2014 ambos var\u00edan")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 2

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(
          navset_tab(
            nav_panel(
              title = tagList(bs_icon("upload", class = "me-1"), "Cargar datos"),
              div(class = "mt-3",
                layout_columns(
                  col_widths = c(6, 6),

                  # Columna izquierda: datasets de ejemplo
                  div(
                    tags$b(bs_icon("database", class = "me-1"), "Datasets de ejemplo"),
                    p(class = "small text-muted mt-1 mb-3",
                      "Selecciona un dataset para explorar la app ",
                      "sin necesidad de tus propios datos."
                    ),
                    radioButtons(
                      ns("fuente_datos"),
                      label   = NULL,
                      choices = c(
                        "Aves \u2014 puntos de conteo (simulado)"                      = "aves",
                        "Ranas \u2014 transectos nocturnos (simulado)"                 = "ranas",
                        "Mallard \u2014 patos (unmarked \u00b7 K\u00e9ry et al. 2005)" = "mallard"
                      ),
                      selected = "aves"
                    ),
                    uiOutput(ns("desc_dataset_ui"))
                  ),

                  # Columna derecha: datos propios
                  div(
                    tags$b(bs_icon("file-earmark-arrow-up", class = "me-1"),
                           "Mis propios datos"),
                    p(class = "small text-muted mt-1 mb-3",
                      "Carga un archivo CSV o XLSX con tus propios conteos."
                    ),
                    fileInput(
                      ns("archivo_usuario"),
                      label       = NULL,
                      accept      = c(".csv", ".xlsx"),
                      buttonLabel = tagList(
                        bs_icon("folder2-open", class = "me-1"), "Examinar\u2026"),
                      placeholder = "Sin archivo seleccionado"
                    ),
                    div(
                      class = "p-3",
                      style = paste0("background:", colores$fondo,
                                     "; border-left: 4px solid ", colores$primario,
                                     "; border-radius: 0 6px 6px 0;"),
                      tags$b(bs_icon("info-circle", class = "me-1",
                                     style = paste0("color:", colores$primario)),
                             "Formato requerido"),
                      p(class = "small text-muted mt-1 mb-2",
                        "Cada fila representa un sitio."),
                      tags$ul(
                        class = "small text-muted mb-3",
                        style = "list-style: none; padding-left: 0;",
                        tags$li(class = "mb-2",
                          bs_icon("arrow-right", class = "me-1",
                                  style = paste0("color:", colores$secundario)),
                          tags$strong("Conteos:"), " columnas ",
                          tags$code("y.1"), ", ", tags$code("y.2"), ", \u2026 ",
                          tags$code("y.J"),
                          " \u2014 enteros \u2265 0. Usa ", tags$code("NA"),
                          " cuando el sitio no fue muestreado en esa ocasi\u00f3n."
                        ),
                        tags$li(class = "mb-2",
                          bs_icon("arrow-right", class = "me-1",
                                  style = paste0("color:", colores$secundario)),
                          tags$strong("Covariables de sitio:"),
                          " columnas adicionales con nombre libre, ej. ",
                          tags$code("bosque"), ", ", tags$code("elev"), "."
                        ),
                        tags$li(class = "mb-0",
                          bs_icon("arrow-right", class = "me-1",
                                  style = paste0("color:", colores$secundario)),
                          tags$strong("Covariables de observaci\u00f3n"),
                          " (opcionales): prefijo + n\u00famero, ej. ",
                          tags$code("esfuerzo.1"), ", ", tags$code("esfuerzo.2"),
                          "\u2026 El prefijo (", tags$code("esfuerzo"),
                          ") se usa en la f\u00f3rmula."
                        )
                      ),
                      tags$b(class = "small",
                             bs_icon("table", class = "me-1",
                                     style = paste0("color:", colores$primario)),
                             "Ejemplo de estructura:"),
                      div(class = "mt-2", style = "overflow-x: auto;",
                        tags$table(
                          class = "table table-sm table-bordered small mb-0",
                          style = "background:#ffffff; font-size: 0.78rem;",
                          tags$thead(
                            style = paste0("background:", colores$primario, "; color:#ffffff;"),
                            tags$tr(
                              tags$th("y.1"), tags$th("y.2"), tags$th("y.3"),
                              tags$th("bosque"), tags$th("elev"),
                              tags$th("esfuerzo.1"), tags$th("esfuerzo.2"), tags$th("esfuerzo.3")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("3"), tags$td("5"), tags$td("2"),
                              tags$td("1.2"), tags$td("340"),
                              tags$td("0.8"), tags$td("1.1"), tags$td("0.9")
                            ),
                            tags$tr(style = paste0("background:", colores$fondo),
                              tags$td("0"), tags$td("1"), tags$td("0"),
                              tags$td("-0.3"), tags$td("210"),
                              tags$td("1.0"),
                              tags$td(style = "color:#A3ACB9; font-style:italic;", "NA"),
                              tags$td("0.7")
                            ),
                            tags$tr(
                              tags$td("8"), tags$td("6"),
                              tags$td(style = "color:#A3ACB9; font-style:italic;", "NA"),
                              tags$td("2.1"), tags$td("180"),
                              tags$td("0.6"), tags$td("0.9"), tags$td("1.2")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            nav_panel(
              title = tagList(bs_icon("eye", class = "me-1"), "Vista previa"),
              div(class = "mt-3",
                uiOutput(ns("resumen_datos_ui")),
                DTOutput(ns("tabla_preview"))
              )
            ),
            nav_panel(
              title = tagList(bs_icon("grid", class = "me-1"), "Conteos"),
              div(class = "mt-3",
                p(class = "small text-muted",
                  "N\u00famero de individuos detectados por sitio y ocasi\u00f3n. ",
                  "NA = sitio no muestreado en esa ocasi\u00f3n."),
                DTOutput(ns("tabla_conteos")),
                div(class = "mt-3",
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    uiOutput(ns("vbox_sitios")),
                    uiOutput(ns("vbox_ocasiones")),
                    uiOutput(ns("vbox_max_conteo"))
                  )
                ),
                div(class = "mt-2",
                  downloadButton(ns("descarga_datos"), "Descargar datos",
                                 class = "btn-sm btn-outline-primary")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 3

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza las relaciones entre las covariables y los conteos ",
            "antes de ajustar el modelo."
          ),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("expl_sel_cov")),
                selectInput(
                  ns("expl_submodelo"),
                  label = "Submodelo:",
                  choices = c(
                    "Abundancia (\u03bb) \u2014 cov. de sitio" = "state",
                    "Detecci\u00f3n (p) \u2014 cov. de ocasi\u00f3n" = "det"
                  )
                ),
                tags$hr(),
                uiOutput(ns("expl_cards_resumen"))
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Conteo medio vs. covariable"),
                card_body(plotOutput(ns("expl_plot_cov"), height = "280px"))
              ),
              card(
                class = "mb-0",
                card_header(bs_icon("grid-3x3", class = "me-1"),
                            "Correlaci\u00f3n entre covariables"),
                card_body(plotOutput(ns("expl_plot_corr"), height = "240px"))
              )
            )
          )
        )
      ), # /PESTAÑA 4

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"), "Ajustar modelo"),
        card_body(
          p(class = "small text-muted mb-3",
            "Define los submodelos de abundancia y detecci\u00f3n. ",
            "Deja vac\u00edo para usar solo el intercepto (~1) \u2014 modelo nulo."
          ),

          # Fila 1: cards de submodelos a ancho completo
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = paste0("border-left: 4px solid ", colores$primario, ";"),
              card_header(bs_icon("people-fill", class = "me-1"),
                          "Submodelo de abundancia (\u03bb)"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Covariables de sitio que explican la abundancia esperada."),
                uiOutput(ns("cov_lambda_ui")),
                div(class = "mt-2",
                  tags$b(class = "small", "F\u00f3rmula:"),
                  verbatimTextOutput(ns("formula_lambda_preview"))
                )
              )
            ),
            card(
              style = paste0("border-left: 4px solid ", colores$acento, ";"),
              card_header(bs_icon("eye-fill", class = "me-1"),
                          "Submodelo de detecci\u00f3n (p)"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Covariables que influyen en la probabilidad de detecci\u00f3n."),
                uiOutput(ns("cov_det_ui")),
                div(class = "mt-2",
                  tags$b(class = "small", "F\u00f3rmula:"),
                  verbatimTextOutput(ns("formula_det_preview"))
                )
              )
            )
          ),

          # Fila 2: opciones compactas + botón
          div(
            class = "mt-3 p-3",
            style = paste0("background:", colores$fondo,
                           "; border-radius: 8px;"),
            layout_columns(
              col_widths = c(4, 3, 5),
              selectInput(
                ns("mixture"),
                label = tagList(bs_icon("distribute-vertical", class = "me-1"),
                                "Distribuci\u00f3n:"),
                choices = c("Poisson" = "P",
                            "Binomial negativa" = "NB",
                            "ZIP (ceros inflados)" = "ZIP"),
                selected = "P"
              ),
              numericInput(
                ns("K_upper"),
                label = tagList(bs_icon("arrow-up", class = "me-1"), "K:"),
                value = 100, min = 10, max = 500, step = 10
              ),
              div(
                class = "pt-4",
                actionButton(
                  ns("ajustar"),
                  label = tagList(bs_icon("play-fill", class = "me-1"),
                                  "Ajustar modelo"),
                  class = "btn btn-primary w-100"
                ),
                p(class = "small text-muted mt-1 mb-0",
                  bs_icon("info-circle", class = "me-1"),
                  "K \u2265 3\u00d7 el conteo m\u00e1ximo.")
              )
            )
          ),

          tags$hr(),
          p(class = "small fw-bold text-muted mb-1",
            bs_icon("floppy", class = "me-1"), "Guardar para comparar"),
          p(class = "small text-muted mb-2",
            "Dale un nombre al modelo ajustado y gu\u00e1rdalo. ",
            "Cambia las covariables, reajusta y guarda otro ",
            "para comparar en la pesta\u00f1a ", strong("Comparar modelos"), "."),
          textInput(ns("nombre_modelo_guardar"), label = NULL,
                    placeholder = "Ej: nulo, habitat, habitat+esfuerzo\u2026"),
          actionButton(
            ns("guardar_modelo"),
            label = tagList(bs_icon("bookmark-plus", class = "me-1"), "Guardar modelo"),
            class = "btn btn-outline-primary btn-sm w-100"
          ),
          div(class = "mt-3", uiOutput(ns("estado_ajuste_ui")))
        )
      ), # /PESTAÑA 5

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("list-ol", class = "me-1"), "Par\u00e1metros"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(6, 6),
            uiOutput(ns("vbox_lambda")),
            uiOutput(ns("vbox_p"))
          ),
          h5(class = "mt-3",
             style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Tabla de coeficientes"),
          DTOutput(ns("tabla_coef")),
          h5(class = "mt-3",
             style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Intervalos de confianza (escala log / logit)"),
          plotOutput(ns("plot_forest"), height = "300px")
        )
      ), # /PESTAÑA 6

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Gráficos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Gr\u00e1ficos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Curva de respuesta de \u03bb o p sobre el rango de la covariable seleccionada, ",
            "manteniendo las dem\u00e1s en su valor promedio."
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput(ns("efecto_submodelo"), "Submodelo:",
                        choices = c("Abundancia (\u03bb)" = "state",
                                    "Detecci\u00f3n (p)" = "det")),
            uiOutput(ns("sel_efecto_cov")),
            numericInput(ns("efecto_ic"), "Nivel IC (%):", 95, 80, 99, 1)
          ),
          plotOutput(ns("plot_efecto"), height = "380px")
        )
      ), # /PESTAÑA 7

      # ════════════════════════════════════════════════
      # PESTAÑA 8: λ por sitio
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("pin-map", class = "me-1"), "\u03bb por sitio"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(4, 8),
            card(
              class = "border-0",
              style = paste0("background:", colores$fondo, ";"),
              card_body(
                sliderInput(ns("umbral_lambda"),
                            "Umbral de abundancia:",
                            min = 0, max = 20, value = 1, step = 1),
                p(class = "small text-muted",
                  "Sitios con \u03bb \u2265 umbral = 'probablemente ocupados'."),
                uiOutput(ns("vbox_sitios_umbral")),
                tags$br(),
                downloadButton(ns("descarga_lambda_sitio"), "Descargar tabla",
                               class = "btn btn-outline-secondary btn-sm w-100")
              )
            ),
            plotOutput(ns("plot_lambda_sitio"), height = "420px")
          ),
          div(class = "mt-3", DTOutput(ns("tabla_lambda_sitio")))
        )
      ), # /PESTAÑA 8

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart-steps", class = "me-1"), "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Guarda modelos desde 'Ajustar modelo' y comp\u00e1ralos por AIC."
          ),
          actionButton(ns("limpiar_modelos"),
                       label = tagList(bs_icon("trash", class = "me-1"), "Limpiar lista"),
                       class = "btn btn-outline-danger btn-sm mb-2"),
          uiOutput(ns("ui_sin_modelos")),
          DTOutput(ns("tabla_aic")),
          plotOutput(ns("plot_aic"), height = "320px"),
          div(
            class = "alert alert-info small py-2 px-3 mt-3",
            bs_icon("info-circle-fill", class = "me-1"),
            tags$strong("\u0394AIC:"),
            " diferencia de AIC con respecto al mejor modelo. ",
            "Modelos con \u0394AIC < 2 tienen apoyo similar; ",
            "mayor de 10 indica muy poco respaldo emp\u00edrico."
          )
        )
      ), # /PESTAÑA 9

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard2-pulse", class = "me-1"), "Diagn\u00f3stico"),
        card_body(
          layout_columns(
            col_widths = c(8, 4),
            div(
              class = "alert mb-0",
              style = paste0("background:", colores$fondo,
                             "; border-left: 4px solid ", colores$secundario, ";"),
              bs_icon("mortarboard-fill", class = "me-2",
                      style = paste0("color:", colores$secundario)),
              tags$strong("\u00bfC\u00f3mo leer este diagn\u00f3stico?"), tags$br(),
              "El test \u03c7\u00b2 param\u00e9trico bootstrap (parboot) eval\u00faa si el modelo ",
              "reproduce adecuadamente la variabilidad observada en los conteos. ",
              "Un p-valor < 0.05 sugiere mal ajuste: considera agregar covariables, ",
              "cambiar la distribuci\u00f3n de mezcla o revisar el supuesto de cierre."
            ),
            card(
              class = "border-0",
              style = paste0("background:", colores$fondo, ";"),
              card_body(
                numericInput(ns("nsim_gof"), "Simulaciones:",
                             500, 99, 2000, 100),
                actionButton(ns("correr_gof"),
                             label = tagList(bs_icon("play-fill", class = "me-1"),
                                             "Ejecutar GoF"),
                             class = "btn btn-primary w-100 mt-1"),
                div(class = "mt-2", uiOutput(ns("res_gof_ui")))
              )
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",
            div(
              p(class = "small text-muted mb-1",
                bs_icon("bar-chart", class = "me-1"),
                tags$strong("Distribuci\u00f3n bootstrap del \u03c7\u00b2")),
              plotOutput(ns("plot_gof"), height = "280px"),
              p(class = "small text-muted mt-1",
                "Barras = \u03c7\u00b2 esperado por azar. ",
                "L\u00ednea roja = \u03c7\u00b2 de tus datos.")
            ),
            div(
              p(class = "small text-muted mb-1",
                bs_icon("bar-chart-steps", class = "me-1"),
                tags$strong("Conteos observados vs. esperados")),
              plotOutput(ns("plot_obs_esp"), height = "280px"),
              p(class = "small text-muted mt-1",
                tags$strong("Azul"), " = observado  |  ",
                tags$strong("Naranja"), " = esperado por el modelo.")
            )
          )
        )
      ), # /PESTAÑA 10

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "C\u00f3digo R"),
        card_body(
          p(class = "small text-muted",
            "C\u00f3digo R reproducible generado a partir del modelo ajustado."),
          div(class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo"), "Descargar .R",
                           class = "btn-sm btn-outline-primary")
          ),
          verbatimTextOutput(ns("codigo_r")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 11

    ) # /navset_card_tab
  ) # /tagList
} # /mod_abund_unmarked_ui


# ── Server ────────────────────────────────────────────────
mod_abund_unmarked_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos reactivos ───────────────────────────────────
    datos_crudos <- reactive({
      # Datos propios tienen prioridad
      if (!is.null(input$archivo_usuario)) {
        ext <- tools::file_ext(input$archivo_usuario$name)
        df  <- if (ext == "xlsx")
          readxl::read_excel(input$archivo_usuario$datapath)
        else
          readr::read_csv(input$archivo_usuario$datapath, show_col_types = FALSE)
        df       <- as.data.frame(df)
        cols_y   <- grep("^y\\.", names(df), value = TRUE)
        obs_cols <- setdiff(names(df), cols_y)
        obs_cols <- obs_cols[grepl("\\.[0-9]+$", obs_cols)]
        prefijos <- unique(sub("\\.[0-9]+$", "", obs_cols))
        cols_si  <- setdiff(names(df), c(cols_y, obs_cols))
        oc <- if (length(prefijos) > 0) {
          lapply(setNames(prefijos, prefijos), function(p) {
            cs <- paste0(p, ".", seq_along(cols_y))
            df[, intersect(cs, names(df)), drop = FALSE]
          })
        } else NULL
        return(list(
          y         = as.matrix(df[, cols_y]),
          cov_sitio = if (length(cols_si) > 0) df[, cols_si, drop = FALSE] else NULL,
          cov_obs   = oc,
          n_sitios  = nrow(df),
          n_ocas    = length(cols_y)
        ))
      }
      switch(input$fuente_datos,
        "aves"    = .cargar_aves(),
        "ranas"   = .cargar_ranas(),
        "mallard" = .cargar_mallard()
      )
    })

    # ── unmarkedFrame ─────────────────────────────────────
    umf <- reactive({
      req(datos_crudos())
      d <- datos_crudos()
      unmarked::unmarkedFramePCount(
        y        = d$y,
        siteCovs = d$cov_sitio,
        obsCovs  = d$cov_obs
      )
    })

    # ── Variables disponibles ─────────────────────────────
    vars_sitio <- reactive({
      req(datos_crudos())
      cs <- datos_crudos()$cov_sitio
      if (is.null(cs)) character(0) else names(cs)
    })

    vars_obs <- reactive({
      req(datos_crudos())
      oc <- datos_crudos()$cov_obs
      if (is.null(oc)) character(0) else names(oc)
    })

    # ── Descripción dataset ───────────────────────────────
    output$desc_dataset_ui <- renderUI({
      req(input$fuente_datos)
      desc <- switch(input$fuente_datos,
        "aves" = list(
          icono  = "binoculars",
          titulo = "Aves \u2014 puntos de conteo",
          items  = list(
            list(icon = "grid-3x3",     txt = "100 sitios \u00b7 4 ocasiones"),
            list(icon = "geo-alt-fill", txt = tagList("Covariables de sitio: ",
              tags$code("bosque"), ", ", tags$code("elev"))),
            list(icon = "eye-fill",     txt = tagList("Covariable de observaci\u00f3n: ",
              tags$code("esfuerzo"))),
            list(icon = "calculator",   txt = tagList("Modelo generador: Poisson con \u03bb ~ ",
              tags$code("exp(1.2 + 0.8\u00b7bosque \u2212 0.3\u00b7elev)")))
          )
        ),
        "ranas" = list(
          icono  = "droplet",
          titulo = "Ranas \u2014 transectos nocturnos",
          items  = list(
            list(icon = "grid-3x3",     txt = "60 sitios \u00b7 3 ocasiones"),
            list(icon = "geo-alt-fill", txt = tagList("Covariables de sitio: ",
              tags$code("humedad"), ", ", tags$code("vegetacion"))),
            list(icon = "eye-fill",     txt = tagList("Covariable de observaci\u00f3n: ",
              tags$code("temp"))),
            list(icon = "calculator",   txt = tagList("Modelo generador: Poisson con \u03bb ~ ",
              tags$code("exp(0.7 + 0.9\u00b7humedad + 0.4\u00b7vegetacion)")))
          )
        ),
        "mallard" = list(
          icono  = "water",
          titulo = "Mallard \u2014 patos (K\u00e9ry et al. 2005)",
          items  = list(
            list(icon = "grid-3x3",     txt = "239 sitios \u00b7 3 ocasiones"),
            list(icon = "geo-alt-fill", txt = tagList(
              "Covariables de sitio: ",
              tags$code("elev"), " (elevaci\u00f3n), ",
              tags$code("forest"), " (cobertura de bosque), ",
              tags$code("length"), " (longitud del transecto)")),
            list(icon = "eye-fill",     txt = tagList(
              "Covariables de observaci\u00f3n: ",
              tags$code("date"), " (fecha del conteo), ",
              tags$code("ivel"), " (velocidad de la corriente de agua)")),
            list(icon = "book",         txt = "Dataset de referencia cl\u00e1sico para N-mixture")
          )
        )
      )
      if (is.null(desc)) return(NULL)
      div(
        class = "mt-2 p-3",
        style = paste0("background:", colores$fondo,
                       "; border-left: 4px solid ", colores$secundario,
                       "; border-radius: 0 6px 6px 0;"),
        tags$b(
          bs_icon(desc$icono, class = "me-1",
                  style = paste0("color:", colores$secundario)),
          style = paste0("color:", colores$primario),
          desc$titulo
        ),
        tags$ul(
          class = "small text-muted mb-0 mt-2",
          style = "list-style: none; padding-left: 0;",
          lapply(desc$items, function(item) {
            tags$li(class = "mb-1",
              bs_icon(item$icon, class = "me-1",
                      style = paste0("color:", colores$secundario)),
              item$txt
            )
          })
        )
      )
    })

    # ── Resumen datos ─────────────────────────────────────
    output$resumen_datos_ui <- renderUI({
      req(datos_crudos())
      d <- datos_crudos()
      div(
        class = "alert alert-info small py-2 px-3 mb-2",
        bs_icon("info-circle-fill", class = "me-1"),
        strong(d$n_sitios), " sitios \u00d7 ",
        strong(d$n_ocas), " ocasiones. ",
        strong(length(vars_sitio())), " covariable(s) de sitio. ",
        if (length(vars_obs()) > 0)
          paste0(length(vars_obs()), " covariable(s) de observaci\u00f3n.")
        else "Sin covariables de observaci\u00f3n."
      )
    })

    output$tabla_preview <- renderDT({
      req(datos_crudos())
      d  <- datos_crudos()
      df <- if (!is.null(d$cov_sitio)) cbind(d$cov_sitio, d$y) else as.data.frame(d$y)
      datatable(df,
                options  = list(pageLength = 10, scrollX = TRUE, dom = "tp"),
                rownames = FALSE, class = "table-sm table-striped")
    })

    output$tabla_conteos <- renderDT({
      req(datos_crudos())
      datatable(
        as.data.frame(datos_crudos()$y),
        options  = list(pageLength = 15, scrollX = TRUE, dom = "tp"),
        rownames = paste("Sitio", seq_len(datos_crudos()$n_sitios)),
        class    = "table-sm table-condensed"
      )
    })

    # ── Value boxes datos ─────────────────────────────────
    vbox_card <- function(icono, label, valor, color) {
      div(class = "card h-100",
        style = paste0("background:", colores$fondo,
                       "; border-radius:8px; padding:1rem; text-align:center;"),
        div(style = paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
            bs_icon(icono, class = "me-1"), label),
        div(style = paste0("font-size:24px; font-weight:500; color:", color, ";"), valor)
      )
    }

    output$vbox_sitios    <- renderUI({
      req(datos_crudos())
      vbox_card("geo-alt", "Sitios", datos_crudos()$n_sitios, colores$primario)
    })
    output$vbox_ocasiones <- renderUI({
      req(datos_crudos())
      vbox_card("calendar3", "Ocasiones", datos_crudos()$n_ocas, colores$acento)
    })
    output$vbox_max_conteo <- renderUI({
      req(datos_crudos())
      vbox_card("bar-chart", "Conteo m\u00e1ximo",
                max(datos_crudos()$y, na.rm = TRUE), colores$secundario)
    })

    output$descarga_datos <- downloadHandler(
      filename = function() paste0("conteos_", Sys.Date(), ".csv"),
      content  = function(file) {
        d  <- datos_crudos()
        df <- if (!is.null(d$cov_sitio)) cbind(d$cov_sitio, d$y) else as.data.frame(d$y)
        write.csv(df, file, row.names = FALSE)
      }
    )

    # ── Explorar ──────────────────────────────────────────
    output$expl_sel_cov <- renderUI({
      sub  <- input$expl_submodelo
      opts <- if (sub == "state") vars_sitio() else vars_obs()
      if (length(opts) == 0) opts <- character(0)
      selectInput(ns("expl_cov"), "Covariable:", choices = opts)
    })

    output$expl_cards_resumen <- renderUI({
      req(datos_crudos(), input$expl_cov, nchar(input$expl_cov) > 0)
      d   <- datos_crudos()
      cov <- input$expl_cov
      sub <- input$expl_submodelo
      cov_data <- if (sub == "state") d$cov_sitio
                  else as.data.frame(lapply(d$cov_obs,
                    function(x) rowMeans(as.data.frame(x), na.rm = TRUE)))
      req(!is.null(cov_data), cov %in% names(cov_data))
      vals <- cov_data[[cov]]
      tagList(
        vbox_card("bar-chart-line", "Media", round(mean(vals, na.rm = TRUE), 2), colores$primario),
        br(),
        vbox_card("arrows-expand", "Rango",
                  paste0(round(min(vals, na.rm = TRUE), 2), " \u2013 ",
                         round(max(vals, na.rm = TRUE), 2)), colores$secundario)
      )
    })

    output$expl_plot_cov <- renderPlot({
      req(datos_crudos(), input$expl_cov, nchar(input$expl_cov) > 0)
      d   <- datos_crudos()
      cov <- input$expl_cov
      sub <- input$expl_submodelo
      cov_data <- if (sub == "state") d$cov_sitio
                  else as.data.frame(lapply(d$cov_obs,
                    function(x) rowMeans(as.data.frame(x), na.rm = TRUE)))
      req(!is.null(cov_data), cov %in% names(cov_data))
      df <- data.frame(
        x = cov_data[[cov]],
        y = rowMeans(d$y, na.rm = TRUE)
      )
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(color = colores$primario, alpha = 0.6, size = 2) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                             color = colores$acento, fill = colores$acento, alpha = 0.15) +
        ggplot2::labs(x = cov, y = "Conteo medio por sitio") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    output$expl_plot_corr <- renderPlot({
      req(datos_crudos())
      d <- datos_crudos()
      todas <- c(
        if (!is.null(d$cov_sitio)) as.list(d$cov_sitio) else list(),
        if (!is.null(d$cov_obs))
          lapply(d$cov_obs, function(x) rowMeans(as.data.frame(x), na.rm = TRUE))
        else list()
      )
      df_num <- as.data.frame(Filter(is.numeric, todas))
      if (ncol(df_num) < 2) {
        return(ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Se necesitan \u2265 2 covariables num\u00e9ricas",
                            size = 5, color = colores$texto) +
          ggplot2::theme_void())
      }
      cor_mat  <- cor(df_num, use = "pairwise.complete.obs")
      nms      <- colnames(cor_mat)
      df_long  <- data.frame(
        x    = rep(nms, each  = length(nms)),
        y    = rep(nms, times = length(nms)),
        corr = as.vector(cor_mat)
      )
      df_long$x <- factor(df_long$x, levels = nms)
      df_long$y <- factor(df_long$y, levels = rev(nms))
      ggplot2::ggplot(df_long, ggplot2::aes(x = x, y = y, fill = corr)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::geom_label(ggplot2::aes(label = round(corr, 2)),
                            size = 3.5, fill = "white",
                            color = colores$texto, label.size = 0) +
        ggplot2::scale_fill_gradient2(
          low      = colores$peligro,
          mid      = "white",
          high     = colores$primario,
          midpoint = 0,
          limits   = c(-1, 1),
          name     = "r"
        ) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid       = ggplot2::element_blank(),
          legend.position  = "right"
        )
    })

    # ── checkboxGroupInput para λ ─────────────────────────
    output$cov_lambda_ui <- renderUI({
      nms <- vars_sitio()
      if (length(nms) == 0)
        return(p(class = "small text-muted", "No hay covariables de sitio disponibles."))
      checkboxGroupInput(ns("cov_lambda"), label = "Covariables de sitio:",
                         choices = nms, selected = NULL)
    })

    output$cov_det_ui <- renderUI({
      nms <- c(vars_obs(), vars_sitio())
      if (length(nms) == 0)
        return(p(class = "small text-muted", "No hay covariables disponibles."))
      checkboxGroupInput(ns("cov_det"), label = "Covariables de detecci\u00f3n:",
                         choices = nms, selected = NULL)
    })

    formula_lambda <- reactive({
      covs <- input$cov_lambda
      rhs  <- if (is.null(covs) || length(covs) == 0) "1"
               else paste(covs, collapse = " + ")
      as.formula(paste("~", rhs), env = baseenv())
    })

    formula_det <- reactive({
      covs <- input$cov_det
      rhs  <- if (is.null(covs) || length(covs) == 0) "1"
               else paste(covs, collapse = " + ")
      as.formula(paste("~", rhs), env = baseenv())
    })

    output$formula_lambda_preview <- renderText({
      paste("\u03bb:", deparse(formula_lambda()))
    })
    output$formula_det_preview <- renderText({
      paste("p:", deparse(formula_det()))
    })

    # ── Ajustar modelo ────────────────────────────────────
    modelo_actual      <- reactiveVal(NULL)
    nombre_modelo_actual <- reactiveVal(NULL)

    observeEvent(input$ajustar, {
      req(umf())
      lam_str <- if (is.null(input$cov_lambda) || length(input$cov_lambda) == 0) "1"
                 else paste(input$cov_lambda, collapse = " + ")
      det_str <- if (is.null(input$cov_det) || length(input$cov_det) == 0) "1"
                 else paste(input$cov_det, collapse = " + ")
      fm_formula <- eval(
        parse(text = paste("~", det_str, "~", lam_str))[[1]],
        envir = new.env(parent = baseenv())
      )
      withProgress(message = "Ajustando modelo N-mixture\u2026", value = 0.5, {
        tryCatch({
          fm <- unmarked::pcount(
            formula = fm_formula,
            data    = umf(),
            K       = input$K_upper,
            mixture = input$mixture
          )
          modelo_actual(fm)
          nombre <- paste0(
            "\u03bb(", ifelse(length(input$cov_lambda) == 0, ".",
                               paste(input$cov_lambda, collapse = "+")), ") ",
            "p(", ifelse(length(input$cov_det) == 0, ".",
                          paste(input$cov_det, collapse = "+")), ")"
          )
          nombre_modelo_actual(nombre)
          incProgress(0.5)
        }, error = function(e) {
          showNotification(paste("Error al ajustar:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    output$estado_ajuste_ui <- renderUI({
      fm <- modelo_actual()
      if (is.null(fm)) return(NULL)
      div(
        class = "alert alert-success small py-2 px-3",
        bs_icon("check-circle-fill", class = "me-1"),
        strong("Modelo ajustado: "), nombre_modelo_actual(),
        " \u2014 log-verosimilitud: ",
        round(as.numeric(logLik(fm)), 2)
      )
    })

    # ── Parámetros ────────────────────────────────────────
    output$vbox_lambda <- renderUI({
      req(modelo_actual())
      val <- round(exp(coef(modelo_actual(), type = "state")[1]), 3)
      vbox_card("people-fill", "\u03bb estimada (intercepto)", val, colores$primario)
    })

    output$vbox_p <- renderUI({
      req(modelo_actual())
      val <- round(plogis(coef(modelo_actual(), type = "det")[1]), 3)
      vbox_card("eye-fill", "p estimada (intercepto)", val, colores$acento)
    })

    hacer_df_coef <- function(fm, tipo, label) {
      est <- coef(fm, type = tipo)
      se  <- unmarked::SE(fm, type = tipo)
      z   <- est / se
      pv  <- 2 * pnorm(-abs(z))
      data.frame(
        Submodelo = label,
        Parametro = names(est),
        Estimado  = est,
        EE        = se,
        z         = z,
        p_valor   = pv,
        check.names = FALSE, row.names = NULL
      )
    }

    output$tabla_coef <- renderDT({
      req(modelo_actual())
      fm <- modelo_actual()
      df <- dplyr::bind_rows(
        hacer_df_coef(fm, "state", "Abundancia (\u03bb)"),
        hacer_df_coef(fm, "det",   "Detecci\u00f3n (p)")
      )
      datatable(df, options = list(dom = "t", pageLength = 20),
                rownames = FALSE, class = "table-sm table-striped") |>
        DT::formatRound(c("Estimado", "EE", "z", "p_valor"), 4) |>
        DT::formatStyle("p_valor",
          color = DT::styleInterval(c(0.05, 0.1),
                                    c("#C85200", "#B85A0D", colores$texto)))
    })

    output$plot_forest <- renderPlot({
      req(modelo_actual())
      fm <- modelo_actual()
      df <- dplyr::bind_rows(
        {d <- hacer_df_coef(fm, "state", "Abundancia (\u03bb)")
         data.frame(d, lower = d$Estimado - 1.96*d$EE, upper = d$Estimado + 1.96*d$EE)},
        {d <- hacer_df_coef(fm, "det",   "Detecci\u00f3n (p)")
         data.frame(d, lower = d$Estimado - 1.96*d$EE, upper = d$Estimado + 1.96*d$EE)}
      )
      ggplot2::ggplot(df,
        ggplot2::aes(x = Estimado, y = reorder(Parametro, Estimado),
                     color = Submodelo, xmin = lower, xmax = upper)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#A3ACB9") +
        ggplot2::geom_errorbarh(height = 0.25, linewidth = 0.8) +
        ggplot2::geom_point(size = 3) +
        ggplot2::scale_color_manual(
          values = c("Abundancia (\u03bb)" = colores$primario,
                     "Detecci\u00f3n (p)"  = colores$acento)) +
        ggplot2::facet_wrap(~Submodelo, scales = "free_y") +
        ggplot2::labs(x = "Estimado (log / logit)", y = NULL, color = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "none",
                       panel.grid.minor = ggplot2::element_blank())
    })

    # ── Gráficos de efectos ───────────────────────────────
    output$sel_efecto_cov <- renderUI({
      sub  <- input$efecto_submodelo
      opts <- if (sub == "state") vars_sitio() else vars_obs()
      if (length(opts) == 0) opts <- character(0)
      selectInput(ns("efecto_cov"), "Covariable:", choices = opts)
    })

    output$plot_efecto <- renderPlot({
      req(modelo_actual(), input$efecto_cov, nchar(input$efecto_cov) > 0)
      fm   <- modelo_actual()
      cov  <- input$efecto_cov
      tipo <- input$efecto_submodelo
      d    <- datos_crudos()
      cov_data <- if (tipo == "state") d$cov_sitio
                  else as.data.frame(lapply(d$cov_obs,
                    function(x) rowMeans(as.data.frame(x), na.rm = TRUE)))
      validate(need(!is.null(cov_data) && cov %in% names(cov_data),
                    paste("Covariable", cov, "no encontrada.")))
      rango  <- seq(min(cov_data[[cov]], na.rm = TRUE),
                    max(cov_data[[cov]], na.rm = TRUE), length.out = 100)
      medias <- lapply(cov_data, function(x) rep(mean(x, na.rm = TRUE), 100))
      medias[[cov]] <- rango
      nd <- as.data.frame(medias)
      preds <- unmarked::predict(fm, type = tipo, newdata = nd, appendData = FALSE)
      df_plot <- data.frame(x = rango, pred = preds$Predicted,
                            lower = preds$lower, upper = preds$upper)
      etiq  <- if (tipo == "state") "\u03bb (abundancia esperada)"
               else "p (probabilidad de detecci\u00f3n)"
      color <- if (tipo == "state") colores$primario else colores$acento
      ggplot2::ggplot(df_plot,
        ggplot2::aes(x = x, y = pred, ymin = lower, ymax = upper)) +
        ggplot2::geom_ribbon(fill = color, alpha = 0.15) +
        ggplot2::geom_line(color = color, linewidth = 1.2) +
        ggplot2::labs(x = cov, y = etiq) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    # ── λ por sitio ───────────────────────────────────────
    lambda_sitios <- reactive({
      req(modelo_actual())
      preds <- unmarked::predict(modelo_actual(), type = "state")
      data.frame(
        sitio = paste("Sitio", seq_len(nrow(preds))),
        lambda = preds$Predicted,
        lower  = preds$lower,
        upper  = preds$upper
      ) |> dplyr::arrange(dplyr::desc(lambda))
    })

    output$vbox_sitios_umbral <- renderUI({
      req(lambda_sitios(), input$umbral_lambda)
      n <- sum(lambda_sitios()$lambda >= input$umbral_lambda)
      vbox_card("geo-alt-fill",
                paste0("Sitios con \u03bb \u2265 ", input$umbral_lambda),
                n, colores$exito)
    })

    output$plot_lambda_sitio <- renderPlot({
      req(lambda_sitios())
      df <- lambda_sitios()
      df$sitio <- factor(df$sitio, levels = df$sitio)
      ggplot2::ggplot(df,
        ggplot2::aes(x = lambda, y = sitio, xmin = lower, xmax = upper,
                     color = lambda >= input$umbral_lambda)) +
        ggplot2::geom_errorbarh(height = 0.4, linewidth = 0.6, alpha = 0.6) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::geom_vline(xintercept = input$umbral_lambda,
                            linetype = "dashed", color = colores$peligro) +
        ggplot2::scale_color_manual(
          values = c("FALSE" = "#A3ACB9", "TRUE" = colores$primario), guide = "none") +
        ggplot2::labs(x = "\u03bb estimada", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                       panel.grid.minor = ggplot2::element_blank())
    })

    output$tabla_lambda_sitio <- renderDT({
      req(lambda_sitios())
      datatable(
        lambda_sitios() |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))),
        options = list(pageLength = 10, scrollX = TRUE, dom = "tp"),
        rownames = FALSE, class = "table-sm table-striped"
      )
    })

    output$descarga_lambda_sitio <- downloadHandler(
      filename = function() "lambda_por_sitio.csv",
      content  = function(file) readr::write_csv(lambda_sitios(), file)
    )

    # ── Comparar modelos ──────────────────────────────────
    modelos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_modelo, {
      fm     <- modelo_actual()
      nombre <- trimws(input$nombre_modelo_guardar)
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.", type = "warning", duration = 3)
        return()
      }
      if (nchar(nombre) == 0) {
        showNotification("Escribe un nombre para el modelo.", type = "warning", duration = 3)
        return()
      }
      nueva <- modelos_guardados()
      nueva[[nombre]] <- fm
      modelos_guardados(nueva)
      updateTextInput(session, "nombre_modelo_guardar", value = "")
      showNotification(paste0("Modelo '", nombre, "' guardado."),
                       type = "message", duration = 3)
    })

    observeEvent(input$limpiar_modelos, { modelos_guardados(list()) })

    output$ui_sin_modelos <- renderUI({
      if (length(modelos_guardados()) == 0)
        div(class = "alert alert-warning small py-2 px-3",
            bs_icon("exclamation-triangle", class = "me-1"),
            "A\u00fan no has guardado ning\u00fan modelo. Ajusta modelos y usa 'Guardar modelo'.")
      else NULL
    })

    tabla_aic_df <- reactive({
      req(length(modelos_guardados()) >= 1)
      nms  <- names(modelos_guardados())
      mods <- modelos_guardados()
      df   <- data.frame(
        Modelo        = nms,
        Num_params    = sapply(nms, function(n) length(coef(mods[[n]]))),
        AIC           = sapply(nms, function(n) mods[[n]]@AIC),
        log_lik       = sapply(nms, function(n) -mods[[n]]@negLogLike),
        check.names   = FALSE, stringsAsFactors = FALSE
      ) |>
        dplyr::arrange(AIC) |>
        dplyr::mutate(
          deltaAIC = AIC - min(AIC),
          wi       = exp(-0.5 * deltaAIC),
          wi       = wi / sum(wi)
        ) |>
        dplyr::select(Modelo, Num_params, AIC, deltaAIC, wi, log_lik)
      df
    })

    output$tabla_aic <- renderDT({
      req(tabla_aic_df())
      datatable(
        tabla_aic_df() |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        options = list(dom = "t", pageLength = 20),
        rownames = FALSE, class = "table-sm table-striped"
      )
    })

    output$plot_aic <- renderPlot({
      req(tabla_aic_df())
      df <- tabla_aic_df()
      df$Modelo <- factor(df$Modelo, levels = rev(df$Modelo))
      ggplot2::ggplot(df, ggplot2::aes(x = wi, y = Modelo)) +
        ggplot2::geom_col(fill = colores$primario, alpha = 0.8, width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0("\u0394AIC = ", round(deltaAIC, 1))),
          hjust = -0.1, size = 3.5, color = colores$texto) +
        ggplot2::xlim(0, max(df$wi) * 1.3) +
        ggplot2::labs(x = "Peso de Akaike (w_i)", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    # ── Diagnóstico (GoF) ─────────────────────────────────
    resultado_gof <- reactiveVal(NULL)

    chi2_fn <- function(fm) {
      obs  <- unmarked::getY(fm@data)
      pred <- fitted(fm)
      sum((obs - pred)^2 / (pred + 0.5), na.rm = TRUE)
    }

    observeEvent(input$correr_gof, {
      fm <- modelo_actual()
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.", type = "warning", duration = 3)
        return()
      }
      withProgress(message = "Corriendo GoF (parboot)\u2026", value = 0.3, {
        tryCatch({
          res <- unmarked::parboot(fm, statistic = chi2_fn, nsim = input$nsim_gof)
          resultado_gof(res)
        }, error = function(e) {
          showNotification(paste("Error en GoF:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    output$res_gof_ui <- renderUI({
      res <- resultado_gof()
      if (is.null(res))
        return(div(class = "alert alert-info small py-2 px-3",
                   bs_icon("info-circle", class = "me-1"),
                   "Haz clic en ", strong("Ejecutar GoF"), " para ver resultados."))
      t0   <- res@t0[1]
      tsim <- res@t.star[, 1]
      pval <- mean(tsim >= t0)
      col  <- if (pval < 0.05) colores$peligro else colores$exito
      div(class = "text-center py-2",
        h3(style = paste0("color:", col, "; font-weight:700;"), round(pval, 3)),
        p(class = "text-muted mb-0", strong("p-valor (GoF)")),
        p(class = "small text-muted",
          if (pval >= 0.05) "\u2714 Buen ajuste" else "\u274c Mal ajuste \u2014 revisar modelo"),
        tags$hr(),
        div(class = "small text-muted",
          p(strong("\u03c7\u00b2 observado: "), round(t0, 2)),
          p(strong("Media \u03c7\u00b2 simulado: "), round(mean(tsim), 2)),
          p(strong("Simulaciones: "), length(tsim)))
      )
    })

    output$plot_gof <- renderPlot({
      res <- resultado_gof()
      if (is.null(res)) return(NULL)
      t0   <- res@t0[1]
      tsim <- res@t.star[, 1]
      df   <- data.frame(chi2 = tsim)
      ggplot2::ggplot(df, ggplot2::aes(x = chi2)) +
        ggplot2::geom_histogram(fill = colores$secundario, color = "white",
                                bins = 30, alpha = 0.8) +
        ggplot2::geom_vline(xintercept = t0, color = colores$peligro,
                            linewidth = 1.2, linetype = "dashed") +
        ggplot2::annotate("text", x = t0, y = Inf,
                          label = paste0("\u03c7\u00b2 obs = ", round(t0, 1)),
                          vjust = 2, hjust = -0.1, size = 3.5, color = colores$peligro) +
        ggplot2::labs(x = "\u03c7\u00b2 simulado", y = "Frecuencia") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$plot_obs_esp <- renderPlot({
      req(modelo_actual())
      fm   <- modelo_actual()
      obs  <- colSums(unmarked::getY(fm@data), na.rm = TRUE)
      pred <- colSums(fitted(fm), na.rm = TRUE)
      J    <- length(obs)
      df   <- data.frame(
        ocasion = rep(paste0("t", seq_len(J)), 2),
        Valor   = c(obs, pred),
        Tipo    = rep(c("Observado", "Esperado"), each = J)
      )
      ggplot2::ggplot(df, ggplot2::aes(x = ocasion, y = Valor, fill = Tipo)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.85) +
        ggplot2::scale_fill_manual(
          values = c("Observado" = colores$primario, "Esperado" = colores$acento)) +
        ggplot2::labs(x = "Ocasi\u00f3n de muestreo", y = "Total de detecciones", fill = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "top")
    })

    # ── Código R ──────────────────────────────────────────
    codigo_generado <- reactive({
      req(modelo_actual())
      lam_str <- if (is.null(input$cov_lambda) || length(input$cov_lambda) == 0) "1"
                 else paste(input$cov_lambda, collapse = " + ")
      det_str <- if (is.null(input$cov_det) || length(input$cov_det) == 0) "1"
                 else paste(input$cov_det, collapse = " + ")
      fuente  <- input$fuente_datos

      encabezado_script("StatAbundance", "Modelos N-mixture") |>
        paste0(
          "# -- Paquetes ------------------------------------------------\n",
          "library(unmarked)\n",
          "library(tidyverse)\n\n",
          "# -- Datos ----------------------------------------------------\n",
          if (!is.null(input$archivo_usuario)) {
            "datos <- read_csv('tu_archivo.csv')  # o read_excel()\n"
          } else {
            paste0("# Dataset: ", fuente,
                   " (exportado desde la pestana 'Los datos')\n")
          },
          "cols_y    <- grep('^y\\\\.', names(datos), value = TRUE)\n",
          "y_mat     <- as.matrix(datos[, cols_y])\n",
          "cov_sitio <- datos[, setdiff(names(datos), cols_y)]\n\n",
          "# -- unmarkedFrame --------------------------------------------\n",
          "umf <- unmarkedFramePCount(\n",
          "  y        = y_mat,\n",
          "  siteCovs = cov_sitio\n",
          "  # obsCovs = list(esfuerzo = esfuerzo_mat)  # si aplica\n",
          ")\n",
          "summary(umf)\n\n",
          "# -- Modelo ---------------------------------------------------\n",
          "# pcount(~deteccion ~abundancia, data, K, mixture)\n",
          "fm <- pcount(\n",
          "  formula = ~", det_str, " ~", lam_str, ",\n",
          "  data    = umf,\n",
          "  K       = ", input$K_upper, ",\n",
          "  mixture = '", input$mixture, "'\n",
          ")\n",
          "summary(fm)\n\n",
          "# -- Estimaciones en escala original --------------------------\n",
          "backTransform(fm, type = 'state')  # lambda media\n",
          "backTransform(fm, type = 'det')    # p media\n\n",
          "# -- Predicciones por sitio -----------------------------------\n",
          "lambda_pred <- predict(fm, type = 'state')\n",
          "head(lambda_pred)\n\n",
          "# -- Comparacion de modelos -----------------------------------\n",
          "m_nulo  <- pcount(~1 ~1, data = umf, K = ", input$K_upper, ")\n",
          "fl      <- fitList(nulo = m_nulo, final = fm)\n",
          "modSel(fl)\n\n",
          "# -- Bondad de ajuste (parboot) --------------------------------\n",
          "chi2 <- function(fm) {\n",
          "  obs  <- getY(fm@data)\n",
          "  pred <- fitted(fm)\n",
          "  sum((obs - pred)^2 / (pred + 0.5), na.rm = TRUE)\n",
          "}\n",
          "gof <- parboot(fm, statistic = chi2, nsim = 500)\n",
          "plot(gof)\n",
          "gof\n"
        )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descarga_codigo <- downloadHandler(
      filename = function() paste0("nmixture_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
} # /mod_abund_unmarked_server
