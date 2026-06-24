# ============================================================
# mod_abund_unmarked.R — Modelos N-mixture de abundancia
# StatAbundance · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: modelos N-mixture de una sola especie (Royle 2004)
# Datos: Ejemplo simulado de aves con puntos de conteo, o CSV/XLSX propio
# Ecosistema: unmarked + tidyverse + ggplot2
# ============================================================

# ── Loaders de datasets desde inst/app/data/ ──────────────

.cargar_aves <- function() {
  df <- readRDS(app_sys("app/data/datos_aves.rds"))
  cols_obs <- grep("^esfuerzo\\.", names(df), value = TRUE)
  obs_covs_df <- df[, cols_obs, drop = FALSE]
  df_clean    <- df[, setdiff(names(df), cols_obs), drop = FALSE]
  list(df = df_clean, obs_covs_df = obs_covs_df)
}

.cargar_ranas <- function() {
  df <- readRDS(app_sys("app/data/datos_ranas.rds"))
  cols_obs <- grep("^temp\\.", names(df), value = TRUE)
  obs_covs_df <- df[, cols_obs, drop = FALSE]
  df_clean    <- df[, setdiff(names(df), cols_obs), drop = FALSE]
  list(df = df_clean, obs_covs_df = obs_covs_df)
}

.cargar_mallard <- function() {
  df <- readRDS(app_sys("app/data/datos_mallard.rds"))
  cols_obs <- grep("^(date|ivel)\\.", names(df), value = TRUE)
  obs_covs_df <- df[, cols_obs, drop = FALSE]
  df_clean    <- df[, setdiff(names(df), cols_obs), drop = FALSE]
  list(df = df_clean, obs_covs_df = obs_covs_df)
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

            # Card 1: El problema
            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "El problema del conteo imperfecto"),
              card_body(
                p(class = "small text-muted mb-3",
                  "Cuando contamos individuos en el campo, solo detectamos una fracción ",
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

            # Card 2: Estructura jerárquica
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
                    "\u03bb puede depender de covariables del sitio ",
                    "(bosque, elevaci\u00f3n, h\u00e1bitat).")
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
                    "Dado N[i] individuos presentes, ",
                    "\u00bfcu\u00e1ntos se detectaron en la ocasi\u00f3n j? ",
                    "p puede depender de covariables de la ocasi\u00f3n.")
                )
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",

            # Card 3: Supuestos
            card(
              card_header(bs_icon("shield-check", class = "me-1"), "Supuestos clave"),
              card_body(
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li(
                    tags$strong("Poblaci\u00f3n cerrada:"),
                    " N[i] no cambia entre ocasiones (sin nacimientos, muertes, ",
                    "inmigraci\u00f3n ni emigraci\u00f3n durante el per\u00edodo de muestreo)."
                  ),
                  tags$li(
                    tags$strong("Independencia:"),
                    " las detecciones de individuos son independientes entre s\u00ed ",
                    "dentro de cada ocasi\u00f3n."
                  ),
                  tags$li(
                    tags$strong("Sin falsos positivos:"),
                    " cada detecci\u00f3n corresponde a un individuo real de la especie."
                  ),
                  tags$li(
                    tags$strong("\u2265 2 ocasiones de muestreo"),
                    " por sitio para poder estimar p separado de \u03bb."
                  )
                )
              )
            ),

            # Card 4: ¿Cuándo usar?
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
                              tags$th("Tipo de dato"),
                              tags$th("Organismo"),
                              tags$th("Covariables \u03bb"),
                              tags$th("Covariables p")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("Puntos de conteo"),
                              tags$td("Aves"),
                              tags$td("Bosque, fragmentaci\u00f3n"),
                              tags$td("Viento, hora, observador")
                            ),
                            tags$tr(
                              style = paste0("background:", colores$fondo),
                              tags$td("Transectos"),
                              tags$td("Anf\u00edbios, mam\u00edferos"),
                              tags$td("Cobertura, elevaci\u00f3n"),
                              tags$td("Lluvia, temperatura")
                            ),
                            tags$tr(
                              tags$td("C\u00e1maras trampa"),
                              tags$td("Mam\u00edferos medianos"),
                              tags$td("H\u00e1bitat, perturbaci\u00f3n"),
                              tags$td("D\u00edas-trampa, fase lunar")
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
                  "valores posibles de N desde el m\u00e1ximo conteo observado hasta K:"
                ),
                div(
                  style = paste0("border-left: 4px solid #1170AA;",
                                 " padding: 8px 12px; background:#E8F4FB;",
                                 " border-radius:0 6px 6px 0; margin-bottom:8px;"),
                  tags$b(class = "small", style = "color:#1170AA", "Verosimilitud marginal"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "L(y[i]) = \u03a3\u2099 P(y[i]|N) \u00b7 P(N|\u03bb)")
                ),
                div(
                  style = paste0("border-left: 4px solid #5FA2CE;",
                                 " padding: 8px 12px; background:#EEF3FA;",
                                 " border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#5FA2CE",
                         "Truncamiento K"),
                  p(class = "small text-muted mb-0 mt-1",
                    "En la pr\u00e1ctica se trunca la suma en K = m\u00e1x(y) + constante. ",
                    "Un K demasiado bajo sesga las estimaciones; ",
                    "uno muy alto aumenta el tiempo de c\u00f3mputo.")
                )
              )
            ),

            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Covariables con log-link"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Para mantener \u03bb > 0 se usa la transformaci\u00f3n log, ",
                  "igual que en la regresi\u00f3n de Poisson:"
                ),
                div(
                  style = paste0("border-left: 4px solid #1170AA;",
                                 " padding: 8px 12px; margin-bottom:8px;",
                                 " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#1170AA",
                         bs_icon("people-fill", class = "me-1"), "Abundancia (\u03bb)"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "log(\u03bb[i]) = \u03b2\u2080 + \u03b2\u2081\u00b7bosque[i]"),
                  p(class = "small text-muted mb-0",
                    "\u03b2\u2081 > 0 \u2192 m\u00e1s bosque = mayor abundancia esperada")
                ),
                div(
                  style = paste0("border-left: 4px solid #FC7D0B;",
                                 " padding: 8px 12px;",
                                 " background:#FFF3E0; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#FC7D0B",
                         bs_icon("eye-fill", class = "me-1"), "Detecci\u00f3n (p)"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "logit(p[i,j]) = \u03b1\u2080 + \u03b1\u2081\u00b7esfuerzo[i,j]"),
                  p(class = "small text-muted mb-0",
                    "\u03b1\u2081 > 0 \u2192 m\u00e1s tiempo de muestreo = mayor detecci\u00f3n")
                )
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",

            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Distribuci\u00f3n de mezcla"),
              card_body(
                p(class = "small text-muted mb-2",
                  "El modelo N-mixture puede especificarse con distintas distribuciones ",
                  "para N, dependiendo de la varianza observada en los datos:"
                ),
                div(
                  style = paste0("border-left: 4px solid #1170AA;",
                                 " padding: 8px 12px; background:#E8F4FB;",
                                 " border-radius:0 6px 6px 0; margin-bottom:6px;"),
                  tags$b(class = "small", style = "color:#1170AA", "Poisson"),
                  p(class = "small text-muted mb-0",
                    "Varianza = media. El caso m\u00e1s simple y el m\u00e1s frecuente.")
                ),
                div(
                  style = paste0("border-left: 4px solid #FC7D0B;",
                                 " padding: 8px 12px; background:#FFF3E0;",
                                 " border-radius:0 6px 6px 0; margin-bottom:6px;"),
                  tags$b(class = "small", style = "color:#FC7D0B",
                         "Binomial negativa"),
                  p(class = "small text-muted mb-0",
                    "Varianza > media (sobredispersi\u00f3n). ",
                    "M\u00e1s robusta cuando los conteos son muy variables.")
                ),
                div(
                  style = paste0("border-left: 4px solid #5FA2CE;",
                                 " padding: 8px 12px; background:#EEF3FA;",
                                 " border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#5FA2CE",
                         "ZIP (Poisson inflado de ceros)"),
                  p(class = "small text-muted mb-0",
                    "Cuando hay demasiados sitios con y = 0 no explicados por baja \u03bb.")
                )
              )
            ),

            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Selecci\u00f3n de modelos (AIC)"),
              card_body(
                p(class = "small text-muted mb-3",
                  "Los par\u00e1metros se estiman por ",
                  tags$strong("m\u00e1xima verosimilitud"),
                  ". Para comparar modelos candidatos se usa el ",
                  tags$strong("AIC"), ":"
                ),
                div(
                  style = paste0("border-left: 4px solid #1170AA;",
                                 " padding: 8px 12px; background:#E8F4FB;",
                                 " border-radius:0 6px 6px 0; margin-bottom:8px;"),
                  tags$b(class = "small", style = "color:#1170AA",
                         "Menor AIC = mejor modelo"),
                  p(class = "small text-muted mb-0",
                    "El AIC penaliza la complejidad: un modelo con m\u00e1s par\u00e1metros ",
                    "solo es mejor si mejora el ajuste suficientemente.")
                ),
                div(
                  style = paste0("border-left: 4px solid #5FA2CE;",
                                 " padding: 8px 12px; background:#EEF3FA;",
                                 " border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#5FA2CE",
                         "Modelos candidatos t\u00edpicos"),
                  tags$ul(
                    class = "small text-muted mb-0 mt-1",
                    style = "padding-left: 1.2rem;",
                    tags$li(tags$code("\u03bb(.) p(.)"),
                            " \u2014 abundancia y detecci\u00f3n constantes (nulo)"),
                    tags$li(tags$code("\u03bb(cov) p(.)"),
                            " \u2014 abundancia var\u00eda por sitio, detecci\u00f3n constante"),
                    tags$li(tags$code("\u03bb(.) p(cov)"),
                            " \u2014 abundancia constante, detecci\u00f3n var\u00eda"),
                    tags$li(tags$code("\u03bb(cov) p(cov)"),
                            " \u2014 ambos procesos dependen de covariables")
                  )
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

            # ── Sub-tab 1: Cargar datos ────────────────────
            nav_panel(
              title = tagList(bs_icon("upload", class = "me-1"), "Cargar datos"),
              div(class = "mt-3",
                layout_columns(
                  col_widths = c(6, 6),

                  # ── Columna izquierda: datasets de ejemplo ──
                  div(
                    tags$b(
                      bs_icon("database", class = "me-1"),
                      "Datasets de ejemplo"
                    ),
                    p(class = "small text-muted mt-1 mb-3",
                      "Selecciona un dataset para explorar la app ",
                      "sin necesidad de tus propios datos."
                    ),
                    selectInput(
                      ns("fuente_datos"),
                      label    = NULL,
                      choices  = c(
                        "Aves \u2014 puntos de conteo (simulado)"                    = "aves",
                        "Ranas \u2014 transectos nocturnos (simulado)"               = "ranas",
                        "Mallard \u2014 patos (unmarked \u00b7 K\u00e9ry et al. 2005)" = "mallard"
                      ),
                      selected = "aves"
                    ),
                    # Descripción dinámica del dataset seleccionado
                    uiOutput(ns("desc_dataset_ui"))
                  ),

                  # ── Columna derecha: datos propios ──────────
                  div(
                    tags$b(
                      bs_icon("file-earmark-arrow-up", class = "me-1"),
                      "Mis propios datos"
                    ),
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
                          " \u2014 valores enteros \u2265 0. ",
                          "Usa ", tags$code("NA"),
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
                          "\u2026 El prefijo (",
                          tags$code("esfuerzo"),
                          ") se usa en la f\u00f3rmula."
                        )
                      ),
                      # Tabla de ejemplo
                      tags$b(class = "small",
                             bs_icon("table", class = "me-1",
                                     style = paste0("color:", colores$primario)),
                             "Ejemplo de estructura:"),
                      div(
                        class = "mt-2",
                        style = "overflow-x: auto;",
                        tags$table(
                          class = "table table-sm table-bordered small mb-0",
                          style = "background:#ffffff; font-size: 0.78rem;",
                          tags$thead(
                            style = paste0("background:", colores$primario,
                                           "; color:#ffffff;"),
                            tags$tr(
                              tags$th("y.1"), tags$th("y.2"), tags$th("y.3"),
                              tags$th("bosque"), tags$th("elev"),
                              tags$th("esfuerzo.1"), tags$th("esfuerzo.2"),
                              tags$th("esfuerzo.3")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("3"), tags$td("5"), tags$td("2"),
                              tags$td("1.2"), tags$td("340"),
                              tags$td("0.8"), tags$td("1.1"), tags$td("0.9")
                            ),
                            tags$tr(
                              style = paste0("background:", colores$fondo),
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

            # ── Sub-tab 2: Vista previa ────────────────────
            nav_panel(
              title = tagList(bs_icon("eye", class = "me-1"), "Vista previa"),
              div(class = "mt-3",
                  uiOutput(ns("resumen_datos_ui")),
                  DTOutput(ns("tabla_preview"))
              )
            ),

            # ── Sub-tab 3: Conteos ─────────────────────────
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
                        card(
                          card_header(bs_icon("geo-alt", class = "me-1"), "Sitios"),
                          card_body(
                            p(class = "text-center mb-0",
                              tags$span(style = "font-size:1.8rem; font-weight:700;
                                                 color:#1170AA;",
                                        uiOutput(ns("n_sitios_txt"))))
                          )
                        ),
                        card(
                          card_header(bs_icon("calendar3", class = "me-1"), "Ocasiones"),
                          card_body(
                            p(class = "text-center mb-0",
                              tags$span(style = "font-size:1.8rem; font-weight:700;
                                                 color:#FC7D0B;",
                                        uiOutput(ns("n_ocasiones_txt"))))
                          )
                        ),
                        card(
                          card_header(bs_icon("bar-chart", class = "me-1"),
                                      "Conteo m\u00e1ximo"),
                          card_body(
                            p(class = "text-center mb-0",
                              tags$span(style = "font-size:1.8rem; font-weight:700;
                                                 color:#5FA2CE;",
                                        uiOutput(ns("max_count_txt"))))
                          )
                        )
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
      # PESTAÑA 4: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"), "Ajustar modelo"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            # Panel izquierdo: controles
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"),
                            "Especificaci\u00f3n del modelo"),
                card_body(
                  uiOutput(ns("sel_cov_lambda")),
                  uiOutput(ns("sel_cov_det")),
                  selectInput(
                    ns("mixture"),
                    label = tagList(bs_icon("distribute-vertical", class = "me-1"),
                                    "Distribuci\u00f3n de mezcla:"),
                    choices = c("Poisson" = "P", "Neg. Binomial" = "NB",
                                "ZIP" = "ZIP"),
                    selected = "P"
                  ),
                  numericInput(
                    ns("K_upper"),
                    label = tagList(bs_icon("arrow-up", class = "me-1"),
                                    "K (l\u00edmite superior para N):"),
                    value = 100, min = 10, max = 500, step = 10
                  ),
                  div(
                    class = "alert alert-info small py-2 px-3 mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    "K debe ser mayor que el conteo m\u00e1ximo observado. ",
                    "Un K = 3\u20135\u00d7 el m\u00e1ximo conteo suele ser suficiente."
                  ),
                  actionButton(
                    ns("ajustar"),
                    label = tagList(bs_icon("play-fill", class = "me-1"),
                                    "Ajustar modelo"),
                    class = "btn-primary w-100"
                  )
                )
              )
            ),

            # Panel derecho: resultados
            div(
              uiOutput(ns("resumen_modelo_ui")),
              uiOutput(ns("tabla_aic_ui"))
            )
          )
        )
      ), # /PESTAÑA 4

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Estimaciones
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("calculator", class = "me-1"), "Estimaciones"),
        card_body(
          uiOutput(ns("estimaciones_ui"))
        )
      ), # /PESTAÑA 5

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Predicciones
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Predicciones"),
        card_body(
          uiOutput(ns("ui_sel_pred")),
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(bs_icon("people-fill", class = "me-1"),
                          "Abundancia predicha (\u03bb) por sitio"),
              card_body(plotOutput(ns("plot_lambda"), height = "300px"))
            ),
            card(
              card_header(bs_icon("eye", class = "me-1"),
                          "Detecci\u00f3n predicha (p) por sitio"),
              card_body(plotOutput(ns("plot_p"), height = "300px"))
            )
          ),
          card(
            class = "mt-3",
            card_header(bs_icon("table", class = "me-1"),
                        "Tabla de predicciones por sitio"),
            card_body(DTOutput(ns("tabla_pred")))
          )
        )
      ), # /PESTAÑA 6

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Diagnóstico (GoF)
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"),
                        "Diagn\u00f3stico"),
        card_body(
          layout_columns(
            col_widths = c(5, 7),

            div(
              card(
                card_header(bs_icon("gear", class = "me-1"),
                            "Bondad de ajuste (parboot)"),
                card_body(
                  p(class = "small text-muted",
                    "Prueba \u03c7\u00b2 param\u00e9trico bootstrap (Royle & Nichols 2003). ",
                    "Un p-valor < 0.05 indica mal ajuste."
                  ),
                  numericInput(
                    ns("nsim_gof"),
                    label = tagList(bs_icon("arrow-repeat", class = "me-1"),
                                    "N\u00ba simulaciones:"),
                    value = 500, min = 100, max = 2000, step = 100
                  ),
                  actionButton(
                    ns("correr_gof"),
                    label = tagList(bs_icon("play-fill", class = "me-1"),
                                    "Correr GoF"),
                    class = "btn-primary w-100"
                  )
                )
              )
            ),

            div(
              uiOutput(ns("gof_resultado_ui")),
              plotOutput(ns("plot_gof"), height = "240px")
            )
          )
        )
      ), # /PESTAÑA 7

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "C\u00f3digo R"),
        card_body(
          p(class = "small text-muted",
            "C\u00f3digo R reproducible generado a partir del modelo ajustado."),
          div(
            class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo"), "Descargar .R",
                           class = "btn-sm btn-outline-primary"),
            actionButton(ns("copiar_codigo"), "Copiar al portapapeles",
                         class = "btn-sm btn-outline-secondary",
                         onclick = paste0(
                           "navigator.clipboard.writeText(",
                           "document.getElementById('", ns("codigo_r"), "').innerText",
                           ")"
                         ))
          ),
          verbatimTextOutput(ns("codigo_r")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 8

    ) # /navset_card_tab
  ) # /tagList
} # /mod_abund_unmarked_ui


# ── Server ────────────────────────────────────────────────
mod_abund_unmarked_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos reactivos ───────────────────────────────────
    datos_raw <- reactive({
      # Si el usuario cargó un archivo, tiene prioridad
      if (!is.null(input$archivo_usuario)) {
        ext <- tools::file_ext(input$archivo_usuario$name)
        df <- if (ext == "xlsx") {
          readxl::read_excel(input$archivo_usuario$datapath)
        } else {
          readr::read_csv(input$archivo_usuario$datapath,
                          show_col_types = FALSE)
        }
        return(list(df = as.data.frame(df), obs_covs_df = NULL))
      }
      # Si no, usa el dataset de ejemplo seleccionado
      switch(input$fuente_datos,
        "aves"    = .cargar_aves(),
        "ranas"   = .cargar_ranas(),
        "mallard" = .cargar_mallard()
      )
    })

    datos_df <- reactive({
      req(datos_raw())
      datos_raw()$df
    })

    cols_y <- reactive({
      req(datos_df())
      grep("^y\\.", names(datos_df()), value = TRUE)
    })

    y_mat <- reactive({
      req(datos_df(), cols_y())
      as.matrix(datos_df()[, cols_y()])
    })

    cov_sitio <- reactive({
      req(datos_df(), cols_y())
      sn <- setdiff(names(datos_df()), cols_y())
      # excluir columnas de obs_cov (prefijo.número)
      sn_site <- sn[!grepl("\\.[0-9]+$", sn)]
      if (length(sn_site) == 0) return(NULL)
      datos_df()[, sn_site, drop = FALSE]
    })

    obs_cov_nombres <- reactive({
      req(datos_df(), cols_y())
      otras <- setdiff(names(datos_df()), cols_y())
      obs <- otras[grepl("\\.[0-9]+$", otras)]
      if (length(obs) == 0 && !is.null(datos_raw()$obs_covs_df))
        return(names(datos_raw()$obs_covs_df))
      unique(sub("\\.[0-9]+$", "", obs))
    })

    # ── Descripción dataset ───────────────────────────────
    output$desc_dataset_ui <- renderUI({
      desc <- switch(input$fuente_datos,
        "aves" = list(
          icono  = "binoculars",
          titulo = "Aves \u2014 puntos de conteo",
          items  = list(
            list(icon = "grid-3x3",     txt = "100 sitios \u00b7 4 ocasiones"),
            list(icon = "geo-alt-fill", txt = tagList("Covariables de sitio: ",
                                                       tags$code("bosque"), ", ",
                                                       tags$code("elev"))),
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
                                                       tags$code("humedad"), ", ",
                                                       tags$code("vegetacion"))),
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
            list(icon = "geo-alt-fill", txt = tagList("Covariables de sitio: ",
                                                       tags$code("elev"), ", ",
                                                       tags$code("forest"), ", ",
                                                       tags$code("length"))),
            list(icon = "eye-fill",     txt = tagList("Covariables de observaci\u00f3n: ",
                                                       tags$code("date"), ", ",
                                                       tags$code("ivel"))),
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
            tags$li(
              class = "mb-1",
              bs_icon(item$icon, class = "me-1",
                      style = paste0("color:", colores$secundario)),
              item$txt
            )
          })
        )
      )
    })

    # ── Selector de obs_covs ──────────────────────────────
    output$sel_cov_obs <- renderUI({
      nms <- obs_cov_nombres()
      if (length(nms) == 0) return(NULL)
      checkboxGroupInput(
        ns("cov_obs_sel"),
        label = tagList(bs_icon("calendar3", class = "me-1"),
                        "Covariables de observaci\u00f3n disponibles:"),
        choices  = nms,
        selected = nms[1]
      )
    })

    # ── Resumen datos ─────────────────────────────────────
    output$resumen_datos_ui <- renderUI({
      req(y_mat())
      y  <- y_mat()
      ns_  <- nrow(y)
      nj_  <- ncol(y)
      max_ <- max(y, na.rm = TRUE)
      mean_ <- round(mean(y, na.rm = TRUE), 2)
      div(
        class = "mb-3",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(class = "text-center",
              tags$span(style = "font-size:1.6rem; font-weight:700; color:#1170AA;", ns_),
              p(class = "small text-muted mb-0", "Sitios")),
          div(class = "text-center",
              tags$span(style = "font-size:1.6rem; font-weight:700; color:#FC7D0B;", nj_),
              p(class = "small text-muted mb-0", "Ocasiones")),
          div(class = "text-center",
              tags$span(style = "font-size:1.6rem; font-weight:700; color:#5FA2CE;", max_),
              p(class = "small text-muted mb-0", "M\u00e1x. conteo")),
          div(class = "text-center",
              tags$span(style = "font-size:1.6rem; font-weight:700; color:#57606C;",
                        mean_),
              p(class = "small text-muted mb-0", "Media conteo"))
        )
      )
    })

    output$tabla_preview <- renderDT({
      req(datos_df())
      datatable(head(datos_df(), 20),
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE)
    })

    output$tabla_conteos <- renderDT({
      req(y_mat())
      df <- as.data.frame(y_mat())
      df$sitio <- seq_len(nrow(df))
      df <- df[, c("sitio", cols_y())]
      datatable(df,
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE)
    })

    output$n_sitios_txt    <- renderUI(nrow(y_mat()))
    output$n_ocasiones_txt <- renderUI(ncol(y_mat()))
    output$max_count_txt   <- renderUI(max(y_mat(), na.rm = TRUE))

    output$descarga_datos <- downloadHandler(
      filename = function() paste0("abundancia_datos_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(datos_df(), file, row.names = FALSE)
    )

    # ── Selectores de covariables ─────────────────────────
    cov_sitio_nombres <- reactive({
      cs <- cov_sitio()
      if (is.null(cs)) character(0) else names(cs)
    })

    output$sel_cov_lambda <- renderUI({
      nms <- cov_sitio_nombres()
      selectInput(
        ns("cov_lambda"),
        label = tagList(bs_icon("people-fill", class = "me-1"),
                        "Covariables para \u03bb (abundancia):"),
        choices  = nms,
        selected = NULL,
        multiple = TRUE
      )
    })

    output$sel_cov_det <- renderUI({
      nms <- c(cov_sitio_nombres(), obs_cov_nombres())
      selectInput(
        ns("cov_det"),
        label = tagList(bs_icon("eye", class = "me-1"),
                        "Covariables para p (detecci\u00f3n):"),
        choices  = nms,
        selected = NULL,
        multiple = TRUE
      )
    })

    # ── unmarkedFrame ─────────────────────────────────────
    umf_actual <- reactive({
      req(y_mat())

      cs <- cov_sitio()
      oc <- NULL

      # obs_covs de dataset simulado
      if (!is.null(datos_raw()$obs_covs_df)) {
        oc_df <- datos_raw()$obs_covs_df
        oc <- lapply(names(oc_df), function(nm) {
          matrix(oc_df[[nm]], nrow = nrow(y_mat()))
        })
        names(oc) <- sub("\\.[0-9]+$", "", names(oc_df))
        # desduplicar
        oc <- oc[!duplicated(names(oc))]
      }

      # obs_covs de datos propios
      otras <- setdiff(names(datos_df()), cols_y())
      obs_cols <- otras[grepl("\\.[0-9]+$", otras)]
      if (length(obs_cols) > 0) {
        prefijos <- unique(sub("\\.[0-9]+$", "", obs_cols))
        oc <- lapply(prefijos, function(pref) {
          cols_p <- paste0(pref, ".", seq_len(ncol(y_mat())))
          as.matrix(datos_df()[, intersect(cols_p, names(datos_df()))])
        })
        names(oc) <- prefijos
      }

      unmarked::unmarkedFramePCount(
        y        = y_mat(),
        siteCovs = cs,
        obsCovs  = oc
      )
    })

    # ── Modelo ────────────────────────────────────────────
    modelo_actual <- reactiveVal(NULL)

    observeEvent(input$ajustar, {
      req(umf_actual())
      lam_covs <- if (is.null(input$cov_lambda) || length(input$cov_lambda) == 0)
        "1" else paste(input$cov_lambda, collapse = " + ")
      det_covs <- if (is.null(input$cov_det) || length(input$cov_det) == 0)
        "1" else paste(input$cov_det, collapse = " + ")

      fm_formula <- as.formula(paste0("~", det_covs, " ~", lam_covs))
      K_val      <- input$K_upper
      mix_val    <- input$mixture

      withProgress(message = "Ajustando modelo N-mixture\u2026", value = 0.5, {
        tryCatch({
          fm <- unmarked::pcount(
            formula  = fm_formula,
            data     = umf_actual(),
            K        = K_val,
            mixture  = mix_val
          )
          modelo_actual(fm)
          showNotification(
            tagList(bs_icon("check-circle-fill", class = "me-1"), "Modelo ajustado"),
            type = "message", duration = 3
          )
        }, error = function(e) {
          showNotification(
            paste("Error al ajustar:", conditionMessage(e)),
            type = "error", duration = 6
          )
        })
      })
    })

    # ── Resumen del modelo ────────────────────────────────
    output$resumen_modelo_ui <- renderUI({
      fm <- modelo_actual()
      if (is.null(fm)) {
        return(div(class = "alert alert-info small py-2 px-3",
                   bs_icon("info-circle", class = "me-1"),
                   "Especifica el modelo y haz clic en ",
                   strong("Ajustar modelo"), "."))
      }
      s   <- summary(fm)
      lam_df <- as.data.frame(s$abundance)
      det_df <- as.data.frame(s$detection)

      tabla_coefs <- function(df, titulo, color) {
        card(
          class = "mb-3",
          card_header(style = paste0("color:", color),
                      bs_icon("table", class = "me-1"), titulo),
          card_body(
            tags$table(
              class = "table table-sm small mb-0",
              tags$thead(tags$tr(lapply(names(df), tags$th))),
              tags$tbody(lapply(seq_len(nrow(df)), function(i) {
                tags$tr(lapply(df[i, ], function(v)
                  tags$td(if (is.numeric(v)) round(v, 4) else v)))
              }))
            )
          )
        )
      }

      tagList(
        tabla_coefs(lam_df, "Abundancia (\u03bb)", colores$primario),
        tabla_coefs(det_df, "Detecci\u00f3n (p)",   colores$acento)
      )
    })

    # ── Estimaciones back-transform ───────────────────────
    output$estimaciones_ui <- renderUI({
      fm <- modelo_actual()
      if (is.null(fm)) {
        return(div(class = "alert alert-warning small py-2 px-3",
                   bs_icon("exclamation-triangle", class = "me-1"),
                   "Ajusta un modelo primero."))
      }

      bt_lam  <- tryCatch(unmarked::backTransform(fm, type = "state"),
                          error = function(e) NULL)
      bt_det  <- tryCatch(unmarked::backTransform(fm, type = "det"),
                          error = function(e) NULL)

      card_bt <- function(bt, titulo, icono, color) {
        if (is.null(bt)) return(NULL)
        est <- round(bt@estimate, 4)
        se  <- round(sqrt(diag(bt@covMat)), 4)
        div(
          class = "p-3 mb-3",
          style = paste0("background:", colores$fondo, ";",
                         " border-left: 4px solid ", color, ";",
                         " border-radius: 0 8px 8px 0;"),
          tags$b(style = paste0("color:", color),
                 bs_icon(icono, class = "me-1"), titulo),
          p(class = "mb-0 mt-1",
            style = "font-size: 1.4rem; font-weight: 700;",
            est, tags$small(class = "text-muted", paste0(" \u00b1 ", se, " (SE)")))
        )
      }

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          card_bt(bt_lam, "Abundancia media (\u03bb)",
                  "people-fill", colores$primario),
          card_bt(bt_det, "Detecci\u00f3n media (p)",
                  "eye-fill",    colores$acento)
        ),
        card(
          class = "mt-2",
          card_header(bs_icon("list-columns", class = "me-1"),
                      "Coeficientes (escala logit / log)"),
          card_body(
            verbatimTextOutput(ns("coef_raw"))
          )
        )
      )
    })

    output$coef_raw <- renderPrint({
      req(modelo_actual())
      coef(modelo_actual())
    })

    # ── Predicciones ──────────────────────────────────────
    pred_lambda <- reactive({
      req(modelo_actual())
      tryCatch(
        predict(modelo_actual(), type = "state"),
        error = function(e) NULL
      )
    })

    pred_det <- reactive({
      req(modelo_actual())
      tryCatch(
        predict(modelo_actual(), type = "det"),
        error = function(e) NULL
      )
    })

    output$ui_sel_pred <- renderUI({
      if (is.null(pred_lambda())) {
        return(div(class = "alert alert-warning small py-2 px-3 mb-3",
                   bs_icon("exclamation-triangle", class = "me-1"),
                   "Ajusta un modelo primero."))
      }
      NULL
    })

    output$plot_lambda <- renderPlot({
      req(pred_lambda())
      df <- pred_lambda()
      df$sitio <- seq_len(nrow(df))
      ggplot(df, aes(x = sitio, y = Predicted)) +
        geom_point(color = colores$primario, size = 1.5, alpha = 0.7) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      width = 0, color = colores$secundario, alpha = 0.5) +
        labs(x = "Sitio", y = "\u03bb predicho", title = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    })

    output$plot_p <- renderPlot({
      req(pred_det())
      df <- pred_det()
      df$sitio <- seq_len(nrow(df))
      ggplot(df, aes(x = sitio, y = Predicted)) +
        geom_point(color = colores$acento, size = 1.5, alpha = 0.7) +
        geom_errorbar(aes(ymin = lower, ymax = upper),
                      width = 0, color = "#F1CE63", alpha = 0.5) +
        labs(x = "Sitio", y = "p predicho", title = NULL) +
        scale_y_continuous(limits = c(0, 1)) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    })

    output$tabla_pred <- renderDT({
      req(pred_lambda(), pred_det())
      df <- data.frame(
        Sitio    = seq_len(nrow(pred_lambda())),
        lambda   = round(pred_lambda()$Predicted, 3),
        lam_SE   = round(pred_lambda()$SE, 3),
        lam_lower = round(pred_lambda()$lower, 3),
        lam_upper = round(pred_lambda()$upper, 3),
        p_media  = round(pred_det()$Predicted[seq_len(nrow(pred_lambda()))], 3)
      )
      datatable(df, rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE))
    })

    # ── GoF ───────────────────────────────────────────────
    resultado_gof <- reactiveVal(NULL)

    observeEvent(input$correr_gof, {
      fm <- modelo_actual()
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3)
        return()
      }
      chi2_fn <- function(fm) {
        obs  <- unmarked::getY(fm@data)
        pred <- fitted(fm)
        sum((obs - pred)^2 / (pred + 0.5), na.rm = TRUE)
      }
      withProgress(message = "Corriendo GoF (parboot)\u2026", value = 0.3, {
        tryCatch({
          res <- unmarked::parboot(fm, statistic = chi2_fn,
                                   nsim = input$nsim_gof)
          resultado_gof(res)
        }, error = function(e) {
          showNotification(paste("Error en GoF:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    output$gof_resultado_ui <- renderUI({
      res <- resultado_gof()
      if (is.null(res)) {
        return(div(class = "alert alert-info small py-2 px-3",
                   bs_icon("info-circle", class = "me-1"),
                   "Haz clic en ", strong("Correr GoF"), " para ver resultados."))
      }
      t0   <- res@t0[1]
      tsim <- res@t.star[, 1]
      pval <- mean(tsim >= t0)
      col  <- if (pval < 0.05) colores$peligro else colores$exito

      div(
        class = "text-center py-3",
        h2(style = paste0("color:", col, "; font-weight:700; font-size:2rem;"),
           round(pval, 3)),
        p(class = "text-muted mb-0", strong("p-valor (GoF)")),
        p(class = "small text-muted",
          if (pval >= 0.05) "\u2714 Buen ajuste (p \u2265 0.05)"
          else "\u274c Mal ajuste \u2014 revisar el modelo"),
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
      ggplot(df, aes(x = chi2)) +
        geom_histogram(fill = colores$secundario, color = "white",
                       bins = 30, alpha = 0.8) +
        geom_vline(xintercept = t0, color = colores$peligro,
                   linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = t0, y = Inf, label = paste0("\u03c7\u00b2 obs = ", round(t0, 1)),
                 vjust = 2, hjust = -0.1, size = 3.5, color = colores$peligro) +
        labs(x = "\u03c7\u00b2 simulado", y = "Frecuencia") +
        theme_minimal(base_size = 12)
    })

    # ── Código R ──────────────────────────────────────────
    codigo_generado <- reactive({
      req(modelo_actual())
      lam_covs <- if (is.null(input$cov_lambda) || length(input$cov_lambda) == 0)
        "1" else paste(input$cov_lambda, collapse = " + ")
      det_covs <- if (is.null(input$cov_det) || length(input$cov_det) == 0)
        "1" else paste(input$cov_det, collapse = " + ")
      fuente  <- input$fuente_datos
      mix_val <- input$mixture
      K_val   <- input$K_upper

      encabezado_script("StatAbundance", "Modelos N-mixture de abundancia") |>
        paste0(
          "# \u2500\u2500 Paquetes \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          "library(unmarked)\n",
          "library(tidyverse)\n\n",
          "# \u2500\u2500 Datos \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          if (fuente == "propio") {
            paste0(
              "datos <- read_csv(\"tu_archivo.csv\")  # o read_excel()\n",
              "cols_y   <- grep(\"^y\\\\.\", names(datos), value = TRUE)\n",
              "y_mat    <- as.matrix(datos[, cols_y])\n",
              "cov_sitio <- datos[, setdiff(names(datos), cols_y)]\n\n"
            )
          } else {
            paste0(
              "# Dataset: ", fuente,
              " (generado internamente en StatAbundance)\n",
              "# Exporta desde la pesta\u00f1a 'Los datos' para reproducir localmente\n\n"
            )
          },
          "# \u2500\u2500 Crear objeto unmarkedFramePCount \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          "umf <- unmarkedFramePCount(\n",
          "  y        = y_mat,      # matrix: sitios \u00d7 ocasiones (conteos enteros)\n",
          "  siteCovs = cov_sitio   # data.frame de covariables de sitio\n",
          "  # obsCovs = obs_cov    # descomenta si tienes cov. de observaci\u00f3n\n",
          ")\n",
          "summary(umf)\n\n",
          "# \u2500\u2500 Modelo ajustado \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          "# Notaci\u00f3n pcount(~detecci\u00f3n ~abundancia, data, K, mixture)\n",
          "fm <- pcount(\n",
          "  formula = ~", det_covs, " ~", lam_covs, ",\n",
          "  data    = umf,\n",
          "  K       = ", K_val, ",     # l\u00edmite superior de integraci\u00f3n\n",
          "  mixture = \"", mix_val, "\"   # \"P\", \"NB\" o \"ZIP\"\n",
          ")\n",
          "summary(fm)\n\n",
          "# \u2500\u2500 Estimaciones en escala original \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          "backTransform(fm, type = \"state\")  # \u03bb media\n",
          "backTransform(fm, type = \"det\")    # p media\n\n",
          "# \u2500\u2500 Predicciones por sitio \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          "lambda_pred <- predict(fm, type = \"state\")\n",
          "head(lambda_pred)\n\n",
          "# \u2500\u2500 Comparaci\u00f3n de modelos \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
          "m_nulo  <- pcount(~1 ~1, data = umf, K = ", K_val, ")\n",
          "m_lam   <- pcount(~1 ~", lam_covs, ", data = umf, K = ", K_val, ")\n",
          "m_final <- fm\n\n",
          "fl  <- fitList(m_nulo, m_lam, m_final)\n",
          "modSel(fl)\n\n",
          "# \u2500\u2500 Bondad de ajuste (parboot) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
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
