# ============================================================
# dev/generar_datos.R
# Corre este script UNA VEZ para generar los datasets de ejemplo
# Requiere estar en la raíz del proyecto StatAbundance
# ============================================================

library(unmarked)

# Crear carpeta si no existe
dir.create("inst/app/data", recursive = TRUE, showWarnings = FALSE)

# ── 1. datos_aves (simulado) ───────────────────────────────
# 100 sitios · 4 ocasiones · puntos de conteo
# Covariables de sitio: bosque, elev
# Covariable de observación: esfuerzo (por ocasión)
set.seed(42)
M  <- 100
J  <- 4
bosque   <- rnorm(M)
elev     <- rnorm(M)
lambda   <- exp(1.2 + 0.8 * bosque - 0.3 * elev)
N        <- rpois(M, lambda)
esfuerzo <- matrix(rnorm(M * J), M, J)
p_mat    <- plogis(-0.5 + 0.4 * esfuerzo)
y        <- matrix(rbinom(M * J, N, p_mat), M, J)
colnames(y) <- paste0("y.", 1:J)
esfuerzo_df <- as.data.frame(esfuerzo)
names(esfuerzo_df) <- paste0("esfuerzo.", 1:J)

datos_aves <- data.frame(y, bosque = bosque, elev = elev, esfuerzo_df)

saveRDS(datos_aves, file = "inst/app/data/datos_aves.rds")
message("✓ datos_aves.rds guardado (", nrow(datos_aves), " sitios, ", J, " ocasiones)")
rm(M, J, bosque, elev, lambda, N, esfuerzo, p_mat, y, esfuerzo_df, datos_aves)


# ── 2. datos_ranas (simulado) ──────────────────────────────
# 60 sitios · 3 ocasiones · transectos nocturnos
# Covariables de sitio: humedad, vegetacion
# Covariable de observación: temp (temperatura por ocasión)
set.seed(77)
M  <- 60
J  <- 3
humedad    <- rnorm(M)
vegetacion <- rnorm(M)
lambda     <- exp(0.7 + 0.9 * humedad + 0.4 * vegetacion)
N          <- rpois(M, lambda)
temp       <- matrix(rnorm(M * J), M, J)
p_mat      <- plogis(-0.8 + 0.3 * temp)
y          <- matrix(rbinom(M * J, N, p_mat), M, J)
colnames(y) <- paste0("y.", 1:J)
temp_df     <- as.data.frame(temp)
names(temp_df) <- paste0("temp.", 1:J)

datos_ranas <- data.frame(y, humedad = humedad, vegetacion = vegetacion, temp_df)

saveRDS(datos_ranas, file = "inst/app/data/datos_ranas.rds")
message("✓ datos_ranas.rds guardado (", nrow(datos_ranas), " sitios, ", J, " ocasiones)")
rm(M, J, humedad, vegetacion, lambda, N, temp, p_mat, y, temp_df, datos_ranas)


# ── 3. datos_mallard (unmarked) ───────────────────────────
# 239 sitios · 3 ocasiones · patos mallard en Suiza
# Covariables de sitio: elev, forest, length
# Covariables de observación: date, ivel
# ── 3. datos_mallard (unmarked) ───────────────────────────
# 239 sitios · 3 ocasiones · patos mallard en Suiza
# Covariables de sitio: elev, forest, length
# Covariables de observación: date, ivel
e <- new.env()
data("mallard", package = "unmarked", envir = e)

y_mall <- as.data.frame(e$mallard.y)
names(y_mall) <- paste0("y.", 1:3)

site_mall <- e$mallard.site  # elev, forest, length

date_df <- as.data.frame(e$mallard.obs$date)
ivel_df <- as.data.frame(e$mallard.obs$ivel)
names(date_df) <- paste0("date.", 1:3)
names(ivel_df) <- paste0("ivel.", 1:3)

datos_mallard <- data.frame(y_mall, site_mall, date_df, ivel_df)

saveRDS(datos_mallard, file = "inst/app/data/datos_mallard.rds")
message("✓ datos_mallard.rds guardado (", nrow(datos_mallard), " sitios, 3 ocasiones)")
rm(mallard, y_mall, site_mall, date_df, ivel_df, datos_mallard)

message("\n✓ Tres datasets guardados en inst/app/data/")
