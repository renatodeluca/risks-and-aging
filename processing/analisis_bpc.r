
# análisis base personas cuidadoras (BPC)
# renato de luca
#29-09

library(dplyr)
library(readxl)
library(territorial)
library(chilemapas)
library(sf)
library(patchwork)
library(tidyverse)

#cargar base

ruta_bpc <- "../input/data-orig/bpc.xlsx"

bpc_raw <- read_excel(ruta_bpc, sheet = "Hoja1")

# columnas de período: 202601, 202602, ... (detectadas por patrón, no fijas)
periodos_disponibles <- grep("^[0-9]{6}$", names(bpc_raw), value = TRUE)
if (length(periodos_disponibles) == 0) {
  stop("No se encontraron columnas de período con formato AAAAMM")
}

bpc <- bpc_raw |>
  rename(
    nivel       = Agrupacion,
    cod_region  = c_region,
    region      = `Región`,
    estadistica = `Estadística`
  ) |>
  pivot_longer(
    cols      = all_of(periodos_disponibles),
    names_to  = "periodo",
    values_to = "n"
  ) |>
  mutate(
    # "202601" + "01" -> "20260101" -> 2026-01-01
    periodo    = as.Date(paste0(periodo, "01"), format = "%Y%m%d"),
    cod_region = as.integer(na_if(as.character(cod_region), "Total")),
    region     = if_else(region == "Total", "Nacional", region),
    n          = as.numeric(n)
  )

# ---- Clasificación de los indicadores ----
# bpc$estadistica es una etiqueta de texto larga; acá se descompone en
# variables analíticas (indicador, sexo, edad, CSE, escolaridad).

bpc <- bpc |>
  mutate(
    est = str_to_lower(estadistica),

    # a) qué se está contando (el orden importa: de lo más específico a lo general)
    indicador = case_when(
      str_detect(est, "mayor o igual a sueldo mínimo") ~ "ingreso_admin_sueldo_min_o_mas",
      str_detect(est, "en registros administrativos")  ~ "ingreso_admin",
      str_detect(est, "con ingreso laboral")           ~ "ingreso_laboral",
      str_detect(est, "cotización de afp")             ~ "cotiza_afp_12m",
      str_detect(est, "reciben alguna pensión")        ~ "recibe_pension",
      str_detect(est, "sin cuidador")                  ~ "req_cuidados_sin_cuidador",
      str_detect(est, "al menos un cuidador")          ~ "req_cuidados_con_cuidador",
      str_detect(est, "requieren cuidados")            ~ "req_cuidados",
      str_detect(est, "cuidadoras")                    ~ "cuidadoras",
      TRUE ~ NA_character_
    ),

    # b) sexo
    sexo = case_when(
      str_detect(est, "\\bmujeres\\b") ~ "Mujer",
      str_detect(est, "\\bhombres\\b") ~ "Hombre",
      TRUE                             ~ "Total"
    ),

    # c) tramo de edad
    tramo_edad = str_extract(est, "\\d+-\\d+ años|\\d+ años o más"),

    # d) tramo de Calificación Socioeconómica (unifica "0-40%" y "0%-40%")
    tramo_cse = str_extract(est, "(?<=tramo cse )\\d+%?-\\d+%") |>
      str_replace("^(\\d+)%?-(\\d+)%$", "\\1-\\2%"),

    # e) escolaridad
    escolaridad = case_when(
      str_detect(est, "sin escolaridad")      ~ "Sin escolaridad",
      str_detect(est, "básica incompleta")    ~ "Básica incompleta",
      str_detect(est, "básica completa")      ~ "Básica completa",
      str_detect(est, "media incompleta")     ~ "Media incompleta",
      str_detect(est, "media completa")       ~ "Media completa",
      str_detect(est, "superior incompleta")  ~ "Superior incompleta",
      str_detect(est, "superior completa")    ~ "Superior completa",
      str_detect(est, "sin información")      ~ "Sin información",
      TRUE ~ NA_character_
    ),

    # f) clave para cruzar con el mapa.
    # OJO: chilemapas usa codigo_region como texto de DOS dígitos ("01".."16").
    # Unir con cod_region (entero 1..16) falla por tipo y, si se fuerza a
    # texto, no matchea nada en silencio. "Total" queda NA a propósito.
    codigo_region = if_else(is.na(cod_region),
                            NA_character_,
                            sprintf("%02d", cod_region))
  ) |>
  select(nivel, cod_region, codigo_region, region, periodo, indicador,
         sexo, tramo_edad, tramo_cse, escolaridad, n, estadistica) |>
  mutate(
    sexo        = factor(sexo, levels = c("Total", "Mujer", "Hombre")),
    tramo_edad  = factor(tramo_edad,
                         levels = c("0-5 años", "6-17 años", "18-29 años", "30-44 años",
                                    "45-59 años", "60 años o más")),
    tramo_cse   = factor(tramo_cse, levels = c("0-40%", "41-60%", "61-80%", "81-100%")),
    escolaridad = factor(escolaridad,
                         levels = c("Sin escolaridad", "Básica incompleta", "Básica completa",
                                    "Media incompleta", "Media completa",
                                    "Superior incompleta", "Superior completa",
                                    "Sin información"))
  )

# ---- Validaciones ----

# 1) toda etiqueta debe quedar clasificada en algún indicador
sin_clasificar <- bpc %>% filter(is.na(indicador)) %>% distinct(estadistica)
stopifnot(nrow(sin_clasificar) == 0)
cat("Indicadores clasificados:",
    n_distinct(bpc$indicador), "sobre", n_distinct(bpc$estadistica), "etiquetas\n")

# 2) valores faltantes
cat("Valores NA:", sum(is.na(bpc$n)), "\n")

# 3) duplicados en indicador x territorio x mes x sexo x tramo
duplicados <- bpc %>%
  count(nivel, codigo_region, periodo, indicador, sexo,
        tramo_edad, tramo_cse, escolaridad, name = "k") %>%
  filter(k > 1)
stopifnot(nrow(duplicados) == 0)
cat("Combinaciones duplicadas:", nrow(duplicados), "\n")

# 4) cobertura territorial
cat("\nFilas:", nrow(bpc), "| Periodos:", n_distinct(bpc$periodo), "\n")
print(range(bpc$periodo))
print(table(bpc$nivel))

regiones_obs <- bpc %>%
  filter(nivel == "Regional") %>%
  distinct(codigo_region) %>%
  pull(codigo_region)

cat("\nRegiones en base regional:", length(regiones_obs), "\n")
stopifnot(length(regiones_obs) == 16)

# 5) consistencia: mujeres + hombres = total (base nacional de cuidadoras)
consistencia_sexo <- bpc %>%
  filter(indicador == "cuidadoras", nivel == "Nacional",
         is.na(tramo_edad), is.na(tramo_cse), is.na(escolaridad)) %>%
  group_by(periodo) %>%
  summarise(total = sum(n[sexo == "Total"]),
            parcial = sum(n[sexo != "Total"]),
            diferencia = total - parcial, .groups = "drop")

cat("\nMujeres + hombres vs total (Nacional):\n")
print(as.data.frame(consistencia_sexo))
stopifnot(all(consistencia_sexo$diferencia == 0))

# 6) consistencia: suma de las 16 regiones = total nacional
consistencia_regional <- bpc %>%
  filter(indicador == "cuidadoras", sexo == "Total",
         is.na(tramo_edad), is.na(tramo_cse), is.na(escolaridad)) %>%
  group_by(periodo) %>%
  summarise(nacional = sum(n[nivel == "Nacional"]),
            suma_regiones = sum(n[nivel == "Regional"]),
            diferencia = nacional - suma_regiones, .groups = "drop")

cat("\nNacional vs suma de 16 regiones:\n")
print(as.data.frame(consistencia_regional))
stopifnot(all(consistencia_regional$diferencia == 0))

# ---- Verificación del cruce con el mapa ----
# "Total" NO es una región: no pasar las filas nacionales por
# territorial::limpiar_regiones(), porque hace coincidencia aproximada
# y devuelve "Antofagasta" para "Total".

mapa_regional <- chilemapas::mapa_comunas |>
  generar_regiones() |>
  st_as_sf() |>
  st_drop_geometry()

corte <- mapa_regional %>%
  left_join(bpc %>% filter(nivel == "Regional") %>% distinct(codigo_region, region),
            by = "codigo_region")

cat("\nRegiones del mapa sin match en la base:", sum(is.na(corte$region)), "\n")
stopifnot(all(!is.na(corte$region)))

# ---- Perfil etario nacional de personas cuidadoras ----

perfil_edad <- bpc %>%
  filter(indicador == "cuidadoras", nivel == "Nacional",
         sexo == "Total", !is.na(tramo_edad)) %>%
  select(periodo, tramo_edad, n)

cat("\nPerfil etario de personas cuidadoras (Nacional):\n")
print(as.data.frame(perfil_edad))

# ---- Porcentaje con ingreso laboral por región ----

por_region <- bpc %>% filter(nivel == "Regional")

pct_ingreso_region <- por_region %>%
  filter(indicador %in% c("cuidadoras", "ingreso_laboral")) %>%
  select(region, periodo, indicador, n) %>%
  pivot_wider(names_from = indicador, values_from = n) %>%
  mutate(pct_con_ingreso = 100 * ingreso_laboral / cuidadoras)

stopifnot(!anyNA(pct_ingreso_region$pct_con_ingreso))

cat("\n% de personas cuidadoras con ingreso laboral, último mes:\n")
print(as.data.frame(
  pct_ingreso_region %>%
    filter(periodo == max(periodo)) %>%
    arrange(desc(pct_con_ingreso))
))

# ---- Guardar ----

dir.create("output", showWarnings = FALSE, recursive = TRUE)

saveRDS(bpc, "output/bpc_larga.rds")
write_csv(bpc, "output/bpc_larga.csv")

cat("\nGuardado: output/bpc_larga.rds y output/bpc_larga.csv\n")
