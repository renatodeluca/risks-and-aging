# genera cajas (bounding boxes) para regiones y comunas
# usando {chilemapas} para geometrías
# y {territorial} para nombres de comunas y regiones.
# guarda los resultados como archivos RDS para usar en los
# selectores de zoom del mapa interactivo.
#
# además cruza las bases del proyecto (clasif_com, bpc_larga,
# fac_suby e igvust) en dos joins: uno por región y otro por comuna,
# y deja los datos unidos en RDS para la shiny app (app.r).
#
# ejecutar desde processing/: Rscript cajas.r

library(chilemapas)
library(territorial)
library(dplyr)
library(sf)
library(purrr)
library(readxl)
library(tidyr)
library(readr)
library(janitor)

dir.create("output", showWarnings = FALSE)

# bases del proyecto ----

clasif_com <- read_excel("../input/data-orig/clasif_com.xlsx") |>
  clean_names()

bpc_larga <- read_rds("output/bpc_larga.rds")

fac_suby <- read_excel("../input/data-orig/fac_suby.xlsx", skip = 1) |>
  clean_names()

igvust <- read_excel("../input/data-orig/igvust.xlsx") |>
  clean_names()

# limpieza mínima ----

fac_suby <- fac_suby |>
  filter(!is.na(codigo), codigo != "N/A") |>
  rename(cod_com = codigo) |>
  mutate(
    cod_com = as.numeric(cod_com),
    across(c(poblacion_censada, x65_anos_o_mas, indice_envejecimiento,
             idh, icfsr, irape), as.numeric)
  ) |>
  select(cod_com, poblacion_censada, x65_anos_o_mas, indice_envejecimiento,
         idh, icfsr, irape)

clasif_com <- clasif_com |>
  transmute(
    cod_com       = as.numeric(cut),
    codigo_region = sprintf("%02d", as.numeric(cod_reg)),
    provincia,
    tipo_com,
    clasificacion,
    pob_2024 = tot_pers_res_2024
  )

igvust <- igvust |>
  transmute(
    cod_com       = as.numeric(cod_com),
    codigo_region = cod_reg,
    Comuna        = comuna,
    Region        = region,
    rank_nac, c_ig_nac, c_ig_reg,
    pob_rsh_com, hog_com, p_cobertura_com,
    clasificacion_rsh = clasificacion
  )

# join por comuna ----
# igvust (base) + factores subyacentes + clasificación comunal

message("Cruce por comuna: igvust + fac_suby + clasif_com")

datos_comuna <- igvust |>
  left_join(fac_suby, by = "cod_com") |>
  # codigo_region ya viene de igvust; se evita el sufijo .x/.y
  left_join(clasif_com |> select(-codigo_region), by = "cod_com")

# join por región ----
# IGVUST y clasif_com agregados a región + BPC (solo nivel regional)

bpc_regional <- bpc_larga |>
  filter(
    nivel == "Regional",
    periodo == max(periodo),
    sexo == "Total",
    is.na(tramo_edad), is.na(tramo_cse), is.na(escolaridad),
    indicador %in% c("cuidadoras", "ingreso_laboral", "cotiza_afp_12m",
                     "req_cuidados", "req_cuidados_sin_cuidador")
  ) |>
  select(periodo, codigo_region, indicador, n) |>
  pivot_wider(names_from = indicador, values_from = n)

message("Cruce por región: IGVUST + clasif_com + BPC (",
        format(unique(bpc_regional$periodo), "%B %Y"), ")")

datos_region <- igvust |>
  group_by(codigo_region) |>
  summarise(
    n_comunas      = n(),
    prom_igvust    = mean(c_ig_nac, na.rm = TRUE),
    prop_cuartil_1 = mean(c_ig_nac == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    clasif_com |>
      group_by(codigo_region) |>
      summarise(
        pob_2024  = sum(pob_2024, na.rm = TRUE),
        n_urbanas = sum(clasificacion == "Urbana", na.rm = TRUE),
        n_rurales = sum(clasificacion == "Rural", na.rm = TRUE),
        .groups   = "drop"
      ),
    by = "codigo_region"
  ) |>
  left_join(bpc_regional, by = "codigo_region")

# llevar los indicadores regionales a cada comuna (para pintar el mapa)

datos_comuna <- datos_comuna |>
  left_join(
    datos_region |>
      select(codigo_region, cuidadoras, req_cuidados, ingreso_laboral,
             cotiza_afp_12m, prop_cuartil_1),
    by = "codigo_region"
  )

write_rds(datos_comuna, "output/datos_comuna.rds")
write_rds(datos_region, "output/datos_region.rds")
message("Guardado output/datos_comuna.rds (", nrow(datos_comuna),
        " comunas) y output/datos_region.rds (", nrow(datos_region),
        " regiones).")

# cargar geometrías de {chilemapas} ----

regiones_geo <- chilemapas::generar_regiones()
comunas_geo  <- chilemapas::mapa_comunas

# limpiar regiones

regiones_geo_2 <- regiones_geo |>
  mutate(codigo_region = as.numeric(codigo_region)) |>
  mutate(nombre_region = territorial::as_nombre_region(codigo_region)) |>
  territorial::ordenar_regiones()

# limpiar comunas

comunas_geo_2 <- comunas_geo |>
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |>
  territorial::contextualizar(codigo_comuna)

# recortar mapa de regiones al chile continental

regiones_geo_3 <- regiones_geo_2 |>
  st_crop(
    c(xmin = -75.7, ymin = -55.9, xmax = -66.4, ymax = -17.5)
  )

# país ----
# calcular caja del territorio completo

pais_bbox_valores <- st_bbox(regiones_geo_3)

pais_bbox <- tibble(
  nombre_region = "País",
  xmin = pais_bbox_valores[["xmin"]],
  ymin = pais_bbox_valores[["ymin"]],
  xmax = pais_bbox_valores[["xmax"]],
  ymax = pais_bbox_valores[["ymax"]]
)

# regiones ----
# calcular cajas por región

region_bbox <- regiones_geo_3 |>
  mutate(
    bbox = map(geometry, st_bbox),
    xmin = map_dbl(bbox, "xmin"),
    ymin = map_dbl(bbox, "ymin"),
    xmax = map_dbl(bbox, "xmax"),
    ymax = map_dbl(bbox, "ymax")
  ) |>
  select(nombre_region, xmin, ymin, xmax, ymax) |>
  st_drop_geometry() |>
  # agregar caja del país
  bind_rows(pais_bbox)

saveRDS(region_bbox, "output/cajas_regiones.rds")
message("Guardado output/cajas_regiones.rds con ", nrow(region_bbox), " filas.")

# comunas ----
# calcular cajas por comuna

comunas_bbox <- comunas_geo_2 |>
  mutate(
    bbox = map(geometry, st_bbox),
    xmin = map_dbl(bbox, "xmin"),
    ymin = map_dbl(bbox, "ymin"),
    xmax = map_dbl(bbox, "xmax"),
    ymax = map_dbl(bbox, "ymax")
  ) |>
  select(nombre_region, nombre_comuna, xmin, ymin, xmax, ymax) |>
  st_drop_geometry()

saveRDS(comunas_bbox, "output/cajas_comunas.rds")
message("Guardado output/cajas_comunas.rds con ", nrow(comunas_bbox), " filas.")
