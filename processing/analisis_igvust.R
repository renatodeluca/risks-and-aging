# cargar librerias
library(dplyr)
library(readxl)
library(territorial)
library(chilemapas)
library(sf)
library(patchwork)
library(tidyverse)
options(scipen = 999)
# cargar bbdd
datos <- read_excel("../input/data-orig/igvust.xlsx")

# limpiar nombres de región (arregla duplicados como VALPARAISO / VALPARAÍSO)
datos <- datos %>%
  limpiar_regiones(Region)

# vuonerabilidad por region invertida
regiones_vulnerabilidad <- datos %>%
  group_by(Region, cod_reg) %>%
  summarise(
    prom_c_ig_nac = mean(c_ig_nac, na.rm = TRUE),
    n_comunas_q1 = sum(c_ig_nac == 1, na.rm = TRUE),  # comunas más vulnerables
    n_comunas = n(),
    prop_comunas_q1 = n_comunas_q1 / n_comunas,  # % de comunas en cuartil 1
    .groups = "drop"
  ) %>%
  rename(codigo_region = cod_reg) %>%
  arrange(desc(prop_comunas_q1))  # el más alto = región más vulnerable

print(regiones_vulnerabilidad)

# región más vulnerable:
region_mas_vulnerable <- regiones_vulnerabilidad$Region[1]
region_mas_vulnerable

# mapas ------

# mapa nacional
# obtener polígonos regionales uniendo las comunas
mapa_regional <- chilemapas::mapa_comunas |>
  generar_regiones() |>
  st_as_sf()

# unir el mapa con los resultados de vulnerabilidad
mapa_vulnerabilidad <- mapa_regional |>
  left_join(regiones_vulnerabilidad, by = "codigo_region")

# verificar que no queden NA 
mapa_vulnerabilidad %>%
  st_drop_geometry() %>%
  count(is.na(prop_comunas_q1))

# graficar la proporción de comunas en cuartil 1 por región
grafico_mapa_nacional <- ggplot(mapa_vulnerabilidad) +
  geom_sf(aes(fill = prop_comunas_q1), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "rocket",
    direction = -1,
    labels = scales::percent,
    name = "% comunas\nen cuartil 1"
  ) +
  labs(
    title = "Vulnerabilidad socioterritorial por región",
    subtitle = "Proporción de comunas en el cuartil nacional más vulnerable (IGVUST)",
    caption = "Fuente: IGVUST"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

grafico_mapa_nacional

mapa_vulnerabilidad

# mapas regionales 
# obtener mapa de comunas con los datos de vulnerabilidad
mapa_comunal <- chilemapas::mapa_comunas %>%
  st_as_sf() %>%
  left_join(
    datos %>% 
      mutate(cod_com = sprintf("%05d", cod_com)),
    by = c("codigo_comuna" = "cod_com")
  )

# las 3 regiones más vulnerables
top3_regiones <- regiones_vulnerabilidad$Region[1:3]

# graficar mapas de las 3 regiones más vulnerables
graficar_region <- function(nombre_region) {

  mapa_region_actual <- mapa_comunal %>%
    filter(Region == nombre_region) %>%
  mutate(c_ig_nac_factor = factor(as.character(as.integer(c_ig_nac)),
                                   levels = c("1", "2", "3", "4")))

  ggplot(mapa_region_actual) +
    geom_sf(aes(fill = c_ig_nac_factor), color = "white", linewidth = 0.15) +
    scale_fill_manual(
      values = c("1" = "#7A0403",
                 "2" = "#E85D04",
                 "3" = "#FFC300",
                 "4" = "#2A9D8F"),
      name = "Cuartil",
      labels = c("1", "2", "3", "4"),
      na.value = "grey90",
      drop = FALSE
    ) +
    labs(title = nombre_region) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11,
                                 margin = margin(b = 5)),
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# generar mapas
mapa_1 <- graficar_region(top3_regiones[1])
mapa_2 <- graficar_region(top3_regiones[2])
mapa_3 <- graficar_region(top3_regiones[3])

# compilar mapas en una lista
mapas_top3 <- list(mapa_1, mapa_2, mapa_3)

mapa_1
mapa_2
mapa_3

# guardar mapas
dir.create("output/graphs", showWarnings = FALSE, recursive = TRUE)

ggsave("output/graphs/mapa_nacional.png", grafico_mapa_nacional, width = 6, height = 8, dpi = 300, bg = "white")
ggsave("output/graphs/mapa_region_1.png", mapa_1, width = 6, height = 8, dpi = 300, bg = "white")
ggsave("output/graphs/mapa_region_2.png", mapa_2, width = 6, height = 8, dpi = 300, bg = "white")
ggsave("output/graphs/mapa_region_3.png", mapa_3, width = 6, height = 8, dpi = 300, bg = "white")



# revisar la relación entre las columnas que arman la cobertura
datos %>%
  filter(p_cobertura_com > 100) %>%
  select(Comuna, Region, pob_rsh_com, hog_com, p_cobertura_com) %>%
  arrange(desc(p_cobertura_com))

# cuántas comunas en total superan el 100%
sum(datos$p_cobertura_com > 100, na.rm = TRUE)