
#igvust + suby

library(readxl)
library(dplyr)
library(tidyverse)
library(janitor)
library(territorial)
library(chilemapas)
library(sf)
library(ggrepel)
library(biscale)
library(cowplot)
library(scales)



# ---- Cargar bases ----

# IGVUST
datos <- read_excel("../input/data-orig/igvust.xlsx") %>%
  limpiar_regiones(Region)

# Factores subyacentes
suby <- read_excel("../input/data-orig/fac_suby.xlsx", skip = 1) %>%
  clean_names() %>%
  select(region, comuna, codigo, poblacion_censada,
         x0_14, x65_anos_o_mas, indice_envejecimiento,
         tasa_envejecimiento_x_100mil, idh, icfsr, irape) %>%
  rename(cod_com = codigo) %>%
  mutate(indice_envejecimiento = as.numeric(indice_envejecimiento))

# confirmar conversión numérica
sum(is.na(suby$indice_envejecimiento))
class(suby$indice_envejecimiento)

# ---- Unir bases por código comunal ----
datos_completos <- datos %>%
  mutate(cod_com = as.numeric(cod_com)) %>%
  left_join(suby %>% mutate(cod_com = as.numeric(cod_com)),
            by = "cod_com", suffix = c("", "_suby"))

# verificar cruce
datos_completos %>% count(is.na(idh))



# Tabla comparativa de índices por región


tabla_regional_comparada <- datos_completos %>%
  group_by(Region) %>%
  summarise(
    prom_c_ig_nac = mean(c_ig_nac, na.rm = TRUE),
    idh_prom = mean(idh, na.rm = TRUE),
    icfsr_prom = mean(icfsr, na.rm = TRUE),
    irape_prom = mean(irape, na.rm = TRUE),
    envejecimiento_prom = mean(indice_envejecimiento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(prom_c_ig_nac)

print(tabla_regional_comparada)




# scatterplot vulnerabilidad IGVUST vs. otros índices


# IGVUST (rank_nac) vs IDH
grafico_idh <- ggplot(datos_completos, aes(x = rank_nac, y = idh)) +
  geom_point(aes(color = c_ig_nac), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_viridis_c(option = "inferno", direction = -1, name = "Cuartil\nIGVUST") +
  labs(
    title = "Vulnerabilidad IGVUST vs. Desarrollo Humano",
    x = "Ranking nacional IGVUST (1 = más vulnerable)",
    y = "Índice de Desarrollo Humano (IDH)"
  ) +
  theme_minimal()

# IGVUST (rank_nac) vs riesgo de desastres (ICFSR)
grafico_icfsr <- ggplot(datos_completos, aes(x = rank_nac, y = icfsr)) +
  geom_point(aes(color = c_ig_nac), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_viridis_c(option = "inferno", direction = -1, name = "Cuartil\nIGVUST") +
  labs(
    title = "Vulnerabilidad IGVUST vs. Riesgo de Desastres (ICFSR)",
    x = "Ranking nacional IGVUST (1 = más vulnerable)",
    y = "ICFSR"
  ) +
  theme_minimal()

grafico_idh
grafico_icfsr

# correlaciones
cor(datos_completos$rank_nac, datos_completos$idh, use = "complete.obs")
cor(datos_completos$rank_nac, datos_completos$icfsr, use = "complete.obs")
cor(datos_completos$rank_nac, datos_completos$irape, use = "complete.obs")
cor(datos_completos$rank_nac, datos_completos$indice_envejecimiento, use = "complete.obs")



# bivariado IGVUST + riesgo de desastres (ICFSR)


datos_mapa <- datos_completos %>%
  mutate(codigo_comuna = sprintf("%05d", as.numeric(cod_com)))

mapa_comunal_biv <- chilemapas::mapa_comunas |>
  st_as_sf() |>
  left_join(datos_mapa, by = "codigo_comuna")

# clasificar en biclases (3x3): vulnerabilidad IGVUST vs riesgo ICFSR
mapa_biv <- bi_class(mapa_comunal_biv,
                      x = rank_nac, y = icfsr,
                      style = "quantile", dim = 3)

mapa_bivariado <- ggplot() +
  geom_sf(data = mapa_biv, aes(fill = bi_class), color = "white", linewidth = 0.05, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue2", dim = 3) +
  labs(title = "Vulnerabilidad IGVUST y riesgo de desastres (ICFSR) por comuna") +
  theme_void()

leyenda_bivariada <- bi_legend(pal = "DkBlue2",
                                dim = 3,
                                xlab = "Más vulnerable (IGVUST) →",
                                ylab = "Mayor riesgo (ICFSR) →",
                                size = 7)

# combinar mapa + leyenda
ggdraw() +
  draw_plot(mapa_bivariado, 0, 0, 1, 1) +
  draw_plot(leyenda_bivariada, 0.05, 0.05, 0.25, 0.25)



 # doble vulnerabilidad (IGVUST + ICFSR)


ranking_combinado <- datos_completos %>%
  filter(!is.na(rank_nac), !is.na(icfsr)) %>%
  mutate(
    percentil_igvust = percent_rank(rank_nac),      # cerca de 0 = más vulnerable (rank 1 es el peor)
    percentil_icfsr = percent_rank(desc(icfsr)),     # invertido para que cerca de 0 = mayor riesgo
    indice_doble_vulnerabilidad = (percentil_igvust + percentil_icfsr) / 2
  ) %>%
  arrange(indice_doble_vulnerabilidad) %>%
  select(Comuna, Region, rank_nac, icfsr, indice_doble_vulnerabilidad)

head(ranking_combinado, 15)



# Vulnerabilidad IGVUST según envejecimiento


grafico_envejecimiento <- ggplot(datos_completos, aes(x = factor(c_ig_nac), y = indice_envejecimiento)) +
  geom_boxplot(aes(fill = factor(c_ig_nac)), alpha = 0.7) +
  scale_fill_manual(values = c("1" = "#7A0403", "2" = "#E85D04",
                                "3" = "#FFC300", "4" = "#2A9D8F"), guide = "none") +
  labs(
    title = "Envejecimiento comunal según cuartil de vulnerabilidad IGVUST",
    x = "Cuartil IGVUST (1 = más vulnerable)",
    y = "Índice de Envejecimiento"
  ) +
  theme_minimal()

grafico_envejecimiento

# tabla resumen
datos_completos %>%
  group_by(c_ig_nac) %>%
  summarise(
    envejecimiento_prom = mean(indice_envejecimiento, na.rm = TRUE),
    envejecimiento_mediana = median(indice_envejecimiento, na.rm = TRUE),
    .groups = "drop"
  )



# comunas donde los índices no calzan

# alta vulnerabilidad IGVUST (cuartil 1) pero bajo riesgo de desastres
caso_a <- datos_completos %>%
  filter(c_ig_nac == 1) %>%
  arrange(icfsr) %>%
  slice_head(n = 5) %>%
  select(Comuna, Region, c_ig_nac, icfsr, idh)

# baja vulnerabilidad IGVUST (cuartil 4) pero alto riesgo de desastres
caso_b <- datos_completos %>%
  filter(c_ig_nac == 4) %>%
  arrange(desc(icfsr)) %>%
  slice_head(n = 5) %>%
  select(Comuna, Region, c_ig_nac, icfsr, idh)

caso_a  # vulnerables socialmente, bajo riesgo de desastres
caso_b  # buen IGVUST, pero alto riesgo de desastres (ej. costeras/volcánicas)



cor(datos_completos$rank_nac, datos_completos$idh, use = "complete.obs", method = "spearman")
cor(datos_completos$rank_nac, datos_completos$icfsr, use = "complete.obs", method = "spearman")
cor(datos_completos$rank_nac, datos_completos$irape, use = "complete.obs", method = "spearman")
cor(datos_completos$rank_nac, datos_completos$indice_envejecimiento, use = "complete.obs", method = "spearman")


# cuántas comunas quedan sin IDH 
datos_completos %>%
  filter(is.na(idh)) %>%
  select(Comuna, Region, cod_com, poblacion_censada)

cor(datos_completos$rank_nac, datos_completos$indice_envejecimiento, use = "complete.obs", method = "pearson")


# tabla bivariada para qmd

tabla_bivariada_regional <- mapa_biv %>%
  sf::st_drop_geometry() %>%
  count(Region, bi_class) %>%
  group_by(Region) %>%
  mutate(total_region = sum(n), pct = n / total_region) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(pct)) %>%
  mutate(
    Vulnerabilidad = case_when(
      substr(bi_class, 1, 1) == "1" ~ "Alta",
      substr(bi_class, 1, 1) == "2" ~ "Media",
      substr(bi_class, 1, 1) == "3" ~ "Baja"
    ),
    `Riesgo desastres` = case_when(
      substr(bi_class, 3, 3) == "1" ~ "Bajo",
      substr(bi_class, 3, 3) == "2" ~ "Medio",
      substr(bi_class, 3, 3) == "3" ~ "Alto"
    ),
    pct = scales::percent(pct, accuracy = 0.1)
  ) %>%
  select(Region, Vulnerabilidad, `Riesgo desastres`, `N° comunas` = n, `Total comunas` = total_region, `% región` = pct)