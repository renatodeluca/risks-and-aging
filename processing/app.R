library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(mapgl) # https://walker-data.com/mapgl/
library(territorial) # https://bastianolea.github.io/territorial/
library(chilemapas) # https://pacha.dev/chilemapas/
library(sf)

# convertir mapa chilemapas::mapa_comunas a sf
mapa_comunas <- sf::st_as_sf(chilemapas::mapa_comunas)

# cargar cajas (bounding boxes) precalculadas para zoom
caja_region <- readRDS("output/cajas_regiones.rds")
caja_comuna <- readRDS("output/cajas_comunas.rds")

# cargar bases unidas (generadas en cajas.r)
# datos_comuna: igvust + fac_suby + clasif_com (join por comuna)
#               + indicadores de bpc_larga agregados por región
datos_comuna <- readRDS("output/datos_comuna.rds")

# capa comunal con los datos de los dos cruces
capa_comunas <- mapa_comunas |>
  transmute(cod_com = as.numeric(codigo_comuna)) |>
  left_join(datos_comuna, by = "cod_com") |>
  mutate(
    c_ig_nac_f     = factor(c_ig_nac, levels = 1:4),
    nombre_comuna  = territorial::as_nombre_comuna(cod_com)
  )

# paleta del proyecto (misma que en los mapas estáticos)
paleta_cuartil <- c("#7A0403", "#E85D04", "#FFC300", "#2A9D8F")

variables_mapa <- c(
  "Cuartil IGVUST"               = "c_ig_nac_f",
  "Riesgo de desastres (ICFSR)"  = "icfsr",
  "Desarrollo humano (IDH)"      = "idh",
  "Índice de envejecimiento"     = "indice_envejecimiento",
  "Clasificación urbana/rural"   = "clasificacion",
  "Personas cuidadoras (región)" = "cuidadoras"
)

# interfaz de la app

ui <- page_fillable(
  padding = 0, # app sin bordes
  useShinyjs(), # para ocultar/mostrar elementos

  # estilos css
  includeCSS("estilos.css"),

  # mapa
  mapgl::maplibreOutput("mapa", height = "100%"),

  # panel
  card(
    class = "panel-mapa",
    full_screen = FALSE,

    markdown(
      "Índice de Vulnerabilidad Socioterritorial (IGVUST), índice de factores subyacentes del riesgo, clasificación comunal (urbana, rural, mixta) y número de personas cuidadoras, con cruces por región y por comuna. Realizado por Renato De Luca."
    ),

    # selector de variable a mostrar
    selectInput(
      inputId = "variable",
      label = "Variable:",
      width = "100%",
      selectize = FALSE,
      choices = variables_mapa
    ),

    # nombres de comunas sobre el mapa
    checkboxInput(
      inputId = "nombres",
      label = "Mostrar nombres de comunas",
      value = TRUE
    ),

    # selector de regiones
    selectInput(
      inputId = "region",
      label = "Región:",
      width = "100%",
      # select nativo: en celulares no abre el teclado
      selectize = FALSE,
      choices = c(
        "País",
        territorial::regiones()
      )
    ),

    # selector de comunas
    selectInput(
      inputId = "comuna",
      label = "Comuna:",
      width = "100%",
      selectize = FALSE,
      choices = NULL
    ) |>
      hidden(),
  ),
)

# lógica de la app

server <- function(input, output, session) {

  # mapa base ----
  output$mapa <- renderMaplibre({
    maplibre(
      style = carto_style("voyager"),
      center = c(-71.7, -35),
      zoom = 4,
      minZoom = 4,
      maxZoom = 14
    )
  })

  # capa de datos ----
  # se limpia y se vuelve a agregar cada vez que cambia la variable

  # nombres de comunas sobre el polígono (centroides); aparecen con zoom >= 7
  agregar_nombres <- function(proxy, visibles = TRUE) {
    if (!visibles) {
      return(clear_layer(proxy, "nombres"))
    }

    proxy |>
      add_symbol_layer(
        id = "nombres",
        source = capa_comunas,
        text_field = list("get", "nombre_comuna"),
        text_size = 11,
        text_color = "#1f1f1f",
        text_halo_color = "white",
        text_halo_width = 1.5,
        text_allow_overlap = FALSE,
        min_zoom = 7
      )
  }

  observeEvent(input$variable, {
    var     <- input$variable
    etiqueta <- names(variables_mapa)[match(var, variables_mapa)]

    proxy <- maplibre_proxy("mapa") |>
      clear_layer("comunas") |>
      clear_legend()

    if (var == "c_ig_nac_f") {
      proxy |>
        add_fill_layer(
          id = "comunas",
          source = capa_comunas,
          fill_color = match_expr("c_ig_nac_f",
                                  values = as.character(1:4),
                                  stops = paleta_cuartil,
                                  default = "#f0f0f0"),
          fill_opacity = 0.8,
          popup = concat(
            "<b>", get_column("nombre_comuna"), "</b><br>",
            "Región: ", get_column("Region"), "<br>",
            "Cuartil IGVUST: ", get_column("c_ig_nac"), "<br>",
            "ICFSR: ", number_format("icfsr", maximum_fraction_digits = 3), "<br>",
            "IDH: ", number_format("idh", maximum_fraction_digits = 3), "<br>",
            "Cuidadoras (región): ", number_format("cuidadoras")
          )
        ) |>
        add_categorical_legend(
          legend_title = etiqueta,
          values = c("1 (más vulnerable)", "2", "3", "4 (menos vulnerable)"),
          colors = paleta_cuartil
        )

    } else if (var == "clasificacion") {
      proxy |>
        add_fill_layer(
          id = "comunas",
          source = capa_comunas,
          fill_color = match_expr("clasificacion",
                                  values = c("Urbana", "Rural"),
                                  stops = c("#4575b4", "#f46d43"),
                                  default = "#f0f0f0"),
          fill_opacity = 0.8,
          popup = concat(
            "<b>", get_column("nombre_comuna"), "</b><br>",
            "Región: ", get_column("Region"), "<br>",
            "Clasificación: ", get_column("clasificacion"), "<br>",
            "Población 2024: ", number_format("pob_2024")
          )
        ) |>
        add_categorical_legend(
          legend_title = etiqueta,
          values = c("Urbana", "Rural"),
          colors = c("#4575b4", "#f46d43")
        )

    } else {
      rango <- range(capa_comunas[[var]], na.rm = TRUE)
      rampa <- switch(var,
                      icfsr = c("#fff7ec", "#7f0000"),
                      idh = c("#ffffcc", "#800026"),
                      indice_envejecimiento = c("#edf8fb", "#88419d"),
                      cuidadoras = c("#fff7ec", "#67000d"))

      proxy |>
        add_fill_layer(
          id = "comunas",
          source = capa_comunas,
          fill_color = interpolate(var,
                                   values = rango,
                                   stops = rampa,
                                   na_color = "#f0f0f0"),
          fill_opacity = 0.8,
          popup = concat(
            "<b>", get_column("nombre_comuna"), "</b><br>",
            "Región: ", get_column("Region"), "<br>",
            "<b>", etiqueta, ": ",
            number_format(var, maximum_fraction_digits = 3), "</b><br>",
            "ICFSR: ", number_format("icfsr", maximum_fraction_digits = 3), "<br>",
            "IDH: ", number_format("idh", maximum_fraction_digits = 3), "<br>",
            "Cuidadoras (región): ", number_format("cuidadoras")
          )
        ) |>
        add_continuous_legend(
          legend_title = etiqueta,
          values = rango,
          colors = rampa
        )
    }

    # nombres de comunas encima del relleno
    agregar_nombres(proxy, visibles = isTRUE(input$nombres))
  }, ignoreInit = FALSE)

  # mostrar/ocultar nombres al desmarcar el checkbox
  observeEvent(input$nombres, {
    proxy <- maplibre_proxy("mapa") |> clear_layer("nombres")
    agregar_nombres(proxy, visibles = isTRUE(input$nombres))
  }, ignoreInit = TRUE)

  # selectores ----

  # mostrar/ocultar selector de comunas según región
  observe({
    if (input$region == "País") {
      hide("comuna")
    } else {
      show("comuna")
    }
  })

  # actualizar opciones de comunas cuando cambia la región
  observeEvent(input$region, {
    updateSelectInput(
      session,
      "comuna",
      choices = c(
        "Todas",
        caja_comuna |>
          filter(nombre_region == input$region) |>
          distinct(nombre_comuna) |>
          arrange(nombre_comuna) |>
          pull(nombre_comuna)
      )
    )
  })

  # zoom ----

  # caja a usar según el territorio elegido: país, región o comuna
  caja_mapa <- reactive({
    req(input$region)

    # ver país completo
    if (input$region == "País") {
      caja_region |>
        filter(nombre_region == "País")
    } else if (is.null(input$comuna) || input$comuna == "Todas") {
      # ver región (sin comuna seleccionada o con "Todas")
      caja_region |>
        filter(nombre_region == input$region)
    } else {
      # ver comuna específica
      caja_comuna |>
        filter(nombre_region == input$region, nombre_comuna == input$comuna)
    }
  })

  # hacer zoom cada vez que cambie la caja seleccionada (o la variable,
  # porque la capa se vuelve a agregar al mapa)
  observe({
    # caja dependiendo de selectores
    caja <- caja_mapa()
    input$variable

    maplibre_proxy("mapa") |>
      fit_bounds(
        c(caja$xmin, caja$ymin, caja$xmax, caja$ymax),
        animate = TRUE
      )
  })
}

shinyApp(ui = ui, server = server)
