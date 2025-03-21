library(dplyr)
library(ggplot2)

# restantes de gráficos ----

## boxplot ----
temp |> distinct(nombre)

temp |> 
  filter(nombre %in% c("Chacalluta, Arica Ap.",           
                       "Diego Aracena Iquique Ap.",
                       "El Loa, Calama Ad.")) |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_boxplot()


# tortas ----

df <- data.frame(value = c(10, 30, 32, 28),
                 group = paste0("G", 1:4))

df |> 
  ggplot() +
  aes(x = 1, y = value, fill = group) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_x_continuous(expand = expansion(c(2, 0))) +
  theme_void()

pueblos <- readr::read_csv2("datos/pueblos_indigenas_chile.csv")


pueblos |> 
  group_by(pueblo) |> 
  summarize(total = sum(n)) |> 
  mutate(p = total/sum(total)) |> 
  ggplot() +
  aes(pueblo, p, fill = pueblo) +
  geom_col()

pueblos |> 
  group_by(pueblo) |> 
  summarize(total = sum(n)) |> 
  mutate(p = total/sum(total)) |> 
  ggplot() +
  aes(1, p, fill = pueblo) +
  geom_col() +
  coord_polar(theta = "y")

pueblos |> 
  mutate(pueblo = ifelse(pueblo != "Mapuche", "Otros", "Mapuche")) |> 
  group_by(pueblo) |> 
  summarize(total = sum(n)) |> 
  mutate(p = total/sum(total)) |> 
  ggplot() +
  aes(1, p, fill = pueblo) +
  geom_col() +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(p)),
            position = position_stack(0.5))



# facet_wrap

# guardar en loop


# temperatura ----

temp <- read_csv2("datos/temperaturas_chile_unificadas.csv")

unique(temp$nombre)

temp |> 
  # filter(nombre == "Quinta Normal, Santiago") |>
  filter(nombre == "General Freire, Curicó Ad.") |> 
  filter(año >= 1980) |> 
  group_by(año, mes) |> 
  summarize(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(año, mes, fill = t_max) +
  geom_tile() +
  coord_equal(expand = F) +
  scale_fill_viridis(option = "magma", begin = 0, name = NULL) +
  theme_minimal() +
  scale_y_continuous(breaks = 1:12) +
  scale_x_continuous(breaks = c(seq(1980, 2020, 10), 2024)) +
  labs(title = "Temperatura máxima mensual",
       subtitle = "Estación meteorológica General Freire, Curicó") +
  theme(legend.key.height = unit(13, "mm"),
        legend.key.width = unit(2, "mm")) +
  theme(panel.grid.major = element_blank()) +
  ggview::canvas(11, 4)





# paletas ----

diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = x, size = carat) +
  geom_point() +
  scale_color_gradient(low = "purple4", high = "red3")


diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = x, size = carat) +
  geom_point() +
  scale_color_gradient2(low = "purple4", mid = "cyan3", high = "deeppink", midpoint = 6.5)

diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = x, size = carat) +
  geom_point() +
  scale_color_gradientn(colours = c("purple3", "yellow", "red", "pink", "blue"))



### viridis ----
library(viridis)

diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  scale_color_viridis()

diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  scale_color_viridis(option = "plasma", begin = 0.2, end = 0.8)


### rcolorbrewer ----
diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  scale_color_distiller(palette = "YlGnBu")


### scico ----
# install.packages("scico")
library(scico)

diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  # scale_color_scico(palette = "acton", end = .8)
  scale_color_scico(palette = "tokyo", end = .8)
# scale_color_viridis_c(option = "plasma")

scico_palette_show()
scico_palette_names()





# — -----------------

library(gt)

temp <- readr::read_csv2("datos/temperaturas_chile_unificadas.csv")

temp |> distinct(nombre)
# temperaturas ----

temp_evolucion <- temp |> 
  # filter(nombre == sample(nombre, 1)) |> 
  filter(nombre == "Quinta Normal, Santiago") |> 
  filter(año >= 1990) |> 
  group_by(año) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T))

temp_evolucion

temp_evolucion |> 
  gt()

## formatos ----

# formatear números
temp_evolucion |> 
  gt() |> 
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",")




## colores ----

# aplicar color a una columna
temp_evolucion |> 
  gt() |> 
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |> 
  data_color(columns = t_max,
             palette = c("white", "red"))

# aplicar distintos colores a dos columnas distintas
temp_evolucion |> 
  gt() |> 
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |> 
  data_color(columns = c(t_max, t_min),
             palette = c("blue2", "white", "red"))

# aplicar distintos colores a dos columnas distintas
temp_evolucion |> 
  gt() |> 
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |> 
  data_color(columns = t_max, palette = c("white", "red")) |> 
  data_color(columns = t_min, palette = c("blue2", "white"))


## estilos ----

# aplicar color a un caso específico
temp_evolucion |>
  gt() |>
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |>
  tab_style(style = cell_text(color = "red"),
            locations = cells_body(columns = t_max,
                                   rows = t_max >= 23.5))

# aplicar dos estilos a casos específicos
temp_evolucion |>
  gt() |>
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |>
  tab_style(style = list(cell_text(color = "red", weight = "bold"),
                         cell_fill(color = "red", alpha = 0.1)),
            locations = cells_body(columns = t_max,
                                   rows = t_max >= 23.5))



# diferencia de temperaturas -----
temp_anual <- temp |> 
  filter(año == 2013 | año == 2023) |> 
  group_by(nombre, zona_geografica, año) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T)) |> 
  ungroup()

temp_anual

temp_anual |> gt()

# alineación de texto de una columna
temp_anual |> 
  gt() |> 
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |> 
  cols_align(align = "right", 
             columns = nombre)




library(tidyr)

temp_anual_ancho <- temp_anual |> 
  select(-t_min) |> 
  pivot_wider(names_from = año, values_from = t_max,
              names_prefix = "año_")

# aplicar color a dos columnas
temp_anual_ancho |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  data_color(columns = where(is.numeric), palette = c("white", "red")) 
# no se notan diferencias


## tabla poniendo color a la diferencia ----

# calcular diferencias
temp_anual_ancho_dif <- temp_anual_ancho |> 
  mutate(diferencia = año_2023 - año_2013) |> 
  mutate(aumento = diferencia > 0)

# aplicar color desde otra columna
temp_anual_ancho_dif |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  data_color(columns = diferencia, palette = c("blue", "white", "red"),
             domain = c(-3, 3),
             target_columns = año_2023) # variale a afectar

# ocultar otras columnas
temp_anual_ancho_dif |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  data_color(columns = diferencia, palette = c("blue", "white", "red"),
             domain = c(-3, 3),
             target_columns = año_2023) |> 
  cols_hide(c(diferencia, aumento))

# renombrar columnas
temp_anual_ancho_dif |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  data_color(columns = diferencia, palette = c("blue", "white", "red"),
             domain = c(-3, 3),
             target_columns = año_2023) |> 
  cols_hide(c(diferencia, aumento)) |> 
  cols_label(nombre ~ "Estación",
             zona_geografica ~ "Zona",
             año_2013 ~ "2013", 
             año_2023 ~ "2023")




## tabla con ícono por aumento ----
temp_anual_ancho_dif |> 
  mutate(aumento = ifelse(aumento, "arrow-up", "arrow-down")) |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  fmt_icon(aumento)

# icono con color
temp_anual_ancho_dif |> 
  mutate(aumento = ifelse(aumento, "arrow-up", "arrow-down")) |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  fmt_icon(aumento) |> 
  data_color(columns = aumento, 
             method = "factor",
             levels = c("arrow-down", "arrow-up"),
             palette = c("blue2", "red2"),
             apply_to = "text") |> 
  cols_label(aumento ~ "")


# icono con color por condicional
temp_anual_ancho_dif |> 
  mutate(aumento = ifelse(aumento, "arrow-up", "arrow-down")) |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  fmt_icon(aumento) |> 
  tab_style(style = cell_text(color = "orange"),
            locations = cells_body(columns = aumento,
                                   rows = aumento == "arrow-up"))
  
# estilo extra
tabla_1 <- temp_anual_ancho_dif |> 
  mutate(aumento = ifelse(aumento, "arrow-up", "arrow-down")) |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",") |> 
  fmt_icon(aumento) |> 
  tab_style(style = cell_text(color = "orange2"),
            locations = cells_body(columns = aumento,
                                   rows = aumento == "arrow-up")) |> 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(nombre))


# personalizar tabla ----

## títulos ----
tabla_1 |> 
# título y subtítulo
tab_header(title = "Temperatura anual", subtitle = "Cambios de temperatura anual promedio por estación") |> 
  # texto fuente
  tab_source_note("Fuente: Datos Abiertos del Estado, Chile")

# estilo de títulos de variables
tabla_1 |> 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels())

# espaciado vertical
tabla_1 |>
opt_vertical_padding(scale = 2)

## ancho ----
tabla_1 |> 
  cols_width(nombre ~ "50%")

# colores ----
color_fondo = "cornsilk"
color_tema = "cornsilk3"
color_texto = "darkseagreen4"

tabla_1 |> 
  # filas alternas
  tab_options(row.striping.include_table_body = TRUE,
              row.striping.background_color = "cornsilk2") |> 
# eliminar bordes de arriba y abajo de la tabla
tab_options(column_labels.border.top.color = color_fondo,
            column_labels.border.bottom.color = color_fondo,
            table_body.border.bottom.color = color_fondo) |>
  # tema de la tabla
  tab_options(table.background.color = color_fondo,
              table.font.color = color_texto,
              table.font.color.light = color_texto,
              table_body.hlines.color = color_tema)

# 
# ## líneas y bordes ----
# 
# ## tipografías ----
# 
# 
# tribble(~icono, ~titulo, ~explicacion,
#         "star", "Recientes", "Código o datos recientemente actualizados",
#         "arrow-up", "Popular", "Conjunto de datos popular en GitHub",
#         "circle-down", "Bajar", "Presionar para descarga directa del dato",
#         "window-restore", "App", "Presionar para visitar aplicación o visualizador",
#         "venus", "Género", "Datos desagregados por género",
#         "map", "Comunas", "Datos desagregados por comunas",
#         "calendar", "Años", "Temporalidad anual de las observaciones",
#         "calendar-days", "Meses", "Temporalidad mensual de las observaciones") |> 
#   gt() |> 
#   fmt_icon(columns = icono, fill_color = color_enlaces) |> 
#   cols_align(columns = icono, "center") |> 
#   cols_label(icono = "", titulo = "", explicacion = "") |> 
#   tab_style(style = cell_text(weight = "bold"),
#             locations = cells_body(columns = titulo)) |> 
#   # eliminar bordes de arriba y abajo de la tabla
#   tab_options(column_labels.border.top.color = color_fondo,
#               column_labels.border.bottom.color = color_fondo,
#               table_body.border.bottom.color = color_fondo) |>
#   # tema de la tabla
#   tab_options(table.background.color = color_fondo,
#               table.font.color = color_texto,
#               table.font.color.light = color_texto,
#               table_body.hlines.color = color_fondo)
# 
# 
# 
# tabla <- datos |> 
#   arrange(desc(popular), desc(fecha)) |>
#   relocate(reciente, popular, bajar, .after = etiquetas) |>
#   mutate(titulo = ifelse(!is.na(titulo_repo), titulo_repo, titulo)) |> 
#   # formato columnas gt
#   rowwise() |>
#   mutate(etiquetas = list(discard(etiquetas, etiquetas %in% c("data", "shiny", "r", "app", "tiempo", "comunas", "chile", "meses")))) |> 
#   # columnas
#   mutate(tiempo = case_match(tiempo,
#                              "anual" ~ "calendar",
#                              "mensual" ~ "calendar-days")) |> 
#   rowwise() |>
#   mutate(etiquetas = col_tag(etiquetas),
#          titulo = col_link(titulo, enlace),
#          # aplicación = col_icon(aplicación, "window-restore"),
#          app = col_icon_link(app, "window-restore", color = color_enlaces, link = enlace_app),
#          bajar = col_icon_link(bajar, "circle-down", color = color_enlaces, link = descarga),
#          popular = col_icon(popular, "arrow-up", color = color_enlaces),
#          genero = col_icon(genero, "venus", color = color_enlaces),
#          comunas = col_icon(comunas, "map", color = color_enlaces),
#          reciente = col_icon(reciente, "star", color = color_enlaces)) |> 
#   ungroup()
# 
# 
# tabla |> 
#   # rowwise() |> 
#   gt() |> 
#   fmt_icon(columns = tiempo, fill_color = color_enlaces) |>
#   sub_missing(columns = everything(), missing_text = "") |>
#   cols_align(columns = titulo, "right") |>
#   cols_align(columns = etiquetas, "left") |>
#   cols_align(columns = c(genero, comunas, app, reciente, popular, tiempo),
#              "center") |>
#   cols_hide(c(enlace, descarga, estrellas, fecha, enlace_app, titulo_repo)) |>
#   fmt_markdown(columns = titulo) |> 
#   fmt_markdown(columns = etiquetas) |> 
#   # fmt_markdown(columns = c(genero, comunas, reciente, popular)) |>
#   fmt_markdown(columns = genero, rows = genero != "") |> 
#   fmt_markdown(columns = comunas, rows = comunas != "") |> 
#   fmt_markdown(columns = reciente, rows = reciente != "") |> 
#   fmt_markdown(columns = popular, rows = popular != "") |>
#   fmt_markdown(columns = app, rows = app != "") |> 
#   fmt_markdown(columns = bajar, rows = bajar != "") |> 
#   cols_width(#descripcion ~ "40%",
#     etiquetas ~ "18%",
#     genero ~ "5%",
#     comunas ~ "5%",
#     reciente ~ "5%",
#     popular ~ "5%",
#     app ~ "5%",
#     bajar ~ "5%"
#   ) |>
#   # centrar verticalmente texto de celdas
#   tab_style(style = "vertical-align:middle",
#             locations = cells_body(columns = everything())
#   ) |>
#   tab_style(style = cell_text(color = color_texto, weight = "bold"),
#             locations = cells_body(columns = titulo)) |>
#   cols_label(titulo = "",
#              descripcion = "",
#              etiquetas = "Temas",
#              reciente = "Reciente",
#              bajar = "Bajar",
#              popular = "Popular",
#              genero = "Género",
#              app = "App",
#              comunas = "Comunas",
#              tiempo = "Tiempo") |>
#   # cols_merge(c(reciente, popular, app, genero, comunas, tiempo)) |> 
#   # eliminar bordes de arriba y abajo de la tabla
#   tab_options(column_labels.border.top.color = color_fondo,
#               column_labels.border.bottom.color = color_fondo,
#               table_body.border.bottom.color = color_fondo,
#               table.additional_css = paste0("a {color:", color_enlaces, "!important;}")) |>
#   # tema de la tabla
#   tab_options(table.background.color = color_fondo,
#               table.font.color = color_texto,
#               table.font.color.light = color_texto,
#               table_body.hlines.color = color_destacado_2)