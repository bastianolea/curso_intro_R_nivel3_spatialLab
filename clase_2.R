# Introducción al análisis de datos con R para principiantes
# Nivel 3: Visualización de datos

# clase 2, 20 de marzo


library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# cargar datos de temperatura
temp <- read_csv2("datos/temperaturas_chile_unificadas.csv")

temp |> distinct(nombre)

# filtrar datos
temp_filt <- temp |> 
  filter(nombre %in% c("Chacalluta, Arica Ap.",
                       "Diego Aracena Iquique Ap.",
                       "El Loa, Calama Ad."))

# gráficos ----

## boxplot ----
temp_filt |> 
  ggplot() +
  aes(y = t_max) +
  geom_boxplot()

# boxplot separando por otra variable
temp_filt |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_boxplot()

# boxplot separando por otra variable y además con color
temp_filt |> 
  ggplot() +
  aes(nombre, t_max, color = nombre, fill = nombre) +
  geom_boxplot(alpha = 0.5)


## dispersión por grupos ----

# primero intentemos mapeando las observaciones a puntos
temp_filt |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_point(size = 10, alpha = 0.1)
# resulta poco legible

# geom_jitter() agrega dispersión a los puntos para hacerlos mas visibles
temp_filt |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_jitter(size = 2, alpha = 0.1, height = 0) +
  theme_minimal()

## violin ----
# los gráficos de violin son gráficos de densidad pero espejados en un eje vertical
temp_filt |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_violin(alpha = 0.4) +
  theme_minimal()

# combinar violín con puntos
temp_filt |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_jitter(size = 2, alpha = 0.1, height = 0) +
  geom_violin(alpha = 0.4) +
  # tema para mayor claridad
  theme_minimal()

# calcular una tabla con los promedios por grupo
temp_prom <- temp_filt |> 
  group_by(nombre) |> 
  summarise(t_max = mean(t_max, na.rm = T))

# calcular una tabla con la mediana por grupo
temp_median <- temp_filt |> 
  group_by(nombre) |> 
  summarise(t_max = median(t_max, na.rm = T))

temp_filt |> 
  ggplot() +
  aes(nombre, t_max) +
  geom_violin(alpha = 0.4, color = "grey80", fill = "grey80") +
  # puntos de promedio
  geom_point(data = temp_prom, size = 4, color = "red", alpha = 0.4) +
  # cuadrados de mediana
  geom_point(data = temp_median, size = 4, shape = "diamond", color = "purple", alpha = 0.6) +
  # tema para mayor claridad
  theme_minimal()



## tortas -----

# cargar datos de pueblos originarios
pueblos <- readr::read_csv2("datos/pueblos_indigenas_chile.csv")

pueblos_n <- pueblos |>
  mutate(pueblo = ifelse(pueblo %in% c("Mapuche", "Aymara", "Diaguita"), pueblo, "Otros")) |> 
  group_by(pueblo) |> 
  summarize(total = sum(n))

# gráfico de barras
pueblos_n |> 
  ggplot() +
  aes(pueblo, total, fill = pueblo) +
  geom_col() +
  theme_minimal() +
  # paleta de colores
  scale_fill_brewer(palette = "Dark2")

# gráfico de barras con anotaciones
cifra_pueblos <- pueblos_n |> filter(pueblo == "Aymara")

pueblos_n |> 
  ggplot() +
  aes(pueblo, total, fill = pueblo) +
  geom_col() +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  # geometría manual
  annotate("text", 
           x = cifra_pueblos$pueblo,
           y = cifra_pueblos$total + 10000, 
           label = "***", size = 6, fontface = "bold")

# otra anotación
pueblos_n |> 
  ggplot() +
  aes(pueblo, total, fill = pueblo) +
  geom_col() +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  # geometría manual
  annotate("text", 
           x = "Mapuche",
           y = 1000000, 
           label = "Mayoría mapuche", 
           size = 5, angle = 90, color = "white")

# un gráfico de torta es un gráfico de barras apilado, y luego enrollado
pueblos_n |> 
  ggplot() +
  aes(1, total, fill = pueblo) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()

# al gráfico anterior le agregamos coord_polar() para volverlo en torta
pueblos_n |> 
  ggplot() +
  aes(1, total, fill = pueblo) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme_void() +
  coord_polar(theta = "y") # enrollar el gráfico para volverlo circular
  
# agregar texto
pueblos_n |> 
  ggplot() +
  aes(1, total, fill = pueblo) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  theme_void() +
  coord_polar(theta = "y") +
  # texto
  geom_text(aes(label = scales::comma(total, big.mark = "."), x = 1.3), color = "white",
          position = position_stack(0.5))


## donas ----
# un gráfico de dona es un gráfico de torta con espacio al medio

pueblos_n |> 
  ggplot() +
  aes(1, total, fill = pueblo) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2")

# para crear el espacio dentro de la torta, necesitamos agregar espacio en el eje horizontal
pueblos_n |> 
  ggplot() +
  aes(1, total, fill = pueblo) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  # agregar espacio en el eje
  scale_x_continuous(expand = expansion(c(2, 0)))

pueblos_n |> 
  ggplot() +
  aes(1, total, fill = pueblo) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  # agregar espacio en el eje
  scale_x_continuous(expand = expansion(c(2, 0))) +
  # hacer en torta
  coord_polar(theta = "y") +
  theme_void()



## facetas ----

# calculemos los datos agrupados otra variable
pueblos_n_sexo <- pueblos |> 
  mutate(pueblo = ifelse(pueblo %in% c("Mapuche", "Aymara", "Diaguita"), pueblo, "Otros")) |> 
  group_by(pueblo, sexo) |> 
  summarize(total = sum(n)) |> 
  group_by(sexo) |> 
  mutate(p = total/sum(total))

# teniendo una variable extra en la base, podemos dividir el gráfico en más de uno
pueblos_n_sexo |> 
  ggplot() +
  aes(1, p, fill = pueblo) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Dark2") +
  theme_void() +
  # dividir por sexo
  facet_wrap(~sexo)

# faceta por otra variable
pueblos |> 
  group_by(pueblo, sexo) |> 
  summarize(total = sum(n)) |> 
  group_by(pueblo) |> 
  mutate(p = total/sum(total)) |> 
  ggplot() +
  aes(1, p, fill = sexo) +
  geom_col() +
  # agregar texto
  geom_text(aes(label = percent(p, accuracy = 1)), color = "white",
            position = position_stack(0.5), fontface = "bold") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~pueblo) +
  theme_void()


## gráficos en lote ----

# calcular datos
pueblos_2 <- pueblos |> 
  group_by(pueblo, sexo) |> 
  summarize(total = sum(n)) |> 
  group_by(pueblo) |> 
  mutate(p = total/sum(total))

# primero un gráfico de prueba, filtrando por una categoría de una variable
# .pueblo <- "Diaguita"
.pueblo <- "Mapuche"

pueblos_2 |> 
  filter(pueblo == .pueblo) |> 
  ggplot() +
  aes(1, p, fill = sexo) +
  geom_col() +
  geom_text(aes(label = percent(p, accuracy = 0.1)), color = "white",
            position = position_stack(0.5), size = 5) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = .pueblo) +
  theme(plot.title = element_text(face = "bold", size = 16, margin = margin(t = 10, b = -20)))

# definir un vector con las categorías de una variable por las cuales dividiremos los gráficos
lista_pueblos <- unique(pueblos_2$pueblo)

# hacer un loop que pase por el vector de categorías, filtrando los datos para generar n gráficos
for (.pueblo in lista_pueblos) {
  message(.pueblo)
  
  pueblos_2 |> 
    filter(pueblo == .pueblo) |> 
    ggplot() +
    aes(1, p, fill = sexo) +
    geom_col() +
    geom_text(aes(label = percent(p, accuracy = 0.1)), color = "white",
              position = position_stack(0.5), size = 5) +
    coord_polar(theta = "y") +
    theme_void() +
    scale_fill_brewer(palette = "Dark2", name = "Género") +
    labs(subtitle = "Distribución de género", title = .pueblo) +
    theme(plot.title = element_text(face = "bold", size = 16, margin = margin(t = 10, b = 0)),
          plot.subtitle = element_text(margin = margin(t = 4, b = -10)),
          plot.margin = margin(6, 6, 6, 6))
  
  # guardar
  ggsave(paste("graficos/grafico_torta", .pueblo, ".jpg"), width = 5, height = 4)
}

# lo mismo podría hacerse para gaurdar archivos excel
for (.pueblo in lista_pueblos) {
  message(.pueblo)
  
  reporte <- pueblos_2 |> 
    filter(pueblo == .pueblo) 
  
  writexl::write_xlsx(reporte, paste("graficos/reporte", .pueblo, ".xlsx"))
}


# cargar datos de temperaturas
temp <- read_csv2("datos/temperaturas_chile_unificadas.csv")

# mapear valores por año horizontalmente, y meses verticalmente
temp |> 
  filter(nombre == "General Freire, Curicó Ad.") |> 
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T)) |> 
  filter(año >= 1980) |> 
  ggplot() +
  aes(año, mes, color = t_max) +
  geom_point(size = 3, alpha = .7) +
  scale_y_continuous(breaks = 1:12) +
  theme_minimal()

library(viridis)

## gráfico de mosaico ----
temp |> 
  filter(nombre == "General Freire, Curicó Ad.") |> 
  group_by(año, mes) |> 
  summarize(t_max = mean(t_max, na.rm = T)) |> 
  filter(año >= 1980) |> 
  ggplot() +
  aes(año, mes, fill = t_max) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis(option = "magma", name = NULL) +
  theme_minimal() +
  scale_y_continuous(breaks = 1:12) +
  scale_x_continuous(breaks = c(seq(1980, 2020, 10), 2024)) +
  labs(title = "Temperatura máxima mensual",
       subtitle = "Estación meteorológica General Freire, Curicó") +
  theme(legend.key.height = unit(13, "mm"),
        legend.key.width = unit(2, "mm")) +
  theme(panel.grid.major = element_blank()) +
  ggview::canvas(11, 4)




# —------



# tablas ----

# install.packages("gt")
library(gt)

temp <- readr::read_csv2("datos/temperaturas_chile_unificadas.csv")

# calcular promedio anual
temp_anual <- temp |> 
  filter(nombre == "Quinta Normal, Santiago") |> 
  filter(año >= 1990) |> 
  group_by(año) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T))

# crear una tabla básica
temp_anual |> gt()

## formato de números ----
# cambiar formato de números
temp_tabla_1 <- temp_anual |> 
  gt() |> 
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",")

temp_tabla_1


## color según valores  ----
temp_tabla_1 |> 
  data_color(columns = t_max,
             palette = c("white", "red"))

# aplicar a más de una variable
temp_tabla_1 |> 
  data_color(columns = c(t_max, t_min),
             palette = c("blue", "white", "red"))

# aplicar a dos variables, por separado
temp_tabla_1 |> 
  data_color(columns = t_max, palette = c("white", "red")) |> 
  data_color(columns = t_min, palette = c("blue", "white"))


## estilos condicionales ----

# aplicar un estilo a las celdas según una condición
temp_tabla_1 |> 
  # primero se define la ubicación que queremos afectar
  tab_style(locations = cells_body(columns = t_max,
                                   rows = t_max > 24),
            # luego definimos el estilo a cambiar
            style = cell_text(color = "red", weight = "bold"))

# cambiar tanto el texto como el relleno de la celda
temp_tabla_1 |> 
  tab_style(locations = cells_body(columns = t_max,
                                   rows = t_max > 23.5),
            style = list(cell_text(color = "red", weight = "bold"),
                         cell_fill(color = "red", alpha = 0.1)))

# calcular temperaturas por grupos
temp_anual_2 <- temp |> 
  filter(año == 2023 | año == 2013) |> 
  group_by(nombre, año, zona_geografica) |> 
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T)) |> 
  ungroup()


temp_anual_2 |> gt()

temp_tabla_2 <- temp_anual_2 |> 
  gt() |>
  fmt_number(columns = c(t_max, t_min),
             decimals = 1, dec_mark = ",") |> 
  cols_align(align = "right", columns = nombre)

temp_tabla_2

# pivotar datos hacia ancho para que cada año quede en una columna
library(tidyr)

temp_anual_ancho <- temp_anual_2 |> 
  select(-t_min) |> 
  pivot_wider(names_from = año, values_from = t_max, 
              names_prefix = "año_")

temp_anual_ancho

temp_tabla_3 <- temp_anual_ancho |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",")

temp_tabla_3

# agregar color a columnas numéricas, sin escribir sus nombres
temp_tabla_3 |> 
  data_color(columns = where(is.numeric), 
             palette = c("white", "red"))

# calcular diferencia y aumento
temp_anual_ancho_dif <- temp_anual_ancho |> 
  mutate(diferencia = año_2023 - año_2013,
         aumento = diferencia > 0)

temp_anual_ancho_dif

temp_tabla_4 <- temp_anual_ancho_dif |> 
  gt() |> 
  fmt_number(decimals = 1, dec_mark = ",")

temp_tabla_4

## color desde otra variable ----
# aplicar color a una columna desde los valores de otra columna
temp_tabla_4 |> 
  data_color(columns = diferencia, 
             palette = c("blue", "white", "red"),
             target_columns = año_2023) |> 
  # ocultar la columna usada
  cols_hide(columns = c(diferencia, aumento)) |> 
  # cambiar etiquetas de columnas
  cols_label(nombre ~ "Estación Meteorológica",
             zona_geografica ~ "Zona",
             año_2013 ~ "2013",
             año_2023 ~ "2023")


## símbolos ----
# crear una columna que use símbolos para representar sus valores
# buscar los nombres de símbolos en: https://fontawesome.com/search
temp_anual_ancho_dif |> 
  mutate(aumento = ifelse(aumento, "fire", "snowflake")) |> 
  gt() |>
  fmt_number(decimals = 1, dec_mark = ",") |> 
  # símbolos
  fmt_icon(aumento) |> 
  # alinear
  cols_align(align = "center", columns = aumento)

# agregar más símbolos
temp_anual_simbolos <- temp_anual_ancho_dif |> 
  mutate(aumento = ifelse(aumento, "fire", "snowflake")) |> 
  mutate(zona_geografica = case_match(zona_geografica,
                                      "Litoral" ~ "umbrella-beach",
                                      "PreCordillera" ~ "mountain",
                                      "Valle" ~ "tree",
                                      "Secano Costero" ~ "mound"))

# aplicar símbolos a mas de una columna
temp_anual_simbolos |> 
  gt() |>
  fmt_number(decimals = 1, dec_mark = ",") |> 
  # font awesome
  fmt_icon(c(aumento, zona_geografica)) |> 
  cols_align(align = "center", columns = c(aumento, zona_geografica))

# aplicar color condicional a los símbolos
temp_tabla_5_1 <- temp_anual_simbolos |> 
  gt() |>
  fmt_number(decimals = 1, dec_mark = ",") |> 
  # símbolos
  fmt_icon(c(aumento, zona_geografica)) |> 
  # alinear
  cols_align(align = "center", columns = c(aumento, zona_geografica)) |> 
  # color condicional
  tab_style(style = cell_text(color = "orange"),
            locations = cells_body(columns = aumento, rows = aumento == "fire"))  |> 
  # texto negrita a una columna
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(nombre))

temp_tabla_5_1


## personalización ----
### agregar títulos ----
temp_tabla_5_2 <- temp_tabla_5_1 |> 
  tab_header(title = "Temperaturas anuales",
             subtitle = "Cambios de temperatura entre 2013 y 2023") |> 
  tab_source_note("Fuente: Datos Abiertos del Estado (datos.gob.cl)")

temp_tabla_5_2

### variables en negrita ----
temp_tabla_5_3 <- temp_tabla_5_2 |> 
  tab_style(style = cell_text(weight = "bold", style = "italic"),
            locations = cells_column_labels())

temp_tabla_5_3

### espaciado vertical ----
temp_tabla_5_4 <- temp_tabla_5_3 |> 
  opt_vertical_padding(scale = 2)

temp_tabla_5_4

### ancho de columnas ----
temp_tabla_5_5 <- temp_tabla_5_4 |> 
  cols_width(nombre ~ "40%") |> 
  cols_align(align = "right", columns = nombre)
  
temp_tabla_5_5

### colores en la tabla ----
color_fondo = "cornsilk"
color_tema = "cornsilk3"
color_texto = "darkseagreen4"

temp_tabla_5_6 <- temp_tabla_5_5 |> 
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

temp_tabla_5_6


## guardar tablas ----
# guardar como imagen
gtsave(temp_tabla_5_6, "graficos/tabla.png")

# guardar en html
gtsave(temp_tabla_5_6, "graficos/tabla.html")
