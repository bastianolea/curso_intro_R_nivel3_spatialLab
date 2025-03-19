# Introducción al análisis de datos con R para principiantes
# Nivel 3: Visualización de datos

# clase 1, 18 de marzo


# introducción a ggplot ----

# instalar ggplot2
# install.packages("ggplot2")

# cargar paquete
library(ggplot2)

# crear un gráfico vacío a partir de una primera capa de datos
iris |> ggplot()

ggplot(data = iris) # equivalente a lo anterior


# gráficos básicos ----

## gráfico de dispersión ----
# primera capa de datos
iris |> 
  ggplot() + # iniciar el gráfico
  # definir el mapeo de variables a características estéticas del gráfico
  aes(x = Sepal.Length, # eje x (horizontal)
      y = Sepal.Width) + # eje y (vertical)
  # agregar una capa de geometría
  geom_point() # puntos

# mismo ejemplo que antes, pero ahora mapeando otra variable a la estética color
iris |> 
  ggplot() +
  aes(x = Sepal.Length, 
      y = Sepal.Width, 
      color = Species) + # variable mapeada al color
  geom_point()

# ahora mapeando una variable al tamaño 
iris |> 
  ggplot() +
  aes(x = Sepal.Length, 
      y = Sepal.Width, 
      size = Petal.Length) + # variable mapeada al tamaño
  # dentro de la geometría, definiremos manualmente el color y la transparencia
  geom_point(color = "purple2", alpha = 0.5)


## gráfico de histograma ----
# los histogramas solo requieren de una variable

iris |> # datos
  ggplot() + # iniciar
  aes(x = Sepal.Length) + # variable horizontal
  geom_histogram() # histograma

iris |> 
  ggplot() +
  # además mapear una variable al color
  aes(x = Sepal.Length, 
      fill = Species) +
  geom_histogram()


## gráfico de densidad ----
# al igual que los histogramas, los gráficos de densidad solo requieren de una variable
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_density()

# aplicarle un relleno para que sea menos feo
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_density(fill = "black", alpha = 0.6)

# dividir la densidad por la variable mapeada al relleno (fill)
# como se trata de una figura con relleno, el color define el borde de la figura
iris |> 
  ggplot() +
  aes(x = Sepal.Length, 
      fill = Species, # relleno de la figura
      color = Species) + # bordes de la figura
  geom_density(alpha = 0.6)


# ejemplos con datos reales ----

## datos de temperaturas ----
library(dplyr) # para manipular datos
library(readr) # para cargar datos

# cargar datos de temperatura
temp <- read_csv2("datos/temperaturas_chile_unificadas.csv")

# revisar columna con nombres de estaciones meteorológicas
temp |> 
  distinct(nombre) |> 
  print(n=Inf)

# contar observaciones por año
temp |> 
  count(año) |> 
  print(n=Inf)

### histograma ----
# explorar observaciones por año con un gráfico
temp |> 
  filter(nombre == "Chacalluta, Arica Ap.") |> 
  ggplot() +
  aes(fecha) +
  geom_histogram()

### dispersión ----
# crear gráfico de dispersión
temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 1980) |> 
  ggplot() +
  aes(t_min, t_max, color = año) +
  geom_point()

# procesar datos antes de la visualización
# para obtener promedios mensuales de temperatura
temp |> 
  filter(nombre == "Quinta Normal, Santiago") |> 
  group_by(año, mes) |> 
  summarise(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(t_min, t_max, color = año) +
  geom_point()


### líneas ----
library(lubridate)

# procesar datos antes de visualizar,
# pero manteniendo la variable fecha para crear un gráfico de líneas
temp |> 
  filter(nombre == "Quinta Normal, Santiago",
         año > 1980) |>
  # redondear fechas al mes
  mutate(fecha = lubridate::floor_date(fecha, "month")) |> 
  # calcular promedios
  group_by(fecha) |> 
  summarise(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  # gráfico
  ggplot() +
  aes(x = fecha, # usar fecha como eje horizontal
      y = t_max, 
      color = t_max) +
  geom_line(linewidth = 1) + # gráfico de líneas
  # especificar colores de la escala
  scale_color_gradient(low = "blue4", high = "red2")



## datos de delincuencia -----
# install.packages("arrow")
library(arrow)
library(lubridate)

# cargar datos (en formato arrow)
delinc <- arrow::read_parquet("datos/cead_delincuencia_chile.parquet")

# revisar tipos de delitos
delinc |> 
  distinct(delito) |> 
  print(n=Inf)


# gráfico de líneas de un delito específico
delinc |> 
  # procesar datos
  filter(delito == "Robos con violencia o intimidación") |> 
  group_by(fecha) |> 
  summarize(n = sum(delito_n)) |> 
  # gráfico
  ggplot() +
  aes(fecha, n) +
  # podemos agregar más de una capa de geometría
  geom_line() +
  geom_point()
# geom_col()


# gráfico de barras vertical
delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  group_by(region) |> 
  summarize(n = sum(delito_n)) |> 
  ggplot() +
  aes(region, n) +
  geom_col() +
  # modificamos el tema del gráfico para que el texto del eje horizontal sea inclinado y justificado a la derecha
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### gráfico de barras horizontal ----
delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  group_by(region) |> 
  summarize(n = sum(delito_n)) |> 
  ggplot() +
  aes(n, region) +
  geom_col()


library(scales) # para mejorar las escalas
library(forcats) # para ordenar variables (tipo factor)

# gráfico de barras horizontal, ordenadas, y con saltos de línea en las etiquetas
delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  group_by(region) |> 
  summarize(n = sum(delito_n), .groups = "drop") |> 
  ungroup() |> 
  # reordenar las barras 
  mutate(region = fct_reorder(region, n)) |>
  ggplot() +
  aes(n, region) +
  geom_col(width = 0.5) + # ancho de las barras
  # modificar escala horizontal
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), # arreglar los miles
                     expand = expansion(c(0, 0.1)) # agregar espacio extra al final de la escala
                     ) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30) # aplicar salto de línea a las etiquetas a los 30 caracteres
                   )



### gráfico de barras horizontal con color ----

# contar delitos por año, para elegir delitos similares
delinc |> 
  group_by(delito) |> 
  summarize(n = sum(delito_n)) |> 
  arrange(desc(n))

# gráfico de barras horizontal con leyenda en dos columnas
delinc |> 
  # seleccionar delitos a incluir
  filter(delito %in% c("Robos con violencia o intimidación",
                       "Violencia intrafamiliar a mujer",
                       "Robo de objetos de o desde vehículo")) |> 
  group_by(region, delito) |> 
  summarize(n = sum(delito_n)) |> 
  ungroup() |> 
  # reordenar las barras
  mutate(region = fct_reorder(region, n)) |>
  # grafico
  ggplot() +
  aes(n, region, 
      fill = delito) + # especificar color
  geom_col(width = 0.5) +
  # escalas
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), 
                     expand = expansion(c(0, 0.1))) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30)) + 
  # modificar leyenda
  guides(fill = guide_legend(position = "bottom", # leyenda abajo
                             ncol = 2)) # en dos columnas


library(stringr) # para trabajar con texto

# gráfico de barras horizontal con leyenda en una fila
delinc |> 
  filter(delito %in% c("Robos con violencia o intimidación",
                       "Violencia intrafamiliar a mujer",
                       "Robo de objetos de o desde vehículo")) |> 
  group_by(region, delito) |> 
  summarize(n = sum(delito_n)) |> 
  ungroup() |> 
  # reordenar las barras
  mutate(region = fct_reorder(region, n)) |>
  mutate(delito = stringr::str_wrap(delito, 25)) |> # arreglar textos de leyenda muy largos
  # grafico
  ggplot() +
  aes(n, region, fill = delito) +
  geom_col(width = 0.5) +
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."),
                     expand = expansion(c(0, 0.1))) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30)) +
  # modificar leyenda
  guides(fill = guide_legend(position = "bottom", nrow = 1)) # en una columna


### gráfico de barras comparando categorías ----
library(lubridate)

# procesar datos primero
delinc_tabla <- delinc |> 
  filter(lubridate::year(fecha) == 2023) |> 
  filter(delito %in% c("Robos con violencia o intimidación",
                       "Violencia intrafamiliar a mujer",
                       "Robo de objetos de o desde vehículo")) |> 
  group_by(region, delito) |> 
  summarize(n = sum(delito_n)) |> 
  ungroup() |> 
  # reordenar las barras
  mutate(region = fct_reorder(region, n)) |>
  mutate(delito = str_wrap(delito, 25))

# hacer gráfico pero guardándolo como objeto
grafico <- delinc_tabla |> 
  ggplot() +
  aes(n, region, 
      fill = delito) + # relleno
  geom_col(width = 0.9, 
           position = position_dodge(), # especificar que las categorías aparezcan lado a lado
           color = "white", linewidth = 0.5) +
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), 
                     expand = expansion(c(0, 0.1))) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30)) +
  # modificar leyenda
  guides(fill = guide_legend(position = "bottom", nrow = 1)) +
  # título
  labs(title = "Delitos",
       subtitle = "Cantidad de delitos en el año 2023")

# ver el gráfico
grafico

### temas ----

# aplicar un tema al gráfico ya creado
grafico +
  theme_minimal()

grafico +
  theme_classic()

# modificar elementos del tema del gráfico
grafico_2 <- grafico +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 11, lineheight = 0.8)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, b = -10)))

grafico_2

# agregar una línea horizontal
grafico_2 +
  geom_vline(xintercept = 30000, linetype = "dashed", alpha = .7)

# calcular promedio
promedio <- mean(delinc_tabla$n)

# línea horizontal del promedio
grafico_2 +
  geom_vline(xintercept = promedio, linetype = "solid", color = "red3", linewidth = 1, alpha = .7)


### gráfico de barras apiladas ----
# calcular datos
delinc_tabla_2 <- delinc |> 
  filter(year(fecha) == 2023) |> 
  filter(delito %in% c("Robos con violencia o intimidación",
                       "Violencia intrafamiliar a mujer",
                       "Robo de objetos de o desde vehículo")) |> 
  group_by(region, delito) |> 
  summarize(n = sum(delito_n)) |> 
  ungroup() |> 
  # reordenar las barras
  mutate(region = fct_reorder(region, n)) |>
  mutate(delito = str_wrap(delito, 25))

# gráfico
grafico_apilado <- delinc_tabla_2 |> 
  ggplot() +
  aes(n, region, fill = delito) +
  geom_col(width = 0.5) +
  # escalas
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), # arreglar los miles
                     expand = expansion(c(0, 0.1))) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30)) +
  # modificar leyenda
  guides(fill = guide_legend(position = "bottom", nrow = 1))

grafico_apilado

# agregar tema
grafico_apilado +
  theme_classic()

# agregar texto 
grafico_apilado +
  theme_classic() +
  geom_text(
    aes(label = n),
    position = position_stack(0.5), # especificar que aparezca al medio de las barras
    size = 3)

# corregir texto para que solo aparezca para cantidades mayores a x usando ifelse()
grafico_apilado +
  theme_classic() +
  geom_text(
    aes(label = ifelse(n > 8000, comma(n, big.mark = "."), "")),
    position = position_stack(0.5),
    size = 3)

# otra forma de lograr lo mismo, filtrando los datos que llegan a geom_text()
grafico_apilado +
  theme_classic() +
  geom_text(data = ~filter(.x, n > 5000), # especificar los datos de geom_text()
    aes(label = n),
    position = position_stack(0.5),
    size = 3)

# calcular totales regionales
delinc_tabla_totales <- delinc_tabla_2 |> 
  group_by(region) |> 
  summarise(n = sum(n))

delinc_tabla_totales

# agregar texto de totales regionales
grafico_apilado_3 <- grafico_apilado +
  theme_classic() +
  geom_text(data = ~filter(.x, n > 5000),
            aes(label = comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3) +
  geom_text(data = delinc_tabla_totales,
            inherit.aes = F, # que esta capa no herede las estéticas globales (aes) del gráfico
            # especificar la estética de esta capa
            aes(x = n, y = region, label = comma(n, big.mark = ".")), 
            hjust = 0, size = 3, nudge_x = 800, 
            fontface = "bold")

# ver
grafico_apilado_3

# modificar escala de colores
grafico_apilado_4 <- grafico_apilado_3 +
  scale_fill_manual(values = c("#D74967", "#B384C0", "#F47066")) +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(linewidth = 0.2))

# ver
grafico_apilado_4


# tipografías ----
library(showtext)

# descargar una tipografía desde google fonts
font_add_google(name = "Montserrat")
# font_add_google(name = "Nunito")

# activar el uso de tipografías
showtext_auto()
showtext_opts(dpi = 300) # resolución para que se vean bien las tipografías

# cambiar tipografía global
grafico_apilado_4 +
  theme_classic(base_family = "Montserrat")


# guardar gráfico ----
ggsave("grafico.jpg", width = 10, height = 6, dpi = 300) 
