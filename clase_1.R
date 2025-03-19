
# install.packages("ggplot2")

library(ggplot2)

ggplot(data = iris)

# dispersión ----
iris |> 
  ggplot() +
  aes(x = Sepal.Length, y = Sepal.Width) +
  geom_point()

iris |> 
  ggplot() +
  aes(x = Sepal.Length, y = Sepal.Width, color = Species) +
  geom_point()

iris |> 
  ggplot() +
  aes(x = Sepal.Length, 
      y = Sepal.Width, 
      # color = Species,
      size = Petal.Length) +
  geom_point(color = "purple2", alpha = 0.5)


# histograma
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram()

iris |> 
  ggplot() +
  aes(x = Sepal.Length, fill = Species) +
  geom_histogram()

# densidad
iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_density()

iris |> 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_density(fill = "black", alpha = 0.6)

iris |> 
  ggplot() +
  aes(x = Sepal.Length, fill = Species, color = Species) +
  geom_density(alpha = 0.6)


# temperaturas ----
library(dplyr)

library(readr)

temp <- read_csv2("datos/temperaturas_chile_unificadas.csv")

temp |> 
  distinct(nombre) |> 
  print(n=Inf)

temp |> 
  count(año) |> 
  print(n=Inf)

temp |> 
  filter(nombre == "Chacalluta, Arica Ap.") |> 
  ggplot() +
  aes(fecha) +
  geom_histogram()

temp |> 
  filter(nombre == "Chacalluta, Arica Ap.") |> 
  ggplot() +
  aes(t_min, t_max, color = año) +
  geom_point()


temp |> 
  filter(nombre == "Chacalluta, Arica Ap.") |> 
  group_by(año, mes) |> 
  summarise(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(t_min, t_max, color = año) +
  geom_point()


temp |> glimpse()

library(lubridate)

temp |> 
  filter(nombre == "Chacalluta, Arica Ap.") |>
  # filter(nombre == "Quinta Normal, Santiago") |>
  mutate(fecha = floor_date(fecha, "month")) |> 
  group_by(fecha) |> 
  summarise(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(x = fecha, y = t_max, color = t_max) +
  geom_line(linewidth = 0.8) +
  scale_color_gradient(low = "blue4", high = "red2")



# delincuencia -----
# install.packages("arrow")
library(arrow)
library(lubridate)

delinc <- arrow::read_parquet("datos/cead_delincuencia_chile.parquet")

delinc |> 
  distinct(delito) |> 
  print(n=Inf)

delinc |> 
  group_by(delito) |> 
  summarize(n = sum(delito_n)) |> 
  arrange(desc(n))


delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  group_by(fecha) |> 
  summarize(n = sum(delito_n)) |> 
  ggplot() +
  aes(fecha, n) +
  geom_line() +
  geom_point()
# geom_col()


delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  group_by(region) |> 
  summarize(n = sum(delito_n)) |> 
  ggplot() +
  aes(n, region) +
  geom_col()

options(scipen = 9999)

library(scales)
library(forcats)

orden_regiones <- tribble(~codigo_region, ~orden_region,
                          15, 1,
                          1, 2,
                          2, 3,
                          3, 4,
                          4, 5,
                          5, 6,
                          13, 7,
                          6, 8,
                          7, 9,
                          16, 10,
                          8, 11,
                          9, 12,
                          14, 13,
                          10, 14,
                          11, 15,
                          12, 16)

delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  left_join(orden_regiones, 
            by = join_by(cut_region == codigo_region)) |> 
  group_by(region, orden_region) |> 
  summarize(n = sum(delito_n), .groups = "drop") |> 
  ungroup() |> 
  # reordenar las barras
  # mutate(region = fct_reorder(region, n)) |> 
  mutate(region = fct_reorder(region, orden_region, .desc = T)) |>
  ggplot() +
  aes(n, region) +
  geom_col(width = 0.5) +
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), # arreglar los miles
                     expand = expansion(c(0, 0.1))
  ) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30))




delinc |> 
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
  aes(n, region, fill = delito) +
  geom_col(width = 0.5) +
  # escalas
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), # arreglar los miles
                     expand = expansion(c(0, 0.1))) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30)) +
  # modificar leyenda
  guides(fill = guide_legend(position = "bottom", ncol = 2))


library(stringr)

delinc |> 
  filter(delito %in% c("Robos con violencia o intimidación",
                       "Violencia intrafamiliar a mujer",
                       "Robo de objetos de o desde vehículo")) |> 
  group_by(region, delito) |> 
  summarize(n = sum(delito_n)) |> 
  ungroup() |> 
  # reordenar las barras
  mutate(region = fct_reorder(region, n)) |>
  mutate(delito = str_wrap(delito, 25)) |> 
  # grafico
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



delinc_tabla <- delinc |> 
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


grafico <- delinc_tabla |> 
  ggplot() +
  aes(n, region, fill = delito) +
  geom_col(width = 0.9, position = position_dodge(width = 0.9),
           color = "white", linewidth = 0.5) +
  # escalas
  scale_x_continuous(name = "Delitos",
                     labels = scales::label_comma(big.mark = "."), # arreglar los miles
                     expand = expansion(c(0, 0.1))) +
  scale_y_discrete(name = "Regiones",
                   labels = label_wrap(30)) +
  # modificar leyenda
  guides(fill = guide_legend(position = "bottom", nrow = 1)) +
  labs(title = "Delitos",
       subtitle = "Cantidad de delitos en el año 2023")

grafico +
  theme_minimal()

grafico +
  theme_classic()

grafico2 <- grafico +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(colour = "black", 
                                   face = "bold", 
                                   size = 11, lineheight = 0.8)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, b = -10)))


grafico2 +
  geom_vline(xintercept = 30000, linetype = "dashed", alpha = .7)


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


grafico_apilado +
  theme_classic() +
  geom_text(
    aes(label = ifelse(n > 8000, comma(n, big.mark = "."), "")),
    position = position_stack(0.5),
    size = 3)

grafico_apilado +
  theme_classic() +
  geom_text(data = ~filter(.x, n > 5000),
    aes(label = n),
    position = position_stack(0.5),
    size = 3)


delinc_tabla_totales <- delinc_tabla_2 |> 
  group_by(region) |> 
  summarise(n = sum(n))


delinc_tabla_totales


grafico_apilado_3 <- grafico_apilado +
  theme_classic() +
  geom_text(data = ~filter(.x, n > 5000),
            aes(label = comma(n, big.mark = ".")),
            position = position_stack(0.5),
            size = 3) +
  geom_text(data = delinc_tabla_totales,
            aes(x = n, y = region, label = comma(n, big.mark = ".")), 
            inherit.aes = F, 
            hjust = 0, size = 3, nudge_x = 800, 
            fontface = "bold")

grafico_apilado_4 <- grafico_apilado_3 +
  scale_fill_manual(values = c("#D74967", "#B384C0", "#F47066")) +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(linewidth = 0.2))


grafico_apilado_4


# library(showtext)
# font_add_google("Cabin")
# 
# showtext_auto()
# showtext_end()
# 
# grafico_apilado_4 +
#   theme_classic(base_size = 20)
#   
# 

library(ggview)

grafico_apilado_4 +
  canvas(width = 10, height = 6)

ggsave("grafico.jpg", width = 10, height = 6) 
