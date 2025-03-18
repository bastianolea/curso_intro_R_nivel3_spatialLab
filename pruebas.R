library(dplyr)

library(ggplot2)

msleep |> 
  ggplot() +
  aes(awake, brainwt) +
  geom_point()








# líneas ----

## temperatura ----

temp <- readr::read_csv2("temperaturas_chile_unificadas.csv")

distinct(temp, nombre)

temp |> 
  filter(nombre == "Quinta Normal, Santiago") |> 
  mutate(fecha = lubridate::floor_date(fecha, "month")) |> 
  group_by(fecha) |> 
  summarize(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(fecha, t_max, color = t_max) +
  geom_line()

## delincuencia ----
delinc <- arrow::read_parquet("datos/cead_delincuencia_chile.parquet")

distinct(delinc, delito) |> print(n=Inf)

delinc |> 
  filter(delito == "Robos con violencia o intimidación") |> 
  filter(region == "Metropolitana de Santiago") |> 
  group_by(fecha) |> 
  summarize(n = sum(delito_n)) |> 
  ggplot() +
  aes(fecha, n) +
  geom_line()



# puntos ----

## temperatura ----
temp |> 
  filter(nombre == "Chacalluta, Arica Ap.") |>
  # filter(nombre == "Quinta Normal, Santiago") |>
  group_by(año, mes) |> 
  summarize(t_min = mean(t_min, na.rm = T),
            t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(t_min, t_max, color = año) +
  geom_point(size = 3, alpha = 0.3)


## diamantes ----
diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = cut, size = carat) +
  geom_point()


# paletas ----

diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  scale_color_gradient(low = "purple4", high = "red3")


diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  scale_color_gradient2(low = "purple4", mid = "cyan3", high = "deeppink", midpoint = 6.5)


### viridis ----
library(viridis)
diamonds |> 
  slice_sample(n = 1000) |>
  ggplot() +
  aes(price, carat, color = y, size = carat) +
  geom_point() +
  scale_color_viridis(option = "plasma")


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


library(RColorBrewer)
display.brewer.all()




# temas ----
# install.packages("ggthemes")
library(ggthemes)



# otros ----

## temperatura ----
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
