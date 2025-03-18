library(dplyr)

library(ggplot2)

msleep |> 
  ggplot() +
  aes(awake, brainwt) +
  geom_point()

iris |> 
  ggplot() +
  aes(Sepal.Length, Sepal.Width, color = Species) +
  geom_point() +
  scale_color_discrete() +
  coord_cartesian(xlim = c(4, 9)) +
  theme_minimal()

# barras

msleep |> count(order)

iris |> count(Species)

element

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


p <- diamonds |> 
  ggplot(aes(carat, price, shape = cut)) +
  geom_point() + 
  facet_wrap(~color)

p

p + theme(
  plot.background=element_rect(fill="#b3e2cd"),
  panel.background=element_rect(fill="#fdcdac"),
  panel.border=element_rect(fill=NA,color="#cbd5e8",size=3),
  legend.background=element_rect(fill="#f4cae4"),
  legend.box.background=element_rect(fill="#e6f5c9"),
  strip.background=element_rect(fill="#fff2ae")
)



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




# tipografías ----

library(showtext)
font_add_google(name = "Montserrat")
font_add_google(name = "Nunito")

showtext_auto()

iris |> 
  ggplot() +
  aes(Sepal.Length, Sepal.Width) +
  geom_text(aes(label = Petal.Width), 
            family = "Nunito",
            check_overlap = T)

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
