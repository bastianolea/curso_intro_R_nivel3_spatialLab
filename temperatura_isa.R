library(tidyverse)

datos_1 <- readxl::read_excel("/Users/baolea/Downloads/Datos Histograma.xlsx") |>
  janitor::clean_names() |>
mutate(tmax = replace_na(tmax, 17.8)) |> 
  select(-tmedia)

datos_2 <- readxl::read_excel("/Users/baolea/Downloads/Datos Histograma-2.xlsx") |>
  janitor::clean_names() #|>
  # mutate(tmax = replace_na(tmax, 17.8))


# datos |>
#   ggplot(aes(x = dia, y = pp_maxima)) +
#   geom_col(width = .3, fill = "#32a4a7", alpha = .7,
#            color = "#32a4a7")+
#   geom_line(aes(y = t_minima), color = "white", linewidth = 1.9, alpha = .8) +
#   # geom_line(aes(y = tmedia), color = "white", linewidth = 1.9, alpha = .8) +
#   geom_line(aes(y = tmax), color = "white", linewidth = 1.9, alpha = .8) +
#   geom_line(aes(y = t_minima), color = "#008ab8", linewidth = 1.3, alpha = .8) +
#   # geom_line(aes(y = tmedia), color = "#6ada36", linewidth = 1.3, alpha = .8) +
#   geom_line(aes(y = tmax), color = "#cc330a", linewidth = 1.3, alpha = .8) +
#   geom_point(aes(y = t_minima), color = "white", size = 4, alpha = .6) +
#   # geom_point(aes(y = tmedia), color = "white", size = 4, alpha = .6) +
#   geom_point(aes(y = tmax), color = "white", size = 4, alpha = .6) +
#   geom_point(aes(y = t_minima), color = "#008ab8", size = 3) +
#   # geom_point(aes(y = tmedia), color = "#6ada36", size = 3) +
#   geom_point(aes(y = tmax), color = "#cc330a", size = 3) +
#   geom_text(aes(label = pp_maxima), vjust = 1,
#             nudge_y = 1.5, size = 3.5) +
#   #
#   shadowtext::geom_shadowtext(aes(label = tmax, y = tmax), vjust = 1,
#             nudge_y = 1.8, size = 2.5, color = "#cc330a", bg.colour = "white", bg.r = .15) +
#   shadowtext::geom_shadowtext(aes(label = t_minima, y = t_minima), vjust = 0,
#             nudge_y = -1.8, size = 2.5, color = "#008ab8", bg.colour = "white", bg.r = .15) +
#   scale_y_continuous(name = "Precipitación (mm)",
#                      expand = expansion(c(0, 0.1)),
#                      sec.axis = sec_axis(name = "Temperatura (°C)",
#                                          transform = ~.x,
#                                          breaks = seq(0, 40, by = 5))) +
#   theme_classic() +
#   labs(x = "Días")

# para grafico 2
espaciado_y = 3
mult = 10 

# para grafico 1
espaciado_y = 1
mult = 1.8

datos_2 |>
  pivot_longer(cols = 2:4, names_to = "variable", values_to = "valor") |> 
  ggplot() +
  aes(x = dia, y = valor) +
  geom_col(data = ~filter(.x, variable == "pp_maxima"),
           aes(),
           width = .3, fill = "#32a4a7", alpha = .7,
           color = "#32a4a7") +
  geom_text(data = ~filter(.x, variable == "pp_maxima"),
            aes(label = valor), vjust = 1,
            nudge_y = espaciado_y*2, size = 3.5) +
  geom_line(data = ~filter(.x, variable != "pp_maxima"),
            aes(y = valor * mult, group = variable), 
            color = "white", linewidth = 1.5, lineend = "round") +
  geom_line(data = ~filter(.x, variable != "pp_maxima"),
            aes(y = valor * mult, color = variable), 
            linewidth = 0.7, lineend = "round") +
  # geom_point(data = ~filter(.x, variable != "pp_maxima"),
  #            aes(y = valor * mult), color = "white", size = 4, alpha = .6) +
  # geom_point(data = ~filter(.x, variable != "pp_maxima"),
  #            aes(y = valor * mult, color = variable), size = 3) +
  scale_color_manual(values = c("#008ab8", "#cc330a")) +
  shadowtext::geom_shadowtext(data = ~filter(.x, variable == "t_minima"),
                              aes(y = valor*mult, label = valor, color = variable), vjust = 1,
                              nudge_y = -espaciado_y, size = 2.5, bg.colour = "white", bg.r = .12) +
  shadowtext::geom_shadowtext(data = ~filter(.x, variable == "tmax"),
                              aes(y = valor*mult, label = valor, color = variable), vjust = 0,
                              nudge_y = espaciado_y, size = 2.5, bg.colour = "white", bg.r = .15) +
  scale_y_continuous(name = "Precipitación (mm)",
                     expand = expansion(c(0, 0.1)),
                     sec.axis = sec_axis(name = "Temperatura (°C)",
                                         transform = ~.x/mult,
                                         breaks = seq(0, 40, by = 5))) +
  theme_classic() +
  labs(x = "Días") +
  theme(legend.position = "none")

ggsave(filename = "~/Desktop/grafico_2.png", width = 5, height = 5, scale = 1.1)
