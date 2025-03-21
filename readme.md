[![](logo_spatialLab.png)](https://spatiallab.cl)

# Introducción al análisis de datos con R para principiantes
## Nivel 3: Visualización de datos

_18 y 20 de marzo, 2025._

En este repositorio se subirán los contenidos que veremos clase a clase

Docente: _Bastián Olea Herrera._ baolea@uc.cl


## Contenidos

### Clase 1: gráficos {ggplot2}
- Introducción a {ggplot2}
- Gráficos básicos
  - Dispersión
  - Histograma
  - Densidad
- Gráficos con datos de temperatura
  - Dispersión
  - Líneas
- Gráficos con datos de delincuencia
  - Barras verticales
  - Barras horizontales
    - Barras comparativas
  - Temas y personalización
- Cambiar tipografías

### Clase 2: gráficos {ggplot2} y tablas {gt}
- Gráficos
  - Tipos de gráficos
    - Boxplot
    - Dispersión (jitter)
    - Violin
    - Tortas
    - Donas
    - Mosaico
  - Facetas (dividir datos en multiples gráficos)
  - Guardar gráficos en lote mediante un loop
- Tablas con {gt}
  - Color en columans según datos
  - Estilos condicionales según datos
  - Símbolos en representación de datos
  - Personalización de tablas


----

## Recursos de {ggplot2}

** Paletas de colores:**
- https://datavizf24.classes.andrewheiss.com/resource/colors.html 
- https://bastianolea.rbind.io/blog/colores/
- viridis: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html#the-color-scales
- colorbrewer: https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=3
- scico: https://www.data-imaginist.com/posts/2018-05-30-scico-and-the-colour-conundrum/ 

**Temas:**
- temas ggplot: https://ggplot2.tidyverse.org/reference/ggtheme.html
- ggthemes: https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/

**Elementos de un gráfico** (para modificar temas):
* https://isabella-b.com/blog/ggplot2-theme-elements-reference/
* https://henrywang.nl/ggplot2-theme-elements-demonstration/
* https://ggplot2.tidyverse.org/reference/theme


## Recursos de {gt}
- Documentación del paquete: https://gt.rstudio.com/index.html
- Tutorial inicial recomendado: https://themockup.blog/static/resources/gt-cookbook.html
- Tutorial avanzado: https://themockup.blog/static/resources/gt-cookbook-advanced.html
- Tutorial para principiantes: https://bastianolea.rbind.io/blog/tutorial_gt/
- Temas de {gt}: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/
- Nombres de los elementos y bordes de las tablas: https://adrumm.quarto.pub/basic-gt-styling-reference/


## Datos 
- Datos de pueblos indígenas en Chile https://github.com/bastianolea/pueblos_indigenas_chile
- Datos de delincuencia en Chile https://github.com/bastianolea/delincuencia_chile
- Datos de temperaturas históricas en Chile https://github.com/bastianolea/temperaturas_chile