# EDA Diabetes — Libro en R (bookdown)

Traducción a **R** (tidyverse / ggplot2) del análisis exploratorio univariado y bivariado del conjunto de datos de indicadores de salud y diabetes (BRFSS 2015), originalmente hecho en Python. Se publica como sitio web con [bookdown](https://bookdown.org/).

## Estructura

```
.
├── index.Rmd              # Portada + carga de datos
├── 01-univariado.Rmd      # Capítulo 1: Análisis Univariado
├── 02-bivariado.Rmd       # Capítulo 2: Análisis Bivariado
├── R/helpers.R            # Funciones auxiliares (tablas, gráficos, pruebas estadísticas)
├── data/                  # Coloca aquí tu archivo diabetes.csv (no se incluye)
├── _bookdown.yml          # Orden de capítulos
├── _output.yml            # Configuración del tema del sitio
├── style.css
└── .github/workflows/
    └── deploy.yml          # Publica automáticamente en GitHub Pages
```

## 1. Instalar R y RStudio (si no los tienes)

- Descarga R: https://cran.r-project.org/
- Descarga RStudio Desktop: https://posit.co/download/rstudio-desktop/

## 2. Instalar los paquetes necesarios

Abre R o RStudio y ejecuta:

```r
install.packages(c(
  "bookdown", "dplyr", "ggplot2", "scales", "knitr",
  "kableExtra", "viridisLite", "nortest", "car"
))
```

## 3. Colocar el dataset

Copia tu archivo de datos (el mismo CSV que usaste en Python:
`diabetes_binary_5050split_health_indicators_BRFSS2015.csv`) dentro de la carpeta `data/` y renómbralo a `diabetes.csv`:

```
data/diabetes.csv
```

Si prefieres otro nombre o ubicación, cambia la ruta en `index.Rmd`, en la línea:

```r
df <- cargar_datos("data/diabetes.csv")
```

## 4. Construir el libro localmente

En la consola de R, con el directorio de trabajo en la carpeta del proyecto:

```r
bookdown::render_book("index.Rmd", output_dir = "_book")
```

Esto genera el sitio en `_book/`. Ábrelo con:

```r
browseURL("_book/index.html")
```

o simplemente abre `_book/index.html` en tu navegador.

## 5. Publicar en GitHub Pages

1. Crea un repositorio en GitHub (puede ser uno nuevo, distinto al del libro en Python).
2. Sube esta carpeta completa a la rama `main`:

   ```bash
   git init
   git add .
   git commit -m "Libro EDA diabetes en R (bookdown)"
   git branch -M main
   git remote add origin https://github.com/TU_USUARIO/TU_REPO.git
   git push -u origin main
   ```

   **Nota:** `data/diabetes.csv` ya viene incluido en este proyecto (tu archivo real) y **sí se subirá** al repositorio junto con el resto de archivos, ya que `data/` no está en `.gitignore`. Si prefieres no subir el dataset (por privacidad o tamaño), agrega `data/` a tu `.gitignore` antes del primer commit — pero ten en cuenta que en ese caso el workflow de GitHub Actions no podrá construir el libro automáticamente, a menos que subas el CSV manualmente después.

3. Ve a **Settings → Pages** de tu repositorio y selecciona la rama **gh-pages**, carpeta `/ (root)`, como fuente (se crea sola tras el primer workflow exitoso).
4. El workflow `.github/workflows/deploy.yml` se ejecuta automáticamente en cada `push` a `main`: instala R y los paquetes, construye el libro con bookdown y publica el HTML resultante en `gh-pages`.
5. Tras unos minutos, tu libro estará disponible en:
   `https://TU_USUARIO.github.io/TU_REPO/`

## Notas sobre la traducción

- Todas las tablas de frecuencias, gráficos (barras, pastel, dona, piruleta, histograma, cajas y bigotes) y pruebas estadísticas (Anderson-Darling, Levene, Mann-Whitney, chi-cuadrado) fueron traducidos a sus equivalentes en R: `dplyr` para manipulación de datos, `ggplot2` para gráficos, y `nortest`/`car`/`stats` para las pruebas.
- Los gráficos de pastel y dona se implementaron con `ggplot2 + coord_polar`, ya que no son nativos de ggplot2 (a diferencia de matplotlib). El resultado visual es equivalente aunque no pixel-perfecto respecto al original en Python.
- Los textos interpretativos (los párrafos en español que explican cada gráfico o prueba) se mantuvieron iguales a los del notebook original, ya que se basan en los mismos datos y por lo tanto los resultados numéricos deberían coincidir al ejecutar el libro con tu dataset.
- El notebook bivariado original apuntaba a una ruta local de Windows; aquí se usa `data/diabetes.csv` como ruta estándar y portable.
