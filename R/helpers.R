# ============================================================
# helpers.R
# Funciones auxiliares para reproducir en R (tidyverse/ggplot2)
# el análisis EDA univariado y bivariado originalmente hecho en
# Python (pandas / matplotlib / seaborn / scipy).
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)

# Genera una paleta de n colores a partir de un mapa tipo viridis.
# Sirve de equivalente a las paletas 'viridis', 'crest', 'mako' y
# 'rocket' de seaborn usadas en el notebook original.
paleta_viridis <- function(n, option = "viridis") {
  opt <- switch(option,
    "viridis" = "D", "crest" = "mako", "mako" = "mako",
    "rocket" = "rocket", option
  )
  viridisLite::viridis(n, option = opt)
}

# ------------------------------------------------------------
# Carga y preparación de datos (equivalente al bloque de
# renombrado de columnas usado en ambos notebooks de Python)
# ------------------------------------------------------------
cargar_datos <- function(path) {
  df <- read.csv(path, sep = ",", stringsAsFactors = FALSE)

  nombres_nuevos <- c(
    Diabetes_binary      = "Diabetes",
    HighBP                = "PA_alta",
    HighChol              = "col_alto",
    CholCheck             = "CheckCol",
    BMI                   = "IMC",
    Smoker                = "Fumador",
    Stroke                = "derrame",
    HeartDiseaseorAttack  = "EnfCardi",
    PhysActivity           = "ActFisi",
    Fruits                 = "fruta",
    Veggies                = "vegetales",
    HvyAlcoholConsump       = "Alcohlico",
    AnyHealthcare           = "eps",
    NoDocbcCost              = "AfordabilidadMedica",
    GenHlth                  = "SaludG",
    MentHlth                 = "SaludM",
    PhysHlth                 = "SaludF",
    DiffWalk                 = "DificultadCaminar",
    Sex                       = "Genero",
    Age                       = "Edad",
    Education                 = "Educacion",
    Income                    = "Ingresos"
  )

  # Renombrar solo las columnas presentes en el archivo
  presentes <- intersect(names(nombres_nuevos), names(df))
  names(df)[match(presentes, names(df))] <- nombres_nuevos[presentes]

  orden_columnas <- c(
    "Diabetes", "PA_alta", "col_alto", "CheckCol",
    "IMC", "Fumador", "derrame", "EnfCardi", "ActFisi",
    "fruta", "vegetales", "Alcohlico", "eps", "AfordabilidadMedica",
    "SaludG", "SaludM", "SaludF", "DificultadCaminar", "Genero",
    "Edad", "Educacion", "Ingresos"
  )
  df <- df[, orden_columnas]
  df <- df[!duplicated(df), ]
  df
}

# ------------------------------------------------------------
# UNIVARIADO — Categóricas
# ------------------------------------------------------------

# Tabla de frecuencias (equivalente a value_counts + %)
# sort_by_index = TRUE reproduce el .sort_index() usado en
# Python para SaludG, Genero, Edad, Educacion e Ingresos.
tabla_frecuencias <- function(df, var, sort_by_index = FALSE) {
  tab <- df %>%
    count(.data[[var]], name = "Frecuencia")

  if (sort_by_index) {
    tab <- tab %>% arrange(.data[[var]])
  } else {
    tab <- tab %>% arrange(desc(Frecuencia))
  }

  tab %>% mutate(Porcentaje = round(Frecuencia / sum(Frecuencia) * 100, 1))
}

mostrar_tabla <- function(tab) {
  kable(tab, format = "html") %>%
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
}

# Gráfico de barras vertical (equivalente a sns.countplot / sns.barplot)
grafico_barras <- function(tab, var, orden = NULL, etiquetas = NULL,
                            colores = NULL, titulo = "", xlab = "", ylab = "Frecuencia",
                            mostrar_pct = TRUE, angulo_x = 0) {
  tab[[var]] <- factor(tab[[var]], levels = if (!is.null(orden)) orden else unique(tab[[var]]))

  p <- ggplot(tab, aes(x = .data[[var]], y = Frecuencia, fill = .data[[var]])) +
    geom_col(width = 0.7) +
    labs(title = titulo, x = xlab, y = ylab) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = angulo_x, hjust = if (angulo_x > 0) 1 else 0.5))

  if (!is.null(colores)) p <- p + scale_fill_manual(values = colores)

  if (mostrar_pct) {
    p <- p + geom_text(aes(label = paste0(comma(Frecuencia), "\n(", Porcentaje, "%)")),
                        vjust = -0.3, size = 3.2, fontface = "bold")
  } else {
    p <- p + geom_text(aes(label = comma(Frecuencia)), vjust = -0.3, size = 3.2)
  }

  if (!is.null(etiquetas)) p <- p + scale_x_discrete(labels = etiquetas)

  p + expand_limits(y = max(tab$Frecuencia) * 1.15)
}

# Gráfico de barras horizontal
grafico_barras_h <- function(tab, var, orden = NULL, etiquetas = NULL,
                              colores = NULL, titulo = "", xlab = "Frecuencia", ylab = "") {
  tab[[var]] <- factor(tab[[var]], levels = if (!is.null(orden)) orden else unique(tab[[var]]))

  p <- ggplot(tab, aes(x = .data[[var]], y = Frecuencia, fill = .data[[var]])) +
    geom_col(width = 0.65) +
    coord_flip() +
    labs(title = titulo, x = ylab, y = xlab) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5)) +
    geom_text(aes(label = paste0(comma(Frecuencia), " (", Porcentaje, "%)")),
              hjust = -0.1, size = 3.4, fontface = "bold")

  if (!is.null(colores)) p <- p + scale_fill_manual(values = colores)
  if (!is.null(etiquetas)) p <- p + scale_x_discrete(labels = etiquetas)

  p + expand_limits(y = max(tab$Frecuencia) * 1.2)
}

# Gráfico de pastel (equivalente a plt.pie)
grafico_pastel <- function(tab, var, etiquetas, colores, titulo = "", donut = FALSE) {
  tab$lab <- paste0(etiquetas, "\n", round(tab$Porcentaje, 1), "%")
  tab$ymax <- cumsum(tab$Frecuencia)
  tab$ymin <- c(0, head(tab$ymax, -1))
  tab$ylab_pos <- (tab$ymax + tab$ymin) / 2

  inner <- if (donut) 3 else 0

  ggplot(tab, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = inner, fill = factor(.data[[var]]))) +
    geom_rect(color = "white") +
    coord_polar(theta = "y") +
    xlim(c(if (donut) 2 else 0, 4)) +
    scale_fill_manual(values = colores) +
    geom_text(aes(x = if (donut) 3.5 else 2, y = ylab_pos, label = lab),
              size = 3.6, fontface = "bold", inherit.aes = TRUE) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5, size = 13)) +
    labs(title = titulo)
}

# Gráfico de piruleta (lollipop)
grafico_lollipop <- function(tab, var, etiquetas, colores, titulo = "",
                              xlab = "Frecuencia (Número de personas)", ylab = "Categoría") {
  tab$categoria <- factor(etiquetas, levels = etiquetas[order(tab$Frecuencia)])

  ggplot(tab, aes(x = Frecuencia, y = categoria)) +
    geom_segment(aes(x = 0, xend = Frecuencia, y = categoria, yend = categoria),
                 color = "#34495e", linewidth = 1.2, alpha = 0.7) +
    geom_point(aes(color = categoria), size = 6) +
    geom_text(aes(label = paste0(comma(Frecuencia), "\n(", Porcentaje, "%)")),
              hjust = -0.15, size = 3.3, fontface = "bold") +
    scale_color_manual(values = colores) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.3))) +
    labs(title = titulo, x = xlab, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
}

# ------------------------------------------------------------
# UNIVARIADO — Numéricas
# ------------------------------------------------------------

tabla_stats_numerica <- function(df, var) {
  x <- df[[var]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  data.frame(
    Estadistica = c("Promedio", "Rango Intercuartil (IQR)", "Mínimo", "Máximo"),
    Valor = c(round(mean(x, na.rm = TRUE), 2),
              paste0(round(q3 - q1, 2), " (Q1: ", q1, ", Q3: ", q3, ")"),
              min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  )
}

grafico_histograma <- function(df, var, color = "#3498db", bins = 40,
                                titulo = "", xlab = "", ylab = "Frecuencia (Número de personas)") {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, fill = color, alpha = 0.85, color = "white") +
    geom_density(color = "black", linewidth = 0.8) +
    labs(title = titulo, x = xlab, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

grafico_boxplot_simple <- function(df, var, color = "#e74c3c", titulo = "", xlab = "") {
  ggplot(df, aes(x = .data[[var]], y = "")) +
    geom_boxplot(fill = color, alpha = 0.6, outlier.alpha = 0.3, width = 0.4) +
    labs(title = titulo, x = xlab, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.text.y = element_blank())
}

# ------------------------------------------------------------
# BIVARIADO — Numéricas (Diabetes vs variable numérica)
# ------------------------------------------------------------

bivar_boxplot <- function(df, target, var, etiquetas_grupo, color = "#4C72B0", titulo = "") {
  df$grupo <- factor(df[[target]], labels = etiquetas_grupo)
  medias <- df %>% group_by(grupo) %>% summarise(media = mean(.data[[var]], na.rm = TRUE))

  ggplot(df, aes(x = grupo, y = .data[[var]])) +
    geom_boxplot(fill = color, alpha = 0.6, outlier.color = "gray40", outlier.alpha = 0.3, outlier.size = 1) +
    geom_point(data = medias, aes(x = grupo, y = media), color = "red", size = 3) +
    labs(title = titulo, x = "¿El paciente tiene diabetes?", y = var) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

bivar_resumen_numerico <- function(df, target, var) {
  resumen_grupo <- function(x) {
    data.frame(
      n = length(x), media = mean(x, na.rm = TRUE), ds = sd(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE), minimo = min(x, na.rm = TRUE), maximo = max(x, na.rm = TRUE),
      Q1 = quantile(x, 0.25, na.rm = TRUE), Q3 = quantile(x, 0.75, na.rm = TRUE)
    )
  }
  por_grupo <- df %>%
    group_by(.data[[target]]) %>%
    summarise(resumen_grupo(.data[[var]]), .groups = "drop") %>%
    mutate(IQR = Q3 - Q1) %>%
    # Convertimos la columna target a texto para poder combinarla (bind_rows)
    # con la fila "TOTAL", que también es texto. Sin esto, R no puede apilar
    # una columna numerica (0/1) con una columna de texto ("TOTAL").
    mutate(!!target := as.character(.data[[target]]))
  
  total <- resumen_grupo(df[[var]]) %>%
    mutate(IQR = Q3 - Q1, !!target := "TOTAL") %>%
    select(all_of(target), everything())
  
  bind_rows(por_grupo, total) %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
}

# Prueba de normalidad Anderson-Darling por grupo (requiere paquete nortest)
prueba_anderson_darling <- function(df, target, var, etiquetas_grupo) {
  niveles <- sort(unique(df[[target]]))
  for (i in seq_along(niveles)) {
    grupo <- df[[var]][df[[target]] == niveles[i]]
    grupo <- grupo[!is.na(grupo)]
    res <- nortest::ad.test(grupo)
    cat("Anderson-Darling:", etiquetas_grupo[i], "\n")
    cat(sprintf("Estadístico: %.4f\n", res$statistic))
    cat(sprintf("p-valor: %.4e\n", res$p.value))
    cat("Resultado:", ifelse(res$p.value < 0.05, "Rechaza H0 (no normal)", "No rechaza H0"), "\n\n")
  }
}

# Prueba de igualdad de varianzas de Levene (requiere paquete car)
prueba_levene <- function(df, target, var) {
  formula_txt <- as.formula(paste0(var, " ~ factor(", target, ")"))
  res <- car::leveneTest(formula_txt, data = df)
  cat(sprintf("Estadístico de Levene: %.4f\n", res$`F value`[1]))
  cat(sprintf("p-valor: %.4e\n", res$`Pr(>F)`[1]))
  cat("Resultado:", ifelse(res$`Pr(>F)`[1] < 0.05,
                            "Rechaza H0 (varianzas distintas)",
                            "No rechaza H0 (varianzas iguales)"), "\n")
}

# Prueba de Mann-Whitney (Wilcoxon rank-sum)
prueba_mann_whitney <- function(df, target, var) {
  niveles <- sort(unique(df[[target]]))
  g0 <- df[[var]][df[[target]] == niveles[1]]
  g1 <- df[[var]][df[[target]] == niveles[2]]
  res <- wilcox.test(g1, g0, alternative = "two.sided", exact = FALSE)
  cat("Mann-Whitney U (Wilcoxon rank-sum)\n")
  cat(sprintf("Estadístico W: %.4f\n", res$statistic))
  cat(sprintf("p-valor: %.6e\n", res$p.value))
  cat("Resultado:", ifelse(res$p.value < 0.05,
                            "Rechaza H0 (distribuciones distintas)",
                            "No rechaza H0"), "\n")
}

# ------------------------------------------------------------
# BIVARIADO — Categóricas (Diabetes vs variable categórica)
# ------------------------------------------------------------

bivar_tabla_frecuencias <- function(df, target, var) {
  df %>%
    group_by(.data[[target]], .data[[var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[target]]) %>%
    mutate(porcentaje = round(n / sum(n) * 100, 2)) %>%
    ungroup() %>%
    arrange(.data[[target]], desc(n)) %>%
    rename(categoria = !!var)
}

bivar_grafico_barras <- function(tab, target, etiquetas_grupo, colores = c("#4C72B0", "#DD8452"), titulo = "", xlab = "") {
  tab$grupo <- factor(tab[[target]], labels = etiquetas_grupo)

  ggplot(tab, aes(x = factor(categoria), y = n, fill = grupo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = paste0(comma(n), " (", porcentaje, "%)")),
              position = position_dodge(width = 0.8), vjust = -0.3, size = 2.9) +
    scale_fill_manual(values = colores, name = "Grupo") +
    scale_y_continuous(labels = comma) +
    labs(title = titulo, x = xlab, y = "Cantidad de pacientes") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

prueba_chi2 <- function(df, target, var) {
  tabla <- table(df[[var]], df[[target]])
  res <- chisq.test(tabla, correct = FALSE)
  cat(sprintf("Estadístico X²: %.4f\n", res$statistic))
  cat(sprintf("p-valor: %.4e\n", res$p.value))
  cat(sprintf("Grados de libertad: %d\n", res$parameter))
  cat("Resultado:", ifelse(res$p.value < 0.05,
                            "Rechaza H0 (dependientes)",
                            "No rechaza H0 (independientes)"), "\n")
}
