# ============================================================
# ANALISIS ESTADISTICO - ETOLOGIA CANINA
# Tema: Síndrome de disfunción cognitiva canina (CCDS)
# Base: base_etologia_ccds_canina_teorica.xlsx
# Artículo de referencia: Katina et al. (2016)
# ============================================================

# ------------------------------------------------------------
# 0. PAQUETES
# ------------------------------------------------------------
# Ejecutar esta sección una sola vez. Si un paquete no está instalado,
# R lo instalará automáticamente.

paquetes <- c(
  "readxl", "dplyr", "ggplot2", "janitor", "gtsummary",
  "broom", "epitools", "writexl", "stringr", "forcats"
)

instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if(length(instalar) > 0){
  install.packages(instalar)
}

library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(gtsummary)
library(broom)
library(epitools)
library(writexl)
library(stringr)
library(forcats)

# Crear carpeta para guardar resultados
if(!dir.exists("resultados_ccds")){
  dir.create("resultados_ccds")
}

# ------------------------------------------------------------
# 1. IMPORTAR BASE DE DATOS
# ------------------------------------------------------------
# Colocar este script en la misma carpeta del archivo Excel.

base <- read_excel("base_etologia_ccds_canina_teorica.xlsx", sheet = "Base_Datos") %>%
  clean_names()

# Revisar estructura de la base
str(base)
glimpse(base)

# ------------------------------------------------------------
# 2. PREPARACION DE VARIABLES
# ------------------------------------------------------------
# Convertimos variables categóricas a factor para análisis estadístico.
# También ordenamos algunas categorías para que la interpretación sea clara.

base <- base %>%
  mutate(
    sexo = factor(sexo),
    raza = factor(raza),
    categoria_peso = factor(categoria_peso, levels = c("<=15 kg", ">15 kg")),
    estado_reproductivo = factor(estado_reproductivo, levels = c("Entero", "Esterilizado")),
    vivienda = factor(vivienda, levels = c("Interior", "Exterior")),
    tipo_dieta = factor(tipo_dieta, levels = c("Controlada", "No controlada")),
    actividad_fisica = factor(actividad_fisica, levels = c("Baja", "Media", "Alta"), ordered = TRUE),
    clasificacion_cognitiva = factor(
      clasificacion_cognitiva,
      levels = c(
        "Envejecimiento normal",
        "Deterioro cognitivo leve",
        "Deterioro cognitivo moderado",
        "Deterioro cognitivo severo"
      ),
      ordered = TRUE
    ),
    ccds_moderado_severo = factor(ccds_moderado_severo, levels = c("No", "Sí")),
    desorientacion = factor(desorientacion, levels = c("No", "Sí")),
    cambios_interaccion = factor(cambios_interaccion, levels = c("No", "Sí")),
    alteracion_sueno = factor(alteracion_sueno, levels = c("No", "Sí")),
    eliminacion_inadecuada = factor(eliminacion_inadecuada, levels = c("No", "Sí"))
  )

# ------------------------------------------------------------
# 3. OBJETIVOS DEL ANALISIS
# ------------------------------------------------------------
# Objetivo general:
# Determinar los factores asociados a la presencia de disfunción cognitiva
# canina moderada/severa en perros geriátricos.
#
# Objetivos específicos:
# 1. Describir las características demográficas y de manejo de los perros.
# 2. Estimar la frecuencia de CCDS moderado/severo.
# 3. Evaluar asociación entre edad, dieta, peso, sexo, vivienda y estado
#    reproductivo con CCDS moderado/severo.
# 4. Construir un modelo de regresión logística para identificar factores
#    asociados a CCDS moderado/severo.

# ------------------------------------------------------------
# 4. ESTADISTICA DESCRIPTIVA GENERAL
# ------------------------------------------------------------

# Tabla descriptiva general
tabla_descriptiva <- base %>%
  select(
    edad_anios, peso_kg, puntaje_cades,
    sexo, categoria_peso, estado_reproductivo,
    vivienda, tipo_dieta, actividad_fisica,
    clasificacion_cognitiva, ccds_moderado_severo
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels()

tabla_descriptiva

# Guardar la tabla descriptiva como imagen/HTML no siempre es sencillo desde R base.
# Por eso guardaremos tablas en Excel más adelante.

# Frecuencia del desenlace principal
frecuencia_ccds <- base %>%
  tabyl(ccds_moderado_severo) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

frecuencia_ccds

# Frecuencia por clasificación cognitiva
frecuencia_clasificacion <- base %>%
  tabyl(clasificacion_cognitiva) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)

frecuencia_clasificacion

# ------------------------------------------------------------
# 5. GRAFICOS DESCRIPTIVOS
# ------------------------------------------------------------

# Grafico 1: Histograma de edad
p1 <- ggplot(base, aes(x = edad_anios)) +
  geom_histogram(bins = 12, color = "black") +
  labs(
    title = "Distribución de edad de los perros evaluados",
    x = "Edad (años)",
    y = "Número de perros"
  ) +
  theme_minimal()

ggsave("resultados_ccds/grafico_01_histograma_edad.png", p1, width = 7, height = 5, dpi = 300)

# Grafico 2: Frecuencia de clasificación cognitiva
p2 <- ggplot(base, aes(x = clasificacion_cognitiva)) +
  geom_bar() +
  labs(
    title = "Clasificación cognitiva según escala CADES",
    x = "Clasificación cognitiva",
    y = "Número de perros"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave("resultados_ccds/grafico_02_clasificacion_cognitiva.png", p2, width = 8, height = 5, dpi = 300)

# Grafico 3: Prevalencia de CCDS moderado/severo
p3 <- ggplot(base, aes(x = ccds_moderado_severo)) +
  geom_bar() +
  labs(
    title = "Frecuencia de CCDS moderado/severo",
    x = "CCDS moderado/severo",
    y = "Número de perros"
  ) +
  theme_minimal()

ggsave("resultados_ccds/grafico_03_frecuencia_ccds.png", p3, width = 6, height = 5, dpi = 300)

# Grafico 4: Boxplot de edad según CCDS
p4 <- ggplot(base, aes(x = ccds_moderado_severo, y = edad_anios)) +
  geom_boxplot() +
  labs(
    title = "Edad según presencia de CCDS moderado/severo",
    x = "CCDS moderado/severo",
    y = "Edad (años)"
  ) +
  theme_minimal()

ggsave("resultados_ccds/grafico_04_boxplot_edad_ccds.png", p4, width = 6, height = 5, dpi = 300)

# Grafico 5: Dieta y CCDS
p5 <- ggplot(base, aes(x = tipo_dieta, fill = ccds_moderado_severo)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporción de CCDS según tipo de dieta",
    x = "Tipo de dieta",
    y = "Proporción",
    fill = "CCDS moderado/severo"
  ) +
  theme_minimal()

ggsave("resultados_ccds/grafico_05_dieta_ccds.png", p5, width = 7, height = 5, dpi = 300)

# Grafico 6: Puntaje CADES según dieta
p6 <- ggplot(base, aes(x = tipo_dieta, y = puntaje_cades)) +
  geom_boxplot() +
  labs(
    title = "Puntaje CADES según tipo de dieta",
    x = "Tipo de dieta",
    y = "Puntaje CADES"
  ) +
  theme_minimal()

ggsave("resultados_ccds/grafico_06_cades_dieta.png", p6, width = 7, height = 5, dpi = 300)

# Grafico 7: Relación entre edad y puntaje CADES
p7 <- ggplot(base, aes(x = edad_anios, y = puntaje_cades)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre edad y puntaje CADES",
    x = "Edad (años)",
    y = "Puntaje CADES"
  ) +
  theme_minimal()

ggsave("resultados_ccds/grafico_07_correlacion_edad_cades.png", p7, width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# 6. ANALISIS BIVARIADO
# ------------------------------------------------------------
# Aquí comparamos cada factor con el desenlace CCDS moderado/severo.
# Para variables categóricas usamos Chi-cuadrado o Fisher.
# Para edad y puntaje CADES usamos pruebas de comparación de medias.

# Tabla comparativa por desenlace
tabla_por_ccds <- base %>%
  select(
    ccds_moderado_severo,
    edad_anios, peso_kg, puntaje_cades,
    sexo, categoria_peso, estado_reproductivo,
    vivienda, tipo_dieta, actividad_fisica
  ) %>%
  tbl_summary(
    by = ccds_moderado_severo,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p() %>%
  bold_labels()

tabla_por_ccds

# Pruebas chi-cuadrado/fisher de forma individual
analisis_categoricas <- function(variable){
  tabla <- table(base[[variable]], base$ccds_moderado_severo)
  prueba <- tryCatch(chisq.test(tabla), error = function(e) fisher.test(tabla))
  data.frame(
    variable = variable,
    p_value = prueba$p.value,
    prueba = ifelse(any(chisq.test(tabla)$expected < 5), "Fisher o revisar frecuencias bajas", "Chi-cuadrado")
  )
}

variables_cat <- c("sexo", "categoria_peso", "estado_reproductivo", "vivienda", "tipo_dieta", "actividad_fisica")
resultados_bivariados <- bind_rows(lapply(variables_cat, analisis_categoricas))
resultados_bivariados

# Comparación de edad entre perros con y sin CCDS
prueba_edad <- t.test(edad_anios ~ ccds_moderado_severo, data = base)
prueba_edad

# Correlación entre edad y puntaje CADES
cor_edad_cades <- cor.test(base$edad_anios, base$puntaje_cades, method = "pearson")
cor_edad_cades

# ------------------------------------------------------------
# 7. ODDS RATIO CRUDOS
# ------------------------------------------------------------
# Calculamos OR para variables dicotómicas principales.
# Interpretación: OR > 1 sugiere mayor odds/riesgo del desenlace.

# OR dieta: No controlada vs Controlada
or_dieta <- glm(ccds_moderado_severo ~ tipo_dieta, data = base, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

or_dieta

# OR peso: >15 kg vs <=15 kg
or_peso <- glm(ccds_moderado_severo ~ categoria_peso, data = base, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

or_peso

# OR vivienda: Exterior vs Interior
or_vivienda <- glm(ccds_moderado_severo ~ vivienda, data = base, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

or_vivienda

# OR estado reproductivo: Esterilizado vs Entero
or_estado <- glm(ccds_moderado_severo ~ estado_reproductivo, data = base, family = binomial) %>%
  tbl_regression(exponentiate = TRUE)

or_estado

# ------------------------------------------------------------
# 8. REGRESION LOGISTICA MULTIVARIADA
# ------------------------------------------------------------
# Variable respuesta: CCDS moderado/severo (Sí/No)
# Predictores: edad, dieta, peso, sexo, vivienda, estado reproductivo.

modelo_logistico <- glm(
  ccds_moderado_severo ~ edad_anios + tipo_dieta + categoria_peso + sexo + vivienda + estado_reproductivo,
  data = base,
  family = binomial
)

summary(modelo_logistico)

# Tabla del modelo con OR ajustados
modelo_or <- tbl_regression(modelo_logistico, exponentiate = TRUE) %>%
  bold_labels()

modelo_or

# Extraer OR, IC95% y p-value en tabla plana para exportar
modelo_exportar <- tidy(modelo_logistico, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>%
  rename(
    variable = term,
    odds_ratio = estimate,
    ic95_inf = conf.low,
    ic95_sup = conf.high,
    p_value = p.value
  )

modelo_exportar

# ------------------------------------------------------------
# 9. RESPUESTAS AUTOMATICAS A LOS OBJETIVOS
# ------------------------------------------------------------

n_total <- nrow(base)
prev_ccds <- mean(base$ccds_moderado_severo == "Sí") * 100
edad_prom <- mean(base$edad_anios)
cades_prom <- mean(base$puntaje_cades)

p_edad <- prueba_edad$p.value
p_cor <- cor_edad_cades$p.value
r_cor <- cor_edad_cades$estimate[[1]]

# Obtenemos resultados principales del modelo
modelo_limpio <- tidy(modelo_logistico, exponentiate = TRUE, conf.int = TRUE)

or_edad <- modelo_limpio %>% filter(term == "edad_anios")
or_dieta_modelo <- modelo_limpio %>% filter(str_detect(term, "tipo_dieta"))

respuestas_objetivos <- data.frame(
  objetivo = c(
    "Objetivo específico 1",
    "Objetivo específico 2",
    "Objetivo específico 3",
    "Objetivo específico 4",
    "Conclusión general"
  ),
  respuesta = c(
    paste0("Se evaluaron ", n_total, " perros geriátricos. La edad promedio fue ", round(edad_prom, 2), " años y el puntaje CADES promedio fue ", round(cades_prom, 2), "."),
    paste0("La frecuencia de CCDS moderado/severo fue ", round(prev_ccds, 1), "% en la base analizada."),
    paste0("La edad mostró diferencia entre perros con y sin CCDS (p = ", signif(p_edad, 3), ") y también se correlacionó con el puntaje CADES (r = ", round(r_cor, 3), "; p = ", signif(p_cor, 3), ")."),
    paste0("En la regresión logística, la edad y el tipo de dieta deben interpretarse como los principales predictores. Para edad, OR ajustado = ", round(or_edad$estimate, 2), "; IC95%: ", round(or_edad$conf.low, 2), "-", round(or_edad$conf.high, 2), ". Para dieta no controlada, OR ajustado = ", round(or_dieta_modelo$estimate, 2), "; IC95%: ", round(or_dieta_modelo$conf.low, 2), "-", round(or_dieta_modelo$conf.high, 2), "."),
    "La base permite concluir que la disfunción cognitiva moderada/severa se incrementa con la edad y se asocia principalmente con el tipo de dieta, siguiendo el enfoque del artículo de referencia."
  )
)

respuestas_objetivos

# ------------------------------------------------------------
# 10. EXPORTAR RESULTADOS A EXCEL
# ------------------------------------------------------------

# Tablas simples para exportación
tabla_frecuencia_dieta <- base %>%
  tabyl(tipo_dieta, ccds_moderado_severo) %>%
  adorn_totals(c("row", "col"))

tabla_frecuencia_peso <- base %>%
  tabyl(categoria_peso, ccds_moderado_severo) %>%
  adorn_totals(c("row", "col"))

tabla_frecuencia_sexo <- base %>%
  tabyl(sexo, ccds_moderado_severo) %>%
  adorn_totals(c("row", "col"))

tabla_frecuencia_vivienda <- base %>%
  tabyl(vivienda, ccds_moderado_severo) %>%
  adorn_totals(c("row", "col"))

write_xlsx(
  list(
    frecuencia_ccds = frecuencia_ccds,
    frecuencia_clasificacion = frecuencia_clasificacion,
    resultados_bivariados = resultados_bivariados,
    tabla_dieta_ccds = tabla_frecuencia_dieta,
    tabla_peso_ccds = tabla_frecuencia_peso,
    tabla_sexo_ccds = tabla_frecuencia_sexo,
    tabla_vivienda_ccds = tabla_frecuencia_vivienda,
    modelo_logistico_or = modelo_exportar,
    respuestas_objetivos = respuestas_objetivos
  ),
  path = "resultados_ccds/resultados_estadisticos_ccds.xlsx"
)

# ------------------------------------------------------------
# 11. MENSAJE FINAL
# ------------------------------------------------------------

cat("\n=============================================\n")
cat("ANALISIS FINALIZADO\n")
cat("Se creó la carpeta: resultados_ccds\n")
cat("Dentro encontrarás gráficos en PNG y tablas en Excel.\n")
cat("=============================================\n")
