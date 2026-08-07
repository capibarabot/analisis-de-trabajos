library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ─── CARGAR AMBOS DATASETS ───
df    <- read.csv("AI_Impact_on_Jobs_2030.csv")
datos <- read.csv("Gastos_salarios_automatizacion_ia_v2.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# ─── PREPARAR SEGUNDO DATASET ───
columnas_numericas <- c("Costo_Inversion_Inicial_USD",
                        "Gasto_Mantenimiento_Operacion_Mensual_USD",
                        "Salario_O_Licencia_Mensual_USD",
                        "Gasto_Total_Anual_USD",
                        "Horas_Operativas_Mes",
                        "Costo_Por_Hora_USD",
                        "Nivel_Eficiencia_Pct",
                        "Tasa_Error_Pct") 
datos[columnas_numericas] <- lapply(datos[columnas_numericas], as.numeric)

datos$Tipo_Recurso <- factor(datos$Tipo_Recurso,
  levels = c("Empleado Real (Humano)",
             "Equipo Automatizado (Hardware/Robótica)",
             "Inteligencia Artificial (Software/SaaS)"),
  labels = c("Humano", "Hardware/Robótica", "IA"))

# Crear carpeta img
dir.create("img", showWarnings = FALSE)

# ─── GRÁFICAS DEL PRIMER DATASET ───
# Top 10 riesgo
df_risk <- df %>%
  group_by(Job_Title) %>%
  summarise(Riesgo_Promedio = mean(Automation_Probability_2030, na.rm = TRUE)) %>%
  arrange(desc(Riesgo_Promedio)) %>%
  slice_head(n = 10)

grafica_riesgo <- ggplot(df_risk, aes(x = reorder(Job_Title, Riesgo_Promedio), 
                                       y = Riesgo_Promedio, fill = Riesgo_Promedio)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(title = "Top 10 Trabajos con Mayor Riesgo de Automatización para 2030",
       x = "Trabajo", y = "Probabilidad Promedio de Automatización") +
  theme_minimal()
ggsave("img/grafica_riesgo.png", grafica_riesgo, width = 10, height = 6, dpi = 150, bg = "white")

# Pastel salarial
df_salary <- df %>%
  group_by(Job_Title) %>%
  summarise(Salario_Promedio = mean(Average_Salary, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(etiqueta = paste0("$", format(round(Salario_Promedio, 0), big.mark = ",")))

grafica_barras <- ggplot(df_salary, aes(x = reorder(Job_Title, Salario_Promedio), 
                                        y = Salario_Promedio, fill = Job_Title)) +
  geom_col(show.legend = FALSE, width = 0.7) +                       # barras sin leyenda
  geom_text(aes(label = etiqueta), 
            hjust = -0.1, size = 3.2, fontface = "bold", color = "black") +  # etiqueta al final de la barra
  coord_flip() +                                                     # barras horizontales
  labs(title = "Promedio Salarial por Trabajo",
       x = "Trabajo",
       y = "Salario Promedio (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.y = element_text(size = 8))

ggsave("img/grafica_barras_salario.png", grafica_barras, width = 10, height = 8, dpi = 150, bg = "white")

# Demanda educativa
df_edu_demand <- df %>% count(Education_Level, name = "Demanda") %>% arrange(desc(Demanda))
grafica_demanda_edu <- ggplot(df_edu_demand, aes(x = reorder(Education_Level, -Demanda), 
                                                  y = Demanda, fill = Education_Level)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Nivel Educativo con Mayor Demanda", x = "Nivel Educativo", y = "Cantidad de Puestos") +
  theme_minimal()
ggsave("img/grafica_demanda_edu.png", grafica_demanda_edu, width = 8, height = 6, dpi = 150, bg = "white")

# Salario por educación
df_salario_edu <- df %>%
  group_by(Education_Level) %>%
  summarise(Salario_Promedio = mean(Average_Salary, na.rm = TRUE))
grafica_salario_edu <- ggplot(df_salario_edu, aes(x = reorder(Education_Level, -Salario_Promedio), 
                                                   y = Salario_Promedio, fill = Education_Level)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Salario Promedio según Nivel Educativo", x = "Nivel Educativo", y = "Salario Promedio ($)") +
  theme_minimal()
ggsave("img/grafica_salario_edu.png", grafica_salario_edu, width = 8, height = 6, dpi = 150, bg = "white")

# GRÁFICAS DEL SEGUNDO DATASET 
datos_humano_ia <- filter(datos, Tipo_Recurso %in% c("Humano", "IA"))

# Gasto total anual
p1 <- ggplot(datos, aes(x = Puesto_Empleo, y = Gasto_Total_Anual_USD, fill = Tipo_Recurso)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(0.9), width = 0.2) +
  labs(title = "Gasto Total Anual Promedio por Puesto y Tipo de Recurso",
       x = "Puesto", y = "Gasto Total Anual (USD)", fill = "Tipo de Recurso") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("img/gastos_gasto_total.png", p1, width = 10, height = 6, dpi = 150, bg = "white")

# Mantenimiento mensual
p2 <- ggplot(datos, aes(x = Puesto_Empleo, y = Gasto_Mantenimiento_Operacion_Mensual_USD, fill = Tipo_Recurso)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(0.9), width = 0.2) +
  labs(title = "Gasto de Mantenimiento/Operación Mensual Promedio por Puesto y Tipo",
       x = "Puesto", y = "Mantenimiento Mensual (USD)", fill = "Tipo de Recurso") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("img/gastos_mantenimiento.png", p2, width = 10, height = 6, dpi = 150, bg = "white")

# Boxplot gasto
p3 <- ggplot(datos, aes(x = Tipo_Recurso, y = Gasto_Total_Anual_USD, fill = Tipo_Recurso)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución del Gasto Total Anual por Tipo de Recurso",
       x = "Tipo de Recurso", y = "Gasto Total Anual (USD)") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) + theme_minimal()
ggsave("img/gastos_boxplot.png", p3, width = 8, height = 6, dpi = 150, bg = "white")

# Scatter eficiencia vs costo
p4 <- ggplot(datos, aes(x = Costo_Por_Hora_USD, y = Nivel_Eficiencia_Pct, color = Tipo_Recurso)) +
  geom_point(alpha = 0.6) +
  labs(title = "Eficiencia vs Costo por Hora", x = "Costo por Hora (USD)", y = "Nivel de Eficiencia (%)") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) + theme_minimal()
ggsave("img/gastos_scatter_eficiencia.png", p4, width = 8, height = 6, dpi = 150, bg = "white")

# Humano vs IA – Gasto
p5 <- ggplot(datos_humano_ia, aes(x = Puesto_Empleo, y = Gasto_Total_Anual_USD, fill = Tipo_Recurso)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(0.9), width = 0.2) +
  labs(title = "Gasto Total Anual Promedio: Humano vs IA por Puesto",
       x = "Puesto", y = "Gasto Total Anual (USD)", fill = "Tipo") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("img/gastos_humano_ia_gasto.png", p5, width = 10, height = 6, dpi = 150, bg = "white")

# Humano vs IA – Costo por hora
p6 <- ggplot(datos_humano_ia, aes(x = Puesto_Empleo, y = Costo_Por_Hora_USD, fill = Tipo_Recurso)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(0.9), width = 0.2) +
  labs(title = "Costo por Hora Promedio: Humano vs IA por Puesto",
       x = "Puesto", y = "Costo por Hora (USD)", fill = "Tipo") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("img/gastos_humano_ia_costo_hora.png", p6, width = 10, height = 6, dpi = 150, bg = "white")

# Humano vs IA – Eficiencia
p7 <- ggplot(datos_humano_ia, aes(x = Puesto_Empleo, y = Nivel_Eficiencia_Pct, fill = Tipo_Recurso)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(0.9), width = 0.2) +
  labs(title = "Eficiencia Promedio: Humano vs IA por Puesto",
       x = "Puesto", y = "Eficiencia (%)", fill = "Tipo") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("img/gastos_humano_ia_eficiencia.png", p7, width = 10, height = 6, dpi = 150, bg = "white")

# Humano vs IA – Tasa de error
p8 <- ggplot(datos_humano_ia, aes(x = Puesto_Empleo, y = Tasa_Error_Pct, fill = Tipo_Recurso)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(0.9)) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(0.9), width = 0.2) +
  labs(title = "Tasa de Error Promedio: Humano vs IA por Puesto",
       x = "Puesto", y = "Tasa de Error (%)", fill = "Tipo") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("img/gastos_humano_ia_error.png", p8, width = 10, height = 6, dpi = 150, bg = "white")

#  KPIs Y JSON
# KPIs dataset 1
total_puestos <- nrow(df)
puestos_alto_riesgo <- sum(df$Automation_Probability_2030 > 0.7, na.rm = TRUE)
iva <- round((puestos_alto_riesgo / total_puestos) * 100, 1)
salario_alto <- mean(df$Average_Salary[df$Automation_Probability_2030 > 0.7], na.rm = TRUE)
salario_bajo <- mean(df$Average_Salary[df$Automation_Probability_2030 < 0.3], na.rm = TRUE)
brecha_salarial <- round(salario_alto - salario_bajo, 0)
tabla_edu <- table(df$Education_Level)
nivel_max <- names(which.max(tabla_edu))
icde <- round((max(tabla_edu) / total_puestos) * 100, 1)
orden_edu <- c("High School", "Associate", "Bachelor's", "Master's", "Doctorate")
salario_por_edu <- df %>%
  filter(Education_Level %in% orden_edu) %>%
  group_by(Education_Level) %>%
  summarise(salario = mean(Average_Salary, na.rm = TRUE)) %>%
  mutate(Education_Level = factor(Education_Level, levels = orden_edu)) %>%
  arrange(Education_Level)
incrementos <- diff(salario_por_edu$salario)
rne <- round(mean(incrementos, na.rm = TRUE), 0)
riesgo_promedio_global <- mean(df$Automation_Probability_2030, na.rm = TRUE)
salario_promedio_global <- mean(df$Average_Salary, na.rm = TRUE)
max_salario_global <- max(df$Average_Salary, na.rm = TRUE)
salario_normalizado <- salario_promedio_global / max_salario_global
rrr <- round(riesgo_promedio_global / salario_normalizado, 2)

kpis <- list(
  IVA = paste0(iva, "%"),
  Brecha_Salarial = paste0("$", format(brecha_salarial, big.mark = ",")),
  ICDE = paste0(icde, "% (", nivel_max, ")"),
  RNE = paste0("$", format(rne, big.mark = ",")),
  RRR = rrr
)

# KPIs dataset 2
gasto_humano   <- mean(datos$Gasto_Total_Anual_USD[datos$Tipo_Recurso == "Humano"], na.rm = TRUE)
gasto_ia       <- mean(datos$Gasto_Total_Anual_USD[datos$Tipo_Recurso == "IA"], na.rm = TRUE)
eficiencia_h   <- mean(datos$Nivel_Eficiencia_Pct[datos$Tipo_Recurso == "Humano"], na.rm = TRUE)
eficiencia_ia  <- mean(datos$Nivel_Eficiencia_Pct[datos$Tipo_Recurso == "IA"], na.rm = TRUE)
error_h        <- mean(datos$Tasa_Error_Pct[datos$Tipo_Recurso == "Humano"], na.rm = TRUE)
error_ia       <- mean(datos$Tasa_Error_Pct[datos$Tipo_Recurso == "IA"], na.rm = TRUE)
costo_hora_h   <- mean(datos$Costo_Por_Hora_USD[datos$Tipo_Recurso == "Humano"], na.rm = TRUE)
costo_hora_ia  <- mean(datos$Costo_Por_Hora_USD[datos$Tipo_Recurso == "IA"], na.rm = TRUE)

resumen_puesto <- datos %>%
  filter(Tipo_Recurso %in% c("Humano", "IA")) %>%
  group_by(Puesto_Empleo, Tipo_Recurso) %>%
  summarise(gasto_medio = mean(Gasto_Total_Anual_USD, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Tipo_Recurso, values_from = gasto_medio) %>%
  filter(!is.na(Humano) & !is.na(IA)) %>%
  mutate(IA_mas_barata = IA < Humano)

pct_ia_barata <- round(sum(resumen_puesto$IA_mas_barata) / nrow(resumen_puesto) * 100, 1)
indice_costo_efic_h  <- gasto_humano / eficiencia_h
indice_costo_efic_ia <- gasto_ia / eficiencia_ia

kpis_gastos <- list(
  Diferencia_Gasto_Anual_Humano_IA_USD   = round(gasto_humano - gasto_ia, 0),
  Porcentaje_Puestos_IA_Mas_Barata       = paste0(pct_ia_barata, "%"),
  Eficiencia_Promedio_Humano_pct         = round(eficiencia_h, 1),
  Eficiencia_Promedio_IA_pct             = round(eficiencia_ia, 1),
  Diferencia_Eficiencia_IA_vs_Humano_pct = round(eficiencia_ia - eficiencia_h, 1),
  Tasa_Error_Promedio_Humano_pct         = round(error_h, 1),
  Tasa_Error_Promedio_IA_pct             = round(error_ia, 1),
  Diferencia_Error_IA_vs_Humano_pct      = round(error_ia - error_h, 1),
  Costo_Hora_Humano_USD                  = round(costo_hora_h, 2),
  Costo_Hora_IA_USD                      = round(costo_hora_ia, 2),
  Indice_Costo_Eficiencia_Humano         = round(indice_costo_efic_h, 2),
  Indice_Costo_Eficiencia_IA             = round(indice_costo_efic_ia, 2)
)

# Función JSON
crear_json <- function(kpis, archivo) {
  escapar <- function(x) gsub('"', '\\"', x)
  lineas <- c("{")
  n <- length(kpis)
  nombres <- names(kpis)
  for (i in seq_along(kpis)) {
    clave <- nombres[i]
    valor <- kpis[[i]]
    coma <- if (i < n) "," else ""
    if (is.numeric(valor)) {
      linea <- sprintf('  "%s": %s%s', clave, valor, coma)
    } else {
      linea <- sprintf('  "%s": "%s"%s', clave, escapar(valor), coma)
    }
    lineas <- c(lineas, linea)
  }
  lineas <- c(lineas, "}")
  writeLines(lineas, archivo)
}

crear_json(kpis, "kpis.json")
crear_json(kpis_gastos, "kpis_gastos.json")

cat("Todo listo: imágenes guardadas en img/ y archivos JSON generados.\n")