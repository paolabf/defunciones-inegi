source("02-codigo/03-analisis-exploratorio.R")
#04-visualziaciones
#tema general
theme_set(theme_minimal(base_size = 12))

# Paleta de colores PRGn
# Usar los extremos de la paleta: verde para hombres, morado para mujeres
colores_prgn <- brewer.pal(11, "PRGn")
colores_sexo <- c("Hombre" = colores_prgn[2], "Mujer" = colores_prgn[10])


# 1. PDiferencias por sexo ----- 

# 1.1 Homicidios por sexo (gráfica apilada)------
g1_sexo <- por_sexo %>%
  mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre"))) %>%
  ggplot(aes(x = total, y = "", fill = sexo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(comma(total), "\n(", porcentaje, "%)")), 
            position = position_stack(vjust = 0.5), 
            size = 5, 
            fontface = "bold",
            color = "white") +
  scale_fill_manual(values = colores_sexo) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Homicidios por sexo en México, 2024",
    subtitle = "9 de cada 10 víctimas de homicidio son hombres",
    x = "Número de homicidios",
    y = NULL,
    fill = "Sexo",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "right",
    axis.text.y = element_blank())
  
  
ggsave("03-output/01_homicidios_por_sexo.png", 
       g1_sexo, width = 8, height = 6, dpi = 300)

# 1.2 Homicidios por edad y sexo-----
orden_edad <- c("Infancia (0-9)", 
                "Adolescentes (10-19)", 
                "Jóvenes (20-35)", 
                "Adultxs (36-59)", 
                "Adultxs mayores (60+)")

g2_edad_sexo <- por_sexo_edad %>%
  mutate(grupo_edad = factor(grupo_edad, levels = orden_edad)) %>%
  ggplot(aes(x = grupo_edad, y = total, fill = sexo)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = comma(total)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = colores_sexo) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Homicidios por grupo de edad y sexo, 2024",
    x = "Grupo de edad",
    y = "Número de homicidios",
    fill = "Sexo",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("03-output/02_homicidios_edad_sexo.png", 
       g2_edad_sexo, width = 10, height = 6, dpi = 300)


#1.2.1 g2_1 -----
colores_prgn <- brewer.pal(11, "PRGn")
colores_edad <- colores_prgn[7:11]

g2_1_edad_sexo <- por_sexo_edad %>%
  mutate(grupo_edad = factor(grupo_edad, levels = orden_edad),
         sexo = factor(sexo, levels = c("Mujer", "Hombre"))) %>%
  ggplot(aes(x = sexo, y = porcentaje_por_sexo, fill = grupo_edad)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(porcentaje_por_sexo, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, 
            fontface = "bold",
            color = "white") +
  scale_fill_manual(values = colores_edad) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Composición de homicidios por grupo de edad según sexo, 2024",
    subtitle = "Distribución porcentual dentro de cada sexo",
    x = NULL,
    y = "Porcentaje",
    fill = "Grupo de edad",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "right")
  
  
  
ggsave("03-output/02_1_homicidios_edad_sexo.png", 
       g2_1_edad_sexo, width = 10, height = 6, dpi = 300)

#2. poblacion indigena y afromexicana -----
# 2.1 Población indígena-----
g3_indigena <- indigena %>%
  filter(indigena != "No indígena") %>%
  mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre"))) %>%
  ggplot(aes(x = "", y = total, fill = sexo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(comma(total), "\n(", porc_dentro_grupo, "%)")), 
            position = position_stack(vjust = 0.5), 
            size = 4,
            fontface = "bold",
            color = "white") +
  scale_fill_manual(values = colores_sexo) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Homicidios de población indígena por sexo, 2024",
    subtitle = "832 personas indígenas fueron víctimas de homicidio",
    x = NULL,
    y = "Número de homicidios",
    fill = "Sexo",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_blank())

ggsave("03-output/03_homicidios_indigenas.png", 
       g3_indigena, width = 8, height = 6, dpi = 300)

#2.1.1.-----
g3_1_indigena <- indigena %>%
  mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre")),
         indigena = factor(indigena, levels = c("No indígena", "Indígena"))) %>%
  ggplot(aes(x = sexo, y = porc_por_sexo, fill = indigena)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(porc_por_sexo, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4,
            fontface = "bold",
            color = "white") +
  scale_fill_manual(values = c("No indígena" = "gray70", "Indígena" = colores_prgn[9])) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Proporción de población indígena en homicidios por sexo, 2024",
    x = NULL,
    y = "Porcentaje",
    fill = "Condición",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "right")
  
  
ggsave("03-output/03_homicidios_indigenas.png", 
       g3_1_indigena, width = 8, height = 6, dpi = 300)

#2.2. afromexicana ---- 
# 2.2 Población afromexicana-----
g4_afromex <- afromexicana %>%
  filter(afromex_desc != "No afromexicana") %>%
  mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre"))) %>%
  ggplot(aes(x = "", y = total, fill = sexo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(comma(total), "\n(", porc_dentro_grupo, "%)")), 
            position = position_stack(vjust = 0.5), 
            size = 4,
            fontface = "bold",
            color = "white") +
  scale_fill_manual(values = colores_sexo) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Homicidios de población afromexicana por sexo, 2024",
    subtitle = "130 personas afromexicanas fueron víctimas de homicidio",
    x = NULL,
    y = "Número de homicidios",
    fill = "Sexo",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "right",
    axis.text.x = element_blank())

ggsave("03-output/04_homicidios_afromexicanos.png", 
       g4_afromex, width = 8, height = 6, dpi = 300)

# 2.2.1 Proporción afromexicana-----
g4_1_afromex <- afromexicana %>%
  mutate(sexo = factor(sexo, levels = c("Mujer", "Hombre")),
         afromex_desc = factor(afromex_desc, levels = c("No afromexicana", "Afromexicana"))) %>%
  ggplot(aes(x = sexo, y = porc_por_sexo, fill = afromex_desc)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(porc_por_sexo, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4,
            fontface = "bold",
            color = "white") +
  scale_fill_manual(values = c("No afromexicana" = "gray70", "Afromexicana" = colores_prgn[9])) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Proporción de población afromexicana en homicidios por sexo, 2024",
    x = NULL,
    y = "Porcentaje",
    fill = "Condición",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "right")

ggsave("03-output/04_1_proporcion_afromexicanos.png", 
       g4_1_afromex, width = 8, height = 6, dpi = 300)

#3. Distribución geográfica ------- 

# 3.1 tasa por entidad -----
g4_tasas_estado <- tasas_entidad %>%
  mutate(nombre_entidad = fct_reorder(nombre_entidad, tasa_total)) %>%
  ggplot(aes(x = nombre_entidad, y = tasa_total)) +
  geom_col(fill = colores_prgn[10], width = 0.7) +
  geom_text(aes(label = round(tasa_total, 1)), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Tasa de homicidios por entidad, 2024",
    subtitle = "Tasa por cada 100,000 habitantes",
    x = NULL,
    y = "Tasa de homicidios por 100,000 habitantes",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024 | Población: CONAPO mitad de año 2020-2070") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5))

ggsave("03-output/04_tasas_entidades.png", 
       g4_tasas_estado, width = 10, height = 7, dpi = 300)

#3.1.1 tasa de hombres, mujeres y totoal por entidad ----- 
g5_tasas_mancuerna <- tasas_entidad %>%
  mutate(nombre_entidad = fct_reorder(nombre_entidad, tasa_total)) %>%
  ggplot(aes(y = nombre_entidad)) +
  geom_segment(aes(x = tasa_mujeres, xend = tasa_hombres, yend = nombre_entidad),
               color = "gray70", size = 1) +
  geom_point(aes(x = tasa_hombres, color = "Hombre"), size = 3) +
  geom_point(aes(x = tasa_mujeres, color = "Mujer"), size = 3) +
  geom_text(aes(x = tasa_hombres, label = round(tasa_hombres, 1)), 
            hjust = -0.3, size = 2.5) +
  geom_text(aes(x = tasa_mujeres, label = round(tasa_mujeres, 1)), 
            hjust = 1.3, size = 2.5) +
  scale_color_manual(values = colores_sexo) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "Diferencias por sexos en las tasas de homicidio, 2024",
    subtitle = "Tasa por cada 100,000 habitantes según sexo por entidad",
    x = "Tasa de homicidios por 100,000 habitantes",
    y = NULL,
    color = "Sexo",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024 | Población: CONAPO mitad de año 2020-2070") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "top")

ggsave("03-output/05_tasas_diferencias_sexo_entidad.png", 
       g5_tasas_mancuerna, width = 10, height = 12, dpi = 300)


#5. Homicidios por mes -----
g6_por_mes <- por_mes_sexo %>%
  ggplot(aes(x = mes_ocurr, y = homicidios, color = sexo, group = sexo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = comma(homicidios)), 
            vjust = -0.8, 
            size = 3,
            show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Hombre" = colores_prgn[2], 
      "Mujer" = colores_prgn[10],
      "Total" = "gray30"),
    breaks = c("Hombre", "Mujer", "Total")) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "Evolución mensual de homicidios por sexo, 2024",
    subtitle = "Los meses de abril y mayo registraron los niveles más altos",
    x = "Mes",
    y = "Número de homicidios",
    color = "Sexo",
    caption = "Fuente: INEGI, Estadísticas de Defunciones Registradas 2024") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0.5),
    legend.position = "top")

ggsave("03-output/06_homicidios_por_mes.png", 
       g6_por_mes, width = 10, height = 6, dpi = 300)