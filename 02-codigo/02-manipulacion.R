#Manipulación - wrangling 
source("02-codigo/01-paquetes-datos.R")

#defunciones inegi ----
# Filtrar solo homicidios (tipo_defun == 2)
df_homicidios <- df_defunciones %>%
  filter(tipo_defun == 2)


df_homicidios_limpio <- df_homicidios %>%
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ "No especificado"),
    
    edad_tipo = substr(as.character(edad), 1, 1),  
    edad_valor = as.numeric(substr(as.character(edad), 2, 4)),  
    
    edad_anios = case_when(
      edad_tipo == "1" ~ 0,  # Horas (recién nacido)
      edad_tipo == "2" ~ 0,  # Días (recién nacido)
      edad_tipo == "3" ~ 0,  # Meses (recién nacido)
      edad_tipo == "4" ~ edad_valor,  # Años
      TRUE ~ NA_real_),
    
    grupo_edad = case_when(
      is.na(edad_anios) ~ "No especificado",
      edad_anios <= 9 ~ "Infancia (0-9)",
      edad_anios >= 10 & edad_anios <= 19 ~ "Adolescentes (10-19)",
      edad_anios >= 20 & edad_anios <= 35 ~ "Jóvenes (20-35)",
      edad_anios >= 36 & edad_anios <= 59 ~ "Adultxs (36-59)",
      edad_anios >= 60 ~ "Adultxs mayores (60+)",
      TRUE ~ "No especificado"),
    
    fecha_ocurr = make_date(anio_ocur, mes_ocurr, dia_ocurr),
    mes_nombre = month(fecha_ocurr, label = TRUE, abbr = FALSE, locale = "es_ES.UTF-8"))



df_homicidios_limpio <- df_homicidios_limpio %>%
  mutate(clave_ent = as.numeric(ent_ocurr))


#cátalogo de entidades-----
cat_entidades <- cat_ent_mun %>%
  filter(cve_mun == 0, cve_loc == 0) %>%
  select(cve_ent, nom_loc) %>%
  rename(nombre_entidad = nom_loc)

#Cátalogo de municipios ------ 
cat_municipios <- cat_ent_mun %>%
  filter(cve_loc == 0, cve_mun != 0) %>%
  select(cve_ent, cve_mun, nom_loc) %>%
  rename(nombre_municipio = nom_loc)

#Proyecciones de poblacion -----
estatal_proyecciones24 <- estatal_proyecciones %>% 
  filter(anio == 2024) %>% 
  group_by(entidad, sexo) %>% 
  summarise(poblacion_total = sum(poblacion, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = sexo, 
              values_from = poblacion_total) %>%
  mutate(Total = Hombres + Mujeres) %>% 
  janitor::clean_names()

estatal_proyecciones24 <- estatal_proyecciones24 %>%
  mutate(entidad = case_when(
    entidad == "Veracruz" ~ "Veracruz de Ignacio de la Llave",
    entidad == "Michoacán" ~ "Michoacán de Ocampo",
    entidad == "Coahuila" ~ "Coahuila de Zaragoza",
    TRUE ~ entidad))


mun_proyecciones24 <- mun_proyecciones %>% 
  filter(ano == 2024) %>% 
  select(clave, clave_ent, nom_ent, nom_mun, sexo, pob_total) %>%
  pivot_wider(names_from = sexo, 
              values_from = pob_total) %>%
  mutate(TOTAL = HOMBRES + MUJERES) %>% 
  janitor::clean_names( )


