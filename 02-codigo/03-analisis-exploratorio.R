source("02-codigo/02-manipulacion.R")

#Análisis exploratorio 

# 1. Perspectiva de género-----

por_sexo <- df_homicidios_limpio %>%
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  group_by(sexo) %>%
  summarise(total = n()) %>%
  mutate(porcentaje = round(total / sum(total) * 100, 1))

print(por_sexo)

# Por edad y sexo
por_sexo_edad <- df_homicidios_limpio %>%
  filter(sexo %in% c("Hombre", "Mujer"), 
         grupo_edad != "No especificado") %>%
  group_by(sexo, grupo_edad) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(porcentaje_por_sexo = round(total / sum(total) * 100, 1)) %>%
  ungroup() %>%
  mutate(porcentaje_del_total = round(total / sum(total) * 100, 1)) %>%
  arrange(sexo, desc(total)) %>%
  mutate(porcentaje_del_total = if_else(
    row_number() == n(),
    100 - sum(porcentaje_del_total[-n()]),
    porcentaje_del_total))

print(por_sexo_edad)


# 2. Comunidades indígenas y afromexicanas------

# Indígena
indigena <- df_homicidios_limpio %>%
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  mutate(indigena = case_when(
    conindig == 1 ~ "Indígena",
    conindig == 2 ~ "No indígena",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(indigena)) %>%
  group_by(sexo, indigena) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(porc_por_sexo = round(total / sum(total) * 100, 1)) %>%
  ungroup() %>%
  mutate(porc_del_total = round(total / sum(total) * 100, 1)) %>%
  group_by(indigena) %>%
  mutate(porc_dentro_grupo = round(total / sum(total) * 100, 1)) %>%
  ungroup()

print(indigena)

# Afromexicana
afromexicana <- df_homicidios_limpio %>%
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  mutate(afromex_desc = case_when(
    afromex == 1 ~ "Afromexicana",
    afromex == 2 ~ "No afromexicana",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(afromex_desc)) %>%
  group_by(sexo, afromex_desc) %>%
  summarise(total = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(porc_por_sexo = round(total / sum(total) * 100, 1)) %>%
  ungroup() %>%
  mutate(porc_del_total = round(total / sum(total) * 100, 1)) %>%
  group_by(afromex_desc) %>%
  mutate(porc_dentro_grupo = round(total / sum(total) * 100, 1)) %>%
  ungroup()

print(afromexicana)

# 3. Distribución por entidad-----
# 3.1 Homicidios y tasas por entidad (total, hombres, mujeres)
homicidios_por_entidad <- df_homicidios_limpio %>%
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  group_by(ent_ocurr) %>%
  summarise(
    total_homicidios = n(),
    hom_hombres = sum(sexo == "Hombre"),
    hom_mujeres = sum(sexo == "Mujer"),
    .groups = "drop")

# Preparar población con códigos
poblacion_con_codigo <- estatal_proyecciones24 %>%
  left_join(cat_entidades, by = c("entidad" = "nombre_entidad")) %>%
  mutate(cve_ent_chr = sprintf("%02d", cve_ent))

# Calcular tasas para total, hombres y mujeres
tasas_entidad <- homicidios_por_entidad %>%
  left_join(
    cat_entidades %>% 
      mutate(cve_ent_chr = sprintf("%02d", cve_ent)) %>%
      select(cve_ent_chr, nombre_entidad),
    by = c("ent_ocurr" = "cve_ent_chr")) %>%
  left_join(
    poblacion_con_codigo %>% 
      select(cve_ent_chr, pob_total = total, pob_hombres = hombres, pob_mujeres = mujeres),
    by = c("ent_ocurr" = "cve_ent_chr")) %>%
  mutate(
    cve_ent = as.numeric(ent_ocurr),
    tasa_total = round(total_homicidios / pob_total * 100000, 2),
    tasa_hombres = round(hom_hombres / pob_hombres * 100000, 2),
    tasa_mujeres = round(hom_mujeres / pob_mujeres * 100000, 2)) %>%
  filter(cve_ent != 99) %>%
  select(
    cve_ent, nombre_entidad,
    total_homicidios, hom_hombres, hom_mujeres,
    pob_total, pob_hombres, pob_mujeres,
    tasa_total, tasa_hombres, tasa_mujeres) %>%
  arrange(desc(tasa_total))

print(tasas_entidad)

#3.2 tasa por municipio 
homicidios_por_municipios <- df_homicidios_limpio %>% 
  filter(sexo %in% c("Hombre", "Mujer")) %>%
  group_by(mun_ocurr, ent_ocurr) %>%
  summarise(
    total_homicidios = n(),
    hom_hombres = sum(sexo == "Hombre"),
    hom_mujeres = sum(sexo == "Mujer"),
    .groups = "drop") %>%
  mutate(cve_mun = paste0(ent_ocurr, mun_ocurr))

# Asegurar formato de clave en población
mun_proyecciones24 <- mun_proyecciones24 %>%
  mutate(cve_mun = sprintf("%05d", clave))

# Calcular tasas
tasas_municipio <- homicidios_por_municipios %>%
  left_join(
    mun_proyecciones24 %>% 
      select(cve_mun, nom_ent, nom_mun, pob_total = total, pob_hombres = hombres, pob_mujeres = mujeres),
    by = "cve_mun") %>%
  mutate(
    tasa_total = round(total_homicidios / pob_total * 100000, 2),
    tasa_hombres = round(hom_hombres / pob_hombres * 100000, 2),
    tasa_mujeres = round(hom_mujeres / pob_mujeres * 100000, 2)) %>%
  select(
    cve_mun, ent_ocurr, mun_ocurr, nom_ent, nom_mun,
    total_homicidios, hom_hombres, hom_mujeres,
    pob_total, pob_hombres, pob_mujeres,
    tasa_total, tasa_hombres, tasa_mujeres) %>%
  arrange(desc(tasa_total))

print(tasas_municipio)

# 5. Análisis por mes-----

por_mes_sexo <- df_homicidios_limpio %>%
  filter(!is.na(mes_ocurr), mes_ocurr != 99, sexo %in% c("Hombre", "Mujer")) %>%
  mutate(
    mes_nombre = case_when(
      mes_ocurr == 1 ~ "Enero",
      mes_ocurr == 2 ~ "Febrero",
      mes_ocurr == 3 ~ "Marzo",
      mes_ocurr == 4 ~ "Abril",
      mes_ocurr == 5 ~ "Mayo",
      mes_ocurr == 6 ~ "Junio",
      mes_ocurr == 7 ~ "Julio",
      mes_ocurr == 8 ~ "Agosto",
      mes_ocurr == 9 ~ "Septiembre",
      mes_ocurr == 10 ~ "Octubre",
      mes_ocurr == 11 ~ "Noviembre",
      mes_ocurr == 12 ~ "Diciembre")) %>%
  group_by(mes_ocurr, mes_nombre, sexo) %>%
  summarise(homicidios = n(), .groups = "drop") %>%
  bind_rows(df_homicidios_limpio %>%
      filter(!is.na(mes_ocurr), mes_ocurr != 99, sexo %in% c("Hombre", "Mujer")) %>%
      mutate(
        mes_nombre = case_when(
          mes_ocurr == 1 ~ "Enero",
          mes_ocurr == 2 ~ "Febrero",
          mes_ocurr == 3 ~ "Marzo",
          mes_ocurr == 4 ~ "Abril",
          mes_ocurr == 5 ~ "Mayo",
          mes_ocurr == 6 ~ "Junio",
          mes_ocurr == 7 ~ "Julio",
          mes_ocurr == 8 ~ "Agosto",
          mes_ocurr == 9 ~ "Septiembre",
          mes_ocurr == 10 ~ "Octubre",
          mes_ocurr == 11 ~ "Noviembre",
          mes_ocurr == 12 ~ "Diciembre")) %>%
      group_by(mes_ocurr, mes_nombre) %>%
      summarise(homicidios = n(), .groups = "drop") %>%
      mutate(sexo = "Total")) %>%
  arrange(mes_ocurr, sexo)

print(por_mes_sexo)
