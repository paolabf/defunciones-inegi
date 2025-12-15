#Paquetes
pacman::p_load(dplyr, readr, lubridate, purrr, stringr, tidyr, ggplot2, scales, forcats, RColorBrewer, officer)

#Cargar datos 
df_defunciones <- read_csv("01-datos/conjunto_de_datos_edr2024_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas24_csv.csv",
                           locale = locale(encoding = "latin1"))

#Cátalogos ----
ruta_base <- "01-datos/conjunto_de_datos_edr2024_csv/"

# Función para leer catálogos con encoding correcto
leer_catalogos <- function() {
  ruta_catalogos <- file.path(ruta_base, "catalogos")
  archivos <- list.files(ruta_catalogos, pattern = "\\.csv$", full.names = TRUE)
  
  catalogos <- list()
  for (archivo in archivos) {
    nombre <- basename(archivo) %>% str_remove("\\.csv$")
    # Leer con encoding UTF-8
    catalogos[[nombre]] <- read.csv(archivo, encoding = "UTF-8", stringsAsFactors = FALSE)
  }
  
  return(catalogos)
}

# Recargar catálogos
catalogos <- leer_catalogos()

# Crear catálogo maestro
lista_resultados <- list()
for (nombre in names(catalogos)) {
  cat <- catalogos[[nombre]]
  if (nrow(cat) > 0 && ncol(cat) >= 2) {
    lista_resultados[[nombre]] <- data.frame(
      catalogo = nombre,
      clave = as.character(cat[,1]),
      descripcion = as.character(cat[,2]),
      stringsAsFactors = FALSE
    )
  }
}

catalogo_maestro <- bind_rows(lista_resultados)


# Verificar
head(catalogo_maestro, 10)

## Cargar catálogo de entidades porque la estructura es diferente al resto 
cat_ent_mun <- read.csv("01-datos/conjunto_de_datos_edr2024_csv/catalogos/entidad_municipio_localidad_2024.csv",
  encoding = "UTF-8",
  stringsAsFactors = FALSE)

#Diccionario de datos 
diccionario <- readr::read_csv("01-datos/conjunto_de_datos_edr2024_csv/diccionario_de_datos/diccionario_datos_defunciones_registradas_2024.csv", 
                               locale = locale(encoding = "latin1")) %>%
  janitor::clean_names() 


#Proyecciones de población------ 
#estatal mitad de año 

estatal_proyecciones <- readr::read_csv("01-datos/00_Pob_Mitad_1950_2070.csv") %>% 
  janitor::clean_names()
#municipal mitad de año 

mun_proyecciones <- readr::read_csv("01-datos/pobproy_quinq1.csv") %>% 
  janitor::clean_names()
