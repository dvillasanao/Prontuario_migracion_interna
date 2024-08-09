################################################################################
#                    Margins |Absolutes |Percentages                           #
################################################################################

library(dplyr)
library(stringr)
library(grDevices)
library(janitor)
library(tibble)

################################################################################
#                     Función Matrices (Reducción)                             #
################################################################################

# Función para calcular totales con parámetros personalizados
totales <- function(tabla1, Inmigrantes = "Salen por estudio", Clave = "CVE_MUN", Emigrantes = "Entran por estudio") {
  lapply(1:length(tabla1), function(x) {
    # Agrupación de totales
    datos_totales <- full_join(
                               tabla1[[x]] %>%
                                as.data.frame() %>%
                                 adorn_totals(c("row", "col"), 
                                              fill = "-", 
                                              na.rm = TRUE, 
                                              ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                                  slice(nrow(.)) %>%
                                   t() %>%
                                    as.data.frame() %>%
                                     rownames_to_column(var = Clave) %>%
                                      rename(!!Emigrantes := "1"),
                               tabla1[[x]] %>%
                                as.data.frame() %>%
                                 adorn_totals(c("row", "col"), 
                                              fill = "-", 
                                              na.rm = TRUE, 
                                              ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                                  rownames_to_column(var = Clave) %>%
                                   select(!!sym(Clave), Total) %>%
                                    mutate(!!Clave := str_replace(!!sym(Clave), "1$", "Total")),
                               by = Clave
                               ) %>%
                     rename(!!Inmigrantes := "Total") %>%
                    select(!!sym(Clave), !!sym(Emigrantes), !!sym(Inmigrantes))
    
    # Devolver los resultados
    return(datos_totales)
  })
}

# Función para calcular porcentajes con parámetros personalizados
porcentajes <- function(tabla1, Inmigrantes = "%Salen por estudio", Clave = "CVE_MUN", Emigrantes = "%Entran por estudio") {
  lapply(1:length(tabla1), function(x) {
    # Agrupación de porcentajes
    datos_porcentajes <- full_join(
      tabla1[[x]] %>%
        as.data.frame() %>%
        adorn_totals(c("col", "row"), 
                     fill = "0", 
                     na.rm = TRUE, 
                     name = "Total",
                     ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
        adorn_percentages("col", na.rm = TRUE) %>%
        rownames_to_column(var = Clave) %>%
        select(!!sym(Clave), Total) %>%
        mutate(!!sym(Clave) := str_replace(!!sym(Clave), "1$", "Total")),
      tabla1[[x]] %>%
        as.data.frame() %>%
        adorn_totals(c("row"), 
                     fill = "0", 
                     na.rm = TRUE, 
                     name = "Total",
                     ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
        adorn_percentages("row", na.rm = TRUE, 
                          ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
        slice(nrow(.)) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column(var = Clave) %>%
        rename("Total_Filas" = "1"),
      by = Clave
    ) %>%
      rename(!!sym(Inmigrantes) := "Total",
             !!sym(Emigrantes) := "Total_Filas") %>%
      select(!!sym(Clave), !!sym(Emigrantes), !!sym(Inmigrantes))
    
    # Devolver los resultados
    datos_porcentajes
  })
}
