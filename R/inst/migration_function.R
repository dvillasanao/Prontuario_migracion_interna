################################################################################
#                    Migration flows functions                                 #
################################################################################

library(reshape2)
library(dplyr)
library(stringr)
library(janitor)
library(tibble)

Inmigrantes_function <- function(ZM, tabla){
                                  # Si tabla1 no es una lista, conviértelo en una lista con un solo elemento
                                  if (!is.list(ZM)) {
                                        tabla %>%
                                         as.data.frame() %>%
                                          tibble::rownames_to_column(var = "rn") %>% 
                                           melt(., id.vars = "rn", variable.name = "cn") %>%
                                            mutate_if(is.factor, as.character) %>%
                                             mutate(value = ifelse((rn != cn) & (rn %in% ZM | cn %in% ZM), value, 0)) %>%
                                              filter(value > 0) %>%
                                               group_by(rn) %>% 
                                                summarise(Inmigrantes = sum(value, na.rm = TRUE))   
                          
                                  } else {
                                  lapply(1:length(ZM), function(x){
                                              tabla %>%
                                               as.data.frame() %>%
                                                tibble::rownames_to_column(var = "rn") %>% 
                                                 melt(., id.vars = "rn", variable.name = "cn") %>%
                                                  mutate_if(is.factor, as.character) %>%
                                                   mutate(value = ifelse((rn != cn) & (rn %in% ZM[[x]] | cn %in% ZM[[x]]), value, 0)) %>%
                                                    filter(value > 0) %>%
                                                     group_by(rn) %>% 
                                                      summarise(Inmigrantes = sum(value, na.rm = TRUE)) 
                                  })
                                  }
}

Emigrantes_function <- function(ZM, tabla){
                                  # Si tabla1 no es una lista, conviértelo en una lista con un solo elemento
                                  if (!is.list(ZM)) {
                                        tabla %>%
                                         as.data.frame() %>%
                                          tibble::rownames_to_column(var = "rn") %>% 
                                           melt(., id.vars = "rn", variable.name = "cn") %>%
                                            mutate_if(is.factor, as.character) %>%
                                             mutate(value = ifelse((rn != cn) & (rn %in% ZM | cn %in% ZM), value, 0)) %>%
                                              filter(value > 0) %>%
                                               group_by(cn) %>% 
                                                summarise(Emigrantes = sum(value, na.rm = TRUE)) 
                                  } else {
                                  lapply(1:length(ZM), function(x){
                                             tabla %>%
                                              as.data.frame() %>%
                                               tibble::rownames_to_column(var = "rn") %>% 
                                                melt(., id.vars = "rn", variable.name = "cn") %>%
                                                 mutate_if(is.factor, as.character) %>%
                                                  mutate(value = ifelse((rn != cn) & (rn %in% ZM[[x]] | cn %in% ZM[[x]]), value, 0)) %>%
                                                   filter(value > 0) %>%
                                                    group_by(cn) %>% 
                                                     summarise(Emigrantes = sum(value, na.rm = TRUE)) 
                                    })
                                  }
}

migration_flows_states <- function(tabla, 
                                   filtro_mig, 
                                   filtro_est, 
                                   category = category, 
                                   group = "Otros grupos"){
                            lapply(1:length(category), function(x) {
                                   # Filtrar y sumar valores para la entrada de migrantes (Inm)
                                   filtro_rn <- filtro_est %>%
                                                 mutate(value = ifelse(rn %in% category[x] | cn %in% category[x], value, 0)) %>%
                                                  group_by(rn) %>%
                                                   summarise(Inm = sum(value, na.rm = TRUE))
                                  
                                   # Filtrar y sumar valores para la salida de migrantes (Emg)
                                   filtro_cn <- filtro_est %>%
                                                 mutate(value = ifelse(rn %in% category[x] | cn %in% category[x], value, 0)) %>%
                                                  group_by(cn) %>%
                                                   summarise(Emg = sum(value, na.rm = TRUE))
                                  
                                   # Combinación de los filtros y cálculo de valores
                                   filtro <- filtro_rn %>%
                                              full_join(filtro_cn, by = c("rn" = "cn")) %>%
                                               mutate(value = sum_row(Inm, Emg, na.rm = TRUE)) %>%
                                                filter(value > filtro_mig[x] & rn != category[x]) %>%
                                                 pull(rn)
                              
                                    # Aplicación de la lógica para transformar la tabla original
                                    tabla %>%
                                     as.data.frame() %>%
                                      tibble::rownames_to_column(var = "rn") %>%
                                       reshape2::melt(id.vars = "rn", variable.name = "cn") %>%
                                        mutate_if(is.factor, as.character) %>%
                                         mutate(value = ifelse(rn %in% category[x] | cn %in% category[x], value, 0)) %>%
                                          mutate(rn = case_when(
                                                                rn %in% category[x] ~ rn,
                                                                rn %in% filtro ~ rn,
                                                                TRUE ~ group
                                                                )) %>%
                                           mutate(cn = case_when(
                                                                 cn %in% category[x] ~ cn, 
                                                                 cn %in% filtro ~ cn,
                                                                 TRUE ~ group
                                                                 )) %>%
                                            reshape2::dcast(rn ~ cn, value.var = "value", sum, na.rm = TRUE) %>%
                                             column_to_rownames(var = "rn")
  })
}

migration_flows_municipality <- function(tabla = tabla, 
                                         filtro_mun = filtro,
                                         filtro_est = filtro_est,
                                         filtro_mig = filtro_mig, 
                                         filtro_out = filtro_out, 
                                         category_group = estados, 
                                         category_names = nom_estados,
                                         group_mun = "Otros municipios",
                                         group_est = "Otros estados"){
  
                                         if(is.null(filtro_est) && is.null(filtro_out)){
                                           lapply(1:length(category_group), function(x) {
                                           #Nivel intramunicipal
                                           filtro  <- Inmigrantes %>%
                                                       full_join(., Emigrantes, by = c("rn" = "cn")) %>%
                                                        mutate(value = sum_row(Inmigrantes, Emigrantes, na.rm = TRUE)) %>%
                                                         filter(value < filtro_mig[x]) %>% 
                                                          pull(rn)
                                            tabla %>%
                                             mutate(rn = case_when(substr(.$rn, 1, 3) %in% category_group[x] & .$rn %in% filtro ~ paste0(group_mun, " (", category_group[x], ")"),
                                                                   substr(.$rn, 1, 3) %in% category_group[x]  & .$rn %nin% filtro ~ .$rn,
                                                                   substr(.$rn, 1, 3) %nin% category_group[x] ~ substr(.$rn, 1, 3)),
                                                    cn = case_when(substr(.$cn, 1, 3) %in% category_group[x] & .$cn %in% filtro ~ paste0(group_mun, "(", category_group[x], ")"),
                                                                   substr(.$cn, 1, 3) %in% category_group[x] & .$cn %nin% filtro ~ .$cn,
                                                                   substr(.$cn, 1, 3) %nin% category_group[x] ~ substr(.$cn, 1, 3))) %>%
                                              mutate(value = ifelse(.$rn != .$cn, .$value, 0)) %>%
                                               filter(value > 0) %>%
                                                dcast(., rn ~ cn, value.var = "value", sum,  na.rm = TRUE) %>%
                                                 column_to_rownames(., var = "rn") 
                                           })
                                            
                                         } else {
                                        lapply(1:length(category_group), function(x) {
                                                    # Filtro de municipios
                                                    filtro_municipios <- filtro_mun %>%
                                                                          filter(substr(.$rn, 1, 3) == category_group[x]) %>%
                                                                           filter(value < filtro_mig[x]) %>%
                                                                            pull(rn)
                                            
                                                    # Filtro de estados
                                                    filtro_rn  <- filtro_est %>%
                                                                   mutate(value = ifelse(substr(.$rn, 1, 3) %in% category_group[x] | substr(.$cn, 1, 3) %in% category_group[x], value, 0)) %>%
                                                                    mutate(rn = substr(.$rn, 1, 3)) %>%
                                                                     group_by(rn) %>%
                                                                      summarise(Inm = sum(value, na.rm = TRUE))
                                            
                                                    filtro_cn  <- filtro_est %>%
                                                                   mutate(value = ifelse(substr(.$rn, 1, 3) %in% category_group[x] | substr(.$cn, 1, 3) %in% category_group[x], value, 0)) %>%
                                                                    mutate(cn = substr(.$cn, 1, 3)) %>%
                                                                     group_by(cn) %>%
                                                                      summarise(Emg = sum(value, na.rm = TRUE))
                                            
                                                    filtro_estados <- filtro_rn %>%
                                                                       full_join(filtro_cn, by = c("rn" = "cn")) %>%
                                                                        mutate(value =  expss::sum_row(Inm, Emg, na.rm = TRUE)) %>%
                                                                         filter(value > filtro_out[x] & rn %nin% category_group[x]) %>%
                                                                          pull(rn)
                                                    
                                                    tabla %>%                                                     # Se reestructura la clave de municipios y se seleccionan solo los municipios que superan el filtro de municipios
                                                      # Se reestructuran a los municipios que superan al filtro de estados
                                                      mutate(rn = case_when(.$rn %in% filtro_municipios ~ paste0(category_group[x], " ", group_mun), 
                                                                            substr(.$rn, 1, 3) %in% category_group[x] & .$rn %nin% filtro_municipios ~ .$rn, 
                                                                            substr(.$rn, 1, 3) %nin% category_group[x] & substr(.$rn, 1, 3) %in% filtro_estados ~ str_wrap(paste0(category_names[as.numeric(substr(.$rn, 1, 3))]), 50),
                                                                            substr(.$rn, 1, 3) %nin% category_group[x] & substr(.$rn, 1, 3) %nin% filtro_estados ~ group_est),
                                                             cn = case_when(
                                                               .$cn %in% filtro_municipios ~ paste0(category_group[x], " ", group_mun), 
                                                               substr(.$cn, 1, 3) %in% category_group[x] & .$cn %nin% filtro_municipios ~ .$cn, 
                                                               substr(.$cn, 1, 3) %nin% category_group[x] & substr(.$cn, 1, 3) %in% filtro_estados ~ str_wrap(paste0(category_names[as.numeric(substr(.$cn, 1, 3))]), 50),
                                                               substr(.$cn, 1, 3) %nin% category_group[x] & substr(.$cn, 1, 3) %nin% filtro_estados ~ group_est))  %>%
                                                      # Se seleccionan los valores de la entidad seleccionada
                                                      mutate(value = ifelse(substr(.$rn, 1, 3) %in% category_group[x] | substr(.$cn, 1, 3) %in% category_group[x], value, 0)) %>% 
                                                      filter(value > 0) %>%
                                                      dcast(., rn ~ cn, value.var = "value", sum,  na.rm = TRUE) %>%
                                                      column_to_rownames(., var = "rn") 
                                          })
                                         }
}

################################################################################
#                Metropolitan  Migration flows functions                       #
################################################################################

migration_flows_metropolitan <- function(tabla = Migrantes, 
                                         filtro_zm = ZM, 
                                         filtro_mig, 
                                         filtro_out, 
                                         Emigrantes, 
                                         Inmigrantes, 
                                         category_group = estados, 
                                         category_names = nom_estados,
                                         group = "Otros estados"){
  
                                          lapply(1:length(filtro_zm), function(x) {
                                                    # Filtro de municipios de la ZM
                                                    filtro_municipios <- Inmigrantes[[x]] %>%
                                                                          full_join(Emigrantes[[x]], by = c("rn" = "cn")) %>%
                                                                           mutate(value = sum_row(Inmigrantes, Emigrantes, na.rm = TRUE)) %>%
                                                                            filter(value > filtro_mig[x]) %>%
                                                                             pull(rn)
                                                    
                                                    # Filtro de estados
                                                    filtro_estados <- Inmigrantes[[x]] %>%
                                                                       full_join(Emigrantes[[x]], by = c("rn" = "cn")) %>%
                                                                        filter(rn %nin% filtro_zm[[x]]) %>%
                                                                         mutate(value = sum_row(Inmigrantes, Emigrantes, na.rm = TRUE)) %>%
                                                                          mutate(rn = substr(rn, 1, 3)) %>%
                                                                           group_by(rn) %>%
                                                                            summarise(value = sum(value)) %>%
                                                                             filter(value >= filtro_out[x]) %>%
                                                                              pull(rn)
                                                    
                                                    tabla %>%
                                                     as.data.frame() %>%
                                                      tibble::rownames_to_column(var = "rn") %>%
                                                       melt(id.vars = "rn", variable.name = "cn") %>%
                                                        mutate_if(is.factor, as.character) %>%
                                                         filter(rn %in% filtro_zm[[x]] | cn %in% filtro_zm[[x]]) %>%
                                                          mutate(value = ifelse(rn != cn & (rn %in% filtro_zm[[x]] | cn %in% filtro_zm[[x]]), value, 0)) %>%
                                                           mutate(
                                                                  rn = case_when(
                                                                                 rn %in% filtro_zm[[x]] & rn %in% filtro_municipios ~ substr(rn, 2, nchar(rn)),
                                                                                 rn %in% filtro_zm[[x]] & rn %nin% filtro_municipios ~ str_wrap(paste(category_group[as.numeric(substr(rn, 1, 3))], "ZM"), 100),
                                                                                 rn %nin% filtro_zm[[x]] & substr(rn, 1, 3) %in% filtro_estados ~ str_wrap(paste0(category_names[as.numeric(substr(rn, 1, 3))]), 100),
                                                                                 TRUE ~ group
                                                                                 ),
                                                                  cn = case_when(
                                                                                 cn %in% filtro_zm[[x]] & cn %in% filtro_municipios ~ substr(cn, 2, nchar(cn)),
                                                                                 cn %in% filtro_zm[[x]] & cn %nin% filtro_municipios ~ str_wrap(paste(category_group[as.numeric(substr(cn, 1, 3))], "ZM"), 100),
                                                                                 cn %nin% filtro_zm[[x]] & substr(cn, 1, 3) %in% filtro_estados ~ str_wrap(paste0(category_names[as.numeric(substr(cn, 1, 3))]), 100),
                                                                                 TRUE ~ group
                                                                                 )) %>%
                                                            filter(value > 0) %>%
                                                             dcast(rn ~ cn, value.var = "value", sum, na.rm = TRUE) %>%
                                                              column_to_rownames(var = "rn")
  })
}

intramunicipal_flows_metropolitan <- function(tabla = Migrantes, 
                                              filtro_zm = ZM, 
                                              filtro_mig = NULL, 
                                              Emigrantes = NULL, 
                                              Inmigrantes= NULL, 
                                              category_group = estados, 
                                              category_names = nom_estados,
                                              group = "Otros municipios"){
                                                  # Si tabla1 no es una lista, conviértelo en una lista con un solo elemento
                                                  # Caso de la ZMVM
                                                  if (!is.list(ZM)) {
                                                        tabla %>%
                                                         as.data.frame() %>%
                                                          tibble::rownames_to_column(var = "rn") %>% 
                                                           melt(., id.vars = "rn", variable.name = "cn") %>%
                                                            mutate_if(is.factor, as.character) %>%
                                                             filter(rn %in% ZM | cn %in% ZM) %>%
                                                              mutate(value = ifelse((rn != cn) & (rn %in% ZM | cn %in% ZM), value, 0)) %>% 
                                                               mutate(rn = ifelse(rn %in% filtro_mig, 
                                                                                  stringr::str_wrap(paste0(substr(as.character(rn), 1, 3), " ", group, " (", category_group[as.numeric(substr(as.character(rn), 1, 3))], ")"), 100),
                                                                                  substr(rn, 2, nchar(rn))),
                                                                      cn = ifelse(cn %in% filtro, 
                                                                                  stringr::str_wrap(paste0(substr(as.character(cn), 1, 3), " ", group, " (", category_group[as.numeric(substr(as.character(cn), 1, 3))], ")"), 100),
                                                                                  substr(cn, 2, nchar(cn)))) %>%
                                                                filter(value > 0) 
                                                    
                                                  } else {
                                                      lapply(1:length(filtro_zm), function(x){
                                                              filtro  <- Inmigrantes[[x]] %>%
                                                                          full_join(., Emigrantes[[x]], by = c("rn" = "cn")) %>%
                                                                           mutate(value = sum_row(Inmigrantes, Emigrantes, na.rm = TRUE)) %>%
                                                                            filter(value < filtro_mig[x]) %>% 
                                                                             pull(rn)
                                                              
                                                                tabla %>%
                                                                 as.data.frame() %>%
                                                                  tibble::rownames_to_column(var = "rn") %>% 
                                                                   melt(., id.vars = "rn", variable.name = "cn") %>%
                                                                    mutate_if(is.factor, as.character) %>%
                                                                     filter(rn %in% filtro_zm[[x]] | cn %in% filtro_zm[[x]]) %>%
                                                                      mutate(value = ifelse((rn != cn) & (rn %in% filtro_zm[[x]] | cn %in% filtro_zm[[x]]), value, 0)) %>% 
                                                                       mutate(rn = ifelse(rn %in% filtro, 
                                                                                          stringr::str_wrap(paste0(substr(as.character(rn), 1, 3), " ", group, " (", category_group[as.numeric(substr(as.character(rn), 1, 3))], ")"), 100), 
                                                                                          substr(rn, 2, nchar(rn))),
                                                                              cn = ifelse(cn %in% filtro, 
                                                                                          stringr::str_wrap(paste0(substr(as.character(cn), 1, 3),  " ", group, " (", category_group[as.numeric(substr(as.character(cn), 1, 3))], ")"), 100), 
                                                                                          substr(cn, 2, nchar(cn)))) %>%
                                                                        filter(value > 0) 
                                                      })
                                                    }
}

migration_flows_metropolitan_city <- function(tabla = Migrantes, 
                                              filtro_zm = ZM, 
                                              filtro_municipios = NULL, 
                                              filtro_estados = NULL, 
                                              category_group = estados, 
                                              category_names = nom_estados,
                                              group = "Otros estados"){ 
                                                    # Migración intramunicipal
                                                    # Si tabla1 no es una lista, conviértelo en una lista con un solo elemento
                                                    # Caso de la ZMVM
                                                    if (!is.list(filtro_zm) && is.null(filtro_municipios)) {
                                                         tabla %>%
                                                          as.data.frame() %>%
                                                           tibble::rownames_to_column(var = "rn") %>% 
                                                            melt(., id.vars = "rn", variable.name = "cn") %>%
                                                             mutate_if(is.factor, as.character) %>%
                                                              filter(rn %in% filtro_zm | cn %in% filtro_zm) %>%
                                                               mutate(value = ifelse((rn != cn) & (rn %in% filtro_zm | cn %in% filtro_zm), value, 0)) %>% 
                                                                mutate(rn = ifelse(rn %in% filtro_municipios, 
                                                                                   stringr::str_wrap(paste(substr(as.character(rn), 2, 3), group), 100),
                                                                                    substr(rn, 2, nchar(rn))),
                                                                       cn = ifelse(cn %in% filtro_municipios, 
                                                                                   stringr::str_wrap(paste(substr(as.character(cn), 2, 3), group), 100),
                                                                                   substr(cn, 2, nchar(cn)))) %>%
                                                                 filter(value > 0) 
                                                    } else { 
                                                        # Migración municipal y intermunicipal
                                                        tabla %>%
                                                         as.data.frame() %>%
                                                          tibble::rownames_to_column(var = "rn") %>%
                                                           melt(id.vars = "rn", variable.name = "cn") %>%
                                                            mutate_if(is.factor, as.character) %>%
                                                             filter(rn %in% filtro_zm | cn %in% filtro_zm) %>%
                                                              mutate(value = ifelse(rn != cn & (rn %in% filtro_zm | cn %in% filtro_zm), value, 0)) %>%
                                                               mutate(
                                                                     rn = case_when(
                                                                                    rn %in% filtro_zm & rn %in% filtro_municipios ~ substr(rn, 2, nchar(rn)),
                                                                                    rn %in% filtro_zm & rn %nin% filtro_municipios ~  str_wrap(paste(substr(category_group[as.numeric(substr(rn, 1, 3))], 2, 3), "ZMVM"), 100),
                                                                                    rn %nin% filtro_zm & substr(rn, 1, 3) %in% filtro_estados ~ str_wrap(paste0(category_names[as.numeric(substr(rn, 1, 3))]), 100),
                                                                                    rn %nin% filtro_zm & substr(rn, 1, 3) %nin% filtro_estados ~ group
                                                                                    ),
                                                                    cn = case_when(
                                                                                   cn %in% filtro_zm & cn %in% filtro_municipios ~ substr(cn, 2, nchar(cn)),
                                                                                   cn %in% filtro_zm & cn %nin% filtro_municipios ~ str_wrap(paste(substr(category_group[as.numeric(substr(cn, 1, 3))], 2, 3), "ZMVM"), 100),
                                                                                   cn %nin% filtro_zm & substr(cn, 1, 3) %in% filtro_estados ~ str_wrap(paste0(category_names[as.numeric(substr(cn, 1, 3))]), 100),
                                                                                   cn %nin% filtro_zm & substr(cn, 1, 3) %nin% filtro_estados ~ group
                                                                                   )) %>%
                                                                filter(value > 0) 
                                                    }
}


metropolitan_flows <- function(tabla = Migrantes, 
                               filtro_zm = ZM, 
                               filtro_mig = NULL, 
                               Emigrantes, 
                               Inmigrantes, 
                               group = "Otras zonas metropolitanas"){
                                    lapply(1:length(filtro_zm), function(x){
                                              # filtro de las zonas metropolitanas
                                              filtro <- Inmigrantes[[x]] %>%
                                                         full_join(., Emigrantes[[x]], by = c("rn" = "cn")) %>%
                                                          filter(rn %nin% filtro_zm[[x]]) %>%
                                                           mutate(value = sum_row(Inmigrantes, Emigrantes, na.rm = TRUE)) %>%
                                                            mutate(rn = substr(rn, 1, 5)) %>%
                                                             group_by(rn) %>%
                                                              summarise(value = sum(value)) %>%
                                                               filter(value >= filtro_mig[x]) %>% 
                                                                pull(rn)
                                              tabla %>%
                                               as.data.frame() %>%
                                                tibble::rownames_to_column(var = "rn") %>% 
                                                 melt(., id.vars = "rn", variable.name = "cn") %>%
                                                  mutate_if(is.factor, as.character) %>%
                                                   filter(.$rn %in% filtro_zm[[x]] | .$cn %in% filtro_zm[[x]]) %>%
                                                    mutate(value = ifelse((.$rn != .$cn) & (.$rn %in% filtro_zm[[x]] | .$cn %in% filtro_zm[[x]]), value, 0)) %>% 
                                                     mutate(rn = case_when(.$rn %in% filtro_zm[[x]] ~ .$rn,
                                                                           .$rn %nin% filtro_zm[[x]] & substr(.$rn, 1, 5) %in% filtro ~ .$rn,
                                                                           .$rn %nin% filtro_zm[[x]] & substr(.$rn, 1, 5) %nin% filtro ~ "Otras zonas metropolitanas"),
                                                            cn = case_when(.$cn %in% filtro_zm[[x]] ~ .$cn,
                                                                           .$cn %nin% filtro_zm[[x]] & substr(.$cn, 1, 5) %in% filtro ~ .$cn,
                                                                           .$cn %nin% filtro_zm[[x]] & substr(.$cn, 1, 5) %nin% filtro ~ "Otras zonas metropolitanas")) %>%
                                                      filter(value > 0) %>%
                                                       dcast(., rn ~ cn, value.var = "value", sum,  na.rm = TRUE) %>%
                                                        column_to_rownames(., var = "rn") 
                                })
}
