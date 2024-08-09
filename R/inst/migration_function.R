################################################################################
#                    Migration flows functions                                 #
################################################################################

migration_flows_states <- function(tabla, filtro_mig, filtro_est, category = category, group = "Otros grupos"){
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
