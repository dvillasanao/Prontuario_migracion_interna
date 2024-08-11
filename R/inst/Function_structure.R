Inmigrantes <- Inmigrantes_function(ZM, Migrantes)

Emigrantes <- Emigrantes_function(ZM, Migrantes)

tabla1 <- migration_flows_metropolitan(tabla = Migrantes, 
                                       filtro_zm = ZM, 
                                       filtro_mig = filtro_mig, 
                                       filtro_out = filtro_out, 
                                       Emigrantes = Emigrantes, 
                                       Inmigrantes = Inmigrantes, 
                                       category_group = estados, 
                                       category_names = nom_estados,
                                       group = "Otros estados")

tabla1 <- intramunicipal_flows_metropolitan(filtro_zm = ZM, 
                                            filtro_mig = filtro, 
                                            Emigrantes = NULL,
                                            Inmigrantes = NULL, 
                                            category_group = estados, 
                                            group = "Otros municipios")
tabla1 <- migration_flows_metropolitan_city(tabla = Migrantes, 
                                            filtro_zm = ZM, 
                                            filtro_municipios = filtro, 
                                            filtro_estados = NULL, 
                                            category_group = estados, 
                                            category_names = nom_estados,
                                            group = "ZMVM") %>%
            dcast(., rn ~ cn, value.var = "value", sum,  na.rm = TRUE) %>%                        
            column_to_rownames(., var = "rn")    


total_tablas <- totales(tabla1 = tabla1, 
                        Clave = "CVE_ENT", 
                        Inmigrantes = "Inmigrantes",  
                        Emigrantes = "Emigrantes")

porcentajes_tablas <- porcentajes(tabla1 = tabla1, 
                                  Clave = "CVE_ENT", 
                                  Inmigrantes = "%Inmigrantes",  
                                  Emigrantes = "%Emigrantes")

tabla2 <- color_chord_diagram(tabla1 = tabla, paleta)

file = "/Graficos/prueba.pdf"
## Gráficos a nivel estatal 
chord_diagram_graph(file = file, 
                    width = 7, 
                    height = 7, 
                    family = "Montserrat Medium", 
                    paleta = paleta, 
                    tabla1 = tabla1, 
                    tabla2 = tabla2, 
                    color_labels = "#000C7D",
                    transparency = 0.25,
                    circo.text = 7,
                    circos.axis.text = 5,
                    adj.text =c(-0.05, 0.5), #Ajuste de las etiquetas (x, y)
                    adj.ylim = 0.2,
                    gap.degree = 3, 
                    clock.wise = TRUE,
                    track.margin = c(-0.2, 0.2),
                    margin = rep(1.5, 4))

## Gráficos a nivel zona metropolitana (ZMVM)
file = "/Graficos/prueba.pdf"
chord_diagram_graph_zmvm(file = file, 
                         width = 10, 
                         height = 10, 
                         family = "Montserrat Medium", 
                         paleta = paleta, 
                         tabla1 = as.matrix(tabla1), 
                         color_labels = "#000C7D",
                         transparency = 0.25,
                         circo.text = 9,
                         circos.axis.text = 7,
                         adj.text = c(-0.01, 0.5),
                         adj.ylim = 0.2,
                         gap.degree = 3, 
                         clock.wise = TRUE,
                         track.margin = c(-0.2, 0.2),
                         margin = rep(0, 4), 
                         group1 = grupo1, 
                         group1.text = "Ciudad de México",
                         group1.col = 1, 
                         group2 = grupo2, 
                         group2.text = "México",
                         group2.col = 15, 
                         group3 = grupo3, 
                         group3.text = "Hidalgo",
                         group3.col = 20, 
                         group4 = grupo4, 
                         group4.text = "Otro municipios",
                         group4.col = 30)

file = "/Graficos/prueba.pdf"
labels_chord_diagram(file = file, 
                     width = 7, 
                     height = 8, 
                     family = "Montserrat Medium", 
                     paleta = paleta, 
                     tabla1 = tabla1, 
                     labels = paste(NOM_ZM_CF[,1], NOM_ZM_CF[,2]))
