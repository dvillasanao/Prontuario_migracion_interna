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

total_tablas <- totales(tabla1 = tabla1, 
                        Clave = "CVE_ENT", 
                        Inmigrantes = "Inmigrantes",  
                        Emigrantes = "Emigrantes")

porcentajes_tablas <- porcentajes(tabla1 = tabla1, 
                                  Clave = "CVE_ENT", 
                                  Inmigrantes = "%Inmigrantes",  
                                  Emigrantes = "%Emigrantes")

tabla2 <- color_chord_diagram(tabla1 = tabla, paleta)

## Gráficos a nivel estatal 
chord_diagram_graph(file, 
                    width = 15, 
                    height = 10, 
                    family = "Montserrat Medium", 
                    paleta = paleta, 
                    tabla1 = tabla1, 
                    tabla2 = tabla2,
                    color_labels = "black",
                    transparency = 0.4,
                    circo.text = 9,
                    circos.axis.text = 6,
                    gap.degree = 2, 
                    clock.wise = FALSE,
                    track.margin = c(-0.07, 0.1),
                    margin = c(0, 0, 0, 0))

labels_chord_diagram(file, 
                     width = 7, 
                     height = 9, 
                     family = "Montserrat Medium", 
                     paleta = paleta, 
                     tabla1 = NULL, 
                     labels = NULL)

labels_chord_diagram(file = file, 
                     width = 7, 
                     height = 8, 
                     family = "Montserrat Medium", 
                     paleta = paleta, 
                     tabla1 = tabla1, 
                     labels = NOM_ZM_CF)


