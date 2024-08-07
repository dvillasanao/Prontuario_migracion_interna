################################################################################
#                         Movilidad Laboral                                    #
################################################################################

library(ggplot2)
library(reshape2)
library(circlize)
library(dplyr)
library(stringr)
library(grDevices)
library(janitor)
library(tibble)

################################################################################
#                         Migration Functions                                  #
################################################################################

Inmigrantes_function <- function(ZM, Migrantes){
                                  lapply(1:length(ZM), function(x){
                                          Migrantes %>%
                                           as.data.frame() %>%
                                            tibble::rownames_to_column(var = "rn") %>% 
                                             melt(., id.vars = "rn", variable.name = "cn") %>%
                                              mutate_if(is.factor, as.character) %>%
                                               mutate(value = ifelse((.$rn != .$cn) & (.$rn %in% ZM[[x]] | .$cn %in% ZM[[x]]), value, 0)) %>%
                                                filter(value > 0) %>%
                                                 group_by(rn) %>% 
                                                  summarise(Inmigrantes = sum(value, na.rm = TRUE)) 
  })
}



Emigrantes_function <- function(ZM, Migrantes){
                                  lapply(1:length(ZM), function(x){
                                           Migrantes %>%
                                            as.data.frame() %>%
                                             tibble::rownames_to_column(var = "rn") %>% 
                                              melt(., id.vars = "rn", variable.name = "cn") %>%
                                               mutate_if(is.factor, as.character) %>%
                                                mutate(value = ifelse((.$rn != .$cn) & (.$rn %in% ZM[[x]] | .$cn %in% ZM[[x]]), value, 0)) %>%
                                                 filter(value > 0) %>%
                                                  group_by(cn) %>% 
                                                   summarise(Emigrantes = sum(value, na.rm = TRUE)) 
  })
}

################################################################################
#                         Chord Diagram Colors                                 #
################################################################################
color_chord_diagram <- function(tabla1, ZM_CF = ZM_CF, paleta = paleta) {
                                    tabla2 <- lapply(1:length(ZM_CF), function(x) {
                                                # Paleta de colores
                                                groupColors <- setNames(colorRampPalette(paleta)(length(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))))),
                                                                        nm = str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))
                                                
                                                circos.clear()
                                                chordDiagram(x = tabla1[[x]] %>% as.matrix(), 
                                                             transparency = 0.4,
                                                             order = str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE),
                                                             grid.col = groupColors) %>% 
                                                pull(col)
                                    })
                                    
                                    return(tabla2)
}


################################################################################
#                         Chord Diagram Graph                                  #
################################################################################
# Definir la función
chord_diagram_graph <- function(file, 
                                width = 15, 
                                height = 10, 
                                family = "Montserrat Medium", 
                                paleta = paleta, 
                                ZM_CF, 
                                tabla1, 
                                tabla2, 
                                color_labels = "black",
                                margin = c(0, 0, 0, 0)) {
  
  # Archivo de salida
  cairo_pdf(filename = paste0(here::here(), file), 
            width = width, 
            height = height, 
            family = family, 
            fallback_resolution = 400, 
            onefile = TRUE)
  
  for (i in 1:length(ZM_CF)) {
    # Paleta de colores
    groupColors <- setNames(colorRampPalette(paleta)(length(unique(c(colnames(tabla1[[i]]), rownames(tabla1[[i]]))))),
                            nm = str_sort(unique(c(colnames(tabla1[[i]]), rownames(tabla1[[i]]))), numeric = TRUE))
    
    circos.clear()
    circos.par(start.degree = 90, 
               gap.degree = 2, 
               clock.wise = FALSE,
               track.margin = c(-0.07, 0.1),
               points.overflow.warning = FALSE)
    
    par(mar = margin)
    
    chordDiagram(x = as.matrix(tabla1[[i]]), 
                 grid.col = groupColors,
                 col = tabla2[[i]],
                 order = str_sort(unique(c(colnames(tabla1[[i]]), rownames(tabla1[[i]])))),
                 keep.diagonal = FALSE,
                 transparency = 0,
                 directional = 1,
                 direction.type = c("arrows", "diffHeight"), 
                 diffHeight = -0.04,
                 annotationTrack = "grid", 
                 annotationTrackHeight = c(0.05, 0.1),
                 preAllocateTracks = 1, 
                 big.gap = 40,
                 link.arr.type = "big.arrow", 
                 link.lwd = 3,
                 link.lty = 1,
                 link.visible = TRUE,
                 link.largest.ontop = TRUE)
    
    # Añadir texto y eje
    circos.trackPlotRegion(track.index = 1,
                           track.height = 0.05,
                           bg.border = NA, 
                           panel.fun = function(x, y) {
                             xlim = get.cell.meta.data("xlim")
                             ylim = get.cell.meta.data("ylim")
                             sector.name = get.cell.meta.data("sector.index")
                             circos.text(x = mean(xlim), 
                                         y = ylim[1] + 0.1, 
                                         labels = sector.name, 
                                         facing = "clockwise",
                                         niceFacing = TRUE, 
                                         adj = c(-0.05, 0.5),
                                         cex = fontsize(9),
                                         col = color_labels,
                                         font = 1)
                             circos.axis(h = "top",
                                         labels = c(0, 10, 20, 50, 100, 200, 300, 400, 500, seq(1000, 20000, by = 1000)),
                                         major.tick.length = 0.5,
                                         minor.ticks = 4, 
                                         labels.cex = fontsize(6),
                                         sector.index = sector.name,
                                         track.index = 2,
                                         labels.niceFacing = TRUE,
                                         labels.pos.adjust = c(0, 0.8))
                           })
  }
  dev.off()
}


################################################################################
#                         Chord Diagram Etiquetas                              #
################################################################################

labels_chord_diagram <- function(file, width = 7, height = 9, family = "Montserrat Medium", paleta = paleta, ZM_CF, tabla1, NOM_ZM_CF) {
  
  ## Labels Chord Diagram 
  etiquetas <- lapply(1:length(ZM_CF), function(x) {
                  # Se ordenan por claves 
                  orden <- str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE)
  
                  p <- tabla1[[x]] %>% 
                        as.data.frame() %>%
                         tibble::rownames_to_column(var = "rn") %>%
                          melt(., id.vars = "rn", variable.name = "cn") %>%
                           as.data.frame() %>%
                            mutate(rn = str_wrap(.$rn, 100),
                                   cn = str_wrap(.$cn, 100)) %>%
                             ggplot() + 
                              geom_bar(aes(x = value, fill = as.character(rn, orden)))  + 
                               geom_bar(aes(x = value, fill = as.character(cn, orden)))  + 
                                theme(plot.margin = margin(t = 1, r = 1.5, b = 1, l = 0, "cm"),
                                      text = element_text(family = family),
                                      axis.text = element_blank(),
                                      axis.title = element_blank(),
                                      strip.text = element_text(size = 9, face = "bold", family = family),  
                                      legend.key.size = unit(0.4, "cm"), 
                                      legend.spacing.y = unit(0.4, "cm"),
                                      legend.text = element_text(size = 7, family = family),
                                      legend.title = element_text(size = 8 , family = family)) + 
                                 scale_fill_manual(values = setNames(colorRampPalette(paleta)(length(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))))),
                                                                     nm = str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) + 
                                  guides(fill = guide_legend(ncol = 1)) + 
                                   labs(fill = stringr::str_wrap(paste(NOM_ZM_CF[x, 1], NOM_ZM_CF[x, 2]), 100),
                                        color = stringr::str_wrap(paste(NOM_ZM_CF[x, 1], NOM_ZM_CF[x, 2]), 100))
                            
                  leg <- cowplot::get_legend(p)
                  
                  ggpubr::as_ggplot(leg)
                  
                  }
                )
  
  # Guardar etiquetas en PDF
  cairo_pdf(filename = paste0(here::here(), file), 
            width = width, 
            height = height, 
            fallback_resolution = 400, 
            family = family, 
            onefile = TRUE)
  for (i in 1:length(ZM_CF)) {
    print(etiquetas[[i]])
  }
  dev.off()
}


################################################################################
#                     Función Matrices (Reducción)                             #
################################################################################

# Función para calcular totales
totales <- function(tabla1, ZM) {
              lapply(1:length(ZM), function(x){
                #Se agrupan los totales
                full_join(
                          tabla1[[x]] %>%
                           as.data.frame() %>%
                            adorn_totals(c("row", "col"), 
                                         fill = "-", 
                                         na.rm = TRUE, 
                                         ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                             slice(nrow(.)) %>% 
                              t() %>%
                               as.data.frame()%>%
                                rownames_to_column(var = "name")  %>% 
                                 rename("Total_Filas" = "1"),
                          tabla1[[x]] %>%
                           as.data.frame() %>%
                            adorn_totals(c("row", "col"), 
                                         fill = "-", 
                                         na.rm = TRUE, 
                                         ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                             rownames_to_column(var = "name") %>%
                              select(name, Total) %>%
                               mutate(name = str_replace(.$name, "1$", "Total")), 
                          by = c("name")
                          ) %>%
                  rename("CVE_MUN" = "name",
                         "Entran por trabajo" = "Total_Filas", # Población que entra a la entidad para trabajar
                         "Salen por trabajo" = "Total") %>% # Población que sale de su entidad de residencia y entra a otra demarcación por motivos de trabajo
                  select(`Entran por trabajo`, CVE_MUN, `Salen por trabajo`)
  })
}

# Función para calcular porcentajes
porcentajes <- function(tabla1, ZM) {
                    lapply(1:length(ZM), function(x){
                      #Se agrupan los porcentajes
                      full_join(tabla1[[x]] %>%
                                 as.data.frame() %>%
                                  adorn_totals(c("col", "row"), 
                                               fill = "0", 
                                               na.rm = TRUE, 
                                               name = "Total",
                                               ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                                   adorn_percentages("col", na.rm = TRUE) %>% 
                                    rownames_to_column(var = "name") %>%
                                     select(name, Total) %>%
                                      mutate(name = str_replace(.$name, "1$", "Total")),
                                tabla1[[x]] %>%
                                 as.data.frame() %>%
                                  adorn_totals(c("row"), 
                                               fill = "0", 
                                               na.rm = TRUE, 
                                               name = "Total", 
                                               ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                                   adorn_percentages("row", 
                                                    na.rm = TRUE, 
                                                    ,,,,contains(str_sort(unique(c(colnames(tabla1[[x]]), rownames(tabla1[[x]]))), numeric = TRUE))) %>%
                                    slice(nrow(.)) %>% 
                                     t() %>%
                                      as.data.frame()%>%
                                       rownames_to_column(var = "name")  %>% 
                                        rename("Total_Filas" = "1"), 
                                by = c("name")
                                )  %>%
                        rename("CVE_MUN" = "name",
                               "%Entran por trabajo" = "Total_Filas", # Población que entra a la entidad para trabajar
                               "%Salen por trabajo" = "Total") %>% # Población que sale de su entidad de residencia y entra a otra demarcación por motivos de trabajo
                        select(`%Entran por trabajo`, CVE_MUN, `%Salen por trabajo`)
  })
}
