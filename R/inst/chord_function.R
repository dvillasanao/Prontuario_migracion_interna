################################################################################
#                         Chord Diagram                                        #
################################################################################

library(ggplot2)
library(reshape2)
library(circlize)
library(dplyr)
library(stringr)
library(grDevices)

################################################################################
#                         Chord Diagram Colors                                 #
################################################################################

#tabla2 <- color_chord_diagram(tabla1, paleta) 

color_chord_diagram <- function(tabla1 = NULL, 
                                paleta = paleta) {
  # Si tabla1 no es una lista, conviértelo en una lista con un solo elemento
  if (!is.list(tabla1)) {
    groupColors <- setNames(colorRampPalette(paleta)(length(unique(c(colnames(tabla1), rownames(tabla1))))),
                            nm = str_sort(unique(c(colnames(tabla1), rownames(tabla1))), numeric = TRUE))
    return(groupColors)
  } else {
  # Aplica la función a cada elemento de la lista
  lapply(1:length(tabla1), function(x) {
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
  }
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
                                tabla1, 
                                tabla2, 
                                color_labels = "black",
                                transparency = 0,
                                margin = c(0, 0, 0, 0)) {
  
  # Archivo de salida
  cairo_pdf(filename = paste0(here::here(), file), 
            width = width, 
            height = height, 
            family = family, 
            fallback_resolution = 400, 
            onefile = TRUE)
  
  # Determina si tabla1 y tabla2 son listas o data.frames únicos
  is_list <- is.list(tabla1) && is.list(tabla2)
  
  if (is_list) {
    for (i in 1:length(tabla1)) {
      tabla <- tabla1[[i]]
      color_table <- tabla2[[i]]
      # Paleta de colores
      groupColors <- setNames(colorRampPalette(paleta)(length(unique(c(colnames(tabla), rownames(tabla))))),
                              nm = str_sort(unique(c(colnames(tabla), rownames(tabla))), numeric = TRUE))
      
      circos.clear()
      circos.par(start.degree = 90, 
                 gap.degree = 2, 
                 clock.wise = FALSE,
                 track.margin = c(-0.07, 0.1),
                 points.overflow.warning = FALSE)
      
      par(mar = margin)
      
      chordDiagram(x = as.matrix(tabla), 
                   grid.col = groupColors,
                   col = color_table,
                   order = str_sort(unique(c(colnames(tabla), rownames(tabla)))),
                   keep.diagonal = FALSE,
                   transparency = transparency,
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
  } else {
    # Si tabla1 y tabla2 no son listas, simplemente usa los data frames directamente
    tabla <- tabla1
    color_table <- tabla2
    
    circos.clear()
    circos.par(start.degree = 90, 
               gap.degree = 2, 
               clock.wise = FALSE,
               track.margin = c(-0.07, 0.1),
               points.overflow.warning = FALSE)
    
    par(mar = margin)
    
    chordDiagram(x = as.matrix(tabla), 
                 grid.col = color_table,
                 order = str_sort(unique(c(colnames(tabla), rownames(tabla)))),
                 keep.diagonal = FALSE,
                 transparency = transparency,
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
labels_chord_diagram <- function(file, 
                                 width = 7, 
                                 height = 9, 
                                 family = "Montserrat Medium", 
                                 paleta = paleta, 
                                 tabla1 = NULL, 
                                 labels = NULL) {
  
  ## Labels Chord Diagram 
  etiquetas <- lapply(1:length(tabla1), function(x) {
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
           labs(fill = str_wrap(labels[x], 100),
                color = str_wrap(labels[x], 100))
    
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
  for (i in 1:length(tabla1)) {
    print(etiquetas[[i]])
  }
  dev.off()
}
