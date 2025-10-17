

theme_set(theme_bw(base_size = 12) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

color_vector <- c('black',
                  #'#636363',
                  '#b2df8a','#33a02c', '#a6cee3','#1f78b4')

color_vector_modern <- c('#b2df8a','#33a02c', '#a6cee3','#1f78b4')
                  
col1 <- "#1f78b4"


# tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
#                        hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
#   
#   gb <- ggplot_build(p)
#   lay <- gb$layout$layout
#   tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
#   p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
#                 vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
# }


tag_facet2 <- function (p, open = "(", close = ")", tag_pool = letters, x = -Inf, 
          y = Inf, hjust = -0.5, vjust = 1.5, fontface = 2, family = "", 
          ...) 
{
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], 
                                    close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), 
                ..., hjust = hjust, vjust = vjust, fontface = fontface, 
                family = family, inherit.aes = FALSE) 
}
