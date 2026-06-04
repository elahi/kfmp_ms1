################################################################################
##' @title MDS plot, combined benthic and kelp views
##' @author Robin Elahi
##' @date 2026-05-27
##' @log 
##' 2026-06-04: cleaned up code and folder structure
################################################################################

here::i_am("fig_mds/2_plot_mds.R")
library(here)

# Load processed raw data
source(here("fig_mds", "1_process_raw_data_for_mds.R"))
d # complete processed dataset
d_sub # only species with at least one observation
m # species matrix for vegan
m_meta # metadata

# File paths 
folder <- "fig_mds"
file_name <- "2_plot_mds"

##### PACKAGES, DATA #####
library(vegan)
source(here("R", "ggplot_settings.R"))

##### WRANGLE DATA #####

## Run MDS
set.seed(100)
m_mds <- metaMDS(comm = m, distance = "bray", k = 2, 
                 try = 20, trymax = 100, 
                 trace = FALSE, autotransform = FALSE)
m_mds$stress

# Join MDS results to metadata
mds_xy <- data.frame(m_mds$points) 
m_meta2 <- cbind(m_meta, mds_xy)

## basic plot
ordiplot(m_mds, type="n") |>
  points("sites", pch=21, col="black", bg="yellow") |>
  text("species", arrows = TRUE, length = 0.05, col="gray", cex=0.5)

#### PLOT BY SITE AND VIEW ####
# Find the convex hull of the points being plotted
hull <- m_meta2 %>%
  group_by(site) %>%
  slice(chull(MDS1, MDS2)) %>%
  ungroup()

names(m)
n_spp <- as.numeric(length(names(m)))
shape_vector <- c(1, 2)

m_meta2 %>% 
  ggplot(aes(MDS1, MDS2, shape = view, color = site, label = year)) + 
  geom_polygon(data = hull, aes(x = MDS1, y = MDS2, color = site), 
               inherit.aes = FALSE, alpha = 0.10) +
  labs(caption = paste(n_spp, " taxa", "; P/A; stress = ", 
                       round(m_mds$stress, 2), sep = "")) +
  geom_text(nudge_x = 0, nudge_y = -0.05, size = 3) + 
  geom_point(size = 2) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 

ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 4, width = 5)
ggsave(paste(folder, "/figs/", file_name, "_a.jpg", sep = ""), height = 4, width = 5)
