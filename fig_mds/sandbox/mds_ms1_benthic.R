################################################################################
##' @title MDS plots for benthic view
##' @author Robin Elahi
##' @date 2026-05-27
##' @log 
################################################################################

here::i_am("kfmp_transect_video_species/mds_ms1_benthic.R")
library(here)

# Load processed raw data
source(here("kfmp_transect_video_species", "_process_raw_data_lump_ms1.R"))

benthic_m
benthic_meta

# Rename data
m <- benthic_m # species matrix for vegan
m_meta <- benthic_meta # metadata

# File paths 
folder <- "kfmp_transect_video_species"
file_name <- "mds_benthic"

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
  ggplot(aes(MDS1, MDS2, color = site, label = year)) + 
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
