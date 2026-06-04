################################################################################
##' @title Prepare video species for MDS plot
##' @author Robin Elahi
##' @date 2026-01-03
##' @log 
################################################################################

#### File paths ####
here::i_am("kfmp_transect_video_species/mds1.R")
library(here)
folder <- "kfmp_transect_video_species"
file_name <- "mds1"

##### PACKAGES, DATA #####
library(tidyverse)
library(readxl)
library(viridis)
library(vegan)
source(here("R", "ggplot_settings.R"))

# Load data from video analysis
dat <- read_excel("data/kfmp_transect_video_species_260103.xlsx") 
glimpse(dat)
head(dat)

##### WRANGLE DATA #####
# Remove numeric columns that contain values <= 0
d <- dat %>%
  select(where(~ !is.numeric(.) || any(. > 0)))

## Create matrix (species x sample matrix) metadata
names(d)
m_meta <- d %>% select(video:view)
m <- d %>% select(-c(video:`_sand`))
m_meta
m

## Data transformations
# m_t <- m
## Square root
# m_t <- m^(1/2)

## Run MDS
set.seed(100)
m_mds <- metaMDS(comm = m, distance = "bray", k = 2, 
                 try = 20, trymax = 100, 
                 trace = FALSE, autotransform = FALSE)
m_mds$stress

# Join MDS results to metadata
mds_xy <- data.frame(m_mds$points) 
m_meta2 <- cbind(m_meta, mds_xy)

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
