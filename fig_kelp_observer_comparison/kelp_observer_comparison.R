#Sept 7, 2026
#Gwendolyn Donahue
#KFMP Figure S3: Comparison of stipe counts from two undergraduate researchers.
#18-Sep-2026: small edits by RE

here::i_am("fig_kelp_observer_comparison/kelp_observer_comparison.R")
library(here)
folder <- "fig_kelp_observer_comparison"
file_name <- "kelp_observer_comparison"

##-----------------------------------------------------Load libraries----------
library(tidyverse)
source(here("R", "ggplot_settings.R"))

##-----------------------------------------------------Load data set-----------
####Notes: only using GZD kelp video count data and JD swath-adjusted Patiria video count data
df_figS3_kelpvideo <- read.csv(here("data", "KFMP_df_KelpVideoSummaryCounts_fullmetadata.csv"))

##-----------------------------------------------------Wrangle data------------
df_observereffect <- df_figS3_kelpvideo %>%
  rename(insitu_macro_individuals_count = macro_individuals,
         insitu_macro_stipes_count= macro_stipes,
         video_macro_individuals_count = video_individuals_count,
         video_macro_stipes_count = video_stipe_count) %>% 
  select(transect_id, video_recorder, video_macro_stipes_count) %>%
  pivot_wider(names_from = video_recorder,
              values_from = video_macro_stipes_count) %>% #looking at stipe counts, not individuals
  drop_na(`Maya Green`, `Gwendolyn Donahue`) %>% #only keep videos both observed
  #rename columns to preserve anonymity 
  rename(observer1 = `Maya Green`,
         observer2 = `Gwendolyn Donahue`)

##-----------------------------------------------------Simple stats------------
observerlinearmodel <- lm(observer2 ~ observer1, data = df_observereffect)
summary(observerlinearmodel)

# Does confidence interval contain 1?
confint(observerlinearmodel, "observer1", level = 0.95) # No!

# Car
library(car)
linearHypothesis(observerlinearmodel, "observer1 = 1")

# Manual t-test: H0: slope = 1
slope <- coef(observerlinearmodel)[2]
se_slope <- summary(observerlinearmodel)$coefficients[2, 2]
t_stat <- (slope - 1) / se_slope
df <- observerlinearmodel$df.residual
p_value <- 1.96 * pt(abs(t_stat), df, lower.tail = FALSE)
p_value

# Symmetric percent difference (relative to mean)
# Used when neither x nor y is the "reference"
# abs(y - x) / ((x + y) / 2) * 100
df_observereffect <- df_observereffect |> 
  mutate(per_diff = ifelse(observer1 == 0 & observer1 == 0, 0, 
                           (abs(observer2 - observer1) / ((observer1 + observer2) / 2)) * 100))

# df_observereffect <- df_observereffect |> 
#   mutate(per_diff = (ifelse(observer1 == 0 & observer1 == 0, 0, 
#                             ((observer2 - observer1) / observer1) * 100)))

df_observereffect |> 
  summarize(mean = mean(per_diff), 
            sd = sd(per_diff), 
            n = n(), 
            se = sd / sqrt(n), 
            ci = 1.96*se, 
            upper = mean + ci, 
            lower = mean - ci)

##-----------------------------------------------------Plot Supp figure S3-----
ggplot(df_observereffect,
       aes(x = observer1, y = observer2)) +
  geom_smooth(method = "lm", se = TRUE, color = kelp_col) +
  #1-1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  #jitter so the urchin barren sites can be clearly seen
  geom_jitter(size = 3, shape = 21, fill = kelp_col, alpha = 0.8, width = 4, height = 4) +
  labs(x = expression("Observer 1 " * italic("Macrocystis pyrifera") * " stipe counts"),
       y = expression("Observer 2 " * italic("Macrocystis pyrifera") * " stipe counts")) 

##-----------------------------------------------------Save plot---------------
ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 4, width = 4)
