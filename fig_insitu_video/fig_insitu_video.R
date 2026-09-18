#Sept 7, 2026
#Gwendolyn Donahue
#KFMP Figure 3: Comparison of video observer counts and in situ counts of Macrocystis pyrifera stipes and Patiria miniata individuals
#18-Sep-2026: small edits by RE

here::i_am("fig_insitu_video/fig_insitu_video.R")
library(here)
folder <- "fig_insitu_video"
file_name <- "fig_insitu_video"

##-----------------------------------------------------Load libraries----------
library(tidyverse)
source(here("R", "ggplot_settings.R"))
library(car)

##-----------------------------------------------------Load data set-----------
####Notes: only using GZD kelp video count data and JD swath-adjusted Patiria video count data
df_fig3_kelpstars_combined <- read.csv(here("data", "KFMP_df_KelpStars_insitu_videoGZDJD_counts.csv"))

##-----------------------------------------------------Data set adjustments----
df_select_kelp_data <- df_fig3_kelpstars_combined %>%
  select(insitu_macro_stipes_count, video_macro_stipes_count) %>%
  mutate(group = "Macrocystis Stipe Counts")
df_select_star_data <- df_fig3_kelpstars_combined %>%
  select(patiria_adjusted, patiria_min_vid) %>%
  rename(insitu_count = patiria_adjusted, video_count = patiria_min_vid) %>%
  mutate(group = "Patiria Counts (Adjusted)")

df_fig3_select_combined_data <- bind_rows(
  df_select_kelp_data %>% 
    rename(insitu_count = insitu_macro_stipes_count, 
           video_count = video_macro_stipes_count),
  df_select_star_data) %>% 
  drop_na()

df_fig3_select_combined_data |> count(group)
df_kelp <- df_fig3_select_combined_data |> filter(group == "Macrocystis Stipe Counts")
df_star <- df_fig3_select_combined_data |> filter(group == "Patiria Counts (Adjusted)")

##-----------------------------------------------------Stats-------------------
models <- df_fig3_select_combined_data %>%
  group_by(group) %>%
  do(model = lm(video_count ~ insitu_count, data = .))

pullstats <- df_fig3_select_combined_data %>%
  group_by(group) %>%
  do(model = lm(video_count ~ insitu_count, data = .)) %>%
  summarize(group = first(group),
            slope = summary(model)$coefficients["insitu_count", "Estimate"], 
            se_slope = summary(model)$coefficients["insitu_count", "Std. Error"], 
            intercept = summary(model)$coefficients["(Intercept)", "Estimate"], 
            se_intercept = summary(model)$coefficients["(Intercept)", "Std. Error"], 
            R2 = summary(model)$r.squared,
            p_value = summary(model)$coefficients[2, 4])
print(pullstats)

# Example with zeros
x <- c(0, 0, 10, 20, 0, 15)
y <- c(0, 5, 12, 20, 0, 18)
m1 <- lm(y ~ x)
summary(m1)$coefficients

# Does intercept differ from a specific value (e.g., 0)?
m1 <- lm(video_count ~ insitu_count, data = df_kelp)
linearHypothesis(m1, "insitu_count = 1")
linearHypothesis(m1, "(Intercept) = 0")

# Stars
m1 <- lm(video_count ~ insitu_count, data = df_star)
linearHypothesis(m1, "insitu_count = 1")
linearHypothesis(m1, "(Intercept) = 0")

##-----------------------------------------------------Plot--------------------
ggplot(df_fig3_select_combined_data, 
                       aes(x = insitu_count, 
                           y = video_count)) +
  geom_smooth(method = "lm", se = TRUE, aes(color = group)) +
  #1-1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_point(fill = ifelse(df_fig3_select_combined_data$group == "Macrocystis Stipe Counts",
                           "#636B2F", #kelp color
                           "#FF5000"), #Patiria color
             shape = 21, size = 3, alpha = 0.7) + #opacity
  facet_wrap(~ group, scales = "free") +
  labs(x = expression(italic("In situ") ~ "counts"),
       y = "Video counts") +
  scale_color_manual(values = c("Macrocystis Stipe Counts" = "#636B2F",
                                "Patiria Counts (Adjusted)" = "#FF5000")) +
  #plot a and b labels
  geom_text(data = data.frame(
    group = c("Macrocystis Stipe Counts", "Patiria Counts (Adjusted)"),
    label = c("A", "B"),
    insitu_count = -Inf, video_count = Inf),
    aes(x = insitu_count, y = video_count, label = label),
    hjust = -0.5, #right - left shifting (more - = farther right)
    vjust = 1.5,
    inherit.aes = FALSE,
    size = 5, fontface = "bold") +
  theme(legend.position = "none", strip.text = element_blank()) #hide titles


##-----------------------------------------------------Save plot----------------
ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 3.5, width = 7)

