################################################################################
##' @title Analyze class urchin data
##' @author Robin Elahi
##' @date 2026-07-29
##' @log 
################################################################################

#### File paths ####
here::i_am("fig_urchin_class_exercise/fig_urchin_class_exercise.R")
library(here)
folder <- "fig_urchin_class_exercise"
file_name <- "fig_urchin_class_exercise"

##### PACKAGES, DATA #####
library(tidyverse)
library(here)
library(readxl)
library(broom)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank()))

d <- read_xlsx(here("data", "urchin_exercise_class_data_230518.xlsx"))

dl <- d %>% 
  pivot_longer(purple_hidden:red_exposed, names_to = "category", values_to = "count")

dl <- dl |> 
  mutate(Urchin = ifelse(str_detect(category, "red") == TRUE, "red", "purple"), 
         Behavior = ifelse(str_detect(category, "hid") == TRUE, "hidden", "exposed"))

dl_student <- dl %>% filter(your_initials != "Robin")
dl_expert <- dl %>% filter(your_initials == "Robin")

##### SUMMARISE DATA #####

student_df <- dl_student %>% 
  group_by(class, date, site, video, vid_segment, category, Urchin, Behavior) %>% 
  summarise(mean = mean(count), 
            sd = sd(count), 
            n = n(), 
            se = sd / n, 
            ci = 1.96*se)

dl_expert2 <- dl_expert |> 
  select(class:instructor, vid_segment, category, Urchin, Behavior, count) |> 
  rename(count_expert = count)

df_summary <- left_join(student_df, dl_expert2) |> ungroup()

##### PLOT DATA #####

urchin_cols <- c("purple", "red")

df_summary |> 
  ggplot(aes(count_expert, mean, fill = Urchin, shape = Behavior)) +
  geom_smooth(aes(fill = NULL, shape = NULL), 
              method = "lm", color = "gray20", linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0) + 
  geom_point(alpha = 1, size = 2) +
  scale_fill_manual(values = urchin_cols) + 
  scale_shape_manual(values = c(21, 22)) +
  guides(fill = guide_legend(override.aes = list(shape = 21)), 
         shape = guide_legend(override.aes = list(fill = "gray"))) +
  theme(legend.key = element_rect(fill = NA)) +
  labs(x = "Expert count", y = "Mean student count")

ggsave(paste(folder, "/figs/", file_name, "_a.pdf", sep = ""), height = 3, width = 4)
ggsave(paste(folder, "/figs/", file_name, "_a.jpg", sep = ""), height = 3, width = 4)

##### LM REGRESSION - COMPLETE POOLING #####

df_summary <- df_summary |> ungroup()
fit <- lm(mean ~ count_expert, data = df_summary)
anova(fit)
summary(fit)
tidy(fit) |> 
  mutate(upper = estimate + 1.96 * std.error, 
         lower = estimate - 1.96 * std.error)
glance(fit)
plot(fit)
summary(fit)$r.squared

# Plot resids vs observed
# Note that variance does increase a bit at higher observed values
# Ok for our purposes
df_summary$resids <- residuals(fit)
df_summary |> 
  ggplot(aes(count_expert, resids, fill = Urchin, shape = Behavior)) + 
  geom_hline(aes(yintercept = 0), color = "gray") + 
  geom_point(alpha = 1, size = 2) + 
  scale_fill_manual(values = urchin_cols) + 
  scale_shape_manual(values = c(21, 22)) + 
  guides(fill = guide_legend(override.aes = list(shape = 21)), 
         shape = guide_legend(override.aes = list(fill = "gray"))) +
  theme(legend.key = element_rect(fill = NA)) +
  scale_y_continuous(limits = \(x) c(-max(abs(x)), max(abs(x)))) + 
  labs(x = "Observed", y = "Residuals")

##### LM REGRESSION - CHECK FOR INTERACTIONS #####

## Interactions
fit2 <- lm(mean ~ count_expert * Urchin * Behavior, data = summary_df)
anova(fit2)
summary(fit2)

fit3 <- lm(mean ~ count_expert * site, data = summary_df)
anova(fit3)
summary(fit3)

AIC(fit, fit2, fit3)
