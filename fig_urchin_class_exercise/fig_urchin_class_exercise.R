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
  mutate(Urchin = ifelse(str_detect(category, "red") == TRUE, "Red urchin", "Purple urchin"), 
         Behavior = ifelse(str_detect(category, "hid") == TRUE, "Hidden", "Exposed"), 
         Forested = ifelse(str_detect(site, "Lovers") == TRUE, "Deforested", "Forested"))

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
fit2 <- lm(mean ~ count_expert * Urchin * Behavior, data = df_summary)
anova(fit2)
summary(fit2)

fit3 <- lm(mean ~ count_expert * site, data = df_summary)
anova(fit3)
summary(fit3)

AIC(fit, fit2, fit3)

##### TEST BEHAVIORAL HYPOTHESIS #####

## Summarize means per video segment
dl_obs_means <- dl %>% 
  group_by(class, date, site, Forested, video, vid_segment, category, Urchin, Behavior) %>% 
  summarise(obs_mean = mean(count))

## Summarize means across video segment observer means
means_summary <- dl_obs_means |> 
  ungroup() |> 
  group_by(Forested, Urchin, Behavior) %>% 
  summarise(mean = mean(obs_mean), 
            sd = sd(obs_mean), 
            n = n(), 
            se = sd / n, 
            ci = 1.96*se)

## Plot
behavior_cols <- c("orange", "#646464")
means_summary |> 
  ggplot(aes(Forested, mean, fill = Behavior, shape = Behavior, group = Behavior)) + 
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.05) + 
  geom_line(aes(color = Behavior)) + 
  geom_point(alpha = 1, size = 3) +
  scale_fill_manual(values = behavior_cols) + 
  scale_color_manual(values = behavior_cols) + 
  scale_shape_manual(values = c(21, 22)) +
  facet_wrap(~ Urchin, scales = "free_y") +
  labs(x = "Site", y = "Mean count per video segment")

ggsave(paste(folder, "/figs/", file_name, "_b.pdf", sep = ""), height = 3, width = 7)
ggsave(paste(folder, "/figs/", file_name, "_b.jpg", sep = ""), height = 3, width = 7)

## Log-scale
min_mean <- min(dl_obs_means$obs_mean[dl_obs_means$obs_mean != 0])
dl_obs_means <- dl_obs_means |> 
  mutate(log_mean = log(obs_mean + min_mean))

## Summarize means across video segment observer means
log_means_summary <- dl_obs_means |> 
  ungroup() |> 
  group_by(Forested, Urchin, Behavior) %>% 
  summarise(mean = mean(log_mean), 
            sd = sd(log_mean), 
            n = n(), 
            se = sd / n, 
            ci = 1.96*se)

## Plot
behavior_cols <- c("orange", "#646464")
log_means_summary |> 
  ggplot(aes(Forested, mean, fill = Behavior, shape = Behavior, group = Behavior)) + 
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci, color = Behavior), width = 0.05) + 
  geom_line(aes(color = Behavior)) + 
  geom_point(alpha = 1, size = 3) +
  scale_fill_manual(values = behavior_cols) + 
  scale_color_manual(values = behavior_cols) + 
  scale_shape_manual(values = c(21, 22)) +
  facet_wrap(~ Urchin, scales = "free_y") +
  labs(x = "Site", y = "Mean log count per video segment")

ggsave(paste(folder, "/figs/", file_name, "_c.pdf", sep = ""), height = 3, width = 7)
ggsave(paste(folder, "/figs/", file_name, "_c.jpg", sep = ""), height = 3, width = 7)

## Back-transformed plot
log_means_summary <- log_means_summary |> 
  mutate(bt_mean = exp(mean) - min_mean, 
         bt_lower = exp(mean - 1.96 * se) - min_mean, 
         bt_upper = exp(mean + 1.96 * se) - min_mean)

## Plot back-transformed
behavior_cols <- c("orange", "#646464")
log_means_summary |> 
  ggplot(aes(Forested, bt_mean, fill = Behavior, shape = Behavior, group = Behavior)) + 
  geom_errorbar(aes(ymin = bt_lower, ymax = bt_upper, color = Behavior), width = 0.05) + 
  geom_line(aes(color = Behavior)) + 
  geom_point(alpha = 1, size = 3) +
  scale_fill_manual(values = behavior_cols) + 
  scale_color_manual(values = behavior_cols) + 
  scale_shape_manual(values = c(21, 22)) +
  facet_wrap(~ Urchin, scales = "free_y") +
  labs(x = "Site", y = "Mean count per video segment")

ggsave(paste(folder, "/figs/", file_name, "_d.pdf", sep = ""), height = 3, width = 7)
ggsave(paste(folder, "/figs/", file_name, "_d.jpg", sep = ""), height = 3, width = 7)

## ANOVA - natural (residuals are not homogeneous)
m1 <- lm(obs_mean ~ Behavior * Forested * Urchin, data = dl_obs_means)
anova(m1)
plot(m1)

## ANOVA - log (much better)
m1 <- lm(log_mean ~ Behavior * Forested * Urchin, data = dl_obs_means)
anova(m1)
plot(m1)

anova_table <- tidy(aov(m1))
anova_table

## Formatted
library(kableExtra)
m1 <- aov(log_mean ~ Behavior * Forested * Urchin, data = dl_obs_means)
summary(m1)

# Format the table
formatted_table <- anova_table %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  rename(
    df = df,
    SS = `sumsq`,
    MS = `meansq`,
    F = `statistic`,
    p = `p.value`
  ) %>%
  mutate(
    # Format p-values
    p = case_when(
      p < 0.001 ~ "< 0.001",
      p < 0.01 ~ sprintf("%.3f", p),
      TRUE ~ sprintf("%.3f", p)
    ),
    # Round other values
    SS = round(SS, 2),
    MS = round(MS, 2),
    F = round(F, 2)
  )

# View in console
print(formatted_table)

# Export to CSV
write.csv(formatted_table, paste(folder, "/figs/", file_name, ".csv", sep = ""), row.names = FALSE)

