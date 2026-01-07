## OBJECTIVE 1: ASSESS RELATIVE ABUNDANCE OF MANGROVE CRABS
## Unilag Stations 2023/2024 Data Analysis

# LOAD REQUIRED PACKAGES
library(tidyverse)
library(dplyr)
library(car)
library(patchwork)
library(MASS)
library(readxl)
library(janitor)
library(knitr)

## Read and Import the data for Analysis
crab_data <- read_excel('PhD Analysis/Borrows and crabs count data.xlsx',
                             sheet = 'Unilag')

crab_data |> 
  str()

crab_data |> 
  view()

## Data Cleaning and Manipulation

crab_data <- crab_data |> 
  janitor::clean_names()

crab_data$months <- factor(
  crab_data$months, 
  levels = unique(crab_data$months)
)

crab_data$season <- as.factor(crab_data$season)

## Calculate the Abundance of the crabs across the sampling stations

crab_abundance <- crab_data |> 
  group_by(season) |>
  summarise(
    station_a = sum(station_a, na.rm = TRUE),
    station_b = sum(station_b, na.rm = TRUE),
    station_c = sum(station_c, na.rm = TRUE),
    station_d = sum(station_d, na.rm = TRUE),
    station_e = sum(station_e, na.rm = TRUE),
    .groups = 'drop'
  )

crab_abundance |> 
  kable()

## Calculate Relative Abundance
crab_relative <- crab_abundance
station_cols <- c("station_a", "station_b", 
                  "station_c", "station_d", "station_e")

crab_relative <- crab_relative |>
  mutate(
    station_a = (station_a / sum(station_a)) * 100,
    station_b  = (station_b  / sum(station_b)) * 100,
    station_c = (station_c / sum(station_c)) * 100,
    station_d = (station_d / sum(station_d)) * 100,
    station_e = (station_e / sum(station_e)) * 100
  )

crab_relative |> 
  kable()

## Convert to long format for analysis

crab_long <- crab_data |> 
  pivot_longer(
    starts_with('station_'),
    names_to = 'Station',
    values_to = 'Count'
  ) |> 
  mutate(Station = gsub("Station_", "Station ", Station))


## Descriptive Statistics
overall_summary <- tibble(
  Statistic = c("Total Samples", "Mean Abundance", "SD", "Median", 
                "Min", "Max", "Range"),
  Value = c(
    nrow(crab_long),
    round(mean(crab_long$Count), 2),
    round(sd(crab_long$Count), 2),
    median(crab_long$Count),
    min(crab_long$Count),
    max(crab_long$Count),
    paste(min(crab_long$Count), "-", max(crab_long$Count))
  )
)

overall_summary |> 
  kable()

## Summary by Station

station_summary <- crab_long |> 
  group_by(Station) |> 
  summarise(
    n = n(),
    Mean = round(mean(Count), 2),
    SD = round(sd(Count), 2),
    SE = round(sd(Count)/sqrt(n()), 2),
    Median = median(Count),
    Min = min(Count),
    Max = max(Count),
    CV = round((sd(Count)/mean(Count))*100, 1)
  ) 

station_summary |> 
  kable()

## Summary by Season
season_summary <- crab_long |> 
  group_by(season) |> 
  summarise(
    n = n(),
    Mean = round(mean(Count), 2),
    SD = round(sd(Count), 2),
    SE = round(sd(Count)/sqrt(n()), 2),
    Median = median(Count),
    Min = min(Count),
    Max = max(Count)
  )

season_summary |> 
  kable()

## Summary by Station and Season
station_season_summary <- crab_long |> 
  group_by(Station, season) %>%
  summarise(
    n = n(),
    Mean = round(mean(Count), 2),
    SD = round(sd(Count), 2),
    SE = round(sd(Count)/sqrt(n()), 2),
    .groups = "drop"
  )

station_season_summary |> 
  kable()


# ============================================================================
# RESEARCH QUESTION 1: DOES ABUNDANCE DIFFER AMONG STATIONS?
# ============================================================================

## Check Normality of Data
## Station A
crab_data$station_a |> 
  shapiro.test() ## Data showed a Normal Distribution (p>0.05)

## Station B
crab_data$station_b |> 
  shapiro.test() ## Data showed a Normal Distribution (p>0.05)

## Station C
crab_data$station_c |> 
  shapiro.test() ## Data showed a Normal Distribution (p>0.05)

## Station D
crab_data$station_d |> 
  shapiro.test() ## Data showed a Normal Distribution (p>0.05)

## Station E
crab_data$station_e |> 
  shapiro.test() ## Data showed a Normal Distribution (p>0.05)

## Test of Homogeneity of Variance
## Levene Test (p>0.05) Indicate Variance are Equal

var_station <- leveneTest(Count ~ Station,
                                 data = crab_long)

var_station

if(var_station$`Pr(>F)`[1] > 0.05) {
  cat("\n  ✓ Variances are equal (p > 0.05)\n")
} else {
  cat("\n  ✗ Variances are unequal (p < 0.05)\n")
}

## One-Way Anova: To check if Abundance differs among the station
## Test of sign difference in the abundance among the stations.

anova_station <- aov(Count ~ Station, data = crab_long)

anova_station |> 
  anova()

anova_summary <- anova_station |> 
  summary()

print(anova_summary)


## Check for significant difference among each stations
## Use posthoc test

tukey_crab <- TukeyHSD(anova_station)
print(tukey_crab)


# Extract significant comparisons
tukey_df <- as.data.frame(tukey_crab$Station)
tukey_df$Comparison <- rownames(tukey_df)
tukey_df$Significant <- ifelse(tukey_df$`p adj` < 0.05, 
                               "Yes", "No")

print(tukey_df)

# ============================================================================
# RESEARCH QUESTION 2: DOES ABUNDANCE DIFFER BETWEEN SEASONS?
# ============================================================================
## Check for normality of data across seasons

## For Wet Season
wet_data <- crab_long |> 
  filter(season == 'WET') |> 
  pull(Count)

wet_data |> 
  shapiro.test() ## (p>0.05) Normality of Data assumed

## For Dry Season
dry_data <- crab_long |> 
  filter(season == 'DRY') |> 
  pull(Count)

dry_data |> 
  shapiro.test() ## (p>0.05) Normality of Data assumed

## Test of Homogeneity of Variance
## Levene Test

var_season <- leveneTest(Count ~ season, data = crab_long)

var_season
print(var_season)

## Indepedent T.test to check the difference in abundance across season

t_result <- t.test(Count ~ season, data = crab_long,
                   var.equal = TRUE)

print(t_result)

if(t_result$p.value < 0.05) {
  cat("✓ SIGNIFICANT difference between seasons (p < 0.05)\n")
  if(mean(wet_data) > mean(dry_data)) {
    cat("CONCLUSION: Wet season has significantly HIGHER abundance than dry season\n")
  } else {
    cat("CONCLUSION: Dry season has significantly HIGHER abundance than wet season\n")
  }
} else {
  cat("✗ NO significant difference between seasons (p > 0.05)\n")
  cat("CONCLUSION: Abundance is similar in both seasons\n")
}

# ============================================================================
# RESEARCH QUESTION 3: STATION × SEASON INTERACTION
# ============================================================================

anova_combined <- aov(Count ~ Station * season,
                      data = crab_long)

anova_combined |> 
  anova()

anova_combined |> 
  summary()

## Check for significant difference among each stations and seasons
## Use posthoc test

tukey_combined <- TukeyHSD(anova_combined)
print(tukey_combined)

tukey_com_df <- as.data.frame(tukey_combined$`Station:season`)
tukey_com_df$comparison <- rownames(tukey_com_df)
tukey_com_df$significant <- ifelse(tukey_com_df$`p adj` <0.05,
                                   'Yes', 'No')
print(tukey_com_df)

# ============================================================================
# STEP 4: VISUALIZATIONS
# ============================================================================

# 1. Time series plot
plot_1 <- ggplot(crab_long, aes(x = months, 
                                   y = Count, 
                                   color = Station, 
                                   group = Station)) +
  geom_line(size = 0.8) +
  geom_point(size = 3) +
  labs(title = "Monthly Abundance of Mangrove Crabs",
       subtitle = "Unilag Stations A to E",
       x = "Month",
       y = "Crab Count",
       color = "Station") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(plot_1)

# 2. Box plot by station
plot_2 <- ggplot(crab_long, aes(x = Station, 
                                 y = Count, 
                                 fill = Station)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 3) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "white", color = "black") +
  labs(title = "Mangrove Crab Abundance Distribution by Station",
       subtitle = "Box = median & quartiles; Diamond = mean",
       x = "Station",
       y = "Crab Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(plot_2)


# 3. Box plot by season
plot_3 <- ggplot(crab_long, aes(x = season, y = Count, fill = season)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 5, 
               fill = "white", color = "black") +
  scale_fill_manual(values = c("WET" = "#3498db", "DRY" = "#e67e22")) +
  labs(title = "Seasonal Variation in Mangrove Crab Abundance",
       subtitle = paste0("t = ", round(t_result$statistic, 2), 
                         ", p = ", format.pval(t_result$p.value, digits = 3)),
       x = "Season",
       y = "Crab Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Add significance bracket if significant
if(t_result$p.value < 0.05) {
  y_max <- max(crab_long$Count)
  plot_3 <- plot_3 + 
    annotate("segment", x = 1, xend = 2, y = y_max + 5, yend = y_max + 5, 
             size = 1) +
    annotate("text", x = 1.5, y = y_max + 8, 
             label = ifelse(t_result$p.value < 0.001, "***",
                            ifelse(t_result$p.value < 0.01, "**", "*")), 
             size = 8)
}

print(plot_3)


# 4. Station × Season interaction plot
plot_4 <- ggplot(station_season_summary, 
             aes(x = season, y = Mean, color = Station, group = Station)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = 'dodge',
                width = 0.25, size = 0.75, alpha = 0.5) +
  labs(title = "Station × Season Interaction",
       subtitle = "Mean abundance with standard error bars",
       x = "Season",
       y = "Mean Crab Count",
       color = "Station") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(plot_4)

# 5. Bar plot with error bars
plot_5 <- ggplot(station_summary, aes(x = Station, 
                                  y = Mean, 
                                  fill = Station)) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.1),
           width = 0.4, color = 'black', size = 0.1) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(width = 0.1),
                width = 0.25, size = 0.5, alpha = 0.3) +
  geom_text(aes(label = round(Mean, 1)), 
            vjust = -1, hjust = 1.25, size = 4, fontface = "bold", alpha = 0.2) +
  scale_fill_brewer(palette = 'Set3')+
  labs(title = "Mean Abundance of Mangrove Crab by Station",
       x = "Station",
       y = "Mean Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(plot_5)


