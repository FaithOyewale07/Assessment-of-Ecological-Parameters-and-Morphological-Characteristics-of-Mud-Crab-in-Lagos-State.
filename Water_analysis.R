## Load the necesssary packages for the analysis
library(tidyverse)
library(dplyr)
library(car)
library(ggpubr)
library(readxl)
library(gridExtra)
library(knitr)
library(corrplot)
library(reshape2)

## Import the dataset for analysis
water_data <- read_excel('PhD Analysis/WaterParameterLocation_1.xlsx', 
                          sheet = 3)

water_data |> 
  str()
water_data |> 
  View()

water_data |> 
  glimpse()

water_data <- water_data |> 
  janitor::clean_names()

water_data |> 
  colnames()

water_data <- water_data |> 
  mutate(
    months = as.factor(months),
    station = as.factor(station),
    season_2023_2024 = as.factor(season_2023_2024)
  )

water_data$months <- factor(water_data$months, 
                            levels = unique(water_data$months))

water_data$station <- factor(water_data$station, 
                             levels = unique(water_data$station))

## Check for Normality of Temp, Salinity, dissolved Oxygen, and pH against
## the stations and the seasons.
## For Temperature

temp_model <- lm(water_temp_0c ~ station * season_2023_2024, 
                 data = water_data)

temp_resd <- residuals(object = temp_model)

temp_resd |> 
  shapiro.test()

## For Salinity

salin_model <- lm(salinity_0_00 ~ station * season_2023_2024, 
                  data = water_data)

salin_resd <- residuals(object = salin_model)

salin_resd |> 
  shapiro.test()

shapiro.test(water_data$salinity_0_00)

## For Dissolved Oxygen

do_model <- lm(do_mg_l ~ station * season_2023_2024,
               data = water_data)

do_resd <- residuals(object = do_model)

do_resd |> 
  shapiro.test()

## For pH Levels

ph_model <- lm(p_h ~ station * season_2023_2024, data = water_data)

ph_resd <- residuals(object = ph_model)

ph_resd |> 
  shapiro.test()


## Homogeneity of variance for the temperature across the stations 
## and seasons.

temp_var <- leveneTest(water_temp_0c ~ station * season_2023_2024,
                        data = water_data)

temp_var |> 
  kable()

## For Dissolved Oxygen
do_var <- leveneTest(do_mg_l ~ station * season_2023_2024,
                       data = water_data)

do_var |> 
  kable()

## For pH 
ph_var <- leveneTest(p_h ~ station * season_2023_2024,
                       data = water_data)

ph_var |> 
  kable()

## For Salinity
salin_var <- leveneTest(salinity_0_00 ~ station * season_2023_2024,
                       data = water_data)

salin_var |> 
  kable()
## For Total Dissolved Solids
tds_var <- leveneTest(tds_mg_l ~ station * season_2023_2024,
                       data = water_data)

tds_var |> 
  kable()



## Check the difference in temp, ph, salinity, total dissolved solid across all 
## four station for the first season.

temp_anova <- aov(water_temp_0c ~ station * season_2023_2024, data = 
                    water_data)
temp_anova |> 
  summary()

salinity_anova <- aov(salinity_0_00 ~ station * season_2023_2024,
                      data = water_data)
salinity_anova |> 
  anova() |> 
  as.data.frame()

ph_anova <- aov(p_h ~ station * season_2023_2024, data = water_data)
ph_anova |> 
  summary()
ph_anova |> 
  anova()

## Exploratory data analysis

water_summary <- water_data |> 
  group_by(station, season_2023_2024) |> 
  summarise(
    n = n(),
    mean_temp = round(mean(water_temp_0c), 2),
    mean_oxygen = round(mean(do_mg_l), 2),
    mean_salinity = round(mean(salinity_0_00), 2),
    mean_ph = round(mean(p_h), 2),
    sd_temp = round(sd(water_temp_0c), 2),
    sd_oxygen = round(sd(do_mg_l), 2),
    sd_salinity = round(sd(salinity_0_00), 2),
    sd_ph = round(sd(p_h), 2),
    se_temp = round(sd(water_temp_0c)/sqrt(n()), 2),
    se_oxygen = round(sd(do_mg_l)/sqrt(n()), 2),
    se_salinity = round(sd(salinity_0_00)/sqrt(n()), 2),
    se_ph = round(sd(p_h)/sqrt(n()), 2),
    .groups = 'drop'
  )

water_summary |> 
  view()

water_pivot <- water_data |> 
  pivot_longer(
    cols = c(water_temp_0c, p_h, do_mg_l, tds_mg_l,
             transparency_m, salinity_0_00, conductivity_u_s_cm),
    names_to = 'Parameter',
    values_to = 'Value'
  ) |> 
  group_by(station, season_2023_2024, Parameter) |> 
  summarise(
    mean_val = round(mean(Value, na.rm = TRUE), 2),
    se = round(sd(Value, na.rm = TRUE)/ sqrt(n()), 2)
  )

water_pivot |> 
  view()

## Seasonal Variation across different Physicochemical Parameters for Temperature
temp_plot <- water_pivot |> 
  filter(Parameter == 'water_temp_0c') 

temp1 <- temp_plot |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.25,alpha = 0.5, size = 3)+
  scale_fill_manual(values = c(
    'DRY' = '#4682B4',
    'WET' = '#567A4A'
  ))+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Water Temperature',
    title = 'Seasonal Variation of Temperature 
    across the Sampling Stations in Unilag Location'
  )+
  theme_bw()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.1)
  )

## Seasonal Variation of dissolved oxygen across the five sampling points.

oxygen_plot <- water_pivot |> 
  filter(Parameter == 'do_mg_l')

oxygen1 <- oxygen_plot |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), 
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.25,alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682B4',
      'WET' = '#567A4A'
    ))+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Dissolved Oxygen',
    title = 'Seasonal Variation of Oxygen 
    across the Sampling Stations in Unilag Location'
  )+
  theme_bw()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )

## Season Variation for SALINITY across the Sampling Stations
salin_plot <- water_pivot |> 
  filter(Parameter == 'salinity_0_00')

salin1 <- salin_plot |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.5,alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682B4',
      'WET' = '#567A4A'
    ))+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Salinity Levels',
    title = 'Seasonal Variation of Salinity Level  
    across the Sampling Stations in Unilag Location'
  )+
  theme_bw()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )

## Seasonal Variation for Total Dissolved solids across the Sampling Stations

tds_plot <- water_pivot |> 
  filter(Parameter == 'tds_mg_l')

tds1 <- tds_plot |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.25,alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682B4',
      'WET' = '#567A4A'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Total Dissolved Solid',
    title = 'Seasonal Variation of Total Dissolved Solids  
    across the Sampling Stations in Unilag Location'
  )+
  theme_bw()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )  
  

## Seasonal Variation of Transparency across all Sampling Stations.

trans_plot <- water_pivot |> 
  filter(Parameter == 'transparency_m')

trans1 <- trans_plot |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7,color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.25,alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682B4',
      'WET' = '#567A4A'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Water Transparency',
    title = 'Seasonal Variation of Water Transperancy 
    across the Sampling Stations in Unilag Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )


## Seasonal Variation for pH across the Five Sampling Stations.

ph_plot <- water_pivot |> 
  filter(Parameter == 'p_h')

ph1 <- ph_plot |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.4,alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'pH Levels',
    title = 'Seasonal Variation of pH levels 
    across the Sampling Stations in Unilag Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )

## Seasonal Variation for Conductivity across the five sampling stations
cond_phy <- water_pivot |> 
  filter(Parameter == 'conductivity_u_s_cm')

EC_1 <- cond_phy |> 
  ggplot(aes(station, mean_val, fill = season_2023_2024, label = mean_val))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust = 1.4,alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Conductivity',
    title = 'Seasonal Variation of EC 
    across the Sampling Stations in Unilag Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )


## Display the different Plots

print(temp1)
print(ph1) 
print(salin1) 
print(trans1) 
print(tds1) 
print(oxygen1)
print(EC_1)


## Combine all Plot into into a grid layout

grid.arrange(temp1, ph1, salin1, trans1, tds1, oxygen1, ncol = 2)


## For the Second Location of the Research Study

physico_data <- read_excel('PhD Analysis/WaterParameterLocation_1.xlsx',
                           sheet = 4)

physico_data |> 
  view()

physico_data |> 
  str()

physico_data <- physico_data |> 
  janitor::clean_names()

physico_data |> 
  glimpse()

physico_data <- physico_data |>
  mutate(
    station = as.factor(station),
    season_2023_2024 = as.factor(season_2023_2024),
    months = as.factor(months)
  )

physico_data$months <- factor(physico_data$months, 
                              levels = unique(physico_data$months))

physico_data$station <- factor(physico_data$station,
                               levels = unique(physico_data$station))
## Test of Homogenity
physico_levene_temp <- leveneTest(water_temp_0c ~ station * season_2023_2024,
                                  data = physico_data)

physico_levene_temp |> 
  summary()
## Check for Normality of Temp, Salinity, dissolved Oxygen, and pH against
## the stations and the seasons.
## For Temperature
temp2_model <- lm(water_temp_0c ~ station * season_2023_2024,
                  data = physico_data)
temp2_resd <- residuals(object = temp2_model)

temp2_resd |> 
  shapiro.test()

## For Dissolved Oxygen
do2_model <- lm(do_mg_l ~ station * season_2023_2024,
                data = physico_data)
do2_resd <- residuals(object = do2_model)

do2_resd |> 
  shapiro.test()

## For Salinity
salin2_model <- lm(salinity_0_00 ~ station * season_2023_2024,
                   data = physico_data)
salin2_resd <- residuals(object = salin2_model)

salin2_resd |> 
  shapiro.test()
## For pH
ph2_model <- lm(p_h ~ station * season_2023_2024,
                data = physico_data)

ph2_resd <- residuals(object = ph2_model)

shapiro.test(ph2_resd)

physico_data |> 
  ggplot(aes(do_mg_l))+
  geom_density()


## Check  for significant difference in the mean of temperature
## across all the sampling stations using two way anova.

temp2_anova <- aov(water_temp_0c ~ station * season_2023_2024,
                   data = physico_data)

temp2_anova |> 
  anova()

## For pH
ph2_anova <- aov(p_h ~ station * season_2023_2024, 
                 data = physico_data)
ph2_anova |> 
  anova()

## For dissolved oxygen
do2_anova <- aov(do_mg_l ~ station * season_2023_2024,
                 data = physico_data)

do2_anova |> 
  anova()


## Plotting of Graphs and Exploratory data analysis
physico_summary <- physico_data |> 
  group_by(station, season_2023_2024) |> 
  summarise(
    mean_temp2 = round(mean(water_temp_0c), 2),
    sd_temp2 = round(sd(water_temp_0c), 2),
    se_temp2 = round(sd(water_temp_0c)/sqrt(n()), 2),
    mean_do2 = round(mean(do_mg_l), 2),
    sd_do2 = round(sd(do_mg_l), 2),
    se_do2 = round(sd(do_mg_l)/sqrt(n()), 2),
    mean_salin2 = round(mean(salinity_0_00), 2),
    sd_salin2 = round(sd(salinity_0_00),2),
    se_salin2 = round(sd(salinity_0_00)/sqrt(n()), 2),
    mean_ph2 = round(mean(p_h), 2),
    sd_ph2 = round(sd(p_h), 2),
    se_ph2 = round(sd(p_h)/sqrt(n()), 2),
    mean_tds = round(mean(tds_mg_l), 2),
    sd_tds = round(sd(tds_mg_l), 2),
    se_tds = round(sd(tds_mg_l)/sqrt(n()), 2),
    mean_trans = round(mean(transparency_m), 2),
    sd_trans = round(sd(transparency_m), 2),
    se_trans = round(sd(transparency_m)/sqrt(n()), 2),
    mean_ec = round(mean(conductivity_u_s_cm), 2),
    sd_ec = round(sd(conductivity_u_s_cm), 2),
    se_ec = round(sd(conductivity_u_s_cm)/sqrt(n()), 2)
  )
 

physico_summary |> 
  View()

temp_phy <- physico_summary |> 
  ggplot(aes(station, mean_temp2, fill = season_2023_2024,  label = mean_temp2))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_temp2 - se_temp2, ymax = mean_temp2 + se_temp2),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust= 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Temperature',
    title = 'Seasonal Variation of Temperature 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )

do_phy <- physico_summary |> 
  ggplot(aes(station, mean_do2, fill = season_2023_2024, label = mean_do2))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_do2 - se_do2, ymax = mean_do2 + se_do2),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust= 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values = c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Dissolved Oxygen Levels',
    title = 'Seasonal Variation of Dissolved Oxygen 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )


ph_phy <- physico_summary |> 
  ggplot(aes(station, mean_ph2, fill = season_2023_2024, label = mean_ph2))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_ph2 - se_ph2, ymax = mean_ph2 + se_ph2),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust= 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values =  c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'pH Levels',
    title = 'Seasonal Variation of pH 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    )

salin_phy <- physico_summary |> 
  ggplot(aes(station, mean_salin2, fill = season_2023_2024, label = mean_salin2))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_salin2 - se_salin2, ymax = mean_salin2 + se_salin2),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust= 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values =  c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Salinity Levels',
    title = 'Seasonal Variation of Salinity 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
    )

tds_phy <- physico_summary |> 
  ggplot(aes(station, mean_tds, fill = season_2023_2024, label = mean_tds))+
  geom_bar(stat = 'identity', position =  position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_tds - se_tds, ymax = mean_tds + se_tds),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust= 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values =  c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Total Dissolved Solids',
    title = 'Seasonal Variation of Total Dissolved Solids 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
    )

trans_phy <- physico_summary |> 
  ggplot(aes(station, mean_trans, fill = season_2023_2024, label = mean_trans))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_trans - se_trans, ymax = mean_trans + se_trans),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust = -1, hjust= 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values =  c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Water Transparency',
    title = 'Seasonal Variation of Water Transparency 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )

ec_phy <- physico_summary |> 
  ggplot(aes(station, mean_ec, fill = season_2023_2024, label = mean_ec))+
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8),
           width = 0.7, color = 'black', size = 0.3)+
  geom_errorbar(aes(ymin = mean_ec - se_ec, ymax = mean_ec + se_ec),
                position = position_dodge(width = 0.8),
                width = 0.25, size = 0.75, alpha = 0.5)+
  geom_text(position = position_dodge(width = 0.8), vjust= -1, hjust = 1.35, alpha = 0.5, size = 3)+
  scale_fill_manual(
    values =  c(
      'DRY' = '#4682b4',
      'WET' = '#567a4a'
    )
  )+
  labs(
    x = 'Selected Sampling Stations',
    y = 'Water Conductivity',
    title = 'Seasonal Variation of Water Conductivity 
    across the Sampling Stations in NT Location'
  )+
  theme_minimal()+
  theme(
    legend.position = 'right',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = 'gray90', size = 0.3),
    panel.border = element_rect(color = 'black',  fill =NA, size = 0.5)
  )


  print(do_phy)
  print(temp_phy)
  print(salin_phy)
  print(ph_phy)
  print(tds_phy)
  print(trans_phy)
  print(ec_phy)

  grid.arrange(do_phy, temp_phy, salin_phy, ph_phy, tds_phy,
               trans_phy, ncol = 2)

 physico_data$season_2023_2024 = as.numeric(physico_data$season_2023_2024)
  



print(round(x_table1, 3))


corrplot(x2, method = 'color', type = 'lower',
         tl.col = 'black', tl.srt = 90,
         addCoef.col = 'black', number.cex = 0.6,
         title = 'Wet Season Correlation', mar = c(0,0,0,0))
cor.test(physico_data$water_temp_0c, physico_data$do_mg_l, 
         method = 'pearson')


wet_season <- physico_data[, -1] |>
  select(water_temp_0c, do_mg_l, salinity_0_00, transparency_m, p_h,                
         tds_mg_l, conductivity_u_s_cm, station, season_2023_2024) |> 
  filter(season_2023_2024 == 'WET') 

y1 <- physico_data |>
  select(water_temp_0c, do_mg_l, salinity_0_00, transparency_m, p_h,                
         tds_mg_l, conductivity_u_s_cm, station, season_2023_2024) |> 
  filter(season_2023_2024 == 'WET') 
x2 <- cor(y1, use = 'complete.obs')

y1$season_2023_2024 <- as.numeric(y1$season_2023_2024)
y1$station <- as.numeric(y1$station)

x_tzble1 <- cor(wet_season, use = 'complete.obs', method = 'pearson')


dry_season2 <- water_data |>
  select(water_temp_0c, do_mg_l, salinity_0_00, transparency_m, p_h,                tds_mg_l, conductivity_u_s_cm, station, season_2023_2024) |> 
  filter(season_2023_2024 == 'DRY') 

dry_season2$season_2023_2024 <- as.numeric(dry_season2$season_2023_2024)
dry_season2$station <- as.numeric(dry_season2$station)

x4 <- cor(dry_season2, use = 'complete.obs')

corrplot(x4, method = 'color', type = 'lower',
         tl.col = 'black', tl.srt = 90,
         addCoef.col = 'black', number.cex = 0.5,
         title = 'Dry Season Correlation', mar = c(0,-0,-0,0))

w1 <- water_data |>
  select(water_temp_0c, do_mg_l, salinity_0_00, transparency_m, p_h,                tds_mg_l, conductivity_u_s_cm, station, season_2023_2024) |> 
  filter(season_2023_2024 == 'WET') 

w1$season_2023_2024 <- as.numeric(w1$season_2023_2024)
w1$station <- as.numeric(w1$station)

x5 <- cor(w1, use = 'complete.obs')


corrplot(x5, method = 'color', type = 'lower',
         tl.col = 'black', tl.srt = 90,
         addCoef.col = 'black', number.cex = 0.5,
         title = 'Wet Season Correlation', mar = c(0,0,0,0))

y1 <- physico_data |>
  select(water_temp_0c, do_mg_l, salinity_0_00, transparency_m, p_h,                tds_mg_l, conductivity_u_s_cm, station, season_2023_2024) |> 
  filter(season_2023_2024 == 'WET') 

y1$season_2023_2024 <- as.numeric(y1$season_2023_2024)
y1$station <- as.numeric(y1$station)

x2 <- cor(y1, use = 'complete.obs')

corrplot(x2, method = 'color', type = 'lower',
         tl.col = 'black', tl.srt = 90,
         addCoef.col = 'black', number.cex = 0.5,
         title = 'Wet Season Correlation', mar = c(0,0,0,0))

dry_season1 <- physico_data |>
  select(water_temp_0c, do_mg_l, salinity_0_00, transparency_m, p_h,                tds_mg_l, conductivity_u_s_cm, station, season_2023_2024) |> 
  filter(season_2023_2024 == 'DRY') 

dry_season1$season_2023_2024 <- as.numeric(dry_season1$season_2023_2024)
dry_season1$station <- as.numeric(dry_season1$station)

x3 <- cor(dry_season1, use = 'complete.obs')

corrplot(x3, method = 'color', type = 'lower',
         tl.col = 'black', tl.srt = 90,
         addCoef.col = 'black', number.cex = 0.5,
         title = 'Dry Season Correlation', mar = c(0,0,0,0))
