## Load the necessary packages for the analysis
library(tidyverse)
library(car)
library(ggpubr)
library(devtools)
library(readxl)

## import the data for the correlation analysis

tree_data <- read_csv('Rprogramming Text/trees.csv')


## Inspect the dataset for better understanding
tree_data |> 
  view()

tree_data |> 
  str()

tree_data |> 
  colnames()

tree_data |> 
  glimpse()
## Format the dataset using Janitor package

tree_data <- tree_data |> 
  janitor::clean_names()

## Conduct Test of Normality: To check the distribution of the data

tree_data$girth_in |> 
  shapiro.test()

tree_data$height_ft |> 
  shapiro.test()

## Correlation Analysis
## Two-tailed correlation analysis

corr_data <- cor.test(tree_data$girth_in, tree_data$height_ft,
                 method = 'pearson')


corr_data |> 
  view()
