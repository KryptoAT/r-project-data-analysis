# Title: WHO TB Burden Data: Incidence, Mortality, and Population
# Author: Avery Terry
# Date: 20NOV2025
# Packages: tidytuesday, knitr
# Description: Tidy Tuesday data on Tuberculosis from 2000-2023


install.packages("tidytuesdayR")
install.packages("knitr")
install.packages("tidyverse")

tuesdata <- tidytuesdayR::tt_load("2025-11-11")

# Read Data
df <- tuesdata$who_tb_data

head(df)

# Are there any years where global TB metrics show unusual spikes or drops?
factor(df$year)

library("dplyr")

# examine percent_change
df_percent_change <- df %>%
    group_by(year) %>%
    summarise(across(colnames(df[7:18]), mean, na.rm = TRUE)) %>%
    mutate(across(colnames(df[7:18]), ~ ((.x - lag(.x)) / lag(.x)) * 100))


# 2019 -> 2020 show a few metrics with a drop c_cdr, c_newinc_100k

# How does TB mortality differ between HIV-positive and HIV-negative populations?
library(tidyverse)
df_hiv <- df %>%
    mutate(e_mort_tbhiv_neg = e_mort_num - e_mort_tbhiv_num) %>%
    select(year, g_whoregion, e_mort_tbhiv_num, e_mort_tbhiv_neg) %>%
    group_by(g_whoregion) %>%
    summarise(across(starts_with("e_mort"), ~ mean(.x, na.rm = TRUE), .names = "{.col}")) %>%
    pivot_longer(!g_whoregion, names_to = "hiv_status", values_to = "e_mort")

library(ggplot2)

ggplot(df_hiv, aes(x = g_whoregion, y = e_mort, fill = hiv_status)) +
    geom_col(position = "dodge") +
    labs(x = "WHO Regions", y = "Mean Estimated number of deaths from TB")

# HIV positive populations

# Which regions show consistent high TB burden across multiple years?


# goals tomorrow finish this
# Create tables showing summary by year
# create 1 graphical output
