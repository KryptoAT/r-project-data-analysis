# Title: WHO TB Burden Data: Incidence, Mortality, and Population
# Author: Avery Terry
# Date: 20NOV2025
# Packages: tidytuesday, knitr
# Description: Tidy Tuesday data on Tuberculosis from 2000-2023


install.packages("tidytuesdayR")
install.packages("knitr")

tuesdata <- tidytuesdayR::tt_load("2025-11-11")

# Read Data
df <- tuesdata$who_tb_data

head(df)

factor(df$year)

# Are there any years where global TB metrics show unusual spikes or drops?
summary(df$c_cdr[!is.na(df$c_cdr)])
df_summary_year <- aggregate(
    x = df[7:18],
    by = list(df$year),
    FUN = function(x) summary(x)
)
print(df_summary_year)


df_summary_year2 <- lapply(df[7:18], df$year, FUN = function(x) summary(x), simplify = TRUE)
df_sum <- as.data.frame(df_summary_year2)
colnames(df_sum)

print(df_sum)
boxplot(list(df[7:18]) ~ df$year)
boxplot(df$c_cdr ~ df$year)
library(knitr)
table <- kable(df_summary_year2, )


# How does TB mortality differ between HIV-positive and HIV-negative populations?
# Which regions show consistent high TB burden across multiple years?
