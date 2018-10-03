library(dplyr)
library(xtable)

setwd("~/docs/NPBR_paper/analysis")

rm(list = ls())

df = read.csv("rawData.csv")

df = read.csv("rawData.csv", stringsAsFactors = FALSE)
df = df %>%
  filter(year >= 1999) %>%
  arrange(rr, year)

#Define variables for estimation
indep.vars = c("quantity.capital",
               "quantity.cars",
               "quantity.locomotives",
               "quantity.fuel",
               "quantity.labor")
output.var = "revenue.ton.miles"

