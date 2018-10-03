library(dplyr)
library(ggplot2)

setwd("~/docs/NPBR_paper/analysis")

rm(list = ls())

#Read and clean data
df = read.csv("rawData.csv", stringsAsFactors = FALSE)
df = df %>%
  filter(year >= 1999) %>%
  arrange(rr, year)

ggplot(df, aes(year, quantity.capital)) + geom_line(aes(color = rr))
ggplot(df, aes(year, quantity.cars)) + geom_line(aes(color = rr))
ggplot(df, aes(year, quantity.locomotives)) + geom_line(aes(color = rr))
ggplot(df, aes(year, quantity.fuel)) + geom_line(aes(color = rr))
ggplot(df, aes(year, quantity.labor)) + geom_line(aes(color = rr))

ggplot(df, aes(year, price.capital)) + geom_line(aes(color = rr))
ggplot(df, aes(year, price.cars)) + geom_line(aes(color = rr))
ggplot(df, aes(year, price.locomotives)) + geom_line(aes(color = rr))
ggplot(df, aes(year, price.fuel)) + geom_line(aes(color = rr))
ggplot(df, aes(year, price.labor)) + geom_line(aes(color = rr))

ggplot(df, aes(year, revenue.ton.miles)) + geom_line(aes(color = rr))
ggplot(df, aes(year, price)) + geom_line(aes(color = rr))
