library(mvnpbr)
library(dplyr)

setwd("~/docs/NPBR_paper/analysis")

rm(list = ls())

#Read and clean data
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

X.scale = apply(df[,indep.vars], 2, mean)
y.scale = mean(df$revenue.ton.miles)

# X.scale = rep(1, length(indep.vars))
# y.scale = 1

#Define price variables
price.vars = c("price.capital",
               "price.cars",
               "price.locomotives",
               "price.fuel",
               "price.labor")
output.price = "price"

#Fitting parameters
# N.fit = 4
X.fit = t(t(df[,indep.vars]) / X.scale)

#Years and firms
years = unique(df$year)
firms = unique(df$rr)

year = years[1]
print(year)
df.year = df %>% filter_(paste0("year == ", year))

y = df.year[,output.var] / y.scale
X = t(t(as.matrix(df.year[,indep.vars])) / X.scale)

H.inv = NA
# X.fit = NA
method = "mc"
H.mult = 1

# X.range = apply(X, 2, range)
# X.seq = lapply(1:ncol(X.range), function(i) seq(X.range[1,i], X.range[2,i], length.out = N.fit))
# X.seq = lapply(X.seq, function(v) v[-c(1, N.fit)])
# X.fit = as.matrix(do.call(expand.grid, X.seq))

frontier = fit.frontier.grad(X, y, X.fit, method = "m", H.mult = 1)

t(t(frontier$gradient.hat) * (y.scale / X.scale))
df.year[,price.vars] / df[,output.price]
frontier$efficiency

