library(mvnpbr)
library(dplyr)
library(ggplot2)

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

num.years = length(years)
num.firms = length(firms)

#lists to store results
productivity.list = list()
price.ratio.list = list()
efficiency.list = list()

for (i in 1:length(years)){
  year = years[i]
  print(year)
  df.year = df %>% filter_(paste0("year == ", year))
  
  y = df.year[,output.var] / y.scale
  X = t(t(as.matrix(df.year[,indep.vars])) / X.scale)
  # X.fit = X
  
  #Concavity, doesn't work for any tested H.mult
  frontier = fit.frontier.grad(X, y, method = "mc", H.mult = 15)
  
  #Monotonicity, works but may need wider bandwidth for good results
  # frontier = fit.frontier.grad(X, y, X.fit, method = "m", H.mult = 10)
  
  #Using ks bandwidth selector
  # library(ks)
  # H = Hpi(x = X)
  
  productivity.list[[i]] = t(t(frontier$gradient.hat) * (y.scale / X.scale))
  # price.ratio.list[[i]] = t(t(df.year[,price.vars] / df.year[,output.price]) * (y.scale / X.scale))
  price.ratio.list[[i]] = df.year[,price.vars] / df.year[,output.price]
  efficiency.list[[i]] = frontier$efficiency
}

#Create data.frames
productivity.df = data.frame(rr = rep(firms, times = num.years),
                             year = rep(years, each = num.firms),
                             productivity = Reduce(rbind, productivity.list))
names(productivity.df) = c("rr", "year", sub("quantity", "productivity", indep.vars))

price.ratio.df = data.frame(rr = rep(firms, times = num.years),
                            year = rep(years, each = num.firms),
                            price.ratio = Reduce(rbind, price.ratio.list))
names(price.ratio.df) = c("rr", "year", sub("quantity", "price.ratio", indep.vars))

efficiency.df = data.frame(rr = rep(firms, times = num.years),
                           year = rep(years, each = num.firms),
                           efficiency = Reduce(c, efficiency.list))

results.df = productivity.df %>%
  inner_join(price.ratio.df, by = c("rr", "year")) %>%
  inner_join(efficiency.df, by = c("rr", "year"))

results.df$productivity.capital = results.df$productivity.capital * results.df$efficiency
results.df$productivity.cars = results.df$productivity.cars * results.df$efficiency
results.df$productivity.locomotives = results.df$productivity.locomotives * results.df$efficiency
results.df$productivity.fuel = results.df$productivity.fuel * results.df$efficiency
results.df$productivity.labor = results.df$productivity.labor * results.df$efficiency

#Plot
ggplot(results.df, aes(year, productivity.capital)) + geom_line(aes(color = rr))
ggplot(results.df, aes(year, price.ratio.capital)) + geom_line(aes(color = rr))

ggplot(results.df, aes(year, productivity.cars)) + geom_line(aes(color = rr))
ggplot(results.df, aes(year, price.ratio.cars)) + geom_line(aes(color = rr))

ggplot(results.df, aes(year, productivity.locomotives)) + geom_line(aes(color = rr))
ggplot(results.df, aes(year, price.ratio.locomotives)) + geom_line(aes(color = rr))

ggplot(results.df, aes(year, productivity.fuel)) + geom_line(aes(color = rr))
ggplot(results.df, aes(year, price.ratio.fuel)) + geom_line(aes(color = rr))

ggplot(results.df, aes(year, productivity.labor)) + geom_line(aes(color = rr))
ggplot(results.df, aes(year, price.ratio.labor)) + geom_line(aes(color = rr))

ggplot(results.df, aes(year, efficiency)) + geom_line(aes(color = rr))

#Misallocation regressions
results.df$allocation.capital = results.df$productivity.capital / results.df$price.ratio.capital
results.df$allocation.cars = results.df$productivity.cars / results.df$price.ratio.cars
results.df$allocation.locomotives = results.df$productivity.locomotives / results.df$price.ratio.locomotives
results.df$allocation.fuel = results.df$productivity.fuel / results.df$price.ratio.fuel
results.df$allocation.labor = results.df$productivity.labor / results.df$price.ratio.labor

# results.df[results.df < 0] = 0 #Some productivity estimates are numerically zero

m.firm.capital = lm(log(allocation.capital) ~ rr + 0, results.df)
m.firm.cars = lm(log(allocation.cars) ~ rr + 0, results.df)
m.firm.locomotives = lm(log(allocation.locomotives) ~ rr + 0, results.df)
m.firm.fuel = lm(log(allocation.fuel) ~ rr + 0, results.df)
m.firm.labor = lm(log(allocation.labor) ~ rr + 0, results.df)

summary(m.firm.capital)
summary(m.firm.cars)
summary(m.firm.locomotives)
summary(m.firm.fuel)
summary(m.firm.labor)

m.year.capital = lm(log(allocation.capital) ~ factor(year) + 0, results.df)
m.year.cars = lm(log(allocation.cars) ~ factor(year) + 0, results.df)
m.year.locomotives = lm(log(allocation.locomotives) ~ factor(year) + 0, results.df)
m.year.fuel = lm(log(allocation.fuel) ~ factor(year) + 0, results.df)
m.year.labor = lm(log(allocation.labor) ~ factor(year) + 0, results.df)

summary(m.year.capital)
summary(m.year.cars)
summary(m.year.locomotives)
summary(m.year.fuel)
summary(m.year.labor)

allocation.df = data.frame(year = years,
                           allocation.capital = m.year.capital$coefficients,
                           allocation.cars = m.year.cars$coefficients,
                           allocation.locomotives = m.year.locomotives$coefficients,
                           allocation.fuel = m.year.fuel$coefficients,
                           allocation.labor = m.year.labor$coefficients)

ggplot(allocation.df, aes(year, allocation.capital)) + geom_line()
ggplot(allocation.df, aes(year, allocation.cars)) + geom_line()
ggplot(allocation.df, aes(year, allocation.locomotives)) + geom_line()
ggplot(allocation.df, aes(year, allocation.fuel)) + geom_line()
ggplot(allocation.df, aes(year, allocation.labor)) + geom_line()

#Cross equation correlations:
firm.residuals = cbind(m.firm.capital$residuals,
                       m.firm.cars$residuals,
                       m.firm.locomotives$residuals,
                       m.firm.fuel$residuals,
                       m.firm.labor$residuals)
year.residuals = cbind(m.year.capital$residuals,
                       m.year.cars$residuals,
                       m.year.locomotives$residuals,
                       m.year.fuel$residuals,
                       m.year.labor$residuals)

firm.cor = cor(firm.residuals)
year.cor = cor(year.residuals)

rownames(firm.cor) = colnames(firm.cor) = indep.vars
rownames(year.cor) = colnames(year.cor) = indep.vars

firm.cor
year.cor
