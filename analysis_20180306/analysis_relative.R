library(mvnpbr)
library(dplyr)
library(ggplot2)

setwd("~/docs/NPBR_paper/analysis")

rm(list = ls())

H.inv = NA
H.mult = 1
method = "mc"

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

#Define price variables
price.vars = c("price.capital",
               "price.cars",
               "price.locomotives",
               "price.fuel",
               "price.labor")
output.price = "price"

#Scaling factors
X.scale = apply(df[,indep.vars], 2, mean)
y.scale = mean(df$revenue.ton.miles)

X.constrained = t(t(as.matrix(df[,indep.vars])) / X.scale)

#Fitting parameters
N.fit = 3

#Years and firms
years = unique(df$year)
firms = unique(df$rr)

num.years = length(years)
num.firms = length(firms)

#lists to store results
productivity.list = list()
price.ratio.list = list()
efficiency.list = list()

# i = 6
# asdf
for (i in 1:length(years)){
  year = years[i]
  print(year)
  df.year = df %>% filter_(paste0("year == ", year))
  
  # y = df.year[,output.var]
  # X = as.matrix(df.year[,indep.vars])
  
  y = df.year[,output.var] / y.scale
  X = t(t(as.matrix(df.year[,indep.vars])) / X.scale)
  
  X.bounded = X
  y.bounded = y
  
  reflected.data = reflect.data(X, y)
  X.eval = reflected.data$X.reflected
  y.eval = reflected.data$y.reflected
  
  X.fit = X
  y.fit.observed = y
  
  # X.constrained = X.bounded
  X.constrained = apply(X, 2, function(v) seq(min(v), max(v), length.out = N.fit))
  # X.constrained = as.matrix(do.call(expand.grid, as.list(as.data.frame(X.constrained))))
  
  frontier = fit.frontier(X.eval, y.eval, X.bounded, y.bounded, X.constrained, X.fit, y.fit.observed, method = "mc", H.mult = 1)
  
  # productivity.list[[i]] = frontier$gradient.fit
  productivity.list[[i]] = t(t(frontier$gradient.fit) * (y.scale / X.scale))
  price.ratio.list[[i]] = df.year[,price.vars] / df.year[,output.price]
  efficiency.list[[i]] = frontier$efficiency
  
  # asdf
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

results.df$relative.productivity.cars = results.df$productivity.cars / results.df$productivity.capital
results.df$relative.productivity.locomotives = results.df$productivity.locomotives / results.df$productivity.capital
results.df$relative.productivity.fuel = results.df$productivity.fuel / results.df$productivity.capital
results.df$relative.productivity.labor = results.df$productivity.labor / results.df$productivity.capital

results.df$relative.price.cars = results.df$price.ratio.cars / results.df$price.ratio.capital
results.df$relative.price.locomotives = results.df$price.ratio.locomotives / results.df$price.ratio.capital
results.df$relative.price.fuel = results.df$price.ratio.fuel / results.df$price.ratio.capital
results.df$relative.price.labor = results.df$price.ratio.labor / results.df$price.ratio.capital

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
results.df$allocation.cars = results.df$relative.productivity.cars / results.df$relative.price.cars
results.df$allocation.locomotives = results.df$relative.productivity.locomotives / results.df$relative.price.locomotives
results.df$allocation.fuel = results.df$relative.productivity.fuel / results.df$relative.price.fuel
results.df$allocation.labor = results.df$relative.productivity.labor / results.df$relative.price.labor

m.firm.cars = lm(log(allocation.cars) ~ rr + 0, results.df)
m.firm.locomotives = lm(log(allocation.locomotives) ~ rr + 0, results.df)
m.firm.fuel = lm(log(allocation.fuel) ~ rr + 0, results.df)
m.firm.labor = lm(log(allocation.labor) ~ rr + 0, results.df)

summary(m.firm.cars)
summary(m.firm.locomotives)
summary(m.firm.fuel)
summary(m.firm.labor)

m.year.cars = lm(log(allocation.cars) ~ factor(year) + 0, results.df)
m.year.locomotives = lm(log(allocation.locomotives) ~ factor(year) + 0, results.df)
m.year.fuel = lm(log(allocation.fuel) ~ factor(year) + 0, results.df)
m.year.labor = lm(log(allocation.labor) ~ factor(year) + 0, results.df)

summary(m.year.cars)
summary(m.year.locomotives)
summary(m.year.fuel)
summary(m.year.labor)


# allocation.firm.df = data.frame(firm = firms,
#                                 allocation.cars = exp(m.firm.cars$coefficients),
#                                 allocation.locomotives = exp(m.firm.locomotives$coefficients),
#                                 allocation.fuel = exp(m.firm.fuel$coefficients),
#                                 allocation.labor = exp(m.firm.labor$coefficients))

# allocation.firm.df = data.frame(firm = firms,
#                                 allocation.cars = (m.firm.cars$coefficients),
#                                 allocation.locomotives = (m.firm.locomotives$coefficients),
#                                 allocation.fuel = (m.firm.fuel$coefficients),
#                                 allocation.labor = (m.firm.labor$coefficients))
# 
# ggplot(allocation.firm.df, aes(firm, allocation.cars)) + geom_bar(stat = "identity")
# ggplot(allocation.firm.df, aes(firm, allocation.locomotives)) + geom_bar(stat = "identity")
# ggplot(allocation.firm.df, aes(firm, allocation.fuel)) + geom_bar(stat = "identity")
# ggplot(allocation.firm.df, aes(firm, allocation.labor)) + geom_bar(stat = "identity")

# allocation.time.df = data.frame(year = years,
#                            allocation.cars = exp(m.year.cars$coefficients),
#                            allocation.locomotives = exp(m.year.locomotives$coefficients),
#                            allocation.fuel = exp(m.year.fuel$coefficients),
#                            allocation.labor = exp(m.year.labor$coefficients))

allocation.time.df = data.frame(year = years,
                                allocation.cars = (m.year.cars$coefficients),
                                allocation.locomotives = (m.year.locomotives$coefficients),
                                allocation.fuel = (m.year.fuel$coefficients),
                                allocation.labor = (m.year.labor$coefficients))

ggplot(allocation.time.df, aes(year, allocation.cars)) + geom_line()
ggplot(allocation.time.df, aes(year, allocation.locomotives)) + geom_line()
ggplot(allocation.time.df, aes(year, allocation.fuel)) + geom_line()
ggplot(allocation.time.df, aes(year, allocation.labor)) + geom_line()
