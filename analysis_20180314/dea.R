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

# X.scale = apply(df[,indep.vars], 2, mean)
# y.scale = mean(df$revenue.ton.miles)

X.scale = rep(1, length(indep.vars))
y.scale = 1

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
  
  #Concavity, doesn't work for any tested H.mult
  dea.result = dea(X, y, X, y, model = "output")
  
  efficiency.list[[i]] = dea.result$thetaOpt
}

#Create data.frames
efficiency.df = data.frame(rr = rep(firms, times = num.years),
                           year = rep(years, each = num.firms),
                           efficiency = Reduce(c, efficiency.list))

ggplot(efficiency.df, aes(year, efficiency)) + geom_line(aes(color = rr))
