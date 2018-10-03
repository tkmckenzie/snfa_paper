library(ggplot2)

setwd("~/docs/SNFA_paper/R")

rm(list = ls())

set.seed(100)

df = data.frame(x = c(2, 3, 5, 7, 8, 10),
                y = c(5, 7, 15, 12, 21, 22))


N = 50
x = runif(N, 10, 100)
y = sapply(x, function(x) 500 * x^0.25 - dnorm(x, mean = 70, sd = 10) * 8000) - abs(rnorm(N, sd = 20))
y = y - min(y) + 10
df = data.frame(x, y)

# gdp.df = read.csv("gdpData.csv")
# gdp.df = subset(gdp.df, t >= 1990)
# df = data.frame(x = gdp.df$K, y = gdp.df$Y)
# df$x = df$x / 1e6
# df$y = df$y / 1e3
# 
ggplot(df, aes(x, y)) + geom_point()

write.csv(df, "data.csv", row.names = FALSE)
save(df, file = "univariate.RData")
