library(rDEA)
library(mvnpbr)
library(truncnorm)
library(ggplot2)
library(zoo)
library(dplyr)

setwd("~/docs/NPBR_paper/sensitivity")

rm(list = ls())

#Data parameters
N = 20
num.sims = 500
N.constrained = 100

x.range = c(100, 1000)

sd = 50

#Function parameters
a = 100
b = 0.2

f = function(x){
  return(a * x^b)
}
f.x = function(x){
  return(a * b * x^(b - 1))
}

# x = matrix(sort(runif(N, x.range[1], x.range[2])), ncol = 1)
# x.constrained = matrix(seq(min(x), max(x), length.out = N.constrained), ncol = 1)
# 
# f.true = f(x)
# f.x.true = f.x(x)

#Run simulations
results.df = data.frame(x = NA,
                        efficiency = NA,
                        slope = NA,
                        efficiency.npbr = NA,
                        slope.npbr = NA,
                        efficiency.dea = NA,
                        slope.dea = NA,
                        sim = NA)
for (sim in 1:num.sims){
  x = matrix(sort(runif(N, x.range[1], x.range[2])), ncol = 1)
  x.constrained = matrix(seq(min(x), max(x), length.out = N.constrained), ncol = 1)
  
  f.true = f(x)
  f.x.true = f.x(x)
  
  y = f.true - rtruncnorm(N, a = 0, sd = sd)
  
  if (any(y < 0)) stop()
  
  #NPBR
  reflected.data = reflect.data(x, y)
  
  x.reflect = reflected.data$X.reflected
  y.reflect = reflected.data$y.reflected
  
  m.npbr = fit.frontier(x.reflect, y.reflect, x, y, x.constrained, x, y, method = "mc")
  
  # point.df = data.frame(x, y)
  # fit.df = data.frame(x, y = m.npbr$y.fit)
  # show(ggplot(point.df, aes(x, y)) + geom_point() + geom_line(data = fit.df))
  
  #DEA
  m.dea = dea(x, y, x, y, model = "output", RTS = "variable")
  
  frontier.x = x
  frontier.y = y / m.dea$thetaOpt
  
  dea.slopes = diff(frontier.y) / diff(frontier.x)
  dea.slopes = c(dea.slopes[1], rollmean(dea.slopes, 2), dea.slopes[N - 1])

  # point.df = data.frame(x, y)
  # fit.df = data.frame(x, y = y / m.dea$thetaOpt)
  # show(ggplot(point.df, aes(x, y)) + geom_point() + geom_line(data = fit.df))
  
  #Save results
  results.df = rbind(results.df, data.frame(x = x,
                                            efficiency = y / f.true,
                                            slope = f.x.true,
                                            efficiency.npbr = m.npbr$efficiency,
                                            slope.npbr = m.npbr$gradient.fit,
                                            efficiency.dea = m.dea$thetaOpt,
                                            slope.dea = dea.slopes,
                                            sim = sim))
  # if (readline("Enter to continue.") == "q") stop()
}
results.df = results.df[-1,]

#Package results
all.results = results.df %>%
  mutate(efficiency.diff.npbr = efficiency - efficiency.npbr,
            efficiency.diff.dea = efficiency - efficiency.dea,
            slope.diff.npbr = slope - slope.npbr,
            slope.diff.dea = slope - slope.dea)
point.results = results.df %>%
  group_by(x) %>%
  summarize(efficiency.diff.npbr = mean(efficiency - efficiency.npbr),
            efficiency.diff.dea = mean(efficiency - efficiency.dea),
            slope.diff.npbr = mean(slope - slope.npbr),
            slope.diff.dea = mean(slope - slope.dea))
overall.results = point.results %>%
  ungroup() %>%
  summarize(efficiency.diff.npbr = mean(efficiency.diff.npbr, na.rm = TRUE),
            efficiency.diff.dea = mean(efficiency.diff.dea, na.rm = TRUE),
            slope.diff.npbr = mean(slope.diff.npbr, na.rm = TRUE),
            slope.diff.dea = mean(slope.diff.dea, na.rm = TRUE))

point.results
overall.results

#Significance testing
# t.test(all.results$efficiency.diff.npbr, mu = 0, alternative = "two.sided")
# t.test(all.results$efficiency.diff.dea, mu = 0, alternative = "two.sided")
t.test(abs(all.results$efficiency.diff.npbr), abs(all.results$efficiency.diff.dea), alternative = "less")

# t.test(all.results$slope.diff.npbr, mu = 0, alternative = "two.sided")
# t.test(all.results$slope.diff.dea, mu = 0, alternative = "two.sided")
t.test(abs(all.results$slope.diff.npbr), abs(all.results$slope.diff.dea), alternative = "less")
