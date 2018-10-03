library(ggplot2)
library(snfa)
library(rDEA)

setwd("~/docs/SNFA_paper/R")

rm(list = ls())

df = read.csv("data.csv")

N.fit = 100
H.mult = 1
H.inv = NA

x = matrix(df$x, ncol = 1)
y = df$y
x.fit = matrix(seq(min(x) - 0.01, max(x) + 0.01, length.out = N.fit))

N = length(y)

#Using reflection

reflected.data = reflect.data(x, y)

x.reflect = reflected.data$X.reflected
y.reflect = reflected.data$y.reflected

#Fitting
frontier.snfa = fit.boundary(x.reflect, y.reflect, x, y, x.fit, method = "mc", H.mult = H.mult, H.inv = NA)
frontier.dea = dea(x, y, x, y, model = "output", RTS = "variable")

fit.df = data.frame(x = c(x.fit, x),
                    y = c(frontier.snfa$y.fit, y / frontier.dea$thetaOpt),
                    Model = c(rep("NPBR", N.fit), rep("DEA", N)))

ggplot(df, aes(x, y)) + geom_point() + geom_line(data = fit.df, aes(color = Model)) +
  theme_bw() + theme(legend.position = "top") +
  xlab("Input") +
  ylab("Ouptut")
ggsave("snfa-dea-comp-plot.pdf", width = 3, height = 3, scale = 1.5)


#Slopes
#DEA
frontier.index = which(frontier.dea$thetaOpt == 1)
frontier.x = x[frontier.index]
frontier.y = y[frontier.index]

if (!(min(x) %in% frontier.x)){
  min.x.index = which.min(x)
  frontier.x = c(frontier.x, x[min.x.index])
  frontier.y = c(y[min.x.index] / frontier.dea$thetaOpt[min.x.index])
}
if (!(max(x) %in% frontier.x)){
  max.x.index = which.max(x)
  frontier.x = c(frontier.x, x[max.x.index])
  frontier.y = c(frontier.y, y[max.x.index] / frontier.dea$thetaOpt[max.x.index])
}

num.frontier.points = length(frontier.x)

frontier.order = order(frontier.x)
frontier.x = frontier.x[frontier.order]
frontier.y = frontier.y[frontier.order]

slopes = diff(frontier.y) / diff(frontier.x)

#Plotting
slope.df = data.frame(x = c(x.fit, frontier.x[1], rep(frontier.x[-c(1, num.frontier.points)], each = 2), frontier.x[num.frontier.points]),
                      slope = c(frontier.snfa$gradient.fit, rep(slopes, each = 2)),
                      group = c(rep(1, N.fit), rep(2:(num.frontier.points), each = 2)),
                      Model = c(rep("SNFA", N.fit), rep("DEA", (num.frontier.points - 1) * 2)))

ggplot(slope.df, aes(x, slope)) + geom_line(aes(group = group, color = Model)) +
  theme_bw() + theme(legend.position = "top") +
  xlab("Input") +
  ylab("Marginal Input Productivity")
ggsave("snfa-dea-comp-grad-plot.pdf", width = 3, height = 3, scale = 1.5)
