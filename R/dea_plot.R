library(ggplot2)
library(rDEA)

setwd("~/docs/SNFA_paper/R")

rm(list = ls())

df = read.csv("data.csv")

x = df$x
y = df$y

m.constant = dea(x, y, x, y, model = "output", RTS = "constant")
m.variable = dea(x, y, x, y, model = "output", RTS = "variable")

plot.df = rbind(df, df)
plot.df$Model = rep(c("Constant", "Variable"), each = nrow(df))

plot.df$efficiency = c(m.constant$thetaOpt, m.variable$thetaOpt)
plot.df$Frontier = plot.df$y / plot.df$efficiency

test.plot = ggplot(subset(plot.df, Model == "Variable"), aes(x, Frontier)) + geom_line()
test.build = ggplot_build(test.plot)
x.range = test.build$layout$panel_ranges[[1]]$x.range
y.range = test.build$layout$panel_ranges[[1]]$y.range

ggplot(plot.df, aes(x, y)) + geom_point() + geom_line(aes(y = Frontier, color = Model)) + 
  theme_bw() + theme(legend.position = "top") + coord_cartesian(xlim = x.range, ylim = y.range) +
  xlab("Input") +
  ylab("Output")
ggsave("dea-plot.pdf", width = 3, height = 3, scale = 1.5)


#Creating, plotting slopes
#Constant RTS
end.index = c(which.min(x), which.max(x))
end.x = c(x[end.index[1]], x[end.index[2]])
end.y = c(y[end.index[1]] / m.constant$thetaOpt[end.index[1]], y[end.index[2]] / m.constant$thetaOpt[end.index[2]])

slope.constant = diff(end.y) / diff(end.x)

#Variable RTS
frontier.index = which(m.variable$thetaOpt == 1)
frontier.x = x[frontier.index]
frontier.y = y[frontier.index]

if (!(min(x) %in% frontier.x)){
  min.x.index = which.min(x)
  frontier.x = c(frontier.x, x[min.x.index])
  frontier.y = c(y[min.x.index] / m.variable$thetaOpt[min.x.index])
}
if (!(max(x) %in% frontier.x)){
  max.x.index = which.max(x)
  frontier.x = c(frontier.x, x[max.x.index])
  frontier.y = c(frontier.y, y[max.x.index] / m.variable$thetaOpt[max.x.index])
}

num.frontier.points = length(frontier.x)

frontier.order = order(frontier.x)
frontier.x = frontier.x[frontier.order]
frontier.y = frontier.y[frontier.order]

slopes = diff(frontier.y) / diff(frontier.x)

slope.df = data.frame(x = c(frontier.x[1], rep(frontier.x[-c(1, num.frontier.points)], each = 2), frontier.x[num.frontier.points]),
                      slope = rep(slopes, each = 2),
                      group = rep(1:(num.frontier.points - 1), each = 2),
                      Model = "Variable")
slope.df = rbind(slope.df, data.frame(x = end.x, slope = slope.constant,
                                      group = num.frontier.points,
                                      Model = "Constant"))
slope.df$group = factor(slope.df$group)
slope.df$Model = factor(slope.df$Model, c("Constant", "Variable"))

ggplot(slope.df, aes(x, slope)) + geom_line(aes(group = group, color = Model)) +
  theme_bw() + theme(legend.position = "top") +
  xlab("Input") +
  ylab("Marginal Input Productivity")
ggsave("dea-grad-plot.pdf", width = 3, height = 3, scale = 1.5)
