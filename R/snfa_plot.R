library(ggplot2)
library(snfa)

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

plot.df = data.frame(x = x.reflect, y = y.reflect,
                     color = c(rep("below", N), rep("data", N), rep("above", N)))
ggplot(plot.df, aes(x, y)) + geom_point(aes(color = color))

#Data for debugging
# X.eval = rbind(x.reflect, x)
# y.eval = c(y.reflect, y)
# 
# X.constrained = x
# y.constrained = y
# 
# X.fit = x.fit
# 
# method = "mc"
# H.mult = H.mult
# H.inv = H.inv
# 
# 
# rm(list = setdiff(ls(), c("X.eval", "y.eval", "X.constrained", "y.constrained", "X.fit", "method", "H.mult", "H.inv", "N.fit", "df")))

# asdf
#Fitting
frontier.0 = fit.boundary(x.reflect, y.reflect, x, y, x.fit, x.fit, method = "u", H.mult = H.mult, H.inv = NA)
frontier.1 = fit.boundary(x.reflect, y.reflect, x, y, x.fit, x.fit, method = "m", H.mult = H.mult, H.inv = NA)
frontier.2 = fit.boundary(x.reflect, y.reflect, x, y, x.fit, x.fit, method = "mc", H.mult = H.mult, H.inv = NA)

fit.df = data.frame(x = rep(x.fit, times = 3),
                    Model = rep(c("U", "M", "MC"), each = N.fit),
                    y = c(frontier.0$y.fit, frontier.1$y.fit, frontier.2$y.fit))

ggplot(df, aes(x, y)) + geom_point() + geom_line(data = fit.df, aes(color = Model)) +
  theme_bw() + theme(legend.position = "top") +
  xlab("Input") +
  ylab("Ouptut")
ggsave("snfa-plot.pdf", width = 3, height = 3, scale = 1.5)


slope.df = data.frame(x = rep(x.fit, times = 3),
                      Model = rep(c("U", "M", "MC"), each = N.fit),
                      slope = c(frontier.0$gradient.fit, frontier.1$gradient.fit, frontier.2$gradient.fit))
ggplot(slope.df, aes(x, slope)) + geom_line(aes(color = Model)) +
  theme_bw() + theme(legend.position = "top") +
  xlab("Input") +
  ylab("Marginal Input Productivity")
ggsave("snfa-grad-plot.pdf", width = 3, height = 3, scale = 1.5)
