library(ggplot2)
library(numDeriv)

setwd("~/docs/SNFA_paper/R")

rm(list = ls())

f = function(x) return(100 * log(x^(3) + 1)^0.5)

N = 1000

x = seq(0, 10, length.out = N)
y = sapply(x, f)
f.df = data.frame(x, y)

x.points = c(3, 6.5, 5)
y.points = c(f(x.points[1]), f(x.points[2]), 150)
points.df = data.frame(x = x.points, y = y.points)

dy.dx = grad(f, x.points[1])
line.length = 3
x.line = c(x.points[1] - 0.5 * line.length, x.points[1] + 0.5 * line.length)
y.line = c(y.points[1] - 0.5 * line.length * dy.dx, y.points[1] + 0.5 * line.length * dy.dx)
line.df = data.frame(x = x.line, y = y.line)

ggplot(f.df, aes(x, y)) +
  geom_line() +
  geom_point(data = points.df) +
  geom_path(data = line.df, color = "red") +
  annotate("text", x = x.points[1] - 0.5, y = y.points[1] + 10, size = 5, label = "C") +
  annotate("text", x = x.points[2] - 0.0, y = y.points[2] + 10, size = 5, label = "B") +
  annotate("text", x = x.points[3] + 0.5, y = y.points[3] - 10, size = 5, label = "A") +
  # annotate("text", x = x.points[1] - 2.5, y = y.points[1] - 20, size = 4, label = "slope == over(p[Y], p[X])", parse = TRUE) +
  theme_bw() +
  xlab("X") +
  ylab("Y")
ggsave("efficiencyExample.pdf", width = 3, height = 3, scale = 1.5)