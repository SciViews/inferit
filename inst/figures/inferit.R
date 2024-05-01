library(ggplot2)
library(inferit)
library(hexSticker)
di1 <- dist_chisq(df = 8)
p <- chart(di1) +
  geom_funfill(fun = dfun(di1), from = 0, to = 16, fill = "cornsilk") +
  geom_funfill(fun = dfun(di1), from = 16, to = 30, fill = "red")
p <- p + theme_void() + theme_transparent()
p
outfile <- "inst/figures/inferit.png"
sticker(p, package = "inferit", filename = outfile,
  s_x = 1, s_y = 0.8, s_width = 1.3, s_height = 0.9,
  p_size = 28, h_fill = "wheat3", h_color = "wheat4")
