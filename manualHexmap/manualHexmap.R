library(sf)

sfc <- st_sfc(st_polygon(list(rbind(c(0, 0), c(3.4, 0), c(3.4, 2), c(0, 2), c(0, 0)))))
grid <- st_make_grid(sfc, cellsize = .1, square = FALSE)

unCountries <- read.csv("unCountries.csv")
res         <- read.csv("es-113.csv")

pal <- c("#73D055FF", "#453781FF", "#FDE725FF", "#EEEEEE")
color <- data.frame(vote = c("Y", "N", "A", "NP"), col = pal)

final <- merge(merge(unCountries, res), color)

fileOut <- "hexmap_es-113.png"
png(
  filename = fileOut,
  width = 1600,
  height = 900,
  units = 'mm',
  res = 100
)

par(mar = c(6, 0, 9, 0)) # Set the margins
plot(
  grid,
  border = "white",
  main = "Results of United Nations General Assembly Resolution ES-11/3:
    Suspension of the rights of membership of the Russian Federation in the Human Rights Council",
  sub = "NOTE: this map considers only the 193 UN Member States | Source: https://digitallibrary.un.org/record/3967778 | Plot by V. Nicoletta @vi__enne",
  cex.main = 5,
  cex.sub = 3
)

for (i in final$n) {
  a <- grid[[i]]
  ncentre <- st_centroid(a)
  ncentre_num <- st_coordinates(ncentre)
  plot(grid[[i]],
       add = T,
       col = final$col[final$n == i])
  text(
    x = ncentre_num[, 1],
    y = ncentre_num[, 2],
    labels = final$ISO_A3[final$n == i],
    cex = 3
  )
}

legend(
  "topright",
  inset = 0.1,
  c("93 Yes", "24 No", "58 Abstain", "18 Absent"),
  fill = pal,
  horiz = F,
  cex = 6
)

dev.off()
