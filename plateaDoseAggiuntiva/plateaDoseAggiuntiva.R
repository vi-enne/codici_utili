#Codice per grafico a barre platea dose aggiuntiva per Regione/Provincia Autonoma

library(ggplot2)
library(scales)
library(viridis)
library(jsonlite)


last_update <- fromJSON("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/last-update-dataset.json")
last_update <- substr(last_update[[1]],1,19)
last_update <- gsub("T", " ", last_update)
last_update <- as.POSIXct(last_update) + 2*60*60 #aggiunge due ore per fuso orario Italia


path <- "https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/platea-dose-aggiuntiva.csv"
data <- read.csv(path)


png(filename = "platea.png", width = 800, height = 600, units='mm', res = 300)

#Plot 
ggplot(data, aes(x=totale_popolazione, y=reorder(nome_area, totale_popolazione), fill = categoria_prevalente,
                 label = comma(totale_popolazione, big.mark = ".", decimal.mark = ","))) +
  geom_bar(stat='identity') +
  geom_label(aes(label = comma(stat(x), big.mark = ".", decimal.mark = ","), group = nome_area), 
    stat = 'summary', fun = sum, show.legend = FALSE,
    position = position_dodge(width = 1), hjust = -.1, size = 8) +
  theme_minimal(base_size = 40) +
  labs(title = paste0("Platea dose aggiuntiva (aggiornato al ", last_update, ")"),
       x = "Platea dose aggiuntiva",
       y = "Regione/Provincia Autonoma",
       caption = "Elaborazione V. Nicoletta | Fonte: https://github.com/italia/covid19-opendata-vaccini/blob/master/dati/platea-dose-aggiuntiva.csv") +
  scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ","),
                     limits = c(0, 1.3*10^6),
                     breaks = seq(0, 1.3*10^6, by = 2.5*10^5)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_viridis_d(name = "Categoria prevalente", option = "plasma") +
  theme(legend.position="bottom")

dev.off()
