#Codice per Green Pass emessi da test negativo

library(ggplot2)
library(ggtext)
library(scales)
library(tidyr)

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'mila'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

Sys.setlocale("LC_TIME", "Italian") #per avere date in italiano


# dati da it-dgc-opendata del Ministero della Salute
dataGP <- read.csv("https://raw.githubusercontent.com/ministero-salute/it-dgc-opendata/master/data/dgc-issued.csv")

dataGP$data <- as.Date(dataGP$data)
dataGP <- dataGP[order(dataGP$data),]
tamponiGP <- dataGP[, c("data", "issued_for_tests")]


dataPC <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
dataPC$data <- as.Date(dataPC$data, format = "%Y-%m-%d")
tamponiPC <- data.frame(data = dataPC$data[-1]-1, tamponi_PC =  diff(dataPC$tamponi))

tamponiTOT <- merge(tamponiGP, tamponiPC)
tamponiTOT <- gather(tamponiTOT, dataset, test, issued_for_tests:tamponi_PC, factor_key=TRUE)


png(filename = paste0("confrontoTamponi_", max(tamponiTOT$data), ".png"), width = 475, height = 250, units='mm', res = 300)

ggplot(tamponiTOT, aes(x=data, y = test, col = dataset)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels="%B %Y" ) +
  scale_y_continuous(labels = addUnits, breaks = seq(0, max(tamponiTOT$test)*1.2, by = 10^5)) +
  theme_minimal(base_size = 24) +
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown()) +
  labs(title = paste0("Confronto <b style='color:#00bfc4'>Tamponi Effettuati</b> e <b style='color:#f8766d'>Green Pass emessi per test negativo</b> al ", 
                      format(max(tamponiTOT$data), format="%d %B %Y")),
       subtitle = "Le comunicazioni del flusso Protezione Civile si riferiscono al giorno precedente e sono qui retrodatate",
       x = "",
       y = "",
       caption = paste0("Elaborazione grafica V. Nicoletta | Fonte: Ministero Salute, Protezione Civile | CC-BY-4.0")
  )

dev.off()

