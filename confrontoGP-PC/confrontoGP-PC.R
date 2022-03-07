#Codice per Green Pass emessi da test negativo e Green Pass emessi per guarigione

library(ggplot2)
library(ggtext)
library(scales)
library(tidyr)

manual_value = 0

addUnits <- function(n) {
  labels <- ifelse(abs(n) < 1000, n,  # less than thousands
                   ifelse(abs(n)  < 1e6, paste0(round(n/1e3), 'mila'),  # in thousands
                          ifelse(abs(n)  < 1e9, paste0(round(n/1e6,2), ' milioni'),  # in millions
                                 ifelse(abs(n)  < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(abs(n) < 1e15, paste0(round(n/1e12), 'T'), # in trillions
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

if(manual_value){
  tamponiGP <- rbind(tamponiGP, data.frame(data = max(tamponiGP$data)+1, issued_for_tests = manual_value))
}

dataPC <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
dataPC$data <- as.Date(dataPC$data, format = "%Y-%m-%d")
tamponiPC <- data.frame(data = dataPC$data[-1]-1, tamponi_PC =  diff(dataPC$tamponi))

tamponiTOT <- merge(tamponiGP, tamponiPC)
tamponiTOT2 <- tamponiTOT
tamponiTOT <- gather(tamponiTOT, dataset, test, issued_for_tests:tamponi_PC, factor_key=TRUE)


png(filename = paste0("confrontoTamponi_", max(tamponiTOT$data), ".png"), width = 475, height = 250, units='mm', res = 300)

ggplot(tamponiTOT, aes(x=data, y = test, col = dataset)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels="%b\n%Y" ) +
  scale_y_continuous(labels = addUnits, breaks = seq(0, max(tamponiTOT$test)*1.2, by = 2.5*10^5)) +
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


tamponiTOT2$diff <- tamponiTOT2$issued_for_tests - tamponiTOT2$tamponi_PC
tamponiTOT2$val <- tamponiTOT2$diff > 0

png(filename = paste0("confrontoTamponiBarre_", max(tamponiTOT$data), ".png"), width = 475, height = 250, units='mm', res = 300)

ggplot(tamponiTOT2[tamponiTOT2$data>"2021-07-31",], aes(x=data, y = diff)) +
  geom_bar(stat='identity', aes(fill = val)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b\n%Y" ) +
  scale_y_continuous(labels = addUnits) +
  theme_minimal(base_size = 24) +
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown()) +
  labs(title = paste0("Differenza tra Green Pass emessi per test negativo e Tamponi Effettuati al ",
                      format(max(tamponiTOT2$data), format="%d %B %Y")),
    # title = paste0("Confronto <b style='color:#00bfc4'>Tamponi Effettuati</b> e <b style='color:#f8766d'>Green Pass emessi per test negativo</b> al ", 
    #                   format(max(tamponiTOT$data), format="%d %B %Y")),
       subtitle = "Le comunicazioni del flusso Protezione Civile si riferiscono al giorno precedente e sono qui retrodatate",
       x = "",
       y = "",
       caption = paste0("Elaborazione grafica V. Nicoletta | Fonte: Ministero Salute, Protezione Civile | CC-BY-4.0")
  ) +
  annotate("text", x = as.Date("2021-09-05"), y = 200000, size = 6,
           label = "Dall'11 ottobre 2021 i Green Pass emessi per test negativo \n sono maggiori dei Tamponi Effettuati") +
  annotate("curve", x = as.Date("2021-09-05"), xend = as.Date("2021-10-11"), y = 170000, yend = 100000, 
           size=1, curvature = 0.15, arrow = arrow(length = unit(5, "mm"))) +
  annotate("text", x = as.Date("2021-08-30"), y = -70000, size = 8, label = "Green Pass < Tamponi", col = "#f8766d") +
  annotate("text", x = as.Date("2021-08-30"), y =  80000, size = 8, label = "Green Pass > Tamponi", col = "#00bfc4") +
  annotate("text", x = as.Date("2021-12-01"), y = -125000, size = 6,
           label = "Il 25 e 26 dicembre 2021 e dal 1 gennaio 2022 \n i Green Pass emessi per test negativo \n sono minori dei Tamponi Effettuati") 
  # annotate("curve", x = as.Date("2021-12-05"), xend = as.Date("2021-12-23"), y = -40000, yend = -30000, 
  #          size=1, curvature = 0, arrow = arrow(length = unit(5, "mm"))) +
  # annotate("curve", x = as.Date("2021-12-01"), xend = as.Date("2022-01-02"), y = -100000, yend = -80000, 
  #          size=1, curvature = 0, arrow = arrow(length = unit(5, "mm"))) 


dev.off()

# dati da it-dgc-opendata del Ministero della Salute
dataGP <- read.csv("https://raw.githubusercontent.com/ministero-salute/it-dgc-opendata/master/data/dgc-issued.csv")

dataGP$data <- as.Date(dataGP$data)
dataGP <- dataGP[order(dataGP$data),]
tamponiGP <- dataGP[, c("data", "issued_for_healing")]

if(manual_value){
  tamponiGP <- rbind(tamponiGP, data.frame(data = max(tamponiGP$data)+1, issued_for_tests = manual_value))
}




dataPC <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
dataPC$data <- as.Date(dataPC$data, format = "%Y-%m-%d")
tamponiPC <- data.frame(data = dataPC$data[-1]-1, tamponi_PC =  diff(dataPC$dimessi_guariti))

tamponiTOT <- merge(tamponiGP, tamponiPC)
tamponiTOT2 <- tamponiTOT
tamponiTOT <- gather(tamponiTOT, dataset, test, issued_for_healing:tamponi_PC, factor_key=TRUE)


png(filename = paste0("confrontoGuariti_", max(tamponiTOT$data), ".png"), width = 475, height = 250, units='mm', res = 300)

ggplot(tamponiTOT[tamponiTOT$data>"2021-07-31",], aes(x=data, y = test, col = dataset)) +
  geom_line(size = 1) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", date_labels="%b\n%Y" ) +
  scale_y_continuous(labels = addUnits) +
  scale_color_manual(values = c("#00bfc4", "#f8766d")) +
  theme_minimal(base_size = 24) +
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown()) +
  labs(title = paste0("Confronto <b style='color:#f8766d'>Dimessi/Guariti</b> e <b style='color:#00bfc4'>Green Pass emessi per guarigione</b> al ", 
                      format(max(tamponiTOT$data), format="%d %B %Y")),
       subtitle = "Le comunicazioni del flusso Protezione Civile si riferiscono al giorno precedente e sono qui retrodatate",
       x = "",
       y = "",
       caption = paste0("Elaborazione grafica V. Nicoletta | Fonte: Ministero Salute, Protezione Civile | CC-BY-4.0")
  )

dev.off()


tamponiTOT2$diff <- tamponiTOT2$issued_for_healing - tamponiTOT2$tamponi_PC
tamponiTOT2$val <- tamponiTOT2$diff > 0

png(filename = paste0("confrontoGuaritiBarre_", max(tamponiTOT$data), ".png"), width = 475, height = 250, units='mm', res = 300)

ggplot(tamponiTOT2[tamponiTOT2$data>"2021-07-31",], aes(x=data, y = diff)) +
  geom_bar(stat='identity', aes(fill = val)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b\n%Y" ) +
  scale_y_continuous(labels = addUnits, limits = c(NA, NA)) +
  theme_minimal(base_size = 24) +
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown()) +
  labs(title = paste0("Differenza tra Green Pass emessi per guarigione e Dimessi/Guariti al ",
                      format(max(tamponiTOT2$data), format="%d %B %Y")),
       # title = paste0("Confronto <b style='color:#00bfc4'>Tamponi Effettuati</b> e <b style='color:#f8766d'>Green Pass emessi per test negativo</b> al ", 
       #                   format(max(tamponiTOT$data), format="%d %B %Y")),
       subtitle = "Le comunicazioni del flusso Protezione Civile si riferiscono al giorno precedente e sono qui retrodatate",
       x = "",
       y = "",
       caption = paste0("Elaborazione grafica V. Nicoletta | Fonte: Ministero Salute, Protezione Civile | CC-BY-4.0")
  ) +
  # annotate("text", x = as.Date("2021-09-05"), y = 200000, size = 6,
  #          label = "Dall'11 ottobre 2021 i Green Pass emessi per test negativo \n sono maggiori dei Tamponi Effettuati") +
  # annotate("curve", x = as.Date("2021-09-05"), xend = as.Date("2021-10-11"), y = 170000, yend = 100000, 
  #          size=1, curvature = 0.15, arrow = arrow(length = unit(5, "mm"))) +
  annotate("text", x = as.Date("2021-08-30"), y = -15000, size = 8, label = "Green Pass < Dimessi/Guariti", col = "#f8766d") +
  annotate("text", x = as.Date("2021-08-30"), y =  40000, size = 8, label = "Green Pass > Dimessi/Guariti", col = "#00bfc4") 
# annotate("text", x = as.Date("2021-11-07"), y = -70000, size = 6,
#          label = "Il 25 e 26 dicembre 2021 e dal 1 gennaio 2022 \n i Green Pass emessi per test negativo \n sono minori dei Tamponi Effettuati") +
# annotate("curve", x = as.Date("2021-12-05"), xend = as.Date("2021-12-23"), y = -40000, yend = -30000, 
#          size=1, curvature = 0, arrow = arrow(length = unit(5, "mm"))) +
# annotate("curve", x = as.Date("2021-12-01"), xend = as.Date("2022-01-02"), y = -100000, yend = -80000, 
#          size=1, curvature = 0, arrow = arrow(length = unit(5, "mm"))) 


dev.off()

