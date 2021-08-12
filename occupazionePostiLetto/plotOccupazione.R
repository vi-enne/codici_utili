# Plot occupazione AM e TI Regioni/PA e colori zone secondo DL 23 luglio 2021

library(ggplot2)
library(ggrepel)

# Scia Settimana

# https://sashamaps.net/docs/resources/20-colors/
col_vector<-c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', 
              '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', 
              '#008080', '#e6beff', '#9a6324', '#fff796', '#800000', 
              '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', 
              '#000000')

zoom = c(0,0.2) #min e max percentuale occupazione da 0 a 1

giorni = 7

filePC = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv"
PC = read.csv(filePC)
filePC_7 = paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-", 
                  gsub("-","",as.Date(PC$data[1]) - giorni),
                  ".csv")
PC_7 = read.csv(filePC_7)
pop = read.csv("pop.csv")

incidenza = (PC$totale_casi - PC_7$totale_casi)/pop$Popolazione * 10^5



file = "https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/agenas/processing/postiletto-e-ricoverati-areaNonCritica_date.csv"
postiLetto = read.csv(file)
postiLetto = postiLetto[postiLetto$fonte %in% tail(unique(postiLetto$fonte),giorni),]
postiLetto = postiLetto[,c(1:5,8)]

output = postiLetto

output$AM = output$Ricoverati.in.Area.Non.Critica/output$PL.in.Area.Non.Critica
output$TI = output$Ricoverati.in.Terapia.intensiva/output$PL.in.Terapia.Intensiva
output$Area = output$Regioni
output$data = as.Date(output$data)

outputLast = output[output$data == max(output$data),]
outputLast$incidenza = incidenza
outputLast$size = ifelse(outputLast$incidenza <= 50, 4, ifelse(outputLast$incidenza <= 150,6,8))
outputLast$size = factor(outputLast$size, levels = c(4,6,8))

png(filename = paste0("occupazioneZoomScia_",  max(output$data), ".png"), width = 465, height = 480, units='mm', res = 300)

ggplot(output, aes(AM, TI, group = Area)) +
  geom_point(col = "gray") +
  geom_path(data=output,
            mapping=aes(x=AM, y=TI, col = Area, alpha=0.1),
            size=2, alpha = 0.5, show.legend=FALSE)+
  geom_point(data = outputLast, aes(col = Area, size=size)) +
  geom_label_repel(data = outputLast,
                   aes(label = ifelse((TI>0.04 | AM>=0.05) ,Area,"")), 
                   size = 8, force = 2,
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   nudge_x = 0.01,
                   segment.color = 'grey50') +
  labs(title = paste("Occupazione Area Medica e Terapie Intensive persone positive a Covid-19 al", max(output$data), " - Zoom"),
       x = "Area Medica",
       y = "Terapia Intensiva",
       subtitle = paste0("Colori Aree secondo DL 23/07/2021 senza considerare incidenza per semplicità. \nLa scia rappresenta le occupazioni nei ", 
                         giorni, " giorni precedenti."),
       caption = "Elaborazione V. Nicoletta | Dati AGENAS (estratti da onData), Protezione Civile e ISTAT") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = zoom, breaks = seq(0, 1, by = 0.05)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = zoom, breaks = seq(0, 1, by = 0.05)) +
  theme_minimal(base_size = 22) +
  theme(legend.position = c(0.3,0.8),
        legend.background = element_rect(fill="white", 
                                         size=0.5, linetype="solid"),
        legend.box.just = "center")+
  scale_color_manual(
    name = paste0("Regione/PA (Incidenza, %AM, %TI) al ", max(output$data)),
    labels = paste0(
      outputLast$Area,
      " (",
      round(outputLast$incidenza, 1),
      ", ",
      scales::percent(outputLast$AM, accuracy = 0.1),
      ", ",
      scales::percent(outputLast$TI, accuracy = 0.1),
      ")"
    ),
    values = col_vector
  ) +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  scale_size_manual(
    name = "Incidenza casi per data di notifica/100mila abitanti su 7 giorni",
    limits = factor(c(4, 6, 8)),
    labels = c("<50", "50-150", ">150"),
    values = c(4, 6, 8)
  ) +
  guides(size = guide_legend(nrow = 1)) +
  geom_rect(fill = "yellow", xmin = .15, xmax = .5, ymin = .10, ymax = .5, alpha = 0.1) +
  geom_rect(fill = "orange", xmin = .30, xmax = .5, ymin = .20, ymax = .5, alpha = 0.1) +
  geom_rect(fill = "red",    xmin = .40, xmax = .5, ymin = .30, ymax = .5, alpha = 0.1) +
  annotate("label", x = 0.075,y = 0.2, label = "Bianca", fill = "white", size = 8) + 
  annotate("label", x = 0.18,y = 0.2, label = "Gialla", fill = "white", size = 8)

dev.off()

