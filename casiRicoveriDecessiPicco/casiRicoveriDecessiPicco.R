# Grafico di confronto fra Casi, Ricoveri e Decessi per Covid-19 in Italia
# Ispirato da @mugecevik (https://twitter.com/mugecevik)

# Dati Istituto Superiore Sanita https://www.epicentro.iss.it/coronavirus/open-data/covid_19-iss.xlsx
# Estratti da onData https://github.com/ondata/covid19italia/tree/master/webservices/iss_epicentro_dati/processing

library(ggplot2)
library(readxl)
library(zoo)
library(reshape)
library(viridis)

Sys.setlocale("LC_TIME", "Italian")

offsetRicoveri <- 2
offsetDecessi <- 12


casi     <- read.csv("https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/iss_epicentro_dati/processing/casi_prelievo_diagnosi-latest.csv")
ricoveri <- read.csv("https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/iss_epicentro_dati/processing/ricoveri-latest.csv")
decessi  <- read.csv("https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/iss_epicentro_dati/processing/decessi-latest.csv")

last_update <- as.Date(casi$iss_date[1], format = "%d/%m/%Y")

# Pulizia dati: assumo "<5" = 1 per convenzione
cleanData <-function(x){
  x <- x[,-1]
  colnames(x)[1] <- "data"
  x$data <- as.Date(x$data, format = "%d/%m/%Y")
  x <- x[order(x$data),]
  x[,2] <- as.numeric(x[,2])
  x[is.na(x[,2]), 2] <- 1
  
  return(na.omit(x))
}

casiClean <- cleanData(casi)
ricoveriClean <- cleanData(ricoveri)
ricoveriClean <- data.frame(data = head(ricoveriClean$data, -offsetRicoveri), RICOVERI = tail(ricoveriClean$RICOVERI,-offsetRicoveri))

decessiClean <- cleanData(decessi)
decessiClean <- data.frame(data = head(decessiClean$data, -offsetDecessi), DECESSI = tail(decessiClean$DECESSI,-offsetDecessi))


data <- merge(casiClean, ricoveriClean)
data <- merge(data, decessiClean)

dataRoll <- data.frame(data = tail(data$data,-6), 
                       casi = rollmean(data$CASI, 7, align ="center"),
                       ricoveri = rollmean(data$RICOVERI, 7, align ="center"),
                       decessi = rollmean(data$DECESSI, 7, align ="center"))

picco <- dataRoll$data[dataRoll$casi==max(dataRoll$casi)]

dataRoll$casi <- dataRoll$casi/dataRoll$casi[dataRoll$data==picco]
dataRoll$ricoveri <- dataRoll$ricoveri/dataRoll$ricoveri[dataRoll$data==picco]
dataRoll$decessi <- dataRoll$decessi/dataRoll$decessi[dataRoll$data==picco]

dataRolllong <- melt(dataRoll, id.vars = 1)



p <- ggplot(dataRolllong, aes(x = data, y = value, col = variable)) +
  geom_line(size = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B \n %Y", ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1.2), breaks = seq(0, 1.2, by = 0.1)) +
  scale_color_viridis_d()+
  labs(title = paste0("Confronto fra Casi, Ricoveri e Decessi Covid-19 in Italia - dati al ", last_update),
       x = "Data",
       y = paste0("Percentuale rispetto al picco casi del ", picco),
       subtitle = paste0("Casi per data di diagnosi - media mobile 7 giorni - ricoveri anticipati di ", offsetRicoveri,
                         " giorni e decessi anticipati di ", offsetDecessi, " giorni solo per allinemento grafico."),
       caption = "Elaborazione V. Nicoletta ispirato da @mugecevik | Dati ISS",
       col = "Variabile") +
  theme_minimal(base_size = 30) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename = paste0("casiDecessiRicoveri_", last_update,".png"), p,
       width = 800, height = 600, units='mm')

