library(data.table)
library(tidyr)
library(qdapRegex)

file <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

data <- as.data.frame(fread(file, encoding = "UTF-8"))

umbria <- data[data$denominazione_regione == "Umbria", c("data", "ricoverati_con_sintomi", "note")]
umbria$data <- as.Date(umbria$data)
umbria <- umbria[umbria$data >= "2021-01-31",]

umbria$note <- sub(".*i fa presente che ", "", umbria$note) 
umbria$note <- sub(".*i fa presente come ", "", umbria$note) 
umbria$note <- sub(" dei ricoveri.*", "", umbria$note) 
umbria$note[umbria$data=="2021-02-02"] <- 45
umbria$note[umbria$data=="2021-03-22"] <- 7
umbria$note[umbria$data=="2021-07-06"] <- 1
umbria$note[umbria$data=="2021-12-24"] <- 0
umbria$note[umbria$data=="2022-01-13"] <- 5


temp <- qdapRegex::ex_between(umbria$note[umbria$data>="2022-01-25"], "i fa presente che ", " dei ricoveri")
umbria$note[umbria$data>="2022-01-25"] <- unlist(lapply(temp, function(x) sum(as.numeric(x))))

umbria$note <- sub("uno", "1", umbria$note) 
umbria$note <- sub("due", "2", umbria$note) 
umbria$note <- sub("tre", "3", umbria$note) 

umbria$note <- as.numeric(umbria$note)
umbria$note[is.na(umbria$note)] <- 0

umbria$ricoverati_AM_covid <- umbria$ricoverati_con_sintomi - umbria$note

umbriaPlot <- umbria[, c("data", "ricoverati_AM_covid", "note")]
colnames(umbriaPlot)[2] <- "Ricoverati positivi NON TI in discipline di Area Medica (qui intesi PER Covid-19)"
colnames(umbriaPlot)[3] <- "Ricoverati positivi NON TI in discipline NON di Area Medica (qui intesi CON Covid-19)"

write.csv(umbriaPlot, file = "hospUmbria/umbriaHosp.csv", row.names = F)
  

