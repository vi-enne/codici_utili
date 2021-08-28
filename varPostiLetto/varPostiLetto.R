library(ggplot2)
library(dplyr)


Sys.setlocale("LC_TIME", "Italian") #per avere date in italiano

# Storico Agenas estratto da onData
file = "https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/agenas/processing/postiletto-e-ricoverati-areaNonCritica_date.csv"

data = read.csv(file)

pop = read.csv("pop.csv") # http://dati.istat.it/Index.aspx?DataSetCode=DCIS_POPRES1
colnames(pop)[1] = "Regioni"


# Rilevazione annuale Ministero Salute
# https://www.dati.salute.gov.it/dati/dettaglioDataset.jsp?menu=dati&idPag=17
plMinSal = read.csv("https://www.dati.salute.gov.it/imgs/C_17_dataset_17_0_upFile.csv", sep = ";")
plMinSal = plMinSal[, c("Anno", "Descrizione.Regione", "Codice.disciplina", "Totale.posti.letto")]
last_year = tail(sort(unique(plMinSal$Anno)), 1)
plMinSal = plMinSal[plMinSal$Anno == last_year,]

#TI: codice 49
plMinSal_ti = plMinSal[plMinSal$Codice.disciplina == 49,] 
plMinSal_ti$Descrizione.Regione = gsub("PROV. AUTON. ", "P.A.", plMinSal_ti$Descrizione.Regione)
plMinSal_ti = plMinSal_ti[order(plMinSal_ti$Descrizione.Regione),]
tot_ti = data.frame(Regioni = pop$Regioni, tiMS = plMinSal_ti$Totale.posti.letto)

#Area Medica: 24 Infettive, 26 Generale e 68 Pneumologia
plMinSal_am = plMinSal[plMinSal$Codice.disciplina %in% c(24, 26, 68),] 
# Group by region
plMinSal_am = plMinSal_am %>% 
  group_by(Descrizione.Regione) %>% 
  summarise(Totale.posti.letto = sum(Totale.posti.letto))

plMinSal_am$Descrizione.Regione = gsub("PROV. AUTON. ", "P.A.", plMinSal_am$Descrizione.Regione)
plMinSal_am = plMinSal_am[order(plMinSal_am$Descrizione.Regione),]
tot_am = data.frame(Regioni = pop$Regioni, amMS = plMinSal_am$Totale.posti.letto)

# Merge
data = merge(data, pop)
data = merge(data, tot_am)
data = merge(data, tot_ti)

#Plots

png(filename = paste0("plAM_", max(as.Date(data$data)), ".png"), width = 800, height = 600, units='mm', res = 300)
ggplot(data, aes(x = as.Date(data), y = PL.in.Area.Non.Critica/Popolazione *10^5, group = Regioni)) +
  geom_line(size = 2, col = "blue") +
  geom_line(aes(y = amMS/Popolazione *10^5) ,size = 2, col = "purple") +
  facet_wrap(~ Regioni, ncol = 7) +
  ylim(c(0,NA)) +
  theme_minimal(base_size = 24) +
  scale_x_date(date_breaks = "3 month", date_labels="%b-%y" ) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  labs(title = paste("Posti Letto attivi in Area Medica ogni 100mila abitanti dal", 
                     min(as.Date(data$data)),
                     "al",
                     max(as.Date(data$data)), 
                     " (in blu)"),
       x = "Data", 
       y = "Posti Letto in Area Medica / Popolazione * 100mila",
       subtitle = paste0("Area Medica/Non Critica: posti letto malattie infettive, medicina generale e pneumologia
       La linea viola si riferisce all'ultima rilevazione Ministero Salute disponibile del ", last_year),
       caption = "Elaborazione V. Nicoletta | Dati ISTAT, Ministero Salute e AGENAS (estratti da onData)") 

dev.off()

png(filename = paste0("plTI_", max(as.Date(data$data)), ".png"), width = 800, height = 600, units='mm', res = 300)
ggplot(data, aes(x = as.Date(data), y = PL.in.Terapia.Intensiva/Popolazione *10^5, group = Regioni)) +
  geom_line(size = 2, col = "darkgreen") +
  geom_line(aes(y = tiMS/Popolazione *10^5) ,size = 2, col = "purple") +
  facet_wrap(~ Regioni, ncol = 7) +
  ylim(c(0,NA)) +
  theme_minimal(base_size = 24) +
  scale_x_date(date_breaks = "3 month", date_labels="%b-%y" ) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  labs(title = paste("Posti Letto attivi in Terapia Intensiva ogni 100mila abitanti dal", 
                     min(as.Date(data$data)),
                     "al",
                     max(as.Date(data$data)),
                     " (in verde)"),
       x = "Data", 
       y = "Posti Letto in Terapia Intensiva / Popolazione * 100mila",
       subtitle = paste0("La linea viola si riferisce all'ultima rilevazione Ministero Salute disponibile del ", last_year),
       caption = "Elaborazione V. Nicoletta | Dati ISTAT, Ministero Salute e AGENAS (estratti da onData)") 

dev.off()

