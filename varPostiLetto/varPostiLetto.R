library(ggplot2)

Sys.setlocale("LC_TIME", "Italian") #per avere date in italiano

# Storico Agenas estratto da onData
file = "https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/agenas/processing/postiletto-e-ricoverati-areaNonCritica_date.csv"

data = read.csv(file)

pop = read.csv("pop.csv") # http://dati.istat.it/Index.aspx?DataSetCode=DCIS_POPRES1
colnames(pop)[1] = "Regioni"

data = merge(data, pop)

png(filename = paste0("plAM_", max(as.Date(data$data)), ".png"), width = 800, height = 600, units='mm', res = 300)
ggplot(data, aes(x = as.Date(data), y = PL.in.Area.Non.Critica/Popolazione *10^5, group = Regioni)) +
  geom_line(size = 2, col = "blue") +
  facet_wrap(~ Regioni, ncol = 7) +
  theme_minimal(base_size = 24) +
  scale_x_date(date_breaks = "3 month", date_labels="%b-%y" ) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  labs(title = paste("Posti Letto attivi in Area Medica ogni 100mila abitanti dal", 
                     min(as.Date(data$data)),
                     "al",
                     max(as.Date(data$data))),
       x = "Data", 
       y = "Posti Letto in Area Medica / Popolazione * 100mila",
       subtitle = "Area Medica/Non Critica: posti letto malattie infettive, medicina generale e pneumologia",
       caption = "Elaborazione V. Nicoletta | Dati ISTAT e AGENAS estratti da onData") 

dev.off()

png(filename = paste0("plTI_", max(as.Date(data$data)), ".png"), width = 800, height = 600, units='mm', res = 300)
ggplot(data, aes(x = as.Date(data), y = PL.in.Terapia.Intensiva/Popolazione *10^5, group = Regioni)) +
  geom_line(size = 2, col = "darkgreen") +
  facet_wrap(~ Regioni, ncol = 7) +
  theme_minimal(base_size = 24) +
  scale_x_date(date_breaks = "3 month", date_labels="%b-%y" ) +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  labs(title = paste("Posti Letto attivi in Terapia Intensiva ogni 100mila abitanti dal", 
                     min(as.Date(data$data)),
                     "al",
                     max(as.Date(data$data))),
       x = "Data", 
       y = "Posti Letto in Terapia Intensiva / Popolazione * 100mila",
#       subtitle = "Area Medica/Non Critica: posti letto malattie infettive, medicina generale e pneumologia",
       caption = "Elaborazione V. Nicoletta | Dati ISTAT e AGENAS estratti da onData") 

dev.off()

