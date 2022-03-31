# Plot occupazione AM e TI Regioni/PA e colori zone secondo DL 23 luglio 2021

library(ggplot2)
library(ggrepel)
library(gganimate)


Sys.setlocale("LC_TIME", "Italian") #per avere date in italiano

# Colori
# https://sashamaps.net/docs/resources/20-colors/
col_vector <- c(
  '#e6194b',
  '#3cb44b',
  '#ffe119',
  '#4363d8',
  '#f58231',
  '#911eb4',
  '#46f0f0',
  '#f032e6',
  '#bcf60c',
  '#fabebe',
  '#008080',
  '#e6beff',
  '#9a6324',
  '#fff796',
  '#800000',
  '#aaffc3',
  '#808000',
  '#ffd8b1',
  '#000075',
  '#808080',
  '#000000'
)

# Zoom
maxZ <- 1.25
zoom <- c(0, maxZ) #min e max percentuale occupazione da 0 a 1

# Lettura dati
file <- "https://raw.githubusercontent.com/ondata/covid19italia/master/webservices/agenas/processing/postiletto-e-ricoverati-areaNonCritica_date.csv"
postiLetto <- read.csv(file)
postiLetto <- postiLetto[, c(1:5, 8)]

output <- unique(rbind(postiLetto, read.csv("agenas2020.csv"))) #alcune date mancanti aggiunte manualmente

# Calcolo occupazioni in percentuale
output$AM <- output$Ricoverati.in.Area.Non.Critica / output$PL.in.Area.Non.Critica
output$TI <- output$Ricoverati.in.Terapia.intensiva / output$PL.in.Terapia.Intensiva

# Nomi regioni e sigle
output$Area = output$Regioni
output$Area2 = c(
  "ABR",
  "BAS",
  "CAL",
  "CAM",
  "EMR",
  "FVG",
  "LAZ",
  "LIG",
  "LOM",
  "MAR",
  "MOL",
  "PAB",
  "PAT",
  "PIE",
  "PUG",
  "SAR",
  "SIC",
  "TOS",
  "UMB",
  "VDA",
  "VEN"
)

# Conversione formato date
output$data = as.Date(output$data)
output = output[order(output$data),]

# Eventualmente selezionare data di partenza
# output = output[output$data >= "2020-12-01",]


# Plot ----
p1 <- ggplot(output, aes(AM, TI, group = Area)) +
  geom_rect(
    fill = "yellow",
    xmin = .15,
    xmax = 1.5,
    ymin = .10,
    ymax = 1.5,
    alpha = 0.03
  ) +
  geom_rect(
    fill = "orange",
    xmin = .30,
    xmax = 1.5,
    ymin = .20,
    ymax = 1.5,
    alpha = 0.03
  ) +
  geom_rect(
    fill = "red",
    xmin = .40,
    xmax = 1.5,
    ymin = .30,
    ymax = 1.5,
    alpha = 0.03
  ) +
  geom_line(data = output,
            aes(col = Area),
            size = 2,
            alpha = 0.5) +
  geom_point(
    data = output,
    aes(fill = Area),
    shape = 21,
    size = 4,
    col = "black"
  ) +
  geom_label(data = output,
             aes(label = Area2),
             size = 8) +
  labs(
    title = paste(
      "Occupazione Area Medica e Terapia Intensiva persone positive Covid-19"
    ),
    x = "Area Medica (letti da AGENAS)",
    y = "Terapia Intensiva (letti da AGENAS)",
    subtitle = paste0(
      "Colori secondo DL 105/2021 senza incidenza per semplicità. ATTENZIONE: questo grafico NON ha senso!"
    ),
    caption = "Elaborazione V. Nicoletta @vi__enne | Dati AGENAS (estratti da onData), Protezione Civile e Ministero Salute"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = zoom,
    breaks = seq(0, maxZ, by = 0.05)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = zoom,
    breaks = seq(0, maxZ, by = 0.05)
  ) +
  theme_minimal(base_size = 24) +
  theme(
    legend.position = c(0.2,0.85),
    legend.background = element_rect(
      fill = "white",
      size = 0.5,
      linetype = "solid"
    ),
    legend.box.just = "center"
  ) +
  geom_label(aes(x = 0.8, y = 0.05,
                 label =   format(data, format="%d %B %Y")), 
             size = 14, col = "black") +
  scale_color_manual(name = paste0("Regione/PA"),
                    labels = unique(output$Area),
                    values = col_vector) +
  scale_fill_manual(name = paste0("Regione/PA"),
                     labels = unique(output$Area),
                     values = col_vector) +
  guides(fill = "none") 


#Animation ----
p2 <- p1 +
  transition_reveal(data) +
  ease_aes('linear')

anim_save(
  p2,
  filename = paste0("occupazioneGIF_", Sys.Date(), ".gif"),
  width = 470,
  height = 485,
  units = 'mm',
  nframes = 500, fps=21,
  end_pause = 30,
  res = 50
)
