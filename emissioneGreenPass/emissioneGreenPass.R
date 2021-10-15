#Codice per Green Pass emessi da test negativo

library(ggplot2)
library(ggtext)
library(scales)

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
data <- read.csv("https://raw.githubusercontent.com/ministero-salute/it-dgc-opendata/master/data/dgc-issued.csv")

data$data <- as.Date(data$data)
data <- data[order(data$data),]

png(filename = paste0("greenpass_", max(data$data), ".png"), width = 475, height = 250, units='mm', res = 300)


ggplot(data, aes(x=data, y=issued_for_tests)) +
  geom_bar(stat='identity', fill = c(rep("lightblue", nrow(data)-1), "#1565c0")) +
  labs(title = paste0("Green Pass emessi per test negativo al <b style='color:#1565c0'>", 
                      format(max(data$data), format="%d %B %Y"), "</b>"),
       x = "",
       y = "",
       subtitle = paste0(comma(tail(data$issued_for_tests,1),big.mark = ".", decimal.mark = ","),
       " emessi il <b style='color:#1565c0'>", format(max(data$data), format="%d %B %Y"), "</b>. ",
       "Il giorno prima ne erano stati emessi ",
       comma(tail(data$issued_for_tests,2)[1],big.mark = ".", decimal.mark = ",")),
       caption = paste0("Elaborazione grafica V. Nicoletta | Fonte: <i>github.com/ministero-salute/it-dgc-opendata</i> | CC-BY-4.0")
  ) +
  scale_x_date(date_breaks = "1 month", date_labels="%B %Y" ) +
  scale_y_continuous(labels = addUnits, breaks = seq(0, max(data$issued_for_tests)*1.2, by = 10^5)) +
  theme_minimal(base_size = 24) +
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown())

dev.off()

