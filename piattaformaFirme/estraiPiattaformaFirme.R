library(jsonlite)

jsonl_file <- "https://raw.githubusercontent.com/ondata/liberiamoli-tutti/main/referendum_iniziative_popolare/data/referendum_iniziative_popolare.jsonl"

df   <- stream_in(file(jsonl_file))
df$tipo <- ifelse(df$quorum < 5*10^5, "Legge di iniziativa popolare", "Referendum abrogativo")
df$url <- paste0("[",df$url,"](", df$url, ")")
df$sito <- paste0("[",df$sito,"](", df$sito, ")")
df$percentualeQuorum <- paste0(round(df$sostenitori/df$quorum * 100,2), "%")
temp <- df$titoloLeggeCostituzionale[df$id == 500013]
df$titoloLeggeCostituzionale[df$id == 500013] <- df$titolo[df$id == 500013]
df$titolo[df$id == 500013] <- temp

write.csv(df, "piattaformaFirme/firmeReferendum.csv", row.names = F)
