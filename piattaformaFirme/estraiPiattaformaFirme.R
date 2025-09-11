library(jsonlite)
dataPatch <- T
jsonl_file <-
  "https://raw.githubusercontent.com/ondata/liberiamoli-tutti/main/referendum_iniziative_popolare/data/referendum_iniziative_popolare.jsonl"

df   <- stream_in(file(jsonl_file))
df$tipo <-
  ifelse(df$quorum < 5 * 10 ^ 5,
         "Legge di iniziativa popolare",
         "Referendum abrogativo")
df$url <- gsub("pnri.", "", df$url)
df$id <- paste0("[", df$id, "](", df$url, ")")
df$url <- paste0("[", df$url, "](", df$url, ")")
df$sito <- paste0("[", df$sito, "](", df$sito, ")")
df$percentualeQuorum <-
  paste0(round(df$sostenitori / df$quorum * 100, 2), "%")
temp <- df$titoloLeggeCostituzionale[df$id == 500013]
df$titoloLeggeCostituzionale[df$id == 500013] <-
  df$titolo[df$id == 500013]
df$titolo[df$id == 500013] <- temp
year <- as.Date(paste0(substr(df$dataApertura, 1, 4), "-09-30"))
df$dataFineRaccolta <- as.Date(df$dataFineRaccolta)
df$dataFineRaccolta <-
  ifelse((df$dataApertura < year) &
           (df$dataFineRaccolta > year) &
           (df$tipo == "Referendum abrogativo"),
         year,
         df$dataFineRaccolta
  )
df$dataFineRaccolta <-
  as.Date(df$dataFineRaccolta, origin = "1970-01-01")

if (dataPatch) {
  df$dataFineRaccolta <-
    ifelse(
      df$tipo == "Referendum abrogativo",
      ifelse(
        df$dataFineRaccolta < as.Date(df$dataInizioRaccolta) + 91,
        df$dataFineRaccolta,
        as.Date(df$dataInizioRaccolta) + 91
      ),
      ifelse(
        df$dataFineRaccolta < as.Date(df$dataInizioRaccolta) + 183,
        df$dataFineRaccolta,
        as.Date(df$dataInizioRaccolta) + 183
      )
    )
  df$dataFineRaccolta <-
    as.Date(df$dataFineRaccolta, origin = "1970-01-01")
}

df$status <-
  ifelse(df$dataFineRaccolta >= Sys.Date(), "In corso", "Conclusa")
df$color <- ifelse(
  df$sostenitori / df$quorum >= 1 ,
  "success",
  ifelse(df$status == "In corso" , "wait",
         "fail")
)

df <- df[order(df$dataApertura, decreasing = T), ]

write.csv(df, "piattaformaFirme/firmeReferendum.csv", row.names = F)
