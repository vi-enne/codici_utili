# Visualizzazione attività consentite senza Green Pass, con Green Pass base e/o con Green Pass rafforzato
# Fonte: https://www.governo.it/sites/governo.it/files/tabella_attivita_consentite.pdf


library(readxl)
library(DT)
library(shiny)
library(shinythemes)

attivita <- as.data.frame(read_xlsx("attivita.xlsx"))
specifiche <- as.data.frame(read_xlsx("specifiche.xlsx"))
data <- merge(attivita, specifiche)

tipo <- c(unique(data$Tipo), "Tutte")
colori <- unique(data$Colore)


server <- function(input, output, session) {
  output$mytable <- DT::renderDataTable(
    #server = FALSE,
    {
      if (input$type != "Tutte") {
        temp <-
          data[data$Tipo == input$type &
                 data$Colore == input$col, c("Tipo", "Esteso", "Colore", "GP")]
      } else{
        temp <-
          data[data$Colore == input$col, c("Tipo", "Esteso", "Colore", "GP")]
      }
      
      colnames(temp) <- c("Tipo", "Attività", "Colore", "Green Pass")
      temp
      
    },
    extensions = 'Buttons',
    options = list(
      # paging = TRUE,
      # dom = 'Bfrtip',
      dom = 't',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 50,
      scrollX = TRUE
    ),
    rownames = FALSE
  )
  
  
  
  
}

# UI ----

ui <- fluidPage(
  theme = shinytheme('readable'),
  headerPanel(title = "Attività consentite senza Green Pass, con Green Pass base e/o con Green Pass rafforzato 6/12/2021 al 15/1/2022"),
  
  radioButtons(
    "type",
    label = "Tipo attività",
    choices = tipo,
    selected = "Spostamenti",
    inline = T
  ),
  
  radioButtons(
    "col",
    label = "Colore",
    choices = colori,
    selected = "Bianca",
    inline = T
  ),
  
  hr(),
  
  DT::dataTableOutput("mytable"),
  
  hr(),
  
  
  tags$p(
    strong("Fonte:"),
    tags$a(
      href = 'https://www.governo.it/it/articolo/domande-frequenti-sulle-misure-adottate-dal-governo/15638',
      "Comunicato Governo Italiano",
      target = "_blank"
    ),
    " | ",
    tags$a(
      href = 'https://www.governo.it/sites/governo.it/files/tabella_attivita_consentite.pdf',
      "Tabella attività consentite",
      target = "_blank"
    ),
    
  ),
  
  tags$i(
    "Disclaimer: Si precisa che questa sezione tiene conto esclusivamente delle misure introdotte da disposizioni
  nazionali. Le Regioni e le Province autonome possono adottare specifiche ulteriori disposizioni restrittive,
  di carattere locale, per conoscere le quali è necessario fare riferimento ai canali informativi istituzionali
  dei singoli enti."
  ),
  
  hr(),
  
  tags$i(
    tags$a(href = 'https://twitter.com/vi__enne', "Vittorio Nicoletta", target =
             "_blank"),
    " | "
  ),
  tags$a(href = 'https://github.com/vi-enne/codici_utili/tree/main/attivitaGP', "Codice", target =
           "_blank")
  
  
  
)


shinyApp(ui = ui, server = server)
