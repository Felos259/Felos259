library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    
    titlePanel("Ladestatistik sortieren"),
    
    sidebarPanel(
      fileInput("upload", "Daten auswählen [.xlsx-Datei]",
                buttonLabel = "Durchsuchen...", placeholder = "keine Datei ausgewählt",
                multiple = FALSE,
                accept = c(".xlsx")),
      downloadButton("download")
    ),
    fluidRow(
      DT::dataTableOutput('x1'),
      DT::dataTableOutput('x2')
      #verbatimTextOutput("print") #Tabellen-Zeug - später rausnehmen
    )
  ),
  server = function(input, output, session) {
    
    # In this edited example x is now a reactive expression, dependent on input$upload
    
    # Key to the solution is the use of reactiveValues, stored as vals
    vals <- reactiveValues(BuH_GmbH = NULL, IB_BuH = NULL, IB_BuH_NRW = NULL, Sonstige = NULL)
    
    
    datenAuslesen = function(x, datensatz){
      output$x = DT::renderDataTable(vals$datensatz, selection = 'none', rownames = FALSE, edit = TRUE)
      
      proxy = dataTableProxy('x')
      
      observeEvent(input$x_cell_edit, {
        c
        str(info)
        i = info$row
        j = info$col + 1
        v = info$value
        # Below is the crucial spot where the reactive value is used where a reactive expression cannot be used
        vals$datensatz[i, j] <<- DT:::coerceValue(v, vals$datensatz[i, j])
        replaceData(proxy, vals$datensatz, resetPaging = FALSE, rownames = FALSE)
      })
    }
    
    
    observe({
      
      
      # input$upload will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$upload)
      tryCatch(
        {
          rohdaten <- read.xlsx(input$upload$datapath,
                                header = TRUE,
                                stringsAsFactors = TRUE, sheetName = "Daten")
          daten <- subset(rohdaten, select=c('Ladepunkt.ID', 'Zugangsmedium..Name', 'Beginn', 'Ende', 'Energie.nach.Korrektur', 'Dauer.nach.Korrektur'))
          daten_clean <- na.omit(daten)
          
          #Möglichkeit die verschiedenen ingenieurbüros zuzuweisen wird dem datensatz hinzugefügt 
          daten_clean["identifier"] <- substring(daten_clean$'Zugangsmedium..Name', first = 1, last = 10)
          
          #subsets mit den speziellen Angaben der Firmen werden eingespeichert         
          datenBuH <-  daten_clean %>%
            filter(grepl('BuH GmbH', daten_clean$identifier))
          
          datenIBBuH <-  daten_clean %>%
            filter(grepl('IB BuH', daten_clean$identifier) & !grepl('IB BuH NRW', daten_clean$identifier))
          
          datenIBBuHNRW <-  daten_clean %>%
            filter(grepl('IB BuH NRW', daten_clean$identifier))
          
          datenSonstige <- daten_clean %>%
            filter(!grepl('BuH', daten_clean$identifier))
          
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      # Reactive values updated from x
      vals$BuH_GmbH <- datenBuH
      vals$IB_BuH <- datenIBBuH
      vals$IB_BuH_NRW <- datenIBBuHNRW
      vals$Sonstige <-  datenSonstige
    })
    
    # output$print <- renderPrint({
    #   vals$BuH_GmbH
    # })
    
    datenAuslesen(1, BuH_GmbH)
    
    datenAuslesen(2, IB_BuH)
    
    datenAuslesen(3, IB_BuH_NRW)
    
    datenAuslesen(4, Sonstige)
    
    data_list <- reactive({
      list(
        BuH_GmbH = vals$BuH_GmbH,
        IB_BuH = vals$IB_BuH,
        IB_BuH_NRW = vals$IB_BuH_NRW,
        Sonstige = vals$Sonstige
      )
    })
    
    output$download <- downloadHandler(filename = function() {
      paste("Ladestatistik", Sys.Date(), ".xlsx", sep=" ")
    }, 
    content = function(file){write_xlsx(data_list(), path = file)#, row.names = FALSE
    })
    
    
    
    
  }
  
  
  
)
