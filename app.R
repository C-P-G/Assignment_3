#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#set working directory 
setwd("U:/SG5/forschung/quellmonitoring/08_Auswertungen/Dashboard/Assignment_3")
library(shiny)
# 0. Packages ------------------------------------------------------------------
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(shinythemes)
library(stringr)
library(mapboxapi)
library(DT) #for datatable
library(thematic) #for automatic coherent color scheme
#library(bslib)


#  Token ----------------------------------------------------------------------------------
#set key
token <- mapboxapi::mb_access_token("pk.eyJ1IjoiY2xhaXJlcGciLCJhIjoiY2toODBrNXVjMDh0czJ5bWljNm12Z3ZxOSJ9.6trWqxgjyLsdJQvZn4ZVVA",
                                    install = TRUE, overwrite = T)


# 1. Import der Datens?tzen ----------------------------------------------------
Thermobuttons <- read.csv2("data/alleThermobuttons.csv")


# 2. Formatierung der Daten ----------------------------------------------------
Thermobuttons$Datum <- as.Date(Thermobuttons$Datum, format = "%d.%m.%Y")

Thermobuttons$WT <- gsub("\\,", ".", Thermobuttons$WT) 
Thermobuttons$WT <- as.numeric(Thermobuttons$WT)#turns the temperatur data from character to numeric. Direct conversion does not work!

#remove trailing whitespaces around strings
Thermobuttons$Quellenname <- str_trim(Thermobuttons$Quellenname)

Thermobuttons$Quellenname <- as.factor(Thermobuttons$Quellenname)
Thermobuttons$Quellnummer <- as.factor(Thermobuttons$Quellnummer)

Thermobuttons <- Thermobuttons[!(Thermobuttons$Quellenname == ""),] # Leere Zellen ohne Quellennamenrausschmei?en 


# 2.2. Koordinaten der Quellen 
Koordinaten <- read.csv("data/Koordinaten_20_Quellen_3.csv", sep=";")
Koordinaten$Quellenname <- as.factor(Koordinaten$Quellenname)
Koordinaten$QUELLNR <- as.factor(Koordinaten$QUELLNR)
Koordinaten <- Koordinaten[!(Koordinaten$Quellenname == ""),] # Leere Zellen ohne Quellennamenrausschmei?en 


# 2.3 Rangerdata 
NPV_Monitoring <- read.csv2("data/Quellmonitoring.csv")
NPV_Monitoring$Datum <- as.Date(NPV_Monitoring$Datum, format = "%d.%m.%Y")
#Karstwasser Daten rausschmei?en (2009er)
NPV_Monitoring <- subset(NPV_Monitoring, BEOCODE != 555)
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"864")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"865")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"1")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"-1")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"-3")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"-4")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"-2")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"K?nigsbach Triftanlage")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"K?nigssee Auslauf")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"Klausbach")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"K?nigsbach Pegel")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"Schapbach Forstamt")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"Schapbach Pegel")
NPV_Monitoring<- subset(NPV_Monitoring, QUELLNR != 	"Wimbach Pegel")
NPV_Monitoring$QUELLNR <- as.factor(NPV_Monitoring$QUELLNR)


#merge Data with spatial information 

Thermobuttons <- merge(x = Thermobuttons, y = Koordinaten, by = "Quellenname")
Rangerspatial <- merge(NPV_Monitoring, Koordinaten, by = )
# Definition der Quellenfarbskala ----------------------------------------------
# f?r alle Graphen verwenden 

Quellenfarben <- c('0'="#F8766D", '300'="#BB9D00", '312'="#00B81F",'350'="#00C0B8", '441'="#00A5FF",'459'= "#E76BF3", '462'="#FF6C90", '503'="#C59900", '519'="#5BB300", '536'="#00C19C", '576'="#00ABFD", '578'="#DC71FA", '592'= "#FF689F", '615'="#E08B00", '816'="#85AD00", '828'="#AC88FF", '862'="#FF61C9", '863'="#ED8141", '978'= "#00BDD0")



Quellnamen <- levels(Thermobuttons$Quellenname)
Quelleigenschaften <- c("O2.Milligram", "O2.Prozent", "Leitfaehigkeit", "pH", "Schuettung")
# ------------------------------------------------------------------------------------

# Karten tab -------------------------------------------------------------------
# Leaflet Karte zeichnen 

#get statistics for water temperature 
sum_temp <- Thermobuttons %>% group_by(Quellenname) %>% 
    summarise(min = min(WT,na.rm = T),
              max = max(WT,na.rm = T),
              mean = mean(WT,na.rm = T),
              median = median(WT,na.rm = T),
              sd = sd(WT,na.rm = T)
    )

sum_ranger <- NPV_Monitoring %>% group_by(QUELLNR) %>%
    summarise_each(funs(mean(., na.rm = TRUE)))

    
    
Koordinaten_Ranger <- merge(x = sum_ranger, y = Koordinaten, by = "QUELLNR")


thematic_shiny() #automatic coherent theme 
# UI --------------------------------------------------------------------------
ui <- navbarPage( "Quellmonitoring NP Berchtesgaden", 
                 theme = shinytheme("darkly"),
    tabPanel("Thermobuttons", # first page 
    sidebarLayout(
        sidebarPanel(
            selectInput("QuellauswahlThermo",
                        label = "Quellauswahl",
                        choices = levels(Thermobuttons$Quellenname),
                        selected = "Klauswandl",
                        multiple = T), #lets choose several Quellen at the same time for comparison 
            dateRangeInput("date", "Zeitraum",
                           max = "2020-12-31",
                           start = "2014-01-01",
                           end =  "2020-12-30"),
            downloadButton("downloadThermoPlot", "Graphik speichern")
        ),
        mainPanel(
            plotOutput(outputId = "WTplot"), 
            br(),
            verbatimTextOutput("Thermostats")
          #  leafletOutput(outputId = "Karte") # add map to show (and later select springs)
        ))
    ),
tabPanel("Rangerdaten", #second page
    sidebarLayout(
        sidebarPanel(
            selectInput("QuellauswahlRanger", # Quelle auswählen
                        label = "Quellauswahl",
                        choices = levels(NPV_Monitoring$QUELLNR),
                        selected = "459",
                        multiple = T),
            selectInput("Parameter", "Parameterauswahl", #Parameter auswählem
                        #choices = names(NPV_Monitoring),
                        choices = c("O2.Milligram", "O2.Prozent", "Leitfaehigkeit", "pH",
                                    "TEMP_WASSER", "Schuettung", "TEMP_LUFT"),
                        selected = "Leitfaehigkeit",
                        multiple = F),
            dateRangeInput("Rangerdate", "Zeitraum",
                           max = "2020-12-31",
                           start = "2012-01-01",
                           end = "2020-12-30"),
            downloadButton("downloadRangerPlot", "Graphik speichern")
        ),
        mainPanel(
            plotOutput("Rangerplot"),
            verbatimTextOutput("Rangerstats")
        )
    )
),
tabPanel("Karte",
         div(class= "outer",
             tags$head(includeCSS("styles.css")),
             leafletOutput("thematic_map", width = "100%", height = "100%"),
             absolutePanel(top = 0, left = 60, 
                           draggable = T, 
                           selectInput("Parameter_Karte", "",
                                       choices = Quelleigenschaften)),
             absolutePanel(id = "logo", bottom = 20, left = 20, fixed = TRUE,
                           draggable = F, width = 60, height = "auto",
                           tags$a(href="https://www.nationalpark-berchtesgaden.bayern.de/",
                                  tags$img(src= "NP_Logo.jpg", height = '40', width = "auto"))))
        ),
tabPanel("Daten", 
         DT:: dataTableOutput("Daten"), #interactive datatable
         downloadButton("downloadAllData", "Download All Data")
         ),
tabPanel("Infos",
         tags$h4("Letztes Update",Sys.Date()),
         tags$a(href="https://www.nationalpark-berchtesgaden.bayern.de/nationalpark/forschung/umweltbeobachtung/quellenmonitoring.htm", "Quellmonitoring")
         
    
)
         
)


# Define server logic required to draw app 
server <- function(input, output) {

    filteredThermo <- reactive({
        req(input$date)
        req(input$QuellauswahlThermo)
        Thermobuttons %>% filter(Thermobuttons$Datum >= input$date[1] & Thermobuttons$Datum <= input$date[2]) %>% 
            filter(Quellenname == input$QuellauswahlThermo)
    })
    
    #Temperature plot
    output$WTplot <- renderPlot({
        ggplot(filteredThermo())+
            geom_line(aes(x = Datum, y = WT, color = Quellnummer))+
            geom_smooth(aes(x = Datum, y = WT))+
            scale_color_manual(values = Quellenfarben) +
            ggtitle(paste("Wassertemperatur Quelle"))+
            ylab("C")+
            xlab("")
    })
    output$downloadThermoPlot <- downloadHandler(
        filename = function(){paste(QuellauswahlThermo, ".png", sep = '')},
        content = function(file){
            ggsave(file, plot = last_plot())
        }
    )
    output$Thermostats <- renderPrint({
        summary(filteredThermo())
    })
    #second tab-------------------------------------------------
    filteredRanger <- reactive({
        req(input$Rangerdate)
        req(input$QuellauswahlRanger)
        NPV_Monitoring %>% filter(NPV_Monitoring$Datum >= input$Rangerdate[1] & NPV_Monitoring$Datum <= input$Rangerdate[2]) %>% 
            filter(QUELLNR == input$QuellauswahlRanger) %>% 
            filter()
    })
    statisticsRanger <- reactive({
        req(input$Rangerdate)
        req(input$QuellauswahlRanger)
        NPV_Monitoring %>% filter(NPV_Monitoring$Datum >= input$Rangerdate[1] & NPV_Monitoring$Datum <= input$Rangerdate[2]) %>% 
            filter(QUELLNR == input$QuellauswahlRanger) %>% select(input$Parameter) %>% 
            filter()
    })
    
    output$Rangerplot <- renderPlot({
        ggplot(filteredRanger())+
            geom_line(aes(x = Datum, y = get(input$Parameter), color = QUELLNR))+
            xlab("")+
            scale_color_manual(values = Quellenfarben)+
            ylab(input$Parameter)
    })
  # statsRanger <- reactive({
  #     filteredRanger %>% select(input$Parameter)
  # })
    output$Rangerstats <- renderPrint({
        summary(statisticsRanger())
    })
    # third tab ---------------------------------------------------------------
    # mapfilter <- eventReactive(input$Parameter_Karte,{
    #   Koordinaten_Ranger %>% select(input$Parameter_Karte)
    # })
  
  
    
   
    
    pal <- reactive(
     colorNumeric(
       palette = "Blues",
       domain = paste(sum_ranger, "input$Parameter_Karte", sep= "")
     )
   )
    
    
    output$thematic_map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lng = Koordinaten_Ranger$Y, lat = Koordinaten_Ranger$X,
                             color = input$Parameter_Karte,
                             stroke = FALSE, 
                             fillOpacity = 1,
                             popup = Koordinaten$Quellenname,
                             group = "CircleMarkers")%>% 
        addMapboxTiles(style_id = "ckkf6r4ov1y6j17pgw2nha4bf", username ="clairepg")
            # addLegend("bottomright", pal = pal, values = ~input$Parameter_Karte,
            #           title = paste("Durchschnittliche", input$Parameter_Karte),
            #           labFormat = labelFormat(suffix = "�S"),
            #           opacity = 1)

        
    })
    observeEvent(input$Parameter_Karte, {
      leafletProxy("thematic_map") %>% 
        clearGroup("CircleMarkers") %>% 
        addCircleMarkers(lng = Koordinaten_Ranger$Y, lat = Koordinaten_Ranger$X,
                         color = ~pal(Koordinaten_Ranger[[input$Parameter_Karte]]),
                         stroke = FALSE, 
                         fillOpacity = 1,
                         popup = Koordinaten$Quellenname,
                         group = "CircleMarkers")
    }
    )

    
   
    # fourth tab---------------------------------------------------------------
    # output$Daten <- DT::renderDataTable(
    #     datatable(data = Thermobuttons, style = "bootstrap", #bootstrap for overall theme
    #               options = list(autowidth = T),  
    #               filter = list(position = "top", clear = FALSE),  # allows filtering
    #               extensions = 'Buttons', options = list(
    #                   dom = 'Bfrtip',
    #                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #               )
    #               )
    # )
    data <- Thermobuttons
    
    #download all data 
    output$downloadAllData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file){
            write.csv(data,file)
        }
    )
    output$Daten <-   renderDT({
        datatable(Thermobuttons, style = "bootstrap",
                  options = list(autowidth = T),
                  filter = list(position = "top", clear = FALSE))
    })
    }


# Run the application 
shinyApp(ui = ui, server = server)
