#####
# Version 1.3
# Shiny Dashboard for displaying MLQ Statistics
# Developed by Joshua Mansfield for Major League Quadball
# Last Updated October 10, 2023

##### Loading necessary packages #####
# Shiny, DT, & leaflet used for dashboarding
# data.table & stringr used for data cleaning and wrangling

library(shiny)
library(data.table)
library(DT)
library(stringr)
library(leaflet)

##### Reading in necessary datasets #####

playerstats <- fread("playerstats.csv")
playoffstats <- fread("playoffstats.csv")
seriesstats <- fread("seriesstats.csv")
teamstats <- fread("teamstat.csv")

##### Cleaning Data #####
# Updating column names for legibility 
setnames(playerstats, c("Quadball Stops", "Beater Stops", "Beater Shifts", "Average Dodgeballs", "Plus/Minus", "Runner Catches", "Games Played"),
         c("QStops", "BStops", "BShifts", "AvgDodge", "PlusMinus", "RunnerCatches", "GamesPlayed"))
setnames(playoffstats, c("Quadball Stops", "Beater Stops", "Beater Shifts", "Average Dodgeballs", "Plus/Minus", "Runner Catches", "Games Played"),
         c("QStops", "BStops", "BShifts", "AvgDodge", "PlusMinus", "RunnerCatches", "GamesPlayed"))
setnames(seriesstats, c("Quadball Stops", "Beater Stops", "Beater Shifts", "Average Dodgeballs", "Plus/Minus", "Runner Catches", "Games Played"),
         c("QStops", "BStops", "BShifts", "AvgDodge", "PlusMinus", "RunnerCatches", "GamesPlayed"))

#Formatting dates
seriesstats[, ShowDate := format(Date, "%B %d")]

#Filtering datasets by position
chasers <- playerstats[(Goals + Assists + QStops + Turnovers + RunnerCatches) > 0]
beaters <- playerstats[BShifts > 0]
pochasers <- playoffstats[(Goals + Assists + QStops + Turnovers + RunnerCatches) > 0]
pobeaters <- playoffstats[BShifts > 0]
serieschasers <- seriesstats[(Goals + Assists + QStops + Turnovers + RunnerCatches) > 0]
seriesbeaters <- seriesstats[BShifts > 0]

#Cleaning positional variables and calculating per game averages
beaters[, AvgDodge := round(as.numeric(AvgDodge),2)]
pobeaters[, AvgDodge := round(as.numeric(AvgDodge),2)]

chasers[, gpg := round(Goals/GamesPlayed,2)]
chasers[, apg := round(Assists/GamesPlayed,2)]
chasers[, spg := round(QStops/GamesPlayed,2)]
chasers[, tpg := round(Turnovers/GamesPlayed,2)]
chasers[, rcpg := round(RunnerCatches/GamesPlayed,2)]
chasers[, gpg := round(Goals/GamesPlayed,2)]

beaters[, stoppg := round(BStops/GamesPlayed,2)]
beaters[, shiftpg := round(BShifts/GamesPlayed,2)]
beaters[, pmpg := round(PlusMinus/GamesPlayed,2)]

pochasers[, gpg := round(Goals/GamesPlayed,2)]
pochasers[, apg := round(Assists/GamesPlayed,2)]
pochasers[, spg := round(QStops/GamesPlayed,2)]
pochasers[, tpg := round(Turnovers/GamesPlayed,2)]
pochasers[, rcpg := round(RunnerCatches/GamesPlayed,2)]
pochasers[, gpg := round(Goals/GamesPlayed,2)]

pobeaters[, stoppg := round(BStops/GamesPlayed,2)]
pobeaters[, shiftpg := round(BShifts/GamesPlayed,2)]
pobeaters[, pmpg := round(PlusMinus/GamesPlayed,2)]

seriesbeaters[, AvgDodge := round(as.numeric(AvgDodge),2)]

#Create list of series names for filtering by list
series_names <- as.character(unique(factor(seriesstats$Series)))

##### Shiny Interface #####
ui <- fluidPage(
  
  includeCSS("webcss.css"), #Style manual for stats page
  
  tabsetPanel(
    type = "tabs",
    tabPanel("Regular Season Stats",
             fluidRow(
               checkboxInput("perGame", label = "Show Stats Per Game", value = FALSE)
             ),
             tabsetPanel(
               type = "tabs",
               tabPanel("Chasers/Keepers/Seekers",
                        fluidRow(
                          dataTableOutput("chaserSeasonTable")
                          )
                        ),
               tabPanel("Beaters",
                        fluidRow(style="padding-left:5em",
                          strong("\tAverage Dodgeballs:"), 
                          span("The average number of dodgeballs that beater's team possesses while they are on defense."),
                          br(),
                          strong("\tPlus/Minus:"),
                          span("Team goals scored minus goals against while on pitch.")
                        ),
                        fluidRow(
                          dataTableOutput("beaterSeasonTable")
                          )
                        )
               )
             ),
    tabPanel("Championship Stats",
             fluidRow(
               checkboxInput("CperGame", label = "Show Stats Per Game", value = FALSE)
             ),
             tabsetPanel(
               type = "tabs",
               tabPanel("Chasers/Keepers/Seekers",
                        fluidRow(
                          dataTableOutput("chaserPlayoffTable")
                        )
               ),
               tabPanel("Beaters",
                        fluidRow(style="padding-left:5em",
                                 strong("\tAverage Dodgeballs:"), 
                                 span("The average number of dodgeballs that beater's team possesses while they are on defense."),
                                 br(),
                                 strong("\tPlus/Minus:"),
                                 span("Team goals scored minus goals against while on pitch.")
                        ),
                        fluidRow(
                          dataTableOutput("beaterPlayoffTable")
                        )
               )
             )
    ),
    tabPanel("Series Stats",
             fluidRow(
               checkboxInput("all", label="Select All/None", value = TRUE)
             ),
             fluidRow(
               checkboxGroupInput(
                 "seriesSelect",
                 h6(""),
                 choices = series_names,
                 inline = TRUE,
                 selected = series_names
               )
             ),
             tabsetPanel(
               type = "tabs",
               tabPanel("Chasers/Keepers/Seekers",
                        fluidRow(
                          dataTableOutput("chaserSeriesTable")
                          )
                        ),
               tabPanel("Beaters",
                        fluidRow(
                          dataTableOutput("beaterSeriesTable")
                          )
                        )
             )),
    tabPanel("Team Stats",
             leafletOutput("map", height = 800)
    )
    )
  )

server <- function(input, output, session) {
  
  #Regular Season Chaser Stats Table
  output$chaserSeasonTable <- DT::renderDataTable( {
    if (!(input$perGame)) {
      datatable(
        chasers[, c("Player", "Team", "Goals", "Assists", "QStops", "Turnovers", "RunnerCatches", "GamesPlayed")][order(-Goals, -Assists, -QStops, Turnovers)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Goals", "Assists", "Stops", "Turnovers", "Runner Catches", "Games Rostered")
      )
    }
    else {
      datatable(
        chasers[, c("Player", "Team", "gpg", "apg", "spg", "tpg", "rcpg", "GamesPlayed")][order(-gpg, -apg, -spg, tpg)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Goals", "Assists", "Stops", "Turnovers", "Runner Catches", "Games Rostered")
      )
    }
  })
  
  #Playoff Chaser Stats Table
  output$chaserPlayoffTable <- DT::renderDataTable( {
    if (!(input$CperGame)) {
      datatable(
        pochasers[, c("Player", "Team", "Goals", "Assists", "QStops", "Turnovers", "RunnerCatches", "GamesPlayed")][order(-Goals, -Assists, -QStops, Turnovers)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Goals", "Assists", "Stops", "Turnovers", "Runner Catches", "Games Rostered")
      )
    }
    else {
      datatable(
        pochasers[, c("Player", "Team", "gpg", "apg", "spg", "tpg", "rcpg", "GamesPlayed")][order(-gpg, -apg, -spg, tpg)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Goals", "Assists", "Stops", "Turnovers", "Runner Catches", "Games Rostered")
      )
    }
  })
  
  #Regular Season Beater Stats Table
  output$beaterSeasonTable <- DT::renderDataTable( {
    if (!(input$perGame)) {
      datatable(
        beaters[, c("Player", "Team", "BStops", "BShifts", "AvgDodge", "PlusMinus", "GamesPlayed")][order(-BStops, -PlusMinus, -AvgDodge, -BShifts)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Stops", "Drives Played", "Average Dodgeballs", "Plus/Minus", "Games Rostered"),
        selection = "none"
      )
    }
    else {
      datatable(
        beaters[, c("Player", "Team", "stoppg", "shiftpg", "AvgDodge", "pmpg", "GamesPlayed")][order(-stoppg, -pmpg, -AvgDodge, -shiftpg)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Stops", "Drives Played", "Average Dodgeballs", "Plus/Minus", "Games Rostered"),
        selection = "none"
      )}
  })
  
  #Playoff Beater Stats Table
  output$beaterPlayoffTable <- DT::renderDataTable( {
    if (!(input$CperGame)) {
      datatable(
        pobeaters[, c("Player", "Team", "BStops", "BShifts", "AvgDodge", "PlusMinus", "GamesPlayed")][order(-BStops, -PlusMinus, -AvgDodge, -BShifts)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Stops", "Drives Played", "Average Dodgeballs", "Plus/Minus", "Games Rostered"),
        selection = "none"
      )
    }
    else {
      datatable(
        pobeaters[, c("Player", "Team", "stoppg", "shiftpg", "AvgDodge", "pmpg", "GamesPlayed")][order(-stoppg, -pmpg, -AvgDodge, -shiftpg)],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Player", "Team", "Stops", "Drives Played", "Average Dodgeballs", "Plus/Minus", "Games Rostered"),
        selection = "none"
      )}
  })
  
  #Create a reactive for which series are selected to display
  observe({
    updateCheckboxGroupInput(
      session, 
      'seriesSelect', 
      choices = series_names,
      inline = TRUE,
      selected = if(input$all) series_names
    )
  })
  
  #Series Chaser Stats Table
  output$chaserSeriesTable <- DT::renderDataTable( {
    if (is.null(input$seriesSelect)) {
      data.frame("No Series Selected" = c("No Series Selected")) }
    else {
      datatable(
        serieschasers[order(Date, Series, -Goals, -Assists, -QStops, Turnovers)][Series %in% input$seriesSelect, c("Series", "ShowDate", "Player", "Team", "Goals", "Assists", "QStops", "Turnovers", "RunnerCatches", "GamesPlayed")],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Series", "Date", "Player", "Team", "Goals", "Assists", "Stops", "Turnovers", "Runner Catches", "Games Rostered"),
        selection = "none"
        )
    }
  })
  
  #Series Beater Stats Table
  output$beaterSeriesTable <- DT::renderDataTable( {
    if (is.null(input$seriesSelect)) {
      data.frame("No Series Selected" = c("No Series Selected")) }
    else {
      datatable(
        seriesbeaters[order(Date, Series, -BStops, -PlusMinus, -AvgDodge, -BShifts)][Series %in% input$seriesSelect, c("Series", "ShowDate", "Player", "Team", "BStops", "BShifts", "AvgDodge", "PlusMinus", "GamesPlayed")],
        options = list(pageLength = 100),
        rownames = FALSE,
        colnames = c("Series", "Date", "Player", "Team", "Stops", "Drives Played", "Average Dodgeballs", "Plus/Minus", "Games Rostered"),
        selection = "none"
      )
    }
  })
  
  #import team icons for map
  AUSIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/d0fc1615-ac36-4026-8d5f-d394a6a126c9/OUTLAWS-PRIMARY.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  BOSIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/744203f9-def0-4a63-8881-c11eca1981b4/Boston+Forge+-+Main+Logo.png?format=300w",
                      iconWidth = 33, iconHeight = 46.97)
  CHIIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/dd59b7bd-ca1a-4b98-a0a6-87843c8badb1/Chicago+Meow+Meows.png?format=300w",
                      iconWidth = 42, iconHeight = 40.6)
  CLEIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/fd5bc346-3da2-45cf-b95e-59994be924b5/RIFF---primary.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  CLTIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/6e6071d9-ef3c-438f-8766-5296e6d35c51/primary-logo.png?format=300w",
                      iconWidth = 45, iconHeight = 38.1)
  DETIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/e671e8dc-ae65-4bcc-a988-b8385f6d340d/INNOVATORS-PRIMARY_LOGO.png?format=300w",
                      iconWidth = 33, iconHeight = 41.69)
  KCIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/cf5e9c22-03b0-4c1c-999a-f88f2f0ee0a9/Kansas_stampede-Primary.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  LCIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/513eb066-bef2-4194-bec6-f6812d7061ad/LEGENDS---primary.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  MPLSIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/7ef2c03d-a338-4896-895a-c783e27e1034/Primary-color.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  NOIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/2686cd0f-577d-4681-9105-0e7d2599e349/New+Orleans+Curse+Type+Final-01.png?format=300w",
                      iconWidth = 48, iconHeight = 37.12)
  NYCIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/38228870-c6bb-4221-aa0a-d9295a0cafec/NY_TITANS-PRIMARY_LOGO.png?format=300w",
                      iconWidth = 39, iconHeight = 40.69)
  OTTIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/3bbd99ce-4efd-4f1c-8871-28ef1e01b51a/OTTAWA-Primary.png?format=300w",
                      iconWidth = 45, iconHeight = 34.5)
  ROCIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/85efeac6-50f0-4b1b-b703-4e28d998123b/ROCHESTER+-+Primary.png?format=300w",
                      iconWidth = 33, iconHeight = 45.32)
  SAIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/d19d3bc3-aa86-49f0-879e-6fbad5136f9d/PRIMARY-color+%281%29.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  TORIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/a9c7394c-4bae-4b1d-b14a-7400c33265cd/PRIMARY---COLOR.png?format=300w",
                      iconWidth = 45, iconHeight = 45)
  WASIcon <- makeIcon(iconUrl = "https://images.squarespace-cdn.com/content/v1/62d08e5ea701e678513c24a6/a8474738-c13c-4634-8993-6592765185ab/ADMIRALS-PRIMARY_LOGO.png?format=300w",
                      iconWidth = 36, iconHeight = 41.28)
  
  #Create map template, manually adding franchise cities
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -83.85, lat = 37.45, zoom = 5) %>%
      addMarkers(layerId = "AUS", lng = -97.74, lat = 30.27, icon = AUSIcon) %>%
      addMarkers(layerId = "BOS", lng = -71.06, lat = 42.36, icon = BOSIcon) %>%
      addMarkers(layerId = "CHI", lng = -87.63, lat = 41.88, icon = CHIIcon) %>%
      addMarkers(layerId = "CLE", lng = -81.69, lat = 41.50, icon = CLEIcon) %>%
      addMarkers(layerId = "CLT", lng = -80.84, lat = 35.23, icon = CLTIcon) %>%
      addMarkers(layerId = "DET", lng = -83.05, lat = 42.33, icon = DETIcon) %>%
      addMarkers(layerId = "KC", lng = -94.58, lat = 39.10, icon = KCIcon) %>%
      addMarkers(layerId = "LC", lng = -95.09, lat = 29.51, icon = LCIcon) %>%
      addMarkers(layerId = "MPLS", lng = -93.27, lat = 44.98, icon = MPLSIcon) %>%
      addMarkers(layerId = "NO", lng = -90.07, lat = 29.95, icon = NOIcon) %>%
      addMarkers(layerId = "NYC", lng = -74.01, lat = 40.71, icon = NYCIcon) %>%
      addMarkers(layerId = "OTT", lng = -75.70, lat = 45.42, icon = OTTIcon) %>%
      addMarkers(layerId = "ROC", lng = -77.61, lat = 43.16, icon = ROCIcon) %>%
      addMarkers(layerId = "SA", lng = -98.49, lat = 29.43, icon = SAIcon) %>%
      addMarkers(layerId = "TOR", lng = -79.38, lat = 43.65, icon = TORIcon) %>%
      addMarkers(layerId = "WAS", lng = -77.04, lat = 38.91, icon = WASIcon)
  })
  
  #Adding interactive layers to the map
  observeEvent(input$map_marker_click, {
    id = input$map_marker_click$id
    showModal(modalDialog(
      title = paste0(id, " Stats"),
      DT::renderDataTable( {
          datatable(
            teamstats[Team == id, c("dodgeballs", "Offense", "Defense")],
            rownames = FALSE,
            colnames = c("Dodgeballs", "Scoring Rate (Offense)", "Stop Rate (Defense)"),
            selection = "none",
            options = list(dom = 't')
          )
      }),
      p("\n"),
      DT::renderDataTable( {
        datatable(
          teamstats[Team == id & dodgeballs == "Total", c("shotperc", "controlperc")],
          rownames = FALSE,
          colnames = c("Shooting Rate", "Control Rate"),
          selection = "none",
          options = list(dom = 't')
        )
      }),
      easyClose = TRUE)
      )
    })
}

shinyApp(ui = ui, server = server)