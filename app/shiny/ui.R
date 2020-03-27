# ui.R - user interface for NavOps app
# Authors: S. Smith, N. Haring,
# Created: 28JAN2020
#
#--------------------------------------------------------------------------------------------

# To clear the workspace
rm(list = ls())

# This automates (mostly) calling up package libraries
if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}
if (!require(DT)) {
  install.packages("DT")
  library(DT)
}
if (!require(fs)) {
  install.packages("fs")
  library(fs)
}
if (!require(Imap)) {
  install.packages("Imap")
  library(Imap)
}
if (!require(leaflet)) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require(leaflet.extras)) {
  install.packages("leaflet.extras")
  library(leaflet.extras)
}
if (!require(leaflet.providers)) {
  install.packages("leaflet.providers")
  library(leaflet.providers)
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
if (!require(serial)) {
  install.packages("serial")
  library(serial)
}
if (!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
if (!require(shinyjqui)) {
  install.packages("shinyjqui")
  library(shinyjqui)
}
if (!require(shinythemes)) {
  install.packages("shinythemes")
  library(shinythemes)
}
if (!require(shinyWidgets)) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}


# Set up the UI ----------------------------------------------------
navbarPage(
  "NavOps",
  theme = shinytheme("superhero"),
  tabPanel("Setup",
           #titlePanel("City of San Diego, NavOps: In House"),
           sidebarLayout(
             sidebarPanel(
               tags$head(tags$style(
                 HTML("label { font-size: 18px; font-weight:normal;}")
               )),
               dropdownButton(
                 radioButtons(
                   "vessel",
                   "Vessel",
                   c("Oceanus",
                     "Monitor III",
                     "Simulator"),
                   selected = "Simulator",
                   inline = FALSE,
                   width = NULL
                 ),
                 circle = FALSE,
                 status = "default",
                 size = "default",
                 icon = NULL,
                 label = "Vessel"
               ),
               dropdownButton(
                 checkboxGroupInput(
                   "equip",
                   label = p("Equipment"),
                   choiceNames = list(
                     "CTD 5",
                     "CTD 6",
                     "Double Van Veen",
                     "Hook and Line",
                     "Double Van Veen",
                     "Otter Trawl",
                     "Real Time Mooring",
                     "Other (see notes)"
                   ),
                   choiceValues = list(
                     "CTD 5",
                     "CTD 6",
                     "Double Van Veen",
                     "Hook and Line",
                     "Double Van Veen",
                     "Otter Trawl",
                     "Real Time Mooring",
                     "Other (see notes)"
                   ),
                   selected = NULL
                 ),
                 circle = FALSE,
                 status = "default",
                 size = "default",
                 icon = NULL,
                 label = "Equipment"
               ),
               dropdownButton(
                 checkboxGroupInput(
                   "crew",
                   label = p("Crew"),
                   choiceNames = list(
                     "Adam Webb",
                     "Adriano Feit",
                     "Andy Davenport",
                     "Ami Latker",
                     "Delci Primous",
                     "Gabriel Rodriguez",
                     "Gregory Welch",
                     "Lauren Valentino",
                     "Maiko Kasuya",
                     "Mark Phillips",
                     "Megan Lilly",
                     "Ryan Kempster",
                     "Stephanie Jaeger",
                     "Tim Douglas",
                     "Wendy Einright",
                     "Other (see notes)"
                   ),
                   choiceValues  = list(
                     "Adam Webb",
                     "Adriano Feit",
                     "Andy Davenport",
                     "Ami Latker" ,
                     "Delci Primous",
                     "Gabriel Rodriguez",
                     "Gregory Welch",
                     "Lauren Valentino",
                     "Maiko Kasuya",
                     "Mark Phillips",
                     "Megan Lilly",
                     "Ryan Kempster",
                     "Stephanie Jaeger",
                     "Tim Douglas",
                     "Wendy Einright",
                     "Other (see notes)"
                   ),
                   selected = NULL
                 ),
                 circle = FALSE,
                 status = "default",
                 size = "default",
                 icon = NULL,
                 label = "Crew"
               )
             ),
             mainPanel(fluidRow(
               column(
                 htmlOutput("day_plan"),
                 htmlOutput("station_selector"),
                 actionButton("submit_stations", "Submit"),
                 width = 6
               ),
               column(
                 checkboxInput("add_stations", "Add additional stations"),
                 conditionalPanel(condition = "input.add_stations == true",
                                  htmlOutput("all_station_selector")),
                 width = 6
               )
             )
             #htmlOutput("station_sorter"))
             ))),
  tabPanel("On Station",

           #titlePanel("City of San Diego, NavOps: In House"),
           sidebarLayout(
             sidebarPanel(
               tags$head(tags$style(
                 HTML("label { font-size: 16px; font-weight:normal;}")
               )),
               uiOutput("date"),
               uiOutput("time"),
               br(),
               
               uiOutput("lat"),
               uiOutput("lon"),
               uiOutput("distance_to_stn"),
               br(),
               uiOutput("depth"),
               br(),
               uiOutput("boat_speed"),
               br(),
               uiOutput("water_depth"),
               br(),
               uiOutput("wind_speed"),
               uiOutput("wind_dir"),
               uiOutput("air_temp"),
               br(),
               div(
                 style = "label-size:20px;",
                 fluidRow(column(
                   htmlOutput("weather"),
                   width = 6
                 ),
                 column(
                   numericInput(
                     "visibility",
                     "Visibiliy (mi)",
                     0,
                     min = 0,
                     max = 100,
                     step = 1,
                   ),
                   width = 6
                 )),
                 fluidRow(
                   column(
                     numericInput(
                       "wave_height",
                       "Wave Height (ft)",
                       0,
                       min = 0,
                       max = 20,
                       step = 1
                     ),
                     width = 6
                   ),
                   column(
                     numericInput(
                       "wave_period",
                       "Wave Period (s)",
                       0,
                       min = 0,
                       max = 30,
                       step = 1
                     ),
                     width = 6
                   )
                 ),
                 fluidRow(column(
                   htmlOutput("sea_state"),
                   width = 6
                 ),
                 column(
                   htmlOutput("water_color"),
                   width = 6
                 )),
                 checkboxInput("CTD", "CTD", value = TRUE, width = NULL),
                 checkboxInput("bacti", "Bacti", value = TRUE, width = NULL),
                 # textAreaInput(
                 #   "stn_notes",
                 #   "Station Notes",
                 #   value = "",
                 #   width = NULL,
                 #   
                 #   height = "100px",
                 #   placeholder = NULL,
                 #   resize = "both"
                 # )
                 htmlOutput("stn_notes"),
                 
               )
             ),
             mainPanel(
               htmlOutput("current_station"),
               actionButton("arrive", "Arrive"),
               tableOutput("arrive_table"),
               actionButton("depart", "Depart"),
               tableOutput("depart_table"),
               actionButton("next_station", "Next Station"),
               #downloadButton("datasheet", "Generate Datasheet")
               actionButton("get_datasheet", "Generate Datasheet"),
               br(),
               tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
               leafletOutput("map")
               
             )
           )),
  tabPanel(
    "Data Viewer",
    tags$style(
      HTML(
        ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #ffffff !important;
        }"
      )
    ), 
    mainPanel(
      fluidRow(
        column(
      DT::dataTableOutput("data_viewer_data", width = "120%"),
      br(),
      actionButton("saveBtn", "Save"),
      width = 12
        )
      ),
      fluidRow(
        column(DT::dataTableOutput("date_table"),
               width = 3),
        column(DT::dataTableOutput("crew_table"),
               width = 3),
        column(DT::dataTableOutput("equip_table"),
               width = 3),
        column(DT::dataTableOutput("tide_table"),
               width = 3), width = "150%"
      )
    )
  )
)

