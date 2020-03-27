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
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}


# Get up-to-date station info from TheCritter-------------------------------------------------

uid <- ""
pwd <- ""

oracle.connect <- function(uid, pwd) {
  if(!require(RODBC)) {install.packages("RODBC"); library(RODBC)}

  connStr <- paste(
    "DRIVER={Oracle in OraClient11g_home1}",#! new updated code following computer upgrades in 2014
    #"DRIVER={Oracle in OraClient10g_home1}", #! old code before computer upgrades in 2014
    "SERVER=CRITTER9",
    paste("UID=", uid, sep = ""),
    paste("PWD=", pwd, sep = ""),
    "DBQ=CRITTER9",
    "QTO=F",
    sep = ";"
  )
  odbcDriverConnect(connStr)
}

CritterCon <- oracle.connect(uid, pwd)

statementSTN <- paste("
    SELECT DISTINCT *
    FROM XR_STATION
    INNER JOIN XR_LOGIN
    ON XR_STATION.STATION = XR_LOGIN.STATION
    WHERE project = 'PL'
    OR project = 'SBOO'
    OR project LIKE 'REG%'
    OR project LIKE 'BIGHT%'
  ", sep="")

# XR_Station <- sqlQuery(CritterCon, statementSTN, errors = FALSE, rows_at_time = 1024) # takes about 4 seconds, keep in mind
# # For now, limit to WQ of interest
# setDT(XR_Station)
# XR_Station_nodupes <- XR_Station[SAMPLE_TYPE == "WQ KELP" | SAMPLE_TYPE == "WQ ITP"] # It's easier to just filter with %in% then remove duplicates, since one station may exist in >1 Sample_type
# names(XR_Station_nodupes) <- tolower(names(XR_Station_nodupes))
# 
# 
# #kelp_wq <- c("")
# ITP_north <- c("I37", "I34", "I33", "I28", "I29", "I30", "I31", "I32", "I36", "I35", "I38")
# ITP_outfall <- c("I27", "I39", "I26", "I25", "I24", "I40", "I19", "I18", "I17", "I16", "I12", "I15", "I14", "I22", "I23")
# ITP_south <- c("I20", "I7", "I1", "I2", "I3", "I4", "I5", "I6", "I11", "I10", "I9", "I8", "I13", "I21")
# 
# kelp_data <- XR_Station_nodupes %>% 
#   dplyr::filter(., sample_type == "WQ KELP")
# kelp_data <- setDT(kelp_data_B)[kelp_data_B[, .I[!(duplicated(station))], ]]
# kelp_data$sample_type <- "IWCP-kelp-WQ"
# 
# IWCP_north <- XR_Station_nodupes %>%
#   dplyr::filter(., station %in% ITP_north)
# IWCP_north <- setDT(IWCP_north_B)[IWCP_north_B[, .I[!(duplicated(station))], ]]
# IWCP_north$sample_type <- "IWCP-north-WQ"
# 
# IWCP_outfall <- XR_Station_nodupes %>%
#   dplyr::filter(., station %in% ITP_outfall)
# IWCP_outfall <- setDT(IWCP_outfall)[IWCP_outfall[, .I[!(duplicated(station))], ]]
# setkey(IWCP_outfall, station)
# I17_df <- data.table(station = "I17", latitude = 32.53783, longitude = -117.1780, depth_meter = 25) #I17 is not in XR_STATION for some reason
# IWCP_outfall <- merge(IWCP_outfall, I17_df, all = TRUE)
# IWCP_outfall$sample_type <- "IWCP-outfall-WQ"
# 
# IWCP_south <- XR_Station_nodupes %>%
#   dplyr::filter(., station %in% ITP_south)
# IWCP_south <- setDT(IWCP_south)[IWCP_south[, .I[!(duplicated(station))], ]]
# IWCP_south$sample_type <- "IWCP-south-WQ"

map_data <-
  read.csv("map-data.csv", stringsAsFactors = FALSE)
kelp_data <- map_data %>%
  dplyr::filter(., sample_type == "kelp-WQ")
IWCP_north <- map_data %>%
  dplyr::filter(., sample_type == "IWCP-north-WQ")
IWCP_south <- map_data %>%
  dplyr::filter(., sample_type == "IWCP-south-WQ")
IWCP_outfall <- map_data %>%
  dplyr::filter(., sample_type == "IWCP-outfall-WQ")

tide_data <- read.table(
  file = "Tide_txt.txt", 
  sep = "\t",
  header = FALSE,
  skip = 18,
  col.names = c("Date", "Day", "Time", "Height", "State"),
  stringsAsFactors = FALSE
)

stations_available <- map_data %>%
  dplyr::filter(., sample_type %in% "kelp-WQ") %>%
  dplyr::arrange(., station_order) %>%
  select("station")

# connect to the NMEA simulator ------------------------------------------------------
s_con <- NULL
NMEA_poll <- NULL
s_con_GPS <- NULL
s_con_wind <- NULL
s_con_depth <- NULL
GPS_poll <- NULL
last_GPS <- "$GPRMC,214629.00,A,3265.5761,N,11723.6008,W,0.1,303.8,270220,11.3,E*7B"
wind_poll <- NULL
last_wind <- "$WIMWV,264,T,008.0,N,AL"
depth_poll <- NULL
incrementer <- 1
curr_station <- NULL
csv_for_PDF <- NULL
raw_data_sofar <- NULL
weather_frame <- NULL
seastate_frame <- NULL
watercolor_frame <- NULL
Mdy <- NULL
prep_datafile <- NULL
master_datatable <- NULL
display_table <- NULL
final_table <- NULL
filename_date <- format(Sys.Date(),"%d%b%Y")
tide_tbl <- NULL


# TCP/IP connection
connect <- function() {
  s_con <<- socketConnection("127.0.0.1", port = 55555, open = "a+")
  Sys.sleep(1)
  NMEA_poll <<- readLines(s_con, n = 18)
  close(s_con)
  return(NMEA_poll)
  
}

#Find all thse com numbers before attempting
# # serial connection
connect_GPS <- function() {
  s_con_GPS <<- serialConnection("BoatCom20", port = "COM20", mode = "4800,n,8,1", buffering = "none")
  open(s_con_GPS)
  Sys.sleep(1) #necessary?
  GPS_poll <<- tryCatch({read.serialConnection(s_con_GPS)},
                        error=function(cond) {
                          message("tryCatch_GPS has caught an error")
                          message("Orig. Masseage:")
                          message(cond)
                          return(last_GPS)
                        })
  #close(s_con_GPS)
  return(GPS_poll)

}

connect_wind <- function() {
  s_con_wind <<- serialConnection("BoatCom8", port = "COM8", mode = "4800,n,8,1", buffering = "none")
  open(s_con_wind)
  Sys.sleep(1)
  wind_poll <<- tryCatch({read.serialConnection(s_con_wind)},
                         error=function(cond) {
                           message("tryCatch_Wind has caught an error")
                           message("Orig. Masseage:")
                           message(cond)
                           return(last_wind)
                         })
  #close(s_con_wind)
  return(wind_poll)

}

 connect_depth <- function() {
   s_con_depth <<- serialConnection("BoatCom7", port = "COM7", mode = "4800,n,8,1", buffering = "none")
   open(s_con_depth)
   Sys.sleep(1)
   depth_poll <<- tryCatch({read.serialConnection(s_con_depth)},
                           error=function(cond) {
                             message("tryCatch_Depth has caught an error")
                             message("Orig. Masseage:")
                             message(cond)
                             return(last_depth)
                           })
   #close(s_con_depth)
   return(depth_poll)

 }

getGPRMC <- function(data) {
  gps_ans <- list(rmc = NULL, rest = data)
  rxp <-
    "\\$GPRMC(,[^,]*){12}\\*[0-9,A-F]{2}"
  beg <- regexpr(rxp, data)
  if (beg == -1)
    return(gps_ans)
  end <-
    beg + attr(beg, "match.length")
  sub <-
    substr(data, beg, end - 6)
  gps_ans$rmc <-
    strsplit(sub, ",")[[1]]
  names(gps_ans$rmc) <- c(
    "id_rmc",
    "UTC",
    "status",
    # A=valid, V=warning (valid satellite fix but but below internal quality threshold)
    "lat",
    "N/S",
    "long",
    "E/W",
    "boat speed (knots)",
    # Speed over ground
    "cog (deg)",
    # course over ground, degs.
    "date (ddmmyy)" # ddmmyy
  )
  gps_ans$rest <- substr(data, end, nchar(data))
  return(gps_ans)
}

# parse wind data
# WI = Weather Wnstrumentation; MWD = wind speed and direction (I think)
getWIMWD <- function(data) {
  wind_ans <- list(mwd = NULL, rest = data)
  rxp <-
    "\\$WIMWD(,[^,]*){8}\\*[0-9,A-F]{2}"
  beg <- regexpr(rxp, data)
  if (beg == -1)
    return(wind_ans)
  end <-
    beg + attr(beg, "match.length")
  sub <-
    substr(data, beg, end - 6)
  wind_ans$mwd <-
    strsplit(sub, ",")[[1]]
  names(wind_ans$mwd) <- c(
    "id_mwd",
    "wind angle",
    # degrees
    "reference",
    # R=relative, T=true
    "wind angle2",
    # degrees
    "units?",
    # K=knots/h, M=Meters/h
    "wind speed",
    "dont know",
    "checksum"
  )
  wind_ans$rest <- substr(data, end, nchar(data))
  return(wind_ans)
}

# Has fewer outputs than simulator, will need to re-evaluate indexing 
getWIMWV_true <- function(data) {
  wind_true_ans <- list(mwd = NULL, rest = data)
  rxp <-
    "\\$WIMWV(,[^,]*){1}\\,T(,[^,]*){3}\\*[0-9,A-F]{2}"
  beg <- regexpr(rxp, data)
  if (beg == -1)
    return(wind_true_ans)
  end <-
    beg + attr(beg, "match.length")
  sub <-
    substr(data, beg, end - 6)
  wind_true_ans$mwd <-
    strsplit(sub, ",")[[1]]
  names(wind_true_ans$mwd) <- c(
    "id_mwv",
    "wind angle",
    # degrees
    "reference",
    # R=relative, T=true
    "wind speed",
    "units"
    # K=knots/h, M=Meters/h
    #"status"
    #A = valid, V = invalid
  )
  wind_true_ans$rest <- substr(data, end, nchar(data))
  return(wind_true_ans)
}

getWIMWV_rel <- function(data) {
  wind_rel_ans <- list(mwd = NULL, rest = data)
  rxp <-
    "\\$WIMWV(,[^,]*){1}\\,R(,[^,]*){3}\\*[0-9,A-F]{2}"
  beg <- regexpr(rxp, data)
  if (beg == -1)
    return(wind_rel_ans)
  end <-
    beg + attr(beg, "match.length")
  sub <-
    substr(data, beg, end - 6)
  wind_rel_ans$mwd <-
    strsplit(sub, ",")[[1]]
  names(wind_rel_ans$mwd) <- c(
    "id_mwv",
    "wind angle",
    # degrees
    "reference",
    # R=relative, T=true
    "wind speed",
    "units"
    # K=knots/h, M=Meters/h
    #"status"
    #A = valid, V = invalid
  )
  wind_rel_ans$rest <- substr(data, end, nchar(data))
  return(wind_rel_ans)
}

getDPSDBS <- function(data) {
  depth_ans <- list(dbs = NULL, rest = data)
  rxp <-
    "\\$SDDBS(,[^,]*){6}\\*[0-9,A-F]{2}"
  beg <- regexpr(rxp, data)
  if (beg == -1)
    return(depth_ans)
  end <-
    beg + attr(beg, "match.length")
  sub <-
    substr(data, beg, end - 6)
  depth_ans$dbs <-
    strsplit(sub, ",")[[1]]

  names(depth_ans$dbs) <- c("id_depth",
                            "depth_f",
                            "unit_feet",
                            "depth_m",
                            "unit_m",
                            "depth_fathom")
  depth_ans$rest <- substr(data, end, nchar(data))
  return(depth_ans)
}


get_distance_to_station <-
  function(target_station, lat_now, lon_now) {
    
    validate(
      need(lat_now != "", "lat_now not yet loaded")
    )
    
    validate(
      need(lon_now != "", "long_now not yet loaded")
    )
    
   # req(input$)
    crnt_station_lat <- map_data %>%
      dplyr::filter(., station == target_station) %>%
      select("lat")
    
    crnt_station_lon <- map_data %>%
      dplyr::filter(., station == target_station) %>%
      select("lon")
    
    
    #requires as.numeric(unlist(unique(...[1]))) in order to transform stations with 
    #multiple mentions in the list to just one numerical value for lat and lon
    dist_to_station <-
      gdist(as.numeric(unlist(unique(
        crnt_station_lon[1]
      ))),
      as.numeric(unlist(unique(
        crnt_station_lat[1]
      ))),
      lon_now,
      lat_now,
      units = "nm")
    
    return(dist_to_station)
  }

get_tides <- function(current_date) {
  high_tide <-
    tide_data %>%
    dplyr::filter(., Date == current_date) %>%
    dplyr::filter(., State == "H") %>%
    dplyr::filter(Height == max(Height)) %>%
    select("Height")
  high_tide <- high_tide %>% rename("high_height" = "Height")
  
  print(high_tide)
  
  high_tide_time <-
    tide_data %>%
    dplyr::filter(., Date == current_date) %>%
    dplyr::filter(., State == "H") %>%
    dplyr::filter(Height == max(Height)) %>%
    select("Time")
  high_tide_time <- high_tide_time %>% rename("high_time" = "Time")
  
  print(high_tide_time)
  
  low_tide <-
    tide_data %>%
    dplyr::filter(., Date == current_date) %>%
    dplyr::filter(., State == "L") %>%
    dplyr::filter(Height == min(Height)) %>%
    select("Height")
  low_tide <- low_tide %>% rename("low_height" = "Height")
  
  
  print(low_tide)
  
  low_tide_time <-
    tide_data %>%
    dplyr::filter(., Date == current_date) %>%
    dplyr::filter(., State == "L") %>%
    dplyr::filter(Height == min(Height)) %>%
    select("Time")
  low_tide_time <- low_tide_time %>% rename("low_time" = "Time")
  
  print(low_tide_time)
  
  tides_out <- c(high_tide, high_tide_time, low_tide, low_tide_time)
  
  return(tides_out)
  
}

server <- function(input, output, session) {
  
  
  output$day_plan = renderUI({
    
    validate(
      need(map_data != "", "Map data not yet loaded")
    )
    
    dropdownButton(
      checkboxGroupInput(
        "day_plan",
        label = p("Day Plan"),
        choices = as.character(unique(map_data$sample_type)),
        selected = "kelp-WQ"
      ),
      circle = FALSE,
      status = "default",
      size = "default",
      icon = NULL,
      label = "Day Plan"
    )

    
  })
  
  observeEvent(input$day_plan, {
    stations_available <<- map_data %>%
      dplyr::filter(., sample_type %in% input$day_plan) %>%
      dplyr::arrange(., station_order) %>%
      select("station")
  })
  
  # Customizes station list based on Day Plan selection. Still needs ability to:
  # update from the critter
  # customize (add new stations)
  output$station_selector <- renderUI({
    
    validate(
      need(input$day_plan != "", "Day plan data not yet loaded")
    )
    
    sortableCheckboxGroupInput(
      "planned_stations",
      label = p("Stations:"),
      choices = c(
        unique(stations_available$station),
        input$extra_planned_stations
      ),
      selected = c(
        unique(stations_available$station),
        input$extra_planned_stations
      )
    )
    
  })
  
  
  observeEvent(input$submit_stations, {
    updateCheckboxGroupInput(session, "planned_stations")
  })
  
  
  output$all_station_selector <- renderUI({
    
    validate(
      need(input$day_plan != "", "Day plan data not yet loaded")
    )
    
    all_stations_available <- map_data %>%
      dplyr::filter(., !sample_type %in% input$day_plan) %>%
      dplyr::arrange(., station_order) %>%
      select("station")
    
    sortableCheckboxGroupInput(
      "extra_planned_stations",
      label = p("Stations:"),
      choices = unique(all_stations_available$station),
      selected = NULL
    )
    
  })
  
  # greenLeafIcon <- makeIcon(
  #   iconUrl = "http://leafletjs.com/examples/custom-icons/YellowTri.png",
  #   iconWidth = 38, iconHeight = 95,
  #   iconAnchorX = 22, iconAnchorY = 94
  # )
  
  # Poll the GPS instrument every 5 seconds ----------------------------------------------------
  
  All_NMEA <- shiny::reactivePoll(
    5000,
    session,
    checkFunc = Sys.time,
    valueFunc = function() {
      if (input$vessel == "Simulator"){
      connect()
      
      NMEA_data <- toString(NMEA_poll)
      GPS_dat <- getGPRMC(NMEA_data)
      # wind_dat <- getWIMWD(NMEA_data)
      wind_dat <- getWIMWV_true(NMEA_data)
      depth_dat <- getDPSDBS(NMEA_data)
      }
      else if (input$vessel == "Oceanus" | input$vessel == "Monitor III"){

      connect_GPS()
      connect_wind()
      connect_depth()

      GPS_str <- toString(GPS_poll)
      GPS_dat <- getGPRMC(GPS_str)
      wind_str <- toString(wind_poll)
      # wind_dat <- getWIMWD(wind_str)
      wind_dat <- getWIMWV_true(wind_str)
      depth_str <- toString(depth_poll)
      depth_dat <- getDPSDBS(depth_str)
      }
      else{
        NMEA_data <- NULL
        GPS_dat <- NULL
        wind_dat <- NULL
        depth_dat <- NULL
      }

      
      lat_deg <- substr(GPS_dat$rmc["lat"], 1, 2)
      lat_mins <- substr(GPS_dat$rmc["lat"], 3, 9)
      lat_for_dist <- as.numeric(lat_deg) + (as.numeric(lat_mins) / 60)
      print(lat_for_dist)
      lon_deg <- substr(GPS_dat$rmc["long"], 1, 3)
      lon_mins <- substr(GPS_dat$rmc["long"], 4, 9)
      lon_for_dist <- (as.numeric(lon_deg) + (as.numeric(lon_mins) / 60))*-1
      print(lon_for_dist)
      #print(input$day_plan)
      
      
      # if (is.null(lat_for_dist)){
      #   lat_for_dist = 32
      # }
      # 
      # if (is.null(lon_for_dist)){
      #   lon_for_dist = -117
      # }
      
      if (is.null(input$crnt_station)) {
        this_station <- "A1"
      }
      else{
        this_station <- input$crnt_station
      }

      dist <-
        get_distance_to_station(this_station, lat_for_dist, lon_for_dist)
      names(dist) <- "dist"
      
      GPS_table <-
        write.table(
          cbind(lat_for_dist, lon_for_dist),
          paste(
            "C:/Users/MiniMe/Documents/GitHub/DesktopDeployR/app/GPS",
            Sys.Date(),
            ".csv",
            sep = ""
          ),
          append = TRUE,
          sep = ",",
          col.names = FALSE,
          row.names = FALSE
        )
      
      leafletProxy("map", session = session) %>%
        #addTiles() %>%
        # addMarkers("YellowTri.png", lng = lon_for_dist, lat = lat_for_dist,
        #            popup = "track")
        addCircleMarkers(
          lng = lon_for_dist,
          lat = lat_for_dist,
          radius = 1,
          fillOpacity = 1, color = "red"
        )
        
      
      NMEA_out <- c(GPS_dat$rmc, wind_dat$mwd, depth_dat$dbs, dist)
      
      return(NMEA_out)
      
    }
  )
  
  
  
  ord <- function(data) {
    print(data)
  }
  
  observe(ord(All_NMEA()))
  #df <- observe(All_NMEA())
  # lat_button <- observe(ord(All_NMEA()["lat"]))
  # long_button <- observe(ord(All_NMEA()["long"]))
  observe(ord(input$planned_stations))

  # print("hello world")
  # observe(ord(All_NMEA()["long"]))
  # observe(ord(All_NMEA()["lat"]))
  # # print(lat_button)
  # # print(long_button)
  # print("goodbye world")
  
  # # Pull NMEA data and export it ---------------------
  # gprmc_data <- as.data.frame(getGPRMC(NMEA_data)) %>%
  #   select(., rmc) %>%
  #   rownames_to_column(., "parameters") %>%
  #   pivot_wider(., names_from = parameters, values_from = rmc) %>%
  #   select(., UTC, lat, long)
  #
  # wimwd_data <- as.data.frame(getWIMWD(NMEA_data)) %>%
  #   select(., mwd) %>%
  #   rownames_to_column(., "parameters") %>%
  #   pivot_wider(., names_from = parameters, values_from = mwd) %>%
  #   select(., 'wind angle', 'wind speed', 'units?')
  
  
  # Output the necessary data from the GPS ----------------------------------------------------
  
  output$date <- renderText({
    d <- substr(All_NMEA()["date (ddmmyy)"], 1, 2)
    M <- substr(All_NMEA()["date (ddmmyy)"], 3, 4)
    y <- substr(All_NMEA()["date (ddmmyy)"], 5, 6)
    Mdy <<- as.Date(paste(M, "/", d, "/", y, sep = ""), format = "%m/%d/%y")
    paste("UTC Date: ", format(Mdy,"%d-%b-%Y"), sep = "")
  })
  
  output$time <- renderText({
    h <- substr(All_NMEA()["UTC"], 1, 2)
    m <- substr(All_NMEA()["UTC"], 3, 4)
    s <- substr(All_NMEA()["UTC"], 5, 6)
   # base_time <- paste(format(Mdy,"%Y/%m/%d")," ",h, ":", m, ":", s, sep = "")
    # hms <- as.POSIXct(base_time, tz = "GMT",  format= "%Y/%m/%d H:M:S")
    base_time <- paste(h, ":", m, ":", s, sep = "")
    hms_time = lubridate::hms(base_time)
    paste("UTC Time: ", hms_time)
  })
  
  output$lat <- renderText({
    lat <- as.numeric(All_NMEA()["lat"]) / 100
    paste("Lat:", lat)
  })
  
  output$lon <- renderText({
    long <- as.numeric(All_NMEA()["long"]) / -100
    paste("Long:", long)
  })
  
  output$distance_to_stn <- renderText({
    #d2stn <- as.numeric(All_NMEA()[25])
    d2stn <- as.numeric(All_NMEA()["dist"])
    if (as.numeric(d2stn) < 0.05){
    paste("<span style=\"color:limegreen\">Distance to station:", round(d2stn, digits = 2),"</span")
    }
    else{
    paste("Distance to station:", round(d2stn, digits = 2))
    }
    #"<span style=\"color:red\">This is red text</span>"
  })
  
  
  output$depth <- renderText({
    paste("Depth (m):", All_NMEA()["depth_m"])
  })
  # For boat:
  # output$depth <- renderText({
  #   paste("Depth (m):", All_NMEA()[20])
  # })
  
  output$speed <- renderText({
    paste("Speed (knots):", All_NMEA()["boat speed (knots)"])
  })
  
  output$wind_speed <- renderText({
    paste("Wind speed (knots):", All_NMEA()["wind speed"])
  })
  # For boat:
  # output$wind_speed <- renderText({
  #   paste("Wind speed (knots):", All_NMEA()[14])
  # })
  
  output$wind_dir <- renderText({
    paste(" Wind Direction:", All_NMEA()["wind angle"], "\u00B0")
  })
  

  output$air_temp <- renderText({
    #paste("Air Temp:", All_NMEA()[12], "\u00B0", "C")
    "Air Temp: N/A (no sim temp provided)"   
  })


  # Do we need the next 3 calls? WHat are they doing? Should we change them to output$something_specific? -SS
  output$weather <- renderUI({
    
    weather_frame <<-
      data.frame(
        "weather_options" = c(
          " ",
          "Clear",
          "Haze",
          "Partly Cloudy",
          "Overcast",
          "Fog",
          "Fog and Drizzle",
          "Drizzle",
          "Rain",
          "Thunderstorm",
          "Smoke",
          "Other (see notes)"
        )
      )
    
    
      selectInput(
       "weather",
       "Weather",
       choices = weather_frame, selected = NULL, multiple = FALSE)
  })

  
  # Do we need the next 3 calls? WHat are they doing? Should we change them to output$something_specific? -SS
  # output$value <- renderPrint({
  #   selectInput(
  #   "weather",
  #   "Weather",
  #   
  #   )
  # })
  
  output$sea_state <- renderUI({
    seastate_frame <<-
      data.frame(
        "seastate_options" = c(
        " ",
        "Calm",
        "Light Chop",
        "Confused Swell",
        "Rough",
        "Wind Ripples",
        "Other (see notes)"
      ))
    
    selectInput("sea_state",
                "Sea State",
                choices = seastate_frame)
    
  })
  
  output$water_color <- renderUI({
    watercolor_frame <<-
      data.frame(
        "watercolor_options" = c(
          " ",
          "Blue",
          "Blueish-Green",
          "Brown",
          "Brownish-Green",
          "Green",
          "Greenish-Blue",
          "Greenish-Brown",
          "Yellowish-Brown",
          "Other (see notes)"
        ))
    
    selectInput("water_color",
                "Water Color",
                choices = watercolor_frame)
    
  })
  
  
  output$stn_notes <- renderUI({
    textAreaInput(
      "stn_notes",
      "Station Notes",
      value = "",
      width = NULL,
      
      height = "100px",
      placeholder = NULL,
      resize = "both"
    )
  })

  
  # Displays the station ID at the top of the main panel ("On Station" tab)
  
  output$current_station <- renderUI({
    
    validate(
      need(input$planned_stations != "", "Planned station data not yet loaded")
    )
    
    selectInput("crnt_station", "Current Station:", input$planned_stations, selected = input$planned_stations[incrementer])
  })
  
  
  #Capture arrival/ departure data (eventually also net on botton, net on deck... etc) -------
  empty_atable <- data.frame(
    utc = "",
    lat = "",
    long = "",
    depth = "",
    wind_speed = "",
    wind_angle = ""
  )
  empty_dtable <- data.frame(
    utc = "",
    lat = "",
    long = ""
  )
  
  capture_headers <- c("Station", "PARAMETER", "VALUE", "UNITS")
  
  output$arrive_table <- renderTable(empty_atable)
  arrival_time <- NULL
  departure_time <- NULL
  
  observeEvent(input$arrive, {
    #table_object <- subset(as.data.frame(All_NMEA()), select = c("UTC", "lat", "long","depth_m", "wind speed", "wind angle", "Air" ))
    atab_vars <- c("UTC","lat","long","depth_m","wind speed","wind angle")
    table_object <- All_NMEA()[atab_vars]
    arrival_time <<- c(input$crnt_station, "ARRIVE TIME",
                       table_object["UTC"],
                       "UTC")
    arrival_lat <<- c(input$crnt_station, "ARRIVE_LATITUTDE",
                      table_object["lat"],
                      "degrees")
    arrival_long <<- c(input$crnt_station, "ARRIVE_LONGITUDE",
                       table_object["long"],
                       "degrees")
    depth <<- c(input$crnt_station, "DEPTH",
                       table_object["depth_m"],
                       "m")
    wind_speed <<- c(input$crnt_station, "WIND SPEED",
                       table_object["wind speed"],
                       "Kts")
    wind_dir <<- c(input$crnt_station, "WIND_DIR",
                       table_object["wind angle"],
                       "degrees")
    air_temp <<- c(input$crnt_station, "AIR TEMP",
                   "N/A",
                   "C")
    #pivot_longer(as.data.frame(arrival_capture), arrival_capture, names_to = "PARAMETER", values_to = "VALUE")
    output$arrive_table <- renderTable(t(table_object))
  })
  
  
  output$depart_table <- renderTable(empty_dtable)
  
  observeEvent(input$depart, {
    dtab_vars <- c("UTC","lat","long")
    table_object <- All_NMEA()[dtab_vars]
    departure_time <<- c(input$crnt_station, "DEPART TIME",
                         table_object["UTC"],
                         "UTC")
    departure_lat <<- c(input$crnt_station, "DEPART_LATITUTDE",
                        table_object["lat"],
                        "degrees")
    departure_long <<- c(input$crnt_station, "DEPART_LONGITUDE",
                         table_object["long"],
                         "degrees")
    
    #pivot_longer(as.data.frame(arrival_capture), arrival_capture, names_to = "PARAMETER", values_to = "VALUE")
    output$depart_table <- renderTable(t(table_object))
  })
  
  
  
  observeEvent(input$next_station, {
    
    today_tides <- get_tides(Sys.Date())
    print(today_tides)
    
    # visibility <<- c(input$crnt_station, "VISIBILITY", input$visibility, "mi")
    # wave_height <<- c(input$crnt_station, "WAVE HEIGHT LOW", input$wave_height, "ft")
    # wave_period <<- c(input$crnt_station, "WAVE PER", input$wave_period, "s")
    # weather <<- c(input$crnt_station, "WEATHER DESC",input$weather, "DESC")
    # water_color <<- c(input$crnt_station, "WATER COLOR CODE", input$water_color, "")
    # sea_state <<- c(input$crnt_station, "SEA STATE", input$sea_state, "")
    # today_high <<- c(input$crnt_station, "HIGH TIDE", today_tides$high_height, "")
    # today_high_time <<- c(input$crnt_station, "HIGH TIDE TIME", today_tides$high_time, "")
    # today_low <<- c(input$crnt_station, "LOW TIDE", today_tides$low_height, "")
    # today_low_time <<- c(input$crnt_station, "LOW TIDE TIME", today_tides$low_time, "")
    # station_notes <<- c(input$crnt_station, "COMMENTS", input$stn_notes, "")
    # ctd <<- c(input$crnt_station, "CTD", input$CTD, "")
    # bacti <<- c(input$crnt_station, "Bacti", input$bacti, "")
    # crew <<- data.frame(input$crnt_station, "CREW", I(list(c(input$crew))), "")
    # vessel <<- c(input$crnt_station, "VESSEL", input$vessel, "")
    # equip <<- data.frame(input$crnt_station, "EQUIPMENT", I(list(c(input$equip))), "")

    
    output$arrive_table <- renderTable(empty_atable)
    output$depart_table <- renderTable(empty_dtable)
    
    
    updateTextAreaInput(session, "stn_notes", "Station Notes", " ")
    
    project <- "PL"
    sample_type <- "WQ Kelp"
    today_date <- format(Sys.Date(),"%d-%b-%Y")
    

    
    
    
    # Look to see whether stations have been taken out of order, if no, increment to next station,
    #if yes, stay on station(increment)
    if (input$crnt_station == input$planned_stations[incrementer]) {
      incrementer <<- incrementer + 1
    } else {
    }
    
    # Auto-update the station selector dropdown on "On Station" tab
    updateSelectInput(session, "crnt_station", choices = input$planned_stations, selected = input$planned_stations[incrementer])

    
    
    # prep_datafile <<-
    #   as.data.frame(t(
    #     cbind(
    #       arrival_time,
    #       arrival_lat,
    #       arrival_long,
    #       departure_time,
    #       departure_lat,
    #       departure_long,
    #       depth,
    #       air_temp,
    #       wind_speed,
    #       wind_dir,
    #       visibility,
    #       wave_height,
    #       wave_period,
    #       weather,
    #       water_color,
    #       sea_state,
    #       today_high,
    #       today_high_time,
    #       today_low,
    #       today_low_time,
    #       station_notes,
    #       ctd,
    #       bacti,
    #       crew,
    #       vessel,
    #       equip
    #     )
    #   ))
    
    lbls <- c(
      "ARRIVE TIME",
      "ARRIVE LATITUDE",
      "ARRIVE LONGITUDE",
      "DEPART TIME",
      "DEPART LATITUDE",
      "DEPART LONGITUDE",
      "DEPTH",
      "WIND SPEED",
      "WIND DIR",
      "AIR TEMP",
      "VISIBILITY",
      "WAVE HEIGHT LOW",
      "WAVE PER",
      "WEATHER DESC",
      "WATER COLOR CODE",
      "SEA STATE",
      "HIGH TIDE",
      "HIGH TIDE TIME",
      "LOW TIDE",
      "LOW TIDE TIME",
      "COMMENTS",
      "CTD",
      "Bacti",
      "CREW",
      "VESSEL",
      "EQUIPMENT"
    )
    
    vals <- I(list(
      c(arrival_time[3]),
      c(arrival_lat[3]),
      c(arrival_long[3]),
      c(departure_time[3]),
      c(departure_lat[3]),
      c(departure_long[3]),
      c(depth[3]),
      c(wind_speed[3]),
      c(wind_dir[3]),
      c(air_temp[3]),
      c(input$visibility),
      c(input$wave_height),
      c(input$wave_period),
      c(input$weather),
      c(input$water_color),
      c(input$sea_state),
      c(today_tides$high_height),
      c(today_tides$high_time),
      c(today_tides$low_height), 
      c(today_tides$low_time),
      c(input$stn_notes),
      c(input$CTD),
      c(input$bacti),
      c(input$crew),
      c(input$vessel),
      c(input$equip)
    ))
    
    units <- c(
      "",
      "Degrees",
      "Degrees",
      "",
      "Degrees",
      "Degrees",
      "m",
      "Kts",
      "Degrees",
      "Degrees (C)",
      "mi",
      "ft",
      "s",
      "",
      "",
      "",
      "ft",
      "",
      "ft",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    )
    
    prep_datafile <<- data.frame(Project = "PL", Sample_Type = input$day_plan, Date = format(Sys.Date(), "%d-%b-%Y"), Station = input$crnt_station, Parameters = lbls, Values = vals, Units = units,
                                 stringsAsFactors = FALSE)
      
    print("prep_datafile: ")
    print(prep_datafile)
    
    master_datatable <<- rbind(master_datatable, prep_datafile)
    
    
    print("master_datatable: ")
    print(master_datatable)
    # n <<- ncol(master_datatable)
    # print(paste("n =",n))
    
    #print(master_datatable$col.names)
    
    # station_forPDF <- as.data.frame(t(c(input$crnt_station, "STATION")))
    # print("station_forPDF")
    # print(station_forPDF)
    # 
    # new_PDF_column = rbind(station_forPDF$V1, prep_datafile$UTC)
    # print("new_PDF_column")
    # print(new_PDF_column)
    
    #raw_data_sofar$data <<- as.data.frame(cbind(raw_data_sofar$data, new_PDF_column))
    # row.names(raw_data_sofar) <- NULL
    # raw_data_sofar$names <- prep_datafile[,2]
    #raw_data_sofar <- rownames_to_column(raw_data_sofar, "VALUE")
    #print(raw_data_sofar)
    
    
    capture_table <-
      write.table(
        as.data.frame(prep_datafile),
        paste("C:/Users/MiniMe/Documents/GitHub/DesktopDeployR/app/WQ", filename_date,".csv", sep = ""),
        append = TRUE,
        sep = ",",
        col.names = FALSE,
        row.names = FALSE
      )
    
    display_table <<- master_datatable %>%
      #select(.,-"Project",-"Sample_Type",-"Date") %>%
     # filter(.,!Parameters %in% c("CREW", "VESSEL", "EQUIPMENT", "HIGH TIDE", "HIGH TIDE TIME", "LOW TIDE", "LOW TIDE TIME")) %>%
      pivot_wider(
        .,
        id_cols = c("Project","Sample_Type","Date", "Parameters", "Units", "Values"),
        names_from = Station,
        values_from = Values
      )
    
    output$data_viewer_data <-
      DT::renderDataTable(
        datatable(
          display_table %>%
            select(.,-"Project",-"Sample_Type",-"Date") %>%
            filter(.,!Parameters %in% c("CREW", "VESSEL", "EQUIPMENT", "HIGH TIDE", "HIGH TIDE TIME", "LOW TIDE", "LOW TIDE TIME")),
          extensions = 'Responsive',
          options = list(pageLength = 50,
          dom = 'ft',
          paging = FALSE),
          rownames = FALSE,
          editable = TRUE
        ) %>%
          formatStyle(1:12, color = 'black', backgroundColor = 'aliceblue', fontSize = '120%', lineHeight='90%')
      )
    
    output$crew_table <- DT::renderDataTable({
      datatable(
        as.data.frame(input$crew),
        colnames = c("Crew:"),
        rownames = FALSE,
        class = NULL,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE
        )
      ) %>%
        formatStyle(1:12, color = 'white', backgroundColor = '#304052', fontSize = '120%', lineHeight='90%')
    })
    
    output$equip_table <- DT::renderDataTable({
      datatable(
        as.data.frame(input$equip),
        colnames = c("Equipment:"),
        rownames = FALSE,
        class = NULL,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE
        )
      ) %>%
        formatStyle(1:12, color = 'white', backgroundColor = '#304052', fontSize = '120%', lineHeight='90%')
    })
    
    
    tide_tbl$highs <- master_datatable %>%
      filter(.,
             Parameters %in% c("HIGH TIDE", "HIGH TIDE TIME")) %>%
      filter(.,
             Station == input$crnt_station) %>%
      select(., "Values")

      
    
    tide_tbl$lows <- master_datatable %>%
      filter(.,
             Parameters %in% c("LOW TIDE", "LOW TIDE TIME")) %>%
      filter(.,
             Station == input$crnt_station) %>%
      select(., "Values")


    date_tbl <- master_datatable %>%
      filter(.,
             Parameters %in% c("VESSEL")) %>%
      filter(.,
             Station == input$crnt_station) %>%
      select(., "Date", "Values")


    
    output$date_table <- DT::renderDataTable({
      datatable(
        t(as.data.table(date_tbl, check.names = FALSE)),
        colnames = "...", # A very inelegant way to get the table to align, see initcomplete below
        rownames = c("Date:", "Ship:"),
        height = '100%',
        class = NULL,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,  initComplete = JS( # sets header font color to same as background color
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#304052', 'color': '#304052'});",
            "}"
        ))
      )  %>%
        formatStyle(0:12, color = 'white', backgroundColor = '#304052', fontSize = '120%', lineHeight='90%')
    })
    
    output$tide_table <- DT::renderDataTable({
      datatable(
        t(as.data.table(tide_tbl)),
        colnames = c("Height", "Time"),
        rownames = c("High", "Low"),
        height = '100%',
        #rownames = TRUE,
        class = NULL,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE
        )
      ) %>%
        formatStyle(0:12, color = 'white', backgroundColor = '#304052', fontSize = '120%', lineHeight='90%') 
    })
    


    
    
   # Maybe add button to toggle to filtered table. Other functionality...
   #datatable(display_table, filter = 'top', options = list(pageLength = 50), rownames= FALSE) %>%
    
    # path_wd("/datasheet_wdoc.docx")
    # dat_doc <- read_docx()
    # dat_doc <- dat_doc %>%
    #   body_add_table(as.data.frame(prep_datafile), style = "table_template")
    # print(dat_doc, target = "datasheet_wdoc.docx")
    
    #return(display_table)
  })
  
  observeEvent(input$data_viewer_data_cell_edit, {
    display_table[input$data_viewer_data_cell_edit$row, input$data_viewer_data_cell_edit$col +
                    1] <<- input$data_viewer_data_cell_edit$value
  })
  
  observeEvent(input$saveBtn, {
    final_table <<- display_table  %>%
    pivot_longer(
      .,
      cols = -c("Project", "Sample_Type", "Date", "Parameters", "Units"),
      names_to = "Station",
      values_to = "Values"
    ) 
    
    one_timers <- final_table %>% 
      filter(Parameters %in% c("VESSEL","CREW", "EQUIPMENT")) %>% 
      distinct(Parameters, .keep_all = TRUE)
    one_timers$Station <- "N/A"
    print(one_timers)
    
    
    #keeping separate from above because thise will eventually need to go in if:else statement
    #to change based on sampling type
    final_table <<- final_table %>% 
      filter(!Parameters %in% c("DEPART LATITUDE", "DEPART LONGITUDE","VESSEL","CREW", "EQUIPMENT")) %>% 
      mutate(Parameters = replace(Parameters, Parameters == "ARRIVE LATITUDE", "GPS LATITUDE")) %>% 
      mutate(Parameters = replace(Parameters, Parameters == "ARRIVE LONGITUDE", "GPS LONGITUDE"))
    

    stat_order <- as.vector(unique(final_table["Station"]))
    print(stat_order)
    str(stat_order)
    
    final_table <<- final_table %>%
      arrange(factor(Station, levels = as.list(stat_order$Station)))
    
    final_table <<- rbind(one_timers, final_table)
    
    print(final_table)

    print(paste("FINAL TABLE:", final_table))
    write.csv(
      final_table,
      paste(
        "C:/Users/MiniMe/Documents/GitHub/DesktopDeployR/app/Post_QC",
        filename_date,
        ".csv",
        sep = ""
      )
    )
  })
  
  
  
  observeEvent(input$get_datasheet, {
    
    report <- master_datatable %>%
      select(., -"Units") %>%
      pivot_wider(., id_cols = NULL, names_from = Station, values_from = Values)
    
    print("report: ")
    print(report)
    
    today_stamp <- Sys.Date()
    format(today_stamp,"%d-%b-%Y")

    foot_ft_c1 <- rbind(select(filter(master_datatable, Parameters == "CREW"), Values), select(filter(master_datatable, Parameters == "VESSEL"), Values))
    print(foot_ft_c1)

    foot_df <- foot_ft_c1
    
    blocks <- block_list(fpar(ftext("hello world", shortcuts$fp_bold())))
    foot_flex <- flextable(as.data.frame(foot_df))
    foot_flex <- autofit(foot_flex)
    
    #template <- system.file(package = "officer", "doc_examples/example.docx")

    path_wd("/datasheet_wdoc.docx")
    dat_doc <- read_docx("template_wdoc.docx")
    dat_doc <- dat_doc %>%
      # ph1 <- ph_location_type(type = "ftr", position_right = FALSE,
      #                  position_top = TRUE, newlabel = ph1, id = 1)
      body_add_table(report, header = TRUE,  style = "Normal Table") %>%
      # footers_replace_text_at_bkm(bookmark = "bmk_footer",
      #                                     value = "This is a footer") %>%
      #footers_replace_all_text(old_value = "another bookmark is here", new_value = "This is a footer") %>%
      footers_flextable_at_bkm(bookmark = "bmk_footer", value = foot_flex) %>%

      #slip_in_footnote(style = NULL, blocks = blocks) 
      #%>%
      #body_add_table(report, header = TRUE,  style = "Light List Accent 2") %>%
      body_end_section_landscape()
    print(dat_doc, target = "datasheet_wdoc.docx")
    
    # footers_replace_all_text(.,
    #                          "",
    #                          report,
    #                          only_at_cursor = FALSE,
    #                          warn = TRUE) %>%
    
  })
  
  
  # output$data_viewer <-
  #   DT::renderDataTable(
  #     datatable(display_table, options = list(pageLength = 50)) %>%
  #     formatStyle(1:12, color = 'black', backgroundColor = 'white')
  #   )
  

  output$map <- renderLeaflet({
    map <- leaflet(map_data) %>%
      # Base groups
      addProviderTiles(providers$Esri.OceanBasemap, group = "ocean basemap (default)") %>%
      addTiles(group = "Basic") %>%
      # Overlay groups
      addProviderTiles(providers$OpenSeaMap, group = "Nav bouys") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("ocean basemap (default)", "Basic"),
        overlayGroups = c("Nav bouys", "Kelp WQ", "IWCP_north", "IWCP_south", "IWCP_outfall"), # 
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Nav bouys", "Kelp WQ", "IWCP_north", "IWCP_south", "IWCP_outfall")) %>%  # 
      # map boundries
      fitBounds( ~ min(lon), ~ min(lat), ~ max(lon), ~ max(lat)) %>%
      # station markers
      addCircleMarkers(
        data = kelp_data, lng = ~ lon, lat = ~ lat,
        radius = 4, stroke = FALSE, fillOpacity = 0.2, color = "navy", label = ~ station,
        group = "Kelp WQ", popup = paste("Nominal depth", "<br>", map_data$depth_m, " m"),
        labelOptions = labelOptions(noHide = TRUE, offset = c(0, 4), textOnly = TRUE, textsize = "10px", direction = "top",
                                    style = list("font-weight" = "bold", padding = "2px 20px"))) %>% 
      addCircleMarkers(
        data = IWCP_north, lng = ~ lon, lat = ~ lat,
        radius = 4, stroke = FALSE, fillOpacity = 0.2, color = "navy", label = ~ station,
        group = "IWCP_north", popup = paste("Nominal depth", "<br>", map_data$depth_m, " m"),
        labelOptions = labelOptions(noHide = TRUE, offset = c(0, 4), textOnly = TRUE, textsize = "10px", direction = "top",
                                    style = list("font-weight" = "bold", padding = "2px 20px"))) %>% 
      addCircleMarkers(
        data = IWCP_south, lng = ~ lon, lat = ~ lat,
        radius = 4, stroke = FALSE, fillOpacity = 0.2, color = "navy", label = ~ station,
        group = "IWCP_south", popup = paste("Nominal depth", "<br>", map_data$depth_m, " m"),
        labelOptions = labelOptions(noHide = TRUE, offset = c(0, 4), textOnly = TRUE, textsize = "10px", direction = "top",
                                    style = list("font-weight" = "bold", padding = "2px 20px"))) %>% 
      addCircleMarkers(
        data = IWCP_outfall, lng = ~ lon, lat = ~ lat,
        radius = 4, stroke = FALSE, fillOpacity = 0.2, color = "navy", label = ~ station,
        group = "IWCP_outfall", popup = paste("Nominal depth", "<br>", map_data$depth_m, " m"),
        labelOptions = labelOptions(noHide = TRUE, offset = c(0, 4), textOnly = TRUE, textsize = "10px", direction = "top",
                                    style = list("font-weight" = "bold", padding = "2px 20px"))) #%>%
      #addEasyButton(easyButton(
      #  icon = "fa-crosshairs", title = "Locate Vessel",
      #  onClick = JS("
      #               function(btn, map) {
      #               map.flyTo([", paste(as.numeric(All_NMEA()["lat"]) / 100), ",", paste(as.numeric(All_NMEA()["long"]) / -100), "], zoom = 10);
      #               }
      #               ")
      #))
      


  })
  


}
