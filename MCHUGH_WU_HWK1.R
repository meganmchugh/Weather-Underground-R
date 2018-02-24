MCHUGH_WU_HWK1 <- function(station_id, wu_month, wu_day, wu_year){
  
  #Load Libraries
  #in command line run install.packages('libary.name') if not already installed
  library('lubridate')
  library('urltools')

  #Build URL
  wu_url <- "https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=KTXAUSTI942&month=1&day=16&year=2018&format=1"
  wu_url <- param_set(wu_url, key = "ID", value = station_id) #character
  wu_url <- param_set(wu_url, key = "month", value = wu_month) #numeric
  wu_url <- param_set(wu_url, key = "day", value = wu_day) #numeric
  wu_url <- param_set(wu_url, key = "year", value = wu_year) #numeric
  wu_url #output 
  print(wu_url)
  
  #Download CSV
  wu_data <- read.csv(wu_url, row.names = NULL)
  
  #correct row names
  wu_data <- wu_data[wu_data$row.names != '<br>',]
  names(wu_data)
  
  #correct column names
  col_names <- names(wu_data)[2:length(names(wu_data))]
  col_names
  names(wu_data) <- col_names
  
  #remove excess columns
  less_col <- dim(wu_data)[2] - 7
  less_col
  wu_data <- wu_data[c(1:less_col)]
  wu_data <- wu_data[,c(1,2,3,7,9)]
  
  #convert from imperial to metric
  wu_data$TemperatureC <- (as.numeric(as.character(wu_data$TemperatureF)) - 32)*(5/9)
  wu_data$DewpointC <- (as.numeric(as.character(wu_data$DewpointF)) - 32)*(5/9)
  wu_data$WindSpeed.ms <- as.integer(as.character(wu_data$WindSpeedMPH))*0.44704
  wu_data$RH <- as.numeric(as.character(wu_data$Humidity))*1
  wu_data <- wu_data[,c(1,9,6,7,8)]
  
  #format time
  wu_data$Time <- as.POSIXct(wu_data$Time)
  wu_data$time_ceiling <- ceiling_date(wu_data$Time, "hour")
  wu_data <- aggregate(wu_data[c(2:5)], by = list(wu_data$time_ceiling), FUN = mean)
  names(wu_data) <- c('Time', 'RH', 'TempC', 'DewpointC', 'WindSpd.ms')
  
  #wetbulb calculation
  wu_data$WetbulbC <- (-5.806+0.672*wu_data$TempC-0.006*wu_data$TempC*wu_data$TempC+(0.061+0.004*wu_data$TempC+0.000099*wu_data$TempC*wu_data$TempC)*wu_data$RH+(-0.000033-0.000005*wu_data$TempC-0.0000001*wu_data$TempC*wu_data$TempC)*wu_data$RH*wu_data$RH)
  
  return(wu_data)
}
