# Assignment 7
# Description: Exploring energy data with dplyr.
# Datasets: 2017, energy generation (Ireland), wind speed data (Mace Head, Galway)

# Task 1: load libraries
library(ggplot2) 
library(dplyr) 
library(tidyr) 
library(lubridate) 
library(readxl)

# reset my timezone as getting warnings when I do not configure it
Sys.setenv( TZ="Europe/Dublin" )
# https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf

# BEGIN function library
setPosixTimeFormat <- function( input ){
  x <- as.POSIXct( theTime <- as.POSIXct( strptime( input, "%Y-%m-%d %H:%M:%S") ))
  x
}

setDayOfWeek <- function( theDateTime ){
  x <- setPosixTimeFormat(theDateTime)
  # x <- as.POSIXct( theTime <- as.POSIXct( strptime( theDateTime, "%Y-%m-%d %H:%M:%S") ))
  theDayOfWeek <- wday(x,label = T, abbr = T)
  theDayOfWeek
}

setHourOfDay <- function( df ){
  # df$HourOfDay <- as.POSIXct( strptime( df$DateTime, "%Y-%m-%d %H:%M:%S") )
  df$HourOfDay <- setPosixTimeFormat( df$DateTime )
  df$HourOfDay <- hour( df$DateTime )
}

setMinuteOfDay <- function( theDateTime ){
  # theTime <- as.POSIXct( theTime <- as.POSIXct( strptime( theDateTime, "%Y-%m-%d %H:%M:%S") ),
  #                        tz = "Europe/Dublin")
  theTime <- setPosixTimeFormat( theDateTime )
  theMinute <- minute(theTime)
}

setDate <- function( df ){
  # https://lubridate.tidyverse.org/
  # df$Date <- as.POSIXct( strptime( df$DateTime, "%Y-%m-%d") ) - my old approach last assignment - longwinded
  # faster, clearner method below which excludes the timezone string
  # https://stackoverflow.com/questions/40222160/simplest-way-to-extract-date-from-timestamp
  # https://rpubs.com/davoodastaraky/lubridate
  df$Date <- as.Date( df$DateTime )
  df$Date
}

# do optimization for Date so it is generic: as per this second dataset it is not generic enough for reuse
setDateGeneric <- function( inputDate ){
  someDate <- as.Date( inputDate )
  someDate
}

setTime <- function( theDateTime ){
  # https://community.rstudio.com/t/how-do-i-get-just-the-time-from-r/6007/5
  # https://stackoverflow.com/questions/49919618/lubridate-not-converting-datetime-to-posixct-correctly-in-r-dd-mm-yy-hhmmss
  theTime <- as.POSIXct( theTime <- as.POSIXct( strptime( theDateTime, "%Y-%m-%d %H:%M:%S") ),
                         tz = "Europe/Dublin")
  theTime <- format(theTime, "%H:%M:%S")
  theTime
}

setNiFlow <- function( df ){
  if( is.data.frame( df ) ){
    NiFlowCol <- ifelse( df$NetImports < 0 ,"Exporting", "Importing")
    df$NIFlow <- NiFlowCol
  }
  df$NIFlow
}

getSummaryMean <- function( groupedInput ){
  theSummary <- summarise(groupedInput, 
                          AvrDailyWindGeneration = mean(Wind, na.rm = T))
  theSummary
}

# END function library

# Task 2: Excel dataset
ener <- read_excel("/Users/niallguerin/rprogramming/week7-energy-datasets-dplyr/IrelandDataJanuary2017.xlsx")

# Task 3: Add new features to the energy data set, based on the original values, and make use of dplyr and
# tidyr functions. These include: Date (String), Time (String), HourOfDay, Minute of Day, DayOfWeek and NIFlow. 
# NIFlow should be “Exporting” if the NetImports is negative, otherwise it should be “Importing.”
my_ener <- ener

# add required columns - optimize functions later so all use lubridate where possible: right now, posix still used in some parts
my_ener$Date <- setDate( my_ener )
my_ener$HourOfDay <- setHourOfDay( ener )
my_ener$Time <- setTime( my_ener$DateTime )
my_ener$MinuteOfDay <- setMinuteOfDay( my_ener$DateTime )
my_ener$DayOfWeek <- setDayOfWeek( my_ener$DateTime)
my_ener$NIFlow <- setNiFlow( my_ener )
my_ener <- select( my_ener, DateTime, Date, Time, HourOfDay, MinuteOfDay, DayOfWeek, NIFlow, Demand, everything() )

# Task 4:
# https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html
plot1 <- ggplot(data=my_ener,aes(x=HourOfDay,y=NetImports,colour=NIFlow)) +
  geom_point() +
  facet_wrap(~ DayOfWeek) +
  ylab( "Net Imports" ) + xlab( "Time(Hour of Day)" ) +
  labs(title="", subtitle="Time v Net Imports By Day of Week", fill="") +
  labs(color="NIFlow")

# Task 5: 
plot2 <- ggplot(data=my_ener,aes(x=Wind,y=CO2,colour=NIFlow)) +
  geom_point() +
  ylab( "CO2 Emissions" ) + 
  xlab( "Wind Generation" ) +
  labs(title="", subtitle="Wind Generation vs CO2 Emissions", fill="") +
  labs(color="NIFlow")

# Task 6: 
# load weather dataset
weather <- read_excel("/Users/niallguerin/rprogramming/week7-energy-datasets-dplyr/MacHeadWindData.xlsx")

# Task 7:
weather$Date <- setDateGeneric( weather$Date )
# weather$Date <- as.Date(weather$Date, format='%m/%d')

plot3<- ggplot(data = weather, aes(x = Date, y = AVRWind)) + 
  geom_point(color='blue') + 
  ylab("Average Wind Speed(Knots)") + 
  xlab("Date") + 
  labs(title="", subtitle="Average Wind Speed", colour="") + geom_line(linetype = 2)

# Task 8:
awg1 <- group_by( my_ener, Date )
awg2 <- getSummaryMean( awg1 )
awg3 <- arrange( awg2, awg2$Date )
avr_daily_wind <- awg3

# Task 9:
my_weather <- weather # copy the awg3 output to a local tibble for my_weather before joining the dataset
# join my_ener and my_weather first and then join the tibble I created for AvrDailyWindGeneration
energy_weather <- inner_join(my_ener, my_weather)
energy_weather_withavgs <- inner_join(energy_weather, awg3)

plot4 <- ggplot(data=energy_weather_withavgs,aes(x=AVRWind,y=AvrDailyWindGeneration)) +
  geom_point() +
  ylab( "Average Wind Generation" ) + 
  xlab( "Average Wind Speed (Mace Head)" ) +
  labs(title="", subtitle="Wind Speed v Wind Power Generated", fill="")

# Task 10: add linear model
# https://stackoverflow.com/questions/35560433/geom-smooth-in-ggplot2-not-working-showing-up
# Double NOT Date. So even though earlier graph 'appeared ok' the format needs fixing
plot5 <- ggplot(data=energy_weather_withavgs,aes(x=AVRWind,y=AvrDailyWindGeneration)) +
  geom_point() +
  ylab( "Average Wind Generation" ) + 
  xlab( "Average Wind Speed (Mace Head)" ) +
  labs(title="", subtitle="Wind Speed v Wind Power Generated, with linear model") +
  geom_smooth(method = "lm") +
  # using this as it is recommended for zoom / scale on most R blogs versus scale x/y continuous.
  # need it to correctly set zero label on the graph
  coord_cartesian(ylim = c(0, 3200))

# Task 10: add linear model
plot6 <- ggplot(data=energy_weather_withavgs,aes(x=AVRWind,y=AvrDailyWindGeneration)) +
  geom_point() +
  ylab( "Average Wind Generation" ) + 
  xlab( "Average Wind Speed (Mace Head)" ) +
  labs(title="", subtitle="Wind Speed v Wind Power Generated, with loess model") +
  geom_smooth(method = "loess") +
  # using this as it is recommended for zoom / scale on most R blogs versus scale x/y continuous.
  # need it to correctly set zero label on the graph
  coord_cartesian(ylim = c(0, 3200))