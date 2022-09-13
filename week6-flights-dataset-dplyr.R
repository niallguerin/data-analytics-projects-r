# Assignment 6: dplyr
# Description: Uses nycflights13 library dataset and dplyr to manipulate the dataset.
# Student ID: 18235079
# Niall Guerin
# Date: 26.10.2018
#

# Task 1:
library(nycflights13)
# configure timezone on mac as getting warnings resulting in NA values due my version of R running on OS High Sierra+.
# likely not needed on windows workstation machine so comment out the next line only if required. I just prefer not to have warnings.
Sys.setenv( TZ="America/New_York" )
library(dplyr)
library(ggplot2)
library(lubridate)

# Task 2:
my_flights <- flights

# Filter	 out	 missing	 values	 for	 dep_delay	 and	 arr_delay,	 and	 select	 the	 following columns	from	the	data	set:	
# time_hour,	origin,	dest,	carrier,	dep_delay,	arr_delay,	air_time,	distance

# exclude missing values in dep_delay and arr_delay
c <- filter(my_flights, !is.na(my_flights$dep_delay), !is.na(my_flights$arr_delay))

# Task 3:1
my_flights <- c

# Task 3 shows it in the output but no filter criteria in task, and it's needed in Task 5. As we are modifying my_flights
# and is.na() column filters prior to this, setting up abbreviate month here so we can add it later as required for tasks.
# Flagged by Joanna, our PHD tutor in DForums so update to use her syntax if time. Currently below gives me same output result.
dmonth <- my_flights$month
abbrev_month <- month.abb[dmonth]

# include the required columns for select criteria
my_flights <- select(my_flights, time_hour, origin, dest, carrier, dep_delay, arr_delay, air_time, distance)

# Task 3:2
# add new columns
# https://stackoverflow.com/questions/9216138/find-the-day-of-a-week
# lubridate method: extract date only from time_hour
# t1 <- as.Date(t1)
# wday(t1, label=TRUE)
# my_flights$DayOfWeek <- as.Date(my_flights$time_hour)
# wday(my_flights$DayOfWeek)
# https://stackoverflow.com/questions/19460608/want-only-the-time-portion-of-a-date-time-object-in-r
my_flights$DayOfWeek <- as.Date(my_flights$time_hour)
my_flights$DayOfWeek <- wday(my_flights$DayOfWeek, label = T)
task3p1 <- my_flights

# Web References: was getting a lot of warnings around posix* output due to my timezone being = "" when I ran get timezone command
# https://www.r-bloggers.com/doing-away-with-%E2%80%9Cunknown-timezone%E2%80%9D-warnings/
# https://rdrr.io/cran/lubridate/man/hour.html
# https://stackoverflow.com/questions/29326428/how-do-i-extract-only-the-time-parameters-from-datetime-variable-in-r
# my_flights$HourOfDay <- format(ymd_hms(my_flights$time_hour), '%H:%M:%S')
# my_flights$HourOfDay <- hour(my_flights$HourOfDay)
# http://r.789695.n4.nabble.com/as-POSIXct-character-string-is-not-in-a-standard-unambiguous-format-td4731054.html
# ?timezone has as.POSIXct examples in documentation
my_flights$HourOfDay <- as.POSIXct(strptime(my_flights$time_hour, "%Y-%m-%d %H:%M:%S"))
my_flights$HourOfDay <- hour(my_flights$HourOfDay)
my_flights$Month <- abbrev_month
task3p2 <- select(my_flights,time_hour,DayOfWeek,HourOfDay,everything())

# Task 4:
# Average departure delay statistics by hour of day, ordered by delay.
dh1 <- group_by(my_flights, HourOfDay)
dh2 <- summarise(dh1, AvrDepDelay = mean(dep_delay, na.rm = T),
                 SD = sd(dep_delay, na.rm = T),
                 MinDelay = min(dep_delay, na.rm = T),
                 MaxDelay = max(dep_delay, na.rm = T),
                 MaxDelayHours = max(dep_delay/60, na.rm = T))
dh3 <- arrange(dh2, desc(dh2$AvrDepDelay))
delay_hourly <- dh3

# Task 5: Average departure delay statistics by month, ordered by delay.
my_flights$Month <- abbrev_month
dm1 <- group_by(my_flights, Month)
dm2 <- summarise(dm1,
                 AvrDepDelay = mean(dep_delay, na.rm = T),
                 SD = sd(dep_delay, na.rm = T),
                 MinDelay = min(dep_delay, na.rm = T),
                 MaxDelay = max(dep_delay, na.rm = T),
                 MaxDelayHours = max(dep_delay/60, na.rm = T))
dm3 <- arrange(dm2, desc(dm2$AvrDepDelay))
delay_monthly <- dm3

# Task 6: Average departure delay statistics by carrier, ordered by delay.
# Need to add Number of Observations count to NObs column - 
#when you output group single line run you'lls ee NObs value on original my_flights. 
# See tip from Joana in DForums for this task using built-in summarise functions.
dc1 <- group_by(my_flights, carrier)
dc2 <- summarise(dc1,
                 AvrDepDelay = mean(dep_delay, na.rm = T),
                 SD = sd(dep_delay, na.rm = T),
                 MinDelay = min(dep_delay, na.rm = T),
                 MaxDelay = max(dep_delay, na.rm = T),
                 MaxDelayHours = max(dep_delay/60, na.rm = T),
                 NObs = n())
dc3 <- arrange(dc2, desc(dc2$AvrDepDelay))
delay_carrier <- dc3

# Task 7: Average departure delay statistics by airport by month, ordered by delay.
my_flights$Month <- dmonth
da1 <- group_by(my_flights, origin, Month)
da2 <- summarise(da1,
                 AvrDepDelay = mean(dep_delay, na.rm = T),
                 SD = sd(dep_delay, na.rm = T),
                 MinDelay = min(dep_delay, na.rm = T),
                 MaxDelay = max(dep_delay, na.rm = T),
                 MaxDelayHours = max(dep_delay/60, na.rm = T),
                 NObs = n())
da3 <- arrange(da2, da2$Month)
# format month display
da3$Month <- month.abb[da3$Month]
delay_airport_month <- da3

# Task 8: Average departure delay statistics by airport by hour, ordered by hour.
dh1 <- group_by(my_flights, HourOfDay, origin)
dh2 <- summarise(dh1,
                 AvrDepDelay = mean(dep_delay, na.rm = T),
                 SD = sd(dep_delay, na.rm = T),
                 MinDelay = min(dep_delay, na.rm = T),
                 MaxDelay = max(dep_delay, na.rm = T),
                 MaxDelayHours = max(dep_delay/60, na.rm = T),
                 NObs = n())
dh3 <- arrange(dh2, dh2$HourOfDay)
delay_airport_time <- dh3

# Task 9: Add a new category, which divides each day into three sections (use case_when - see ifelse rules for day time categories).
my_flights_clone <- my_flights
DaySection <- ifelse(my_flights_clone$HourOfDay >= 18,"Evening",ifelse(my_flights_clone$HourOfDay >= 12,"Afternoon", "Morning"))
my_flights_clone$DaySection <- DaySection

task9select <- select(my_flights_clone,DaySection,everything())

# Task 10: Create a sample dataset (using sample_n()), and remove all departure delay values greater that 180 minutes.
# # You should use a sample_n() of 10000 per DFourms.
set.seed(99)
myf_sample <- sample_n(my_flights_clone, 10000, replace = F)
myf_sample <- filter(myf_sample, dep_delay <= 180)
myf_sample$Month <- month.abb[myf_sample$Month]

task10sample <- myf_sample

# Task 11:  sections. Use a boxplot to visualise the departure delay by the three different time sections.
# Web Reference: fix chronological order of my months once switching to abbreviated month display: https://stackoverflow.com/questions/36020146/how-can-i-order-the-months-chronologically-in-ggplot2-short-of-writing-the-month
# The following are two versions of same graph: first uses factor. Not sure if we are allowed use it, so second one uses scale_x_discrete as alternative. Forgot to ask yesterday lab as didn't get to this task during it.
# plot1 <- ggplot(data = myf_sample, mapping = aes(x = factor(myf_sample$Month, levels = month.abb), y = myf_sample$dep_delay, colour=myf_sample$DaySection)) + labs(color="DaySection")  + ylab("Departure Delay") + xlab("Month") + geom_boxplot()
plot2 <- ggplot(data = myf_sample, mapping = aes(x = Month, y = dep_delay, colour=DaySection)) + scale_x_discrete(limits = month.abb) + labs(color="Time of Day")  + ylab("Departure Delay in Minutes") + xlab("Month") + geom_boxplot()

# display graph of departure delays by morning, evening, and afternoon on each month
plot2

# function library: the tasks here are doing similar functions so we can fold them into single re-usable functions.
functionNameDateFormatter <- function(df){
  # function template
}

functionNameTimeFormatter <- function(df){
  # function template
}

functionNameMonthFormatter <- function(df){
  # function template
}

functionNameSummarizer <- function(df){
  # function template
}

functionNameNewCategory <- function(df){
  # function template  
}

functionNamePlotter <- function(df){
  # function template  
  # let x, y, and legend be customizable
}

functionSetSeed <- function(seedValue){
  x = set.seed(seedValue)
  x
}

functionSetSample <- function(sampleDetails){
  
}