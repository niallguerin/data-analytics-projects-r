# Assignment 5: ggplot
# Description: This code is using the titanic dataset and generating visualizations from it.
# Student ID: 18235079
# Niall Guerin
# 21.10.2018
#
# Dataset contains empty values so will need to allow for handling NA per tutor lab notes
library(readxl)
library(ggplot2)
library(tidyverse)

orig_list <- data.frame(readxl::read_excel("/Users/niallguerin/rprogramming/week5-ggplot-data-visualization/titanic3_assignment.xls"))
plist <- orig_list

dim(plist)

# convert plist$survived to logical values
# Web Reference: https://stackoverflow.com/questions/37707060/converting-data-frame-column-from-character-to-numeric
plist$survived <- as.logical(as.numeric(plist$survived))
# convert plist$pclass to string of cabin class
plist$pclass = ifelse(plist$pclass == 1,"First",ifelse(plist$pclass == 2,"Second", "Third"))
unique(plist$pclass)
# imputation of age for missing values - replace missing values with existing average e.g. mean
# mean = 29.8811345124283
# summary of age with NA incomplete cases removed
# slides week 5 - change, updating, handling na values
# Web Reference: https://stackoverflow.com/questions/25835643/replace-missing-values-with-column-mean
# update the fares in similar fashion
plist$age[is.na(plist$age)] <- mean(plist$age, na.rm = T)
plist$fare[is.na(plist$fare)] <- mean(plist$fare, na.rm = T)

# update the embark values
# use same format for next section and ensure only S, C, and Q are allowed in that column
# Web Reference: http://www.rexamples.com/14/Sample()
set.seed(99)
plist$embarked[is.na(plist$embarked)] <- sample(c("S", "C", "Q"), replace = T)
# this gives a warning not error in my macbook when I run it from Source function in IDE. In console it worked. It did not affect graphs
# and unique(plist$embark) gives required result per assignment dataset prep validation step later in pdf assignment document

# add new dataframe category
# rules for category: Child (<16), Adults (>=16 & <60) and Elderly (>=60)
# validation column: uncomment to validate your changes are pulling back correct age_cohort when you pull back all row data by age
# Initially my ifelse had errors due to a <= 60 on the adult which caused incorrect tallies during later validation step of dataset
# And my ifelse order was also initially wrong as elderly were getting ignored. After fixing per below correct tallies returned
# plist[plist$age_cohort == "Child", ]
# plist[plist$age_cohort == "Elderly", ]
# plist[plist$age_cohort == "Adult", ]
# plist[plist$age < 16, ]
# plist[plist$age == 16, ]
# plist[plist$age == 60, ]
# plist[plist$age > 60, ]
# plist[plist$age == 63, ]
plist$age_cohort <- ifelse(plist$age < 16,"Child",ifelse(plist$age >= 16 & plist$age < 60,"Adult", "Elderly"))

# update embark points to use full strings and replacement per rules
plist$embarked = ifelse(plist$embarked == "Q","Cobh",ifelse(plist$embarked == "S","Southhampton", "Cherbourg"))

# validate dataset before graphing: uncomment to validate against pp. 11
# head(plist)
# dim(plist)
# table(plist$survived,plist$sex)
# table(plist$survived)
# table(plist$survived, plist$pclass)
# table(plist$survived, plist$age_cohort)
# table(plist$survived, plist$embarked)

# plot configurations for titanic plist dataframe plots 1 - 14
# Web Reference: https://rstudio-pubs-static.s3.amazonaws.com/291083_2b0374fddc464ed08b4eb16c95d84075.html
# Web Reference: https://stackoverflow.com/questions/6022898/how-can-i-remove-the-legend-title-in-ggplot2
# Web Reference: http://r-statistics.co/ggplot2-Tutorial-With-R.html
# The following are mainly driven off of template code I had created during the week 5 lecture on ggplot2 in lecture lab in IT101 when
# working with samples on the projector from lecture for the cars dataset so I just adapted it here for titanic dataset and main
# additions which I was not familiar with were the legends, titles, subtitles, and label customizations.
plot1 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = pclass, y = ..count..), stat = "count") + ylab("Number") + xlab("Survived") + labs(title="Plot 1", subtitle="Survival Numbers by Travel Class", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot2 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = sex, y = ..count..), stat = "count") + ylab("Number") + xlab("Survived") + labs(title = "Plot 2", subtitle="Survival Numbers by Gender", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot3 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = age_cohort, y = ..count..), stat = "count") + ylab("Number") + xlab("Survived") + labs(title="Plot 3", subtitle="Survival Numbers by Age Cohort", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot4 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = embarked, y = ..count..), stat = "count") + ylab("Number") + xlab("Survived") + labs(title="Plot 4", subtitle="Survival Numbers by Embarkation Location", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot5 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = pclass), position="fill") + ylab("Proportion") + xlab("Survived") + labs(title="Plot 5", subtitle="Survival Proportions by Class", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot6 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = sex), position="fill") + ylab("Proportion") + xlab("Survived") + labs(title="Plot 6", subtitle="Survival Proportions by Gender", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot7 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = age_cohort), position="fill") + ylab("Proportion") + xlab("Survived") + labs(title="Plot 7", subtitle="Survival Proportions by Age Cohort", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot8 <- ggplot( data=plist, aes(survived) ) + geom_bar(mapping = aes(fill = embarked), position="fill") + ylab("Proportion") + xlab("Survived") + labs(title="Plot 8", subtitle="Survival Proportions by Place of Embarkation", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot9 <- ggplot( data=plist, aes(x = survived, fill= age_cohort) ) + geom_bar(mapping = aes(fill = age_cohort, y = ..count..), stat = "count") + ylab("Number") + xlab("Survived") + facet_grid(~ pclass) + labs(title="Plot 9", subtitle="Survival Numbers by Cohort and Travel Class", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot10 <- ggplot( data=plist, aes(x = survived, fill= sex) ) + geom_bar(mapping = aes(fill = sex, y = ..count..), stat = "count") + ylab("Number") + xlab("Survived") + facet_grid(~ pclass) + labs(title="Plot 10", subtitle="Survival Numbers by Gender and Travel Class", fill="") + theme(legend.position="top", legend.direction="horizontal")
plot11 <- ggplot(data=plist) + geom_point(mapping = aes(x = age, y = fare, colour = embarked)) + ylab("Fare") + xlab("Age") + labs(title="Plot 11", subtitle="Age v Fare by Place of Embarkation", colour="") + theme(legend.position="top", legend.direction="horizontal")
plot12 <- ggplot(data = plist, aes(x = age, y = fare)) + geom_point(color='black') + ylab("Fare") + xlab("Age") + labs(title="Plot 12", subtitle="Age v Fare with Linear Model", colour="") + theme(legend.position="top", legend.direction="horizontal")  + geom_smooth(method = "lm")
plot13 <- ggplot(data=plist) + geom_point(mapping = aes(x = age, y = fare, colour = survived)) + ylab("Fare") + xlab("Age") + labs(title="Plot 13", subtitle="Age v Fare with Survival Info", colour="") + theme(legend.position="top", legend.direction="horizontal")
plot14 <- ggplot(data=plist) + geom_point(mapping = aes(x = age, y = fare, colour = embarked)) + facet_wrap(~pclass) + ylab("Fare") + xlab("Age") + labs(title="Plot 14", subtitle="Age v Fare by Travel Class and Point of Depature", colour="") + theme(legend.position="top", legend.direction="horizontal")

# validation against assignment visualization graphs - uncomment and run in console to display graph by variable name plot1, plot2 etc.
plot1
plot2
plot3
plot4
plot5
plot6
plot8
plot9
plot10
plot11
plot12
plot13
plot14