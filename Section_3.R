####---ADVANCED ANALYTICS: SECTION 3---###
##Objective: investigate a machine
#Dataset shows what percentage of capacity for each machine was idle (unused) in any given hour
#Create an R list with the following components {
#########Character: name
#########Vector (min, mean, max): utilisation for the month
#########Logical: has utilisation ever fallen below 90%?
#########Vector: hours where utilisation is unkown
#########Dataframe: For this machine
#########Plot: For all machines

#Import the data

util <- read.csv(file.choose())

#Examine the data
head(util, n=5)
tail(util, n=5)
str(util)
summary(util)
#N.B timestamp is a factor and in European format
#Derive utilization

util[is.na(util$Utilization)==FALSE,]

#How do we handle the date-times in util$Timestamp
##Convert timestamp to POSIXct

util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")
#How to rearrange columns in a DF:
util$Timestamp <- NULL

util <- util[,c(4,1,2,3)]

#What is a list?

#Subset util dataframe to only look at RL1
RL1 <- util[util$Machine=="RL1",]

#Remove the other factors
RL1$Machine <- factor(RL1$Machine)

#Create an R list with the following components 
#########Character: name
#########Vector (min, mean, max): utilisation for the month
#########Logical: has utilisation ever fallen below 90%?

#Need min, mean, and max of utilization as a vector

util_stats_rl1 <- c(min(RL1$Utilization, na.rm=T), 
                    mean(RL1$Utilization, na.rm=T), 
                    max(RL1$Utilization, na.rm=T))

#Create the flag of utilization
#If the length of utilization values under 0.9 is greater than 0, it has fallen under 90% at least once and thus TRUE.
util_under_90 <- length(which(RL1$Utilization < 0.90)) > 0 #which ignores NAs

#Now we can put our list together

list_RL1 <- list("RL1", util_stats_rl1, util_under_90)
list_RL1

#How do we name components of our list??
list_RL1
names(list_RL1) <- c("Machine", "Stats (min, mean, max)", "Low threshold")
#Another way
rm(list_RL1)
list_RL1 <- list(Machine="RL1", "Stats (min mean max)"=util_stats_rl1, LowThreshold=util_under_90)

#Three ways to extract components
#[] - always returns a list
#[[]] - always return an object within the list
# $ - same as [[]] but prettier

list_RL1
list_RL1[1] #Returns a list
list_RL1[[1]] #Returns a vector
list_RL1$Machine #Returns


#How would you access the 3rd element of the vector?
(list_RL1[[2]])[3] #If you used list_RL1[2] you'd get a list, so wouldn't be able to pull the 3rd value of the vector.

#Adding and deleting components
list_RL1
list_RL1[4] <- "New Information" #One way

#Another way
#Add in all hours where utilization = NA
list_RL1$UnknownHours <- RL1[is.na(RL1$Utilization)==T, "PosixTime"]

#Dropping a component
list_RL1[4] <- NULL
#Numeration shifts automatically

list_RL1[4] #is now UnknownHours. This is because lists are not ordered.

#Add another component
#Dataframe for this machine

list_RL1$Data <- RL1
summary(list_RL1)
str(list_RL1)

#SUBSETTING A LIST
#Select the first unknown date in unknown hours

list_RL1$UnknownHours[1]
list_RL1[[4]][1]
list_RL1[c(1,4)]

#Creating a timeseries plot
library(ggplot2)

plot <- ggplot(data=util)

myplot <-plot + geom_line(aes(x=PosixTime, 
                       y=Utilization, 
                       colour=Machine),
                   size = 1.2) +
          facet_grid(Machine~.) +
          geom_hline(yintercept = 0.9, 
                     colour="Black", 
                     size=1.2, 
                     alpha=0.5,
                     linetype=3)

list_RL1$Plot <- myplot
list_RL1




