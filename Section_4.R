#SECTION 4 - THE APPLY FUNCTIONS
#Supplied weather data for Chicago, NYC, Houston, and SF (in both metric and American)
#Create the following deliverables;
# Table showing annual MEANS of each observed metric for each city

# A table showing average temperature fluctuation from the mean for
# each month in %age

# Table showing annual MAXIMUMS of each observed metric

# Table showing annual MINIMUMS of each observed metric

# A table showing in which months the annual maximums of each metric
# were observed in each city.

#----------------------------------------EXERCISES

#Import our data
setwd("C:\\Users\\amontagut\\Desktop\\R\\Advanced_Analytics\\Section_4\\Weather_Data")
Chicago <- read.csv("Chicago-F.csv", row.names=1) #row.names takes row names from the first column
Houston <- read.csv("Houston-F.csv", row.names=1)
NewYork <- read.csv("NewYork-F.csv", row.names=1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names=1)

#Explore
is.data.frame(Chicago)
#Convert df's to matrices since everything is the same data type
Chicago <- as.matrix(Chicago)
Houston <- as.matrix(Houston)
NewYork <- as.matrix(NewYork)
SanFrancisco <- as.matrix(SanFrancisco)

#Put everything in a list

Weather <- list(Chicago=Chicago, Houston=Houston, NewYork=NewYork, SanFrancisco=SanFrancisco)

Weather$Houston

##Using apply()
#We can use this to get the average, max, and min metrics for each city
Averages_Chicago <- apply(Chicago, 1, mean)
Averages_Houston <- apply(Houston, 1, mean)
Averages_NewYork <- apply(NewYork, 1, mean)
Averages_SanFrancisco <- apply(SanFrancisco, 1, mean) 

#average_metrics <- list(Chicago = Averages_Chicago, Houston=Averages_Houston, NewYork=Averages_NewYork, SanFrancisco=Averages_SanFrancisco)
#This is deliverable 1 but there's a faster way

#---------------------------------------Recreating the apply() function with loops
Chicago
#find the mean of every row with loops
output <- NULL #prepare an empty vector
for(i in 1:5){ #create the loop
  output[i] <- mean(Chicago[i,]) 
}
names(output) <- rownames(Chicago)
output
#This is a loooooot of code for something that can be done in one line via apply()

#---------------------------------------Using lapply()
#lapply applies a function over a list

#Example 1
Weather_t <- lapply(Weather, t)

#Example 2
rbind(Chicago, NewRow=1:12)

lapply(Weather, rbind, NewRow=1:12)

#Example 3
?rowMeans()
lapply(Weather, rowMeans) #Deliverable 1 but even more useful
#rowMeans
#colMeans
#rowSums
#colSums
#Useful to double-check apply()/lapply() output

#---------------------------------------lapply() and square brackets []
Weather$Chicago[1,1]
Weather[[1]][1,1]

#What if we wanted to extract the average high temeprature for each city
lapply(Weather, "[", 1,)

#Create a list that contains all the metrics but ONLY FOR MARCH
lapply(Weather, "[",,3)

#---------------------------------------Adding your own functions and using lapply() and apply()
lapply(Weather,rowMeans)

lapply(Weather, function(x) x[,1]) #For every component of Weather, return the first row

lapply(Weather, function(z) z[1,]-z[2,])
lapply(Weather, function(z) round((z[1,]-z[2,])/z[2,],2)) #Deliverable 2! To be improved

#---------------------------------------sapply()
?sapply()
lapply(Weather, "[", 1, 7) #Avg high temperature for each city in July
sapply(Weather, "[", 1, 7) #Returns a named vector

#Avg_F for 4th quarter
lapply(Weather, "[", 1,10:12)
sapply(Weather, "[", 1,10:12)

#Another example
lapply(Weather, rowMeans)
round(sapply(Weather, rowMeans),2) #Deliverable 1, finally. 

#Another example
sapply(Weather, function(z) round((z[1,]-z[2,])/z[2,],2)) #Deliverable 2, but sexy

#sapply is still a friendly version of lapply
sapply(Weather, rowMeans, simplify=FALSE, use.names=FALSE) #Returns a list, aka it's still the same as lapply

#---------------------------------------Nesting apply()s
lapply(Weather, rowMeans)
#Create a "rowMax" and "rowMin" function and apply it via lapply() or sapply()
sapply(Weather, apply, 1, max) #sapply iterates through the list, performing apply(x, 1, max) for component x in the list
#Alternatively,
sapply(Weather, function(x) apply(x,1,max)) #Deliverable 3
sapply(Weather, function(x) apply(x,1,min)) #Deliverable 4

#---------------------------------------which.max() and which.min()
#This is for the last deliverable, aka the month where the maximum metric for each occurred
#which.max()
?which.max()
Chicago[1,]
which.max(Chicago[1,])
names(which.max(Chicago[1,]))

apply(Chicago, 1, function(x) names(which.max(x)))

#This gives us the month of the max value for one row in one city. Use two apply()s

sapply(Weather, 
       function(y) apply(y, 1, function(x) names(which.max(x))))
#So, we apply function 1 to component y of Weather
#function 1 applies function 2 to all the rows (1) of component y
#function 2 pulls the name of the index value which matches the max value of row x






