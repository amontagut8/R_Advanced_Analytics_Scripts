####---ADVANCED ANALYTICS: SECTION 2---###
#Project brief:
##Scatterplot classified by industry showing revenue, expenses, and profit
##Scatterplot showing industry trend for the expenses~revenue relationship
##Boxplots showing growth by industry
#Dataset has discrepancies that need to be addressed before analysis is done.

#set wd
setwd('C:\\Users\\amontagut\\Desktop\\R\\Advanced Analytics\\Section_2')
#import data

fin <- read.csv(file.choose(), na.strings=c(""))
str(fin)

#Convert Inception, ID to factors
fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

#FVT
#Convert vectors into numerics
# a <- c("12", "13", "14", "12", "12")
# a
# b<- as.numeric(a)
# 
# #Convert factors into numerics
# z <- factor(c("12", "13", "14", "12", "12"))
# y <- as.numeric(z)

#Factors need to be converted into characters first, and then numbers.
# x <- as.numeric(as.character(z))
# x

#FVT Example
#Convert revenue and expenses into numbers
#Difficult because they have commas and dollar signs
# fin$Profit <- factor(fin$Profit)
# str(fin)

#Convert Profit back into a numeric
# fin$Profit <- as.numeric(as.character(fin$Profit))
# str(fin)

#sub() and gsub()
#Clean up the expenses and revenue columns

#Get rid of " dollars" suffix on expenses
fin$Expenses <- gsub(" Dollars", "",fin$Expenses) #Pattern to be replaced, pattern to replace, where to look
#Get rid of commas in expenses
fin$Expenses <- gsub(",", "", fin$Expenses)

#Get rid of $ in revenue
fin$Revenue <- gsub("\\$", "", fin$Revenue) #$ is a special character. Addressing it in a string requires an escape sequence.
#Get rid of comma in revenue
fin$Revenue <- gsub(",", "", fin$Revenue)

#Do the same thing for growth - get rid of the percent sign
fin$Growth <- gsub("\\%","", fin$Growth)

#Convert growth, expenses, and revenues to numerics
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)

#Locate missing data (elegantly)
complete.cases(fin)
fin[complete.cases(fin)==FALSE,] ##or !complete.cases(fin)

#This only picks up rows with NA. Not empty values. 

#Filtering non-mising data with which()

fin[which(fin$Revenue == 9746272),]

fin[which(fin$STATE == 'MA'),]

#.isna() filtering
head(fin, 24)

fin[is.na(fin$State),]

#Remove records with missing data
fin_backup <- fin

#Call rows with missing industry fields

fin<- fin[!is.na(fin$Industry),] #opposite - rows for which industry is NOT NA

#Reset the index

rownames(fin) <- 1:nrow(fin)

#Replacing missing data, factual analysis
fin[is.na(fin$State) & fin$City=="New York","State"] <- "NY"
fin[is.na(fin$State) & fin$City =="San Francisco","State"] <- "CA"
#check 
fin[c(11,377,82,265),] #row indices for the formerly-missing state rows

#----------------------MEDIAN IMPUTATION
fin[complete.cases(fin$Employees)==FALSE,]

median_empl_retail <- median(fin[fin$Industry=="Retail","Employees"],na.rm=T)

fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"] <- median_empl_retail

##Exercise - repeat the above for the OTHER missing employee count
#Find it first

fin[is.na(fin$Employees),]

#Calculate the median employee number for industry="Financial Services"
median(fin$Employees[fin$Industry=="Financial Services"], na.rm=T)

median_employees_finserv <- median(fin$Employees[fin$Industry=="Financial Services"], na.rm=T)

#Plug it into the dataframe

fin[is.na(fin$Employees) & fin$Industry == "Financial Services", "Employees"] <- median_employees_finserv

fin[complete.cases(fin)==FALSE,]

#Median imputation to growth column
fin[is.na(fin$Growth)==TRUE,]

#Calculate the median for growth in the construction industry (we only have one line)

median_growth_constr <- median(fin$Growth[fin$Industry=="Construction"], na.rm=T)

#Plug it into the missing growth value
fin[is.na(fin$Growth==TRUE) & fin$Industry =="Construction","Growth"] <- median_growth_constr

fin[8,]

#####Impute median values for revenue and expenses

fin[complete.cases(fin)==FALSE,]

#Companies with missing revenue are in Construction industry
##Calculate median revenue for construction industry

median_revenue_constr <- median(fin$Revenue[fin$Industry=="Construction"], na.rm=T)

#Assign it to the missing values

fin[is.na(fin$Revenue)==T & fin$Industry=="Construction", "Revenue"] <- median_revenue_constr

#Expenses
###Companies with missing expense info are also in Construction
#Calculate median expenses for construction industry

median_expenses_constr <- median(fin$Expenses[fin$Industry=="Construction"], na.rm=T)

#Assign it to missing values

fin[is.na(fin$Expenses)==T & fin$Industry=="Construction","Expenses"] <- median_expenses_constr

fin[c(8,42),]

#There is also a missing expense in the IT Services industry

median_expenses_IT <- median(fin$Expense[fin$Industry=="IT Services"], na.rm=T)

fin[is.na(fin$Expenses)==T & fin$Industry=="IT Services", "Expenses"] <- median_expenses_IT

##Adding the profit column
#This, conversely, is way easier
#Revenue - Expenses = Profit

fin[is.na(fin$Profit),"Profit"] <- (fin[is.na(fin$Profit),"Revenue"]) - (fin[is.na(fin$Profit),"Expenses"])

#Remember that you imputed the expenses for the IT services, so it will NOT add up! Fix this.

fin[15,"Profit"] <- fin[15,"Revenue"] - fin[15, "Profit"]


#------------------------------------------------------VISUALIZING DATA
##Scatterplot classified by industry showing revenue, expenses, and profit
##Scatterplot showing industry trend for the expenses~revenue relationship
##Boxplots showing growth by industry
#Dataset has discrepancies that need to be addressed before analysis is done.

library(ggplot2)
##Scatterplot classified by industry showing revenue, expenses, and profit

p <- ggplot(data=fin, aes(x=Revenue, y=Expenses, colour=Industry, size=Profit)) +
     geom_point()

##Scatterplot showing industry trend for the expenses~revenue relationship
q <- ggplot(data=fin) +
     geom_point(aes(x=Revenue,y=Expenses, colour=Industry)) +
     geom_smooth(aes(x=Revenue, y=Expenses, colour=Industry), size=1.2, fill=NA)

q

##Boxplots showing growth by industry
r <- ggplot(data=fin, aes(x=Industry, y=Growth, colour=Industry)) +
  geom_jitter() +
  geom_boxplot(alpha=0.5, outlier.color=NA) 
     
r



