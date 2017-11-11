#converting the json data format into csv
#install.packages("RJSONIO")
#install.packages("RCurl")
library("RJSONIO")
library("RCurl")
library("rjson")
library("stringr")

#converting data into csv
raw=getURL("http://ist.gmu.edu/~hpurohit/courses/ait582-proj-data-spring16.json")
json_data <- fromJSON(raw)
data<-do.call(rbind,json_data)
data=data.frame(data,row.names = NULL)
write.csv(data,"final_data.csv")

#loading data from csv
data=read.csv("final_data.csv")
data=data[,-1]

#string matching for description to get titles
a=gsub(".*, (.*.) .*;.*","\\1",data[,2])
a

b=str_extract(string = a,pattern = "(Mr|Miss|Mrs|Master|Sir|Ms|Lady|Mlle|Mme|the Countess|Jonkheer|Don)\\.")
b

#extracting age
age=gsub(".*;(.*)","\\1",data[,2])
age=as.integer(age)
age


data=cbind(data,data.frame(b),data.frame(age))

##removing rows with no description about gender
na_rows_gender=which(is.na(data$b))
data=data[-na_rows_gender,]

#getting male or female with respective titles.
gender=ifelse(data$b=="Mrs.","Female",ifelse(data$b=="Miss.","Female",ifelse(data$b=="Master.","Male",
ifelse(data$b=="Mr.","Male",ifelse(data$b=="Sir.","Male",ifelse(data$b=="Ms.","Female",
ifelse(data$b=="Lady.","Female",ifelse(data$b=="Mlle.","Female",
ifelse(data$b=="Mme.","Female",ifelse(data$b=="the Countess.","Female",
                                      ifelse(data$b=="Jonkheer.","Male",ifelse(data$b=="Don.","Male",""))))))))))))

data=cbind(data,data.frame(gender))

#imputing null values with their medians with respective the title.
age_Mrs=data[which(data$b=="Mrs." & !is.na(data$age)),8]
floor(mean(age_Mrs))
median(age_Mrs)
data[which(data$b=="Mrs." & is.na(data$age)),8]=median(age_Mrs)

age_Miss=data[which(data$b=="Miss."& !is.na(data$age)),8]
floor(mean(age_Miss))
median(age_Miss)
data[which(data$b=="Miss." & is.na(data$age)),8]=median(age_Miss)

age_Master=data[which(data$b=="Master."& !is.na(data$age)),8]
floor(mean(age_Master))
median(age_Master)
data[which(data$b=="Master." & is.na(data$age)),8]=round(median(age_Master))

age_Mr=data[which(data$b=="Mr."& !is.na(data$age)),8]
floor(mean(age_Mr))
median(age_Mr)
data[which(data$b=="Mr." & is.na(data$age)),8]=median(age_Mr)


age_sir=data[which(data$b=="Sir."),8] ##no missing values in sir
age_Ms=data[which(data$b=="Ms."),8] ##no missing values in ms
age_Lady=data[which(data$b=="Lady."),8] ##no missing values in lady

colnames(data)=c("FARE","DESCRIPTION","SUCCESS","SEATCLASS","GUESTS","CUSTOMERID","Title","age","gender")

mydata=data
mydata$DESCRIPTION=NULL
mydata$CUSTOMERID=NULL
mydata$FARE=as.numeric(as.character(data$FARE))
write.csv(mydata,"Airline.csv")

