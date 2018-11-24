install.packages("tidyr")
library(tidyr)

#import weather data using the GUI.
#Go to file and choose import dataset and finally choose from excel
#OR you can import the file using XLConnect/readxl package as well

weather<-read.csv("weather.xlsx")
dim(weather)

#Current problem with this data is that TIME as a dimension is present 
#in both rows as well as columns. Also, column names are values not 
#variable names. We will use gather function to correct this ie. pull 
#multiple columns and save them into 2 columns

weather1<-gather(weather,"day","value",X1:X31)
weather_1<-gather(weather,"day","value",X1:X31,na.rm=T)
dim(weather1)
dim(weather_1)

#The difference is due to removal of NA from the data 
filter(weather_1,day=="X31",month==2)

#Explained <NA> because of wrong date,see that R does not understand 
#this <NA>,it only understands NA
#Total number of values having <NA>
length(unique(weather$measure))*7

#Spreading the data helps putting the variables which are part of 
#single column i.e. measure into multiple columns

#What is the expected dimensions on the dataset after spreading
#372*26
weather_2<- spread(weather1,measure,value)
dim(weather2)

#count the number of rows in the data after spreading that has day
#="X1" and month=12 and year=2014
nrow(filter(weather_2,year == 2014 & month == 12 & day == "X1"))

#Change the coloumn name of 'year ' coloum to 'year'
names(weather_2)[2] <- "year" 


#count the number of rows in the data after spreading that has day
#="X1" and month=12 and year=2014
nrow(filter(weather_2,year == 2014 & month == 12 & day == "X1"))

#View the data and find the reason for error
filter(weather_2,year == 2014 & month == 12 & day == "X1")

#Remove first column from the dataframe weather2 and spread it after that
weather1<- weather1[-1]

#Spreading the data
weather2<- spread(weather1,measure,value)
dim(weather2)

#Clean the day column into "Number"
#Method 1
#Using Stringr package for string manipulation
library(stringr)
weather2$day<- str_replace(weather2$day,"X","")
head(weather2)

#Method 2
#Using separate function from tidyr package only
weather3 <- separate(weather2,day,c("Crap","day_data"),1)
head(weather3)
weather3<- weather3[-3]


#Converting the name to "year"
names(weather3)[1] <- "year" 

#Method 1
#mutate
weather4<-mutate(weather3,Date=as.Date(paste(year,month,day_data,sep="-"),format="%Y-%m-%d"))
head(weather4)

#Method 2
#Using Unite function from tidyr package
weather4<-unite(weather3,Date,Day,month,year,sep = '/')
class(weather5$Date)
#Convert to date type
weather5$Date <- as.Date(weather5$Date,format="%d/%m/%Y")
#if you want to set a particular format of the date
weather5$Date <- format(weather5$Date,format="%d/%m/%Y")

#remove rows which have NA values in all the columns
#Either pull the values which are not NA
B<-which(!is.na(weather4$Date))
weather5<-weather4[B,]

#OR pull the rows with all NA values and delete those rows
A<-which(is.na(weather4$Date))
weather5<- weather4[-A,]

glimpse(weather5)

#We see that Precipitation column has values "T" as well as numeric 
#data. COnvert "T" value to 0
weather5$PrecipitationIn[weather5$PrecipitationIn == "T"] <- 0

#delete the day_data,year,month column as you already have date now
weather5<- weather5[,-(1:3)]

#Change the data types of the columns whose datatype is incorrect
weather5[,3:22]<-lapply(weather5[,3:22],as.numeric)


#Count the NA values in the entire dataset as well 
length(which(is.na(weather4)))

#Count NA values in the Events column 
length(which(is.na(weather4$Events)))
