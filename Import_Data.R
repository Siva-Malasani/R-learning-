getwd()
setwd()

# List the files in your working directory
dir()

# Import the hotdogs.txt file: hotdogs
hotdogs <- read.table("hotdogs.txt",
                      header=FALSE, 
                      sep="\t", 
                      stringsAsFactors=TRUE, 
                      colClasses = c("character","numeric","numeric"))

colnames(hotdogs1)<-c("Type","Calorie","Carb")

#Select the hot dog with the least calories
which.min(hotdogs1$Calorie)
hotdogs1[which.min(hotdogs1$Calorie),]
OR
filter(hotdogs1,hotdogs1$Calorie==min(hotdogs1$Calorie))

# Summarize hotdogs
summary(hotdogs)

# Select the observation with the most carbohydrate
hotdogs1[which.max(hotdogs1$Carb), ]

# Print a vector representing the classes of the columns
unlist(lapply(hotdogs1,class))

# Edit the colClasses argument to import the data correctly: hotdogs2
hotdogs2 <- read.delim("hotdogs.txt", stringsAsFactors = F,header = FALSE, 
                       col.names = c("type", "calories", "sodium"),
                       colClasses = c("factor","numeric", "numeric"))


# Import swimming_pools.csv: pools
pools<-read.csv("swimming_pools.csv",stringsAsFactors = F)
pools_rt<-read.table("swimming_pools.csv",header=T,sep=",",stringsAsFactors = F)

# Print the structure of pools
str(pools)


#FREAD FUNCTION
# load the data.table package
library(data.table)

# Import potatoes.txt with fread(): potatoes
potatoes <- fread("potatoes.txt")



#EXCELSHEETS
# Load the readxl package
install.packages("readxl")
library(readxl)

# Find the names of both spreadsheets: sheets
sheets <- excel_sheets("weather.xlsx")

# Find out the class of the sheets vector
class(sheets)

# Read the first sheet of weather.xlsx
weather_1 <- read_excel("weather.xlsx", sheet= "Clean_Data")


# Read the third sheet of weather.xlsx
weather_2 <- read_excel("weather.xlsx", sheet= 1)

#Create a list with the optput of the previous 2 statements
weather_list <- list(weather_1, weather_2)

# Display the structure of weather_list
str(weather_list)


# Read all Excel sheets with lapply(): All_data
All_data<-lapply(sheets, read_excel, path = "weather.xlsx")


# Import the second sheet of weather.xlsx, skipping the first 21 rows: weather_sel
weather_sel <- read_excel("weather.xlsx", sheet =1, skip=25, col_names=FALSE)


#XLCONNECT PACKAGE
# weather.xlsx is available in your working directory

# Load the XLConnect package
install.packages(XLConnect)
library(XLConnect)

# Build connection to weather.xlsx: my_book
my_book <- loadWorkbook("weather.xlsx")

# Print out the class of my_book
class(my_book)


# Build connection to weather.xlsx
library(XLConnect)
my_book <- loadWorkbook("weather.xlsx")

# List the sheets in my_book
getSheets(my_book)

# Import the second sheet in my_book
readWorksheet(my_book, 2)
readWorksheet(my_book, "1900") # also works



# weather.xlsx is available in your working directory

# Build connection to weather.xlsx
library(XLConnect)
my_book <- loadWorkbook("weather.xlsx")

# Create data frame: summ
dims1 <- dim(readWorksheet(my_book, 1))
dims2 <- dim(readWorksheet(my_book, 2))
summ <- data.frame(sheets = getSheets(my_book), 
                   nrows = c(dims1[1], dims2[1]), 
                   ncols = c(dims1[2], dims2[2]))

# Add a worksheet to my_book, named "data_summary"
createSheet(my_book, "data_summary")

# Populate "data_summary" with summ
writeWorksheet(my_book, summ, "data_summary")

# Save workbook as weather_with_summ.xlsx
saveWorkbook(my_book, "weather_with_summ.xlsx")



#Connecting to mysql through AWS

mysqlconnection = dbConnect(MySQL(), user='dimension123', password='dimension123', 
                            host='dimensionless.ciauzkng9005.us-west-2.rds.amazonaws.com',port=3306,dbname="wine")
summary(mysqlconnection)
dbListConnections(mysqlconnection)

dbDisconnect(mysqlconnection)


dbListTables(mysqlconnection)
dbSendQuery(mysqlconnection, "CREATE DATABASE wine;")
dbSendQuery(mysqlconnection, "USE IPL")

# creating tables in wine:
wine=read.csv("https://storage.googleapis.com/dimensionless/Analytics/wine.csv")
wineTest=read.csv("https://storage.googleapis.com/dimensionless/Analytics/wine_test.csv")
dbSendQuery(mysqlconnection, "
            CREATE TABLE wine (
            Year INT,
            Price INT,
            WinterRain INT, AGST INT,HarvestRain INT,Age INT,FrancePop INT);")
dbDisconnect(mysqlconnection)
dbListTables(mysqlconnection)
xyz<-dbGetQuery(mysqlconnection,"SELECT * from wine")
dbWriteTable(conn = mysqlconnection, name = 'wine', value = (wine),overwrite=T)
dbWriteTable(conn = mysqlconnection, name = 'winetest', value = (wineTest),overwrite=T)
dbSendQuery(mysqlconnection, "
            CREATE TABLE winetest (Year INT)")
xyz<-dbGetQuery(mysqlconnection,"SELECT Year from wine")
dbReadTable(mysqlconnection,name = wine)
library(help=DBI)

# http://www.jason-french.com/blog/2014/07/03/using-r-with-mysql-databases/
