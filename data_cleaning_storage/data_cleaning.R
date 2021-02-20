# data cleaning file
# responsible for cleaning data and putting them into database
library(tidyverse)
library(RPostgreSQL)
library(lubridate)

# read in data
data <- read_csv("./new.csv", locale = locale(encoding = "UTF-8")) %>%
  mutate(floor = str_trim(str_extract(floor,"( .*)"), side = "both"))

# I remove DOM because there are too many missing values
data <- select(data, -url, -id, -Cid, -DOM)

# Coerce floor from string to integer
data$floor = as.numeric(data$floor)

# I notice that the constructionTime is a string variable, so I process it to become a number
# FIXME: When changing datatypes, some constructionTime that has non-numeric characters become NA
data$constructionTime = as.numeric(data$constructionTime)

# TODO: 318851 -> 297701, is it a big loss?
data = na.omit(data)

data <- data %>% 
  mutate(buildingType = case_when(buildingType == 1 ~ "Tower",
                                  buildingType == 2 ~ "Bungalow",
                                  buildingType == 3 ~ "Plate/Tower",
                                  buildingType == 4 ~ "Plate"))

data <- data %>% filter(buildingType!="Bungalow")
data <- data %>% 
  mutate(renovationCondition = case_when(renovationCondition == 1 ~ "A_Other",
                                         renovationCondition == 2 ~ "Rough",
                                         renovationCondition == 3 ~ "Simplicity",
                                         renovationCondition == 4 ~ "Hardcover"))

data <- data %>% 
  mutate(buildingStructure = case_when(buildingStructure == 1 ~ "A_Unavailable",
                                       buildingStructure == 2 ~ "Mixed",
                                       buildingStructure == 3 ~ "Brick/Wood",
                                       buildingStructure == 4 ~ "Brick/Concrete",
                                       buildingStructure == 5 ~ "Steel",
                                       buildingStructure == 6 ~ "Steel/Concrete")) 


# Both 3 and 4 are DaXing District
data <- data %>% 
  mutate(district = case_when(district == 1 ~ "DongCheng",
                              district == 2 ~ "FengTai",
                              district == 3 ~ "DaXing",
                              district == 4 ~ "DaXing",
                              district == 5 ~ "FangShan",
                              district == 6 ~ "ChangPing",
                              district == 7 ~ "ChaoYang",
                              district == 8 ~ "HaiDian",
                              district == 9 ~ "ShiJingShan",
                              district == 10 ~ "XiCheng",
                              district == 11 ~ "TongZhou",
                              district == 12 ~ "MenTouGou",
                              district == 13 ~ "ShunYi"))

names(data) = tolower(names(data))

data$square = data$square - mean(data$square) # mean of the area is 82.69
data$constructiontime = data$constructiontime - median(data$constructiontime) # median of construction time is 2001

data$year = year(data$tradetime)
data = data %>% filter(year >= 2010) # only 5 observations before 2010
data$quarter = quarter(data$tradetime)
data = data %>% mutate(season = (year - 2010) *4 + quarter)

data = data %>% filter(price > 500)


# insert into database
pg = dbDriver("PostgreSQL")

endpoint <- 'shinystandard.cpjmvej4z4fe.us-west-2.rds.amazonaws.com'
portnum <- 5432
username <- 'postgres'
dbname <- 'fuckaws'
pwd <- 'Zhousky123'

# connect to database
con = dbConnect(pg, user=username, password=pwd,
                host=endpoint, port=portnum, dbname=dbname)

# dbWriteTable(con, dbname, data)
dbWriteTable(con, 'beijinghousing', data)

# disconnect from database
dbDisconnect(con)

