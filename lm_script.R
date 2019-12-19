setwd("D:/Courses/BIOS625/lianjia")
data <- read.csv("new.csv")
data <- data[,c(-1,-2)]
data <- data[,c(-3,-5)]
data <- data %>% 
  mutate(buildingType = case_when(buildingType == 1 ~ "Tower",
                                  buildingType == 2 ~ "Bungalow",
                                  buildingType == 3 ~ "Plate/Tower",
                                  buildingType == 4 ~ "Plate"))
data$buildingType <- as.factor(data$buildingType)
summary(data)
data1 <- data
data1$constructionTime <- as.numeric(data1$constructionTime)
data1 <- na.omit(data)
dim(data1)

data1 <- data1 %>% 
  mutate(renovationCondition = case_when(renovationCondition == 1 ~ "A_Other",
                                         renovationCondition == 2 ~ "Rough",
                                         renovationCondition == 3 ~ "Simplicity",
                                         renovationCondition == 4 ~ "Hardcover"))
data1$renovationCondition <- as.factor(data1$renovationCondition)

data1 <- data1 %>% 
  mutate(buildingStructure = case_when(buildingStructure == 1 ~ "A_Unavailable",
                                       buildingStructure == 2 ~ "Mixed",
                                       buildingStructure == 3 ~ "Brick/Wood",
                                       buildingStructure == 4 ~ "Brick/Concrete",
                                       buildingStructure == 5 ~ "Steel",
                                       buildingStructure == 6 ~ "Steel/Concrete"))
data1$buildingStructure <- as.factor(data1$buildingStructure)

data1 <- data1 %>% 
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
                              district == 12 ~ "ShunYi",
                              district == 13 ~ "MenTouGou"))
data1$buildingStructure <- as.factor(data1$buildingStructure)
summary(data1)

data2 <- na.omit(data1)
dim(data2)
full_model <- lm(price ~ square + livingRoom + drawingRoom + kitchen + bathRoom + floor + buildingType + constructionTime + renovationCondition + buildingStructure + elevator + fiveYearsProperty + subway + district, data = data)

tradetime_num <-as.numeric(data$tradeTime)-min(as.numeric(data$tradeTime))
data[,dim(data)[2] + 1] <- tradetime_num
names(data)[23] <- "tradetime_num"
full_model <- lm(price ~ square + livingRoom + drawingRoom + kitchen + bathRoom + floor + buildingType + constructionTime + renovationCondition + buildingStructure + elevator + fiveYearsProperty + subway + district + tradetime_num, data = data)
summary(full_model)
num_model <- lm(price ~ square + livingRoom + drawingRoom + kitchen + bathRoom + floor  + constructionTime + elevator + fiveYearsProperty + subway, data = data)

year <- as.numeric(format(data$tradeTime,"%Y"))
month <- as.numeric(format(data$tradeTime,"%m"))
season <- floor((month-1)/3) + 1
total_season <- year*4 + season - 1 
datecondition = dbGetQuery(con, "SELECT totalprice,lng,lat,buildingstructure,tradetime FROM housing
                       WHERE tradetime <'2011-01-01' ORDER BY tradetime DESC, totalprice DESC;")
data[,dim(data)[2] + 1] <- year
names(data)[dim(data)[2]] <- "year"
data[,dim(data)[2] + 1] <- month
names(data)[dim(data)[2]] <- "month"
data[,dim(data)[2] + 1] <- season
names(data)[dim(data)[2]] <- "season"
data[,dim(data)[2] + 1] <- total_season
names(data)[dim(data)[2]] <- "total_season"
data <- data[-which(data$year<2010),]
data$total_season <- data$total_season - min(data$total_season) + 1
data$total_season <- as.factor(data$total_season)
data$season <- as.factor(data$season)
data$constructionTime <- data$constructionTime - median(data$constructionTime)

data$square <- (data$square - mean(data$square))/sd(data$square)
data <- data[-which(data$buildingType == "Bungalow"),]

library(leaps)
full_model <- lm(price ~ square + livingRoom + drawingRoom + kitchen + bathRoom + floor + buildingType + constructionTime + renovationCondition + buildingStructure + elevator + fiveYearsProperty + subway + district + total_season, data = data)
full_model$model
mat_full <- model.matrix(full_model)
##mat_full
##selection <-regsubsets(mat_full,data$price,int = F,really.big = T)
selection <-regsubsets(mat_full,data$price,int = F,really.big = T,method = "seqrep", nvmax = 40)
summary(selection)
coef(selection,40)
plot(selection,scale = "Cp")

model_selct <- lm(price ~ square + bathRoom + buildingType + constructionTime + renovationCondition + elevator + subway + district + total_season, data = data)
mat_full <- model.matrix(model_selct)
##mat_full
##selection <-regsubsets(mat_full,data$price,int = F,really.big = T)
selection <-regsubsets(mat_full,data$price,int = F,really.big = T,method = "seqrep", nvmax = 30)
summary(selection)
coef(selection,30)
plot(selection,scale = "Cp")

betas <- c(0,model_selct$coefficients[23:54])
district_stat <- table(data$district)
weight_dist <- district_stat/sum(district_stat)
beta0 <- model_selct$coefficients[1] + mean(data$bathRoom)*model_selct$coefficients[3] + sum(weight_dist*c(0,model_selct$coefficients[12:22])) 
betas <- betas + beta0
beta_lower <- betas
beta_upper <- betas

beta_rate <- betas[2:length(betas)]/betas[-length(betas)]
ln_rate <- log(beta_rate)
predict(arima(ln_rate,c(8,1,1)),n.ahead = 1000)$pred
plot(ln_rate)
plot(ln_rate[2:length(ln_rate)]-ln_rate[-length(ln_rate)])
##lower raising speed
lower_beta <- c(beta_lower,beta_lower[length(beta_lower)]*exp(predict_ln_rate$lower[1,1]))
beta_low_rate <- lower_beta[2:length(lower_beta)]/lower_beta[-length(lower_beta)]
ln_low_rate <- log(beta_low_rate)
predict(arima(ln_low_rate,c(8,1,1)),n.ahead = 1000)$pred
predict_low_rate <- forecast(arima(ln_low_rate,c(8,1,1)))
##higher raising speed
higher_beta <- c(beta_upper,beta_upper[length(beta_upper)]*exp(predict_ln_rate$upper[1,1]))
beta_high_rate <- higher_beta[2:length(higher_beta)]/higher_beta[-length(higher_beta)]
ln_high_rate <- log(beta_high_rate)
predict(arima(ln_high_rate,c(8,1,1)),n.ahead = 1000)$pred
predict_high_rate <- forecast(arima(ln_high_rate,c(8,1,1)))
library(forecast)
predict_ln_rate <- forecast(arima(ln_rate,c(8,1,1)))
predict_beta <- rep(0,10)
for (i in 1:10){
  betas[length(betas)+1] <- exp(predict_ln_rate$mean[i])*betas[length(betas)]
  lower_beta[length(lower_beta)+1] <- exp(predict_low_rate$mean[i])*lower_beta[length(lower_beta)]
  higher_beta[length(higher_beta)+1] <- exp(predict_high_rate$mean[i])*higher_beta[length(higher_beta)]
}
lower_beta <- lower_beta[-length(lower_beta)]
higher_beta <- higher_beta[-length(higher_beta)]
plot(betas)
plot(higher_beta)
plot(lower_beta)
beta_data <- data.frame(matrix(c(1:43,lower_beta,higher_beta,betas),43,4))
beta_data <- gather(beta_data,"class","price",2:4)
ggplot(beta_data) + aes(x = X1, y = price , color = class, stroke = 2) + geom_line(size=1.5)


##Data in ChaoYang
Filter_model <- function(inquaried_data){
  library(forecast)
  subdata <- inquaried_data[[1]]
  subdata$season <- as.factor(subdata$season)
  if (inquaried_data[[2]]==F && inquaried_data[[3]]==F){
    model_build <- lm(price ~ square + bathroom + constructiontime + renovationcondition + elevator + subway + season, data = subdata)
  }
  num_season <- length(table(subdata$season))
  model_build$coefficients
  betas <- c(0,model_build$coefficients[10:(8+num_season)])

  beta0 <- model_build$coefficients[1] + mean(subdata$bathroom)*model_build$coefficients[3]
  betas <- betas + beta0
  beta_lower <- betas
  beta_upper <- betas

  beta_rate <- betas[2:length(betas)]/betas[-length(betas)]
  ln_rate <- log(beta_rate)
  predict_ln_rate <- forecast(arima(ln_rate,c(8,1,1)))
  predict_beta <- rep(0,10)
  ##lower raising speed
  lower_beta <- c(beta_lower,beta_lower[length(beta_lower)]*exp(predict_ln_rate$lower[1,1]))
  beta_low_rate <- lower_beta[2:length(lower_beta)]/lower_beta[-length(lower_beta)]
  ln_low_rate <- log(beta_low_rate)
  predict_low_rate <- forecast(arima(ln_low_rate,c(8,1,1)))
  ##higher raising speed
  higher_beta <- c(beta_upper,beta_upper[length(beta_upper)]*exp(predict_ln_rate$upper[1,1]))
  beta_high_rate <- higher_beta[2:length(higher_beta)]/higher_beta[-length(higher_beta)]
  ln_high_rate <- log(beta_high_rate)
  predict_high_rate <- forecast(arima(ln_high_rate,c(8,1,1)))
  for (i in 1:10){
    betas[length(betas)+1] <- exp(predict_ln_rate$mean[i])*betas[length(betas)]
    lower_beta[length(lower_beta)+1] <- exp(predict_low_rate$mean[i])*lower_beta[length(lower_beta)]
    higher_beta[length(higher_beta)+1] <- exp(predict_high_rate$mean[i])*higher_beta[length(higher_beta)]
  }
  lower_beta <- lower_beta[-length(lower_beta)]
  higher_beta <- higher_beta[-length(higher_beta)]
  beta_data <- data.frame(matrix(c(1:(10+num_season),lower_beta,higher_beta,betas),(10+num_season),4))
  beta_data <- gather(beta_data,"class","price",2:4)
  ggplot(beta_data) + aes(x = X1, y = price , color = class, stroke = 2) + geom_line(size=1.5)
}

##Data in Xicheng
data_Xicheng <- data[which(data$district == "XiCheng"),]
model_selct <- lm(price ~ square + bathRoom + buildingType + constructionTime + renovationCondition + elevator + subway + total_season, data = data_Xicheng)
mat_full <- model.matrix(model_selct)
##mat_full
##selection <-regsubsets(mat_full,data$price,int = F,really.big = T)
selection <-regsubsets(mat_full,data$price,int = F,really.big = T,method = "seqrep", nvmax = 30)
summary(selection)
coef(selection,30)
plot(selection,scale = "Cp")
summary(model_selct)
table(data_Xicheng$total_season)

model_selct$coefficients
betas <- c(0,model_selct$coefficients[12:40])
##district_stat <- table(data$district)
##weight_dist <- district_stat/sum(district_stat)
beta0 <- model_selct$coefficients[1] + mean(data_Xicheng$bathRoom)*model_selct$coefficients[3]
betas <- betas + beta0
beta_lower <- betas
beta_upper <- betas

beta_rate <- betas[2:length(betas)]/betas[-length(betas)]
ln_rate <- log(beta_rate)
predict(arima(ln_rate,c(8,1,1)),n.ahead = 100)$pred
plot(ln_rate)
plot(ln_rate[2:length(ln_rate)]-ln_rate[-length(ln_rate)])
library(forecast)
##predict(arima(ln_rate,c(4,1,0)),n.ahead = 100)
predict_ln_rate <- forecast(arima(ln_rate,c(8,1,1)))
predict_beta <- rep(0,10)
##lower raising speed
lower_beta <- c(beta_lower,beta_lower[length(beta_lower)]*exp(predict_ln_rate$lower[1,1]))
beta_low_rate <- lower_beta[2:length(lower_beta)]/lower_beta[-length(lower_beta)]
ln_low_rate <- log(beta_low_rate)
predict(arima(ln_low_rate,c(8,1,1)),n.ahead = 1000)$pred
predict_low_rate <- forecast(arima(ln_low_rate,c(8,1,1)))
##higher raising speed
higher_beta <- c(beta_upper,beta_upper[length(beta_upper)]*exp(predict_ln_rate$upper[1,1]))
beta_high_rate <- higher_beta[2:length(higher_beta)]/higher_beta[-length(higher_beta)]
ln_high_rate <- log(beta_high_rate)
predict(arima(ln_high_rate,c(8,1,1)),n.ahead = 100)$pred
predict_high_rate <- forecast(arima(ln_high_rate,c(8,1,1)))
for (i in 1:10){
  betas[length(betas)+1] <- exp(predict_ln_rate$mean[i])*betas[length(betas)]
  lower_beta[length(lower_beta)+1] <- exp(predict_low_rate$mean[i])*lower_beta[length(lower_beta)]
  higher_beta[length(higher_beta)+1] <- exp(predict_high_rate$mean[i])*higher_beta[length(higher_beta)]
}
lower_beta <- lower_beta[-length(lower_beta)]
higher_beta <- higher_beta[-length(higher_beta)]
plot(betas)
plot(lower_beta)
plot(higher_beta)
beta_data <- data.frame(matrix(c(1:40,lower_beta,higher_beta,betas),40,4))
beta_data <- gather(beta_data,"class","price",2:4)
ggplot(beta_data) + aes(x = X1, y = price , color = class, stroke = 2) + geom_line(size=1.5)

##Data in Haidian
data_Haidian <- data[which(data$district == "HaiDian"),]
model_selct <- lm(price ~ square + bathRoom + buildingType + constructionTime + renovationCondition + elevator + subway + total_season, data = data_Haidian)
##mat_full
##selection <-regsubsets(mat_full,data$price,int = F,really.big = T)
summary(model_selct)
table(data_Haidian$total_season)

model_selct$coefficients
betas <- c(0,model_selct$coefficients[12:39])
##district_stat <- table(data$district)
##weight_dist <- district_stat/sum(district_stat)
beta0 <- model_selct$coefficients[1] + mean(data_Haidian$bathRoom)*model_selct$coefficients[3]
betas <- betas + beta0
beta_lower <- betas
beta_upper <- betas

beta_rate <- betas[2:length(betas)]/betas[-length(betas)]
ln_rate <- log(beta_rate)
predict(arima(ln_rate,c(8,1,1)),n.ahead = 100)$pred
plot(ln_rate)
plot(ln_rate[2:length(ln_rate)]-ln_rate[-length(ln_rate)])
library(forecast)
##predict(arima(ln_rate,c(4,1,0)),n.ahead = 100)
predict_ln_rate <- forecast(arima(ln_rate,c(8,1,1)))
predict_beta <- rep(0,10)
##lower raising speed
lower_beta <- c(beta_lower,beta_lower[length(beta_lower)]*exp(predict_ln_rate$lower[1,1]))
beta_low_rate <- lower_beta[2:length(lower_beta)]/lower_beta[-length(lower_beta)]
ln_low_rate <- log(beta_low_rate)
predict(arima(ln_low_rate,c(8,1,1)),n.ahead = 1000)$pred
predict_low_rate <- forecast(arima(ln_low_rate,c(8,1,1)))
##higher raising speed
higher_beta <- c(beta_upper,beta_upper[length(beta_upper)]*exp(predict_ln_rate$upper[1,1]))
beta_high_rate <- higher_beta[2:length(higher_beta)]/higher_beta[-length(higher_beta)]
ln_high_rate <- log(beta_high_rate)
predict(arima(ln_high_rate,c(8,1,1)),n.ahead = 100)$pred
predict_high_rate <- forecast(arima(ln_high_rate,c(8,1,1)))
for (i in 1:10){
  betas[length(betas)+1] <- exp(predict_ln_rate$mean[i])*betas[length(betas)]
  lower_beta[length(lower_beta)+1] <- exp(predict_low_rate$mean[i])*lower_beta[length(lower_beta)]
  higher_beta[length(higher_beta)+1] <- exp(predict_high_rate$mean[i])*higher_beta[length(higher_beta)]
}
lower_beta <- lower_beta[-length(lower_beta)]
higher_beta <- higher_beta[-length(higher_beta)]
plot(betas)
plot(lower_beta)
plot(higher_beta)
beta_data <- data.frame(matrix(c(1:39,lower_beta,higher_beta,betas),39,4))
beta_data <- gather(beta_data,"class","price",2:4)
ggplot(beta_data) + aes(x = X1, y = price , color = class, stroke = 2) + geom_line(size=1.5)

betas <- c(0,model_selct$coefficients[23:54])


model_time <- lm(price ~ square + subway , data = data)