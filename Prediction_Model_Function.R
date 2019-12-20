library(forecast)
library(tidyverse)
Filter_model <- function(inquaried_data){
  subdata <- inquaried_data$data # the dataframe
  plotxaxis <-as.numeric(names(table(subdata$season))) # extract the season as the x axis
  subdata$season <- as.factor(subdata$season)
  if (inquaried_data$inference == F){ # too few observations
    return(warning("Inference Unavailable due to too small subset of houses selected"))
  }
  if (inquaried_data$multidistrict==F && inquaried_data$multibdtype==F){# single district, single bdtype
    model_build <- lm(price ~ square + bathroom + constructiontime + renovationcondition + elevator + subway + season, data = subdata)
    num_season <- length(table(subdata$season))
    betas <- c(0,model_build$coefficients[10:(8+num_season)]) # coefficients of seasons only
    beta0 <- model_build$coefficients[1] + mean(subdata$bathroom)*model_build$coefficients[3]
  }else if(inquaried_data$multidistrict==T && inquaried_data$multibdtype==F){ # all districts and only one bdtype
    model_build <- lm(price ~ square + bathroom + constructiontime + renovationcondition + elevator + subway + district + season, data = subdata)
    num_season <- length(table(subdata$season))
    betas <- c(0,model_build$coefficients[21:(19+num_season)])
    district_stat <- table(subdata$district)
    weight_dist <- district_stat/sum(district_stat)# proportion of observations in each district
    # beta 0 is the reference, the reference has been processed based all the factors other than seasons
    beta0 <- model_build$coefficients[1] + mean(subdata$bathroom)*model_build$coefficients[3] +sum(weight_dist*c(0,model_build$coefficients[10:20]))
  }else if(inquaried_data$multidistrict==F && inquaried_data$multibdtype==T){ # single district multiple bdtype
    model_build <- lm(price ~ square + bathroom + buildingtype + constructiontime + renovationcondition + elevator + subway + season, data = subdata)
    num_season <- length(table(subdata$season))
    if (length(model_build$coefficients) - num_season + 1 == 11){
      betas <- c(0,model_build$coefficients[12:(10+num_season)])
    }else{
      betas <- c(0,model_build$coefficients[11:(9+num_season)])
    }
    beta0 <- model_build$coefficients[1] + mean(subdata$bathroom)*model_build$coefficients[3]
  }else{ # all district multiple bdtype
    model_build <- lm(price ~ square + bathroom + buildingtype + constructiontime + renovationcondition + elevator + subway + district + season, data = subdata)
    num_season <- length(table(subdata$season))
    if (length(model_build$coefficients) - num_season + 1 == 22){
      betas <- c(0,model_build$coefficients[23:(21+num_season)])
    }else{
      betas <- c(0,model_build$coefficients[22:(20+num_season)])
    }
    district_stat <- table(subdata$district)
    weight_dist <- district_stat/sum(district_stat)
    beta0 <- model_build$coefficients[1] + mean(subdata$bathroom)*model_build$coefficients[3] +sum(weight_dist*c(0,model_build$coefficients[12:22]))
  }
  betas <- betas + beta0
  beta_lower <- betas
  beta_upper <- betas
  
  beta_rate <- betas[2:length(betas)]/betas[-length(betas)] # proportion of adjacent seasons
  ln_rate <- log(beta_rate)
  predict_ln_rate <- forecast(arima(ln_rate,c(8,1,1)))
  predict_beta <- rep(0,10)
  ##lower raising speed
  lower_beta <- c(beta_lower,beta_lower[length(beta_lower)]*exp(predict_ln_rate$lower[1,1]))
  beta_low_rate <- lower_beta[2:length(lower_beta)]/lower_beta[-length(lower_beta)]
  ln_low_rate <- log(beta_low_rate)
  predict_low_rate <- forecast(arima(ln_low_rate,c(8,1,1)))
  ##higher raising speed
  ##higher_beta <- c(beta_upper,beta_upper[length(beta_upper)]*exp(predict_ln_rate$upper[1,1]))
  ##beta_high_rate <- higher_beta[2:length(higher_beta)]/higher_beta[-length(higher_beta)]
  ##ln_high_rate <- log(beta_high_rate)
  ##predict_high_rate <- forecast(arima(ln_high_rate,c(8,1,1)))
  for (i in 1:8){ # predict betas for the next 2 years(8 seasons)
    betas[length(betas)+1] <- exp(predict_ln_rate$mean[i])*betas[length(betas)] # betas grow at this point
    lower_beta[length(lower_beta)+1] <- exp(predict_low_rate$mean[i])*lower_beta[length(lower_beta)]
    ##higher_beta[length(higher_beta)+1] <- exp(predict_high_rate$mean[i])*higher_beta[length(higher_beta)]
  }
  lower_beta <- lower_beta[-length(lower_beta)]
  ##higher_beta <- higher_beta[-length(higher_beta)]
  beta_data <- data.frame(matrix(c(plotxaxis,(plotxaxis[length(plotxaxis)]+1):(plotxaxis[length(plotxaxis)]+8),lower_beta,betas),(8+num_season),3))
  beta_data <- gather(beta_data,"class","price",2:3)
  beta_data[,1] <- (beta_data[,1]-1)/4 + 2010
  names(beta_data)[1] <- "year"
  ggplot(beta_data) + aes(x = year, y = price , color = class, stroke = 2) + geom_line(size=1.5)
}