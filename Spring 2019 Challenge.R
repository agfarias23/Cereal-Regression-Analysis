###Responses####


##Apple Jacks -  32.75299 
##Corn Flakes - 44.87348
##Fruity Pebbles - 27.46800 
##Just Right Fruit & Nut - 38.91645
##Total Corn Flakes - 41.04639

####FOR PREDICTIONS###
#import data
data<-read.csv(file.choose(),header = TRUE)


#visualize data 

data2<-na.omit(data[]) ##removing 5 cereals that have NA rating

summary(data2$rating)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.04   33.54   40.50   43.10   51.71   93.70
hist(data2$rating)

install.packages('dummies') ##casting categorical variables to dummy variables
library(dummies)

data2.dummies<-NA
data2.dummies.mfr<-dummy(data2$mfr,sep = ".")
data2.dummies.type<-dummy(data2$type,sep = ".")
data2.dummies<-as.data.frame(cbind(data2[4:16],data2.dummies.mfr,data2.dummies.type))
data2.dummies<-data2.dummies[-4,]

install.packages (dplyr)

install.packages("ggcorrplot") ##visualizing data as correlation matrix to make more 
                               ##informed decision about which predictor variables to model
library(ggcorrplot)
corr<-round(cor(data2.dummies),2)
corr
                   
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

#split data into training and testing set
sample_size <-0.8 * nrow(data2.dummies)
set.seed(120)
index <- sample(seq_len(nrow(data2.dummies)), size = sample_size)
training_data <- na.omit(data2.dummies[index, ])
testing_data <- na.omit(data2.dummies[-index, ])

#based on correlation matrix, test variables of interest
head(data)
test_model<-lm(rating~calories+protein+fat+sodium+fiber+sugars+potass+mfr.G+mfr.N,
               data=training_data)

#run linear regression on data using AIC stepwise regreession
library(MASS)
initial.model<-lm(rating~1, data=training_data)
k_value<-qchisq(0.99,1)
AIC.model<-stepAIC(initial.model, formula(test_model), direction=c("forward"),k=k_value)
AIC.model_summary<-summary(AIC.model)
AIC.model_summary

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 57.214352   1.380264  41.452  < 2e-16 ***
#  sugars      -1.699325   0.096526 -17.605  < 2e-16 ***
#  sodium      -0.050130   0.004329 -11.580 9.21e-16 ***
#  fiber        2.509609   0.296570   8.462 3.24e-11 ***
#  fat         -4.129831   0.453678  -9.103 3.44e-12 ***
#  protein      1.542896   0.415289   3.715 0.000512 ***
#  ---


#Residual standard error: 2.798 on 50 degrees of freedom
#Multiple R-squared:  0.9581,	Adjusted R-squared:  0.9539 
#F-statistic: 228.5 on 5 and 50 DF,  p-value: < 2.2e-16

#predict on holdout data
predictions_test<-predict(AIC.model,testing_data)
testing_data.df<-as.data.frame(cbind(
  testing_data$rating,predictions_test))
colnames(testing_data.df)[colnames(testing_data.df)=="V1"]<-"Actual_Rating"
residual_fit<-testing_data.df$Actual_Rating-testing_data.df$predictions
testing_data.df<-cbind(testing_data.df,residual_fit)
rmse.testing_data<-sqrt(mean(residual_fit^2))

rmse.testing_data
rmse.training_data<-AIC.model_summary$sigma
print(rmse.training_data)

#find 95% confidence interval of RMSE on testing data using bootstrapping
#to find statistical significance of RMSE training vs. RMSE testing
library(boot)
rmse_function<-function(data, indices){
  sample<-data[indices,]
  model.boot<-lm(rating ~ sugars+
                   fiber+
                   sodium+
                   fat+
                   protein, 
                 data=sample)
  rmse.boot<-summary(model.boot)$sigma
  return(rmse.boot)
}
results<-boot(data=data2, statistic=rmse_function, R=2000)
results
x<-mean(results$t)
n<-length(results$t)
population.var<-var(results$t)
error_right<-qt(0.975,df=n)*sqrt(population.var)
error_left<-qt(0.025,df=n)*sqrt(population.var)
left<-x+error_left
right<-x+error_right

##because RMSE of test and holdout is within 95% confidence interval we can confirm that
##the model has not overfit the data

rmse.training_data
#[1] 2.797515
rmse.testing_data
#[1] 2.00383
print(left)
#1.102253
print(right)
#3.642994

#test for collinearity
install.packages("car")
library(car)
vif(AIC.model)
#sugars   sodium    fiber      fat  protein 
#1.341136 1.105030 1.397386 1.262383 1.668528 

#plot predictions
par(mfrow=c(1,1))
plot(testing_data.df$Actual_Rating, 
     testing_data.df$predictions,
     pch = 16, 
     cex = 1.3, 
     col = "blue", 
     main = "Testing Set - Predicted vs. Actual Cereal Rating", 
     xlab = "Actual Rating", 
     ylab = "Predicted Rating")

abline(lm(testing_data.df$predictions~testing_data.df$Actual_Rating),col="red")

#test predictions in question
predict_data<- rbind(
  subset(data, name =="Apple Jacks"),
  subset(data, name =="Corn Flakes"),
  subset(data, name =="Fruity Pebbles"),
  subset(data, name =="Just Right Fruit & Nut"),
  subset(data, name =="Total Corn Flakes"))
predict_data$rating<-predict(AIC.model,predict_data)

predict_data$rating
#[1] 32.75299 44.87348 27.46800 38.91645 41.04639

###FOR KELLOG###
kellog<-subset(data2,mfr == "K")
kellog_drop<-kellog[-2,]
all_other<- subset(data2,mfr != "K")
top_25p<-data2[data2$rating>quantile(data2$rating,prob=0.75),]
top_25p<-top_25p[-3,]
top_25p

summary(kellog$rating)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#29.92   34.89   40.63   44.87   53.68   93.70 
summary(kellog_drop$rating)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#29.92   34.00   40.56   42.30   50.01   59.64 

hist(kellog$rating)
hist(kellog_drop$rating)

summary(all_other$rating)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.04   33.26   40.25   42.41   51.02   74.47 
hist(all_other$rating)

#Analzye top 25 percentile of cereals
install.packages('dummies') ##casting categorical variables to dummy variables
library(dummies)
data_k.dummies<-NA
data_k.dummies.mfr<-dummy(top_25p$mfr,sep = ".")
data_k.dummies.type<-dummy(top_25p$type,sep = ".")
data_k.dummies<-as.data.frame(cbind(top_25p[4:16],data_k.dummies.mfr,data_k.dummies.type))

install.packages("ggcorrplot") ##visualizing data as correlation matrix to make more 
##informed decision about which predictor variables to model
library(ggcorrplot)
corr_k<-round(cor(data_k.dummies),2)
corr_k

ggcorrplot(corr_k, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

top25p_model<-lm(rating~calories + protein + fat + sodium + fiber + sugars +vitamins + weight+
                 mfr.N + mfr.P,
                 data = data_k.dummies)


library(MASS)
initial.model.25p<-lm(rating~1, data=data_k.dummies)
k_value<-qchisq(0.99,1)
AIC.model.25p<-stepAIC(initial.model.25p, formula(top25p_model), direction=c("forward"),k=k_value)
AIC.model.25p_summary<-summary(AIC.model.25p)
AIC.model.25p_summary

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 62.19088    1.36528  45.552 8.17e-15 ***
#  mfr.N        4.84713    1.61801   2.996 0.011155 *  
#  vitamins    -0.33923    0.06562  -5.170 0.000233 ***
#  fiber        0.93937    0.26263   3.577 0.003805 ** 
#  mfr.P       -4.23392    1.66913  -2.537 0.026101 *  
#  ---

#Residual standard error: 2.4 on 12 degrees of freedom
#Multiple R-squared:  0.912,	Adjusted R-squared:  0.8826 
#F-statistic: 31.08 on 4 and 12 DF,  p-value: 3.012e-06

##Discovery is while Fiber remains a strong predictor of rating especially in the top 25%
##Fat does not play a statistically significant role, and rather vitamins come into the 
##picture. Inaddition to Manufacturing Brand. 

#visualize results for full sample#

par(mfrow=c(1,1))

data2
data_drop<-data2[-4,]

data_all_predictions<-predict(AIC.model,data_drop)
plot_data_total<-as.data.frame(cbind(data_drop$name,data_drop$rating,data_all_predictions))
colnames(plot_data_total)[colnames(plot_data_total)=="V2"]<-"Actual_Rating"

plot(plot_data_total$Actual_Rating, 
     plot_data_total$data_all_predictions,
     pch = 16, 
     cex = 1.3, 
     col = "blue", 
     main = "Predicted vs. Actual Cereal Rating Overall Sample", 
     xlab = "Actual Rating", 
     ylab = "Predicted Rating")

abline(lm(plot_data_total$data_all_predictions~plot_data_total$Actual_Rating),col="red")

data_all_predictions_25p<-predict(AIC.model,top_25p)
plot_data_total_25p<-as.data.frame(cbind(top_25p$name,top_25p$rating,data_all_predictions_25p))
colnames(plot_data_total_25p)[colnames(plot_data_total_25p)=="V2"]<-"Actual_Rating"


plot(plot_data_total_25p$Actual_Rating, 
     plot_data_total_25p$data_all_predictions_25p,
     pch = 16, 
     cex = 1.3, 
     col = "blue", 
     main = "Predicted vs. Actual Cereal Rating 25th Percentile", 
     xlab = "Actual Rating", 
     ylab = "Predicted Rating")

abline(lm(plot_data_total_25p$data_all_predictions_25p~plot_data_total_25p$Actual_Rating),col="red")

AIC.model_summary
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 57.214352   1.380264  41.452  < 2e-16 ***
#  sugars      -1.699325   0.096526 -17.605  < 2e-16 ***
# sodium      -0.050130   0.004329 -11.580 9.21e-16 ***
# fiber        2.509609   0.296570   8.462 3.24e-11 ***
# fat         -4.129831   0.453678  -9.103 3.44e-12 ***
# protein      1.542896   0.415289   3.715 0.000512 ***
  ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2.798 on 50 degrees of freedom
#Multiple R-squared:  0.9581,	Adjusted R-squared:  0.9539 
#F-statistic: 228.5 on 5 and 50 DF,  p-value: < 2.2e-16

AIC.model.25p_summary
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 62.19088    1.36528  45.552 8.17e-15 ***
#  mfr.N        4.84713    1.61801   2.996 0.011155 *  
#  vitamins    -0.33923    0.06562  -5.170 0.000233 ***
#  fiber        0.93937    0.26263   3.577 0.003805 ** 
#  mfr.P       -4.23392    1.66913  -2.537 0.026101 *  
#  ---

#Residual standard error: 2.4 on 12 degrees of freedom
#Multiple R-squared:  0.912,	Adjusted R-squared:  0.8826 
#F-statistic: 31.08 on 4 and 12 DF,  p-value: 3.012e-06

##Discovery is while Fiber remains a strong predictor of rating especially in the top 25%
##Fat does not play a statistically significant role, and rather vitamins come into the 
##picture. In addition to Manufacturing Brand.

library(dplyr) #dataplier
data3<-filter(data2,protein>3 & fiber>5)
data2[order(fiber),]


