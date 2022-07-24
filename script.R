
library(tidyverse)
library(dplyr)
library(naniar)
library(ggplot2)
library(glmnet,leaps,DAAG)
library(caTools)
library(caret)
library(lubridate)
data<-read.csv("AB_NYC_2019.CSV")
##48895 Observations and 16 Variables
str(data)
data<- data%>%
  mutate(name = as.factor(name), host_name = as.factor(host_name),neighbourhood_group = as.factor(neighbourhood_group),
           neighbourhood = as.factor(neighbourhood), room_type = as.factor(room_type),last_review = as.factor(last_review))


str(data)
         
summary(data)
#Summary statistics reveal ‘Entire home or apartment’ is the most common type of Airbnb

#Checking for Na's
colSums(is.na(data))
#Review_per/month = 10052


naniar::gg_miss_var(data) +
  theme_minimal()+
  labs(y = "Look at all the Missing Values") 


vis_miss(data, cluster = F)
# Total missing Data: 1.3% 

#EDA
tidy =theme(panel.grid.major =element_blank(),
               panel.grid.minor =element_blank(), 
               panel.background =element_blank(), 
               axis.line.x =element_line(color ="black"),
               axis.line.y =element_line(color ="black"),
               legend.key =element_rect(fill ="white"),
               text =element_text(size =15))




#Property types in Neighborhood Group 
ggplot(data, aes(x = fct_infreq(neighbourhood_group), fill= room_type))+
  geom_bar()+
  tidy+
  labs(title = "Property types in Neighbourhood_group ",
       x = "Neighbourhood Group", y = "No. of listings") +
  theme(legend.position = "right")



#Average Price by room Type 
par(mfrow=c(1,3))
ggplot(data, aes(x = room_type, y = mean(price), fill = room_type))+
  geom_bar(stat = "identity")+theme_minimal()+
  tidy+
  labs(title = "Average price by Room type",
       x = "Room Type", y = "Average Price") 



#Average Price by Neigbourhood
par(mfrow=c(1,3))
ggplot(data, aes(x = room_type, y = mean(price), fill = room_type))+
  geom_bar(stat = "identity")+theme_minimal()+
  tidy+
  labs(title = "Average price estimate  by Room type",
       x = "Room Type", y = "Average Price") 

##Average Pice by Neigbourhood groups#
ggplot(data, aes(x = fct_infreq(neighbourhood_group), y = mean(price), fill = neighbourhood_group))+
  geom_bar(stat = "identity")+
  tidy+
  labs(title = "Average price each Neighborhood Group",
       x = "Neighbourhood Group", y = "Price") +
  theme(legend.position = "right")




#Private room is the most common listing type in all neigbourhoods except 
#Manhattan where Entire Home/apartment is the most common type.
#Shared room is the least common in all neighborhoods.


#Mean price for Neigbourhood Group


data %>% 
  filter(!(is.na(neighbourhood_group))) %>% 
  filter(!(neighbourhood_group == "Unknown")) %>% 
  group_by(neighbourhood_group) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(neighbourhood_group, mean_price), y = mean_price, fill = neighbourhood_group)) +
  geom_col(stat ="identity", color = "black", fill="purple") +
  coord_flip() +
  theme_gray() +
  labs(x = "Neighbourhood Group", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparison for each Neighbourhood Group", subtitle = "Price vs Neighbourhood Group") + 
  xlab("Neighbourhood Group") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())

#Average price of listings is the highest for Manhattan (196.88 USD) 
#followed by Brookyln (124.28). One possible reason for high average price in Manhattan could be that whole apartments/home are the most common type of listings there.
#Bronx has the cheapest listings with an average price of 87.5 USD.  

#Mean Price Comparison for each Room Type
data %>% 
  filter(!(is.na(room_type))) %>% 
  filter(!(room_type == "Unknown")) %>% 
  group_by(room_type) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(room_type, mean_price), y = mean_price, fill = room_type)) +
  geom_col(stat ="identity", color = "black", fill="brown") +
  coord_flip() +
  theme_gray() +
  labs(x = "Room Type", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparison with all Room Types", subtitle = "Price vs Room Type") + 
  xlab("Room Type") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())
#Average price is the highest for Entire home.apartment followed by private room and shared room which is quite expected.


#Top 10 Neighbourhood

data%>%
  group_by(neighbourhood) %>%
  dplyr::summarize(num_listings = n(), 
                   borough = unique(neighbourhood_group)) %>%
  top_n(n = 10, wt = num_listings) %>%
  ggplot(aes(x = fct_reorder(neighbourhood, num_listings), 
             y = num_listings, fill = borough)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Top 10 neighborhoods by no. of listings",
       x = "Neighborhood", y = "No. of listings")


#Preparing Data For Model

#Filtering Outliers in price from 0 and extreme price values 

airbnb_data <- data %>% 
  filter(price < quantile(data$price, 0.9))%>%
filter (price > quantile(data$price, 0.1)) %>% 
  drop_na()

set.seed(123)
split <- sample.split(airbnb_data, SplitRatio = 0.7)


airbnb_data<- airbnb_data%>%
  mutate(id = row_number())

train <- subset(x = airbnb_data , split == TRUE)
test<- subset(x = airbnb_data, split==FALSE)

#Sanity Check
nrow(train) + nrow(test) == nrow(airbnb_data %>% filter(price > 0))

#We wont consider some variables for our Model
#id: Unique Identifier, so not relevant to the study
#name: Identifier, so not relevant to the study
#host_id: Unique Identifier, so not relevant to the study
#host_name: Identifier, so not relevant to the study
#neighbourhood: Redundant variable as we are already taking neighbourhood_group in our study
#last_review: categorical variable with a high number of categories, will unnecessarity complicate our model


#So we predict price based on the remaining variable 

#neighbourhood_group
#latitude
#longitude
#room_type
#minimum_nights
#number_of_reviews
#reviews_per_month
#calculated_host_listings_count
#availability_365
model_1 <- lm (price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + reviews_per_month + calculated_host_listings_count +
                        availability_365, data = train)

summary_model_1 <- summary(model_1)
mse_1 <- summary_model_1$sigma^2
r_sq_1 <- summary_model_1$r.squared
adj_r_sq_1 <- summary_model_1$adj.r.squared

#MSE = 1646
#R-Squared = 0.4462
#Adjusted Squared = 0.4465

#Plotting of 1st Linear Regression Model
par(mfrow = c(2,2))
plot(model_1)
#Residuals vs fitted values shows that the dots are not evenly distributed 
#around zero and do not show a constant variance around X. This means 
#that linearity and equal variance assumptions are not satisifed.
#QQ plot shows a 45 degree line meaning that Nomrality assumptions are met.

#Variable Selection Method 
#We will use both Subset regression method as well as 
#step-wise regression for variable selection and see how the models differ in from each other.
#----------------------------######``````
#Best-Fit Selection Method 
library(leaps)
best_fit_model <- regsubsets(price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
                                                 reviews_per_month +calculated_host_listings_count + availability_365, data = train,  nbest = 1, nvmax = 9)
plot(best_fit_model, scale="bic")
#Based on the results, the covariates that we have to select here are:
#neighbourhood_group
#latitude
#longitude
#minimum_nights
#room_type
#availablity_365
#calculated_host_listings_count

model_2 <- lm (price ~ room_type + neighbourhood_group  + latitude + longitude  + minimum_nights +
                        availability_365 + calculated_host_listings_count , data = train, nbest = 2, nvmax = 9)



summary_model_2 <- summary(model_2)
mse_2 <- summary_model_2$sigma^2
r_sq_2 <- summary_model_2$r.squared
adj_r_sq_2 <- summary_model_2$adj.r.squared
#MSE = 1648
#R-Squared = 0.445
#Adjusted Squared = 0.445

info_1<-summary(best_fit_model)
library(MASS)
cbind(info_1$which, round(cbind(rsq = info_1$rsq, adjr2 =info_1$adjr2, cp =info_1$cp,
                                bic=info_1$bic, rss=info_1$rss), 3))
#Model 9 offers the best model with the higest AIC and also Lowest BIC

#Stepwise Regression with AIC

#Stepwise Regression using Direction (Direction = "Both")

void <- lm(price~1, data = train)
full <- lm(price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
             reviews_per_month +calculated_host_listings_count + availability_365, data = train)

step(void, scope =list(lower=void, upper= full), direction = "both")

#The covariates given by this fit are as follows:
#room_type
#neighbourhood_group
#longitude
#availability_365
#calculated_host_listings_count
#minimum_nights
#latitude
#number_of_reviews
model_3 <- lm(price ~ room_type + neighbourhood_group + longitude + availability_365 + calculated_host_listings_count + minimum_nights + latitude + number_of_reviews, 
                     data = train, nbest = 2, nvmax = 9)

summary_model_3<- summary(model_3)
mse_3 <- summary_model_3$sigma^2
r_sq_3 <- summary_model_3$r.squared
adj_r_sq_3 <- summary_model_3$adj.r.squared
#MSE = 1646.45
#R-Squared = 0.446
#Adjusted Squared = 0.446

  #------Step wise Selection Using bic (Direction="Both")--------#

void_1<- lm(price~1, data = train)
full_1<- lm(price ~ neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
             reviews_per_month +calculated_host_listings_count + availability_365, data = train)

n=dim(train[1])
step(void_1, scope =list(lower=void_1, upper= full_1), k=log(n), direction = "both")
#The covariates given by this fit are as follows:
#room_type
#neighbourhood_group
#longitude
#availability_365
#minimum_nights
#calculated_host_listings_count
#latitude
#number_of_reviews
#reviews_per_month
#Hence, Building a Model using this co variates:
  
model_4 <- lm(price ~ room_type + neighbourhood_group + longitude + availability_365 + minimum_nights + latitude + calculated_host_listings_count + 
                       number_of_reviews + reviews_per_month, data = train, nbest = 2, nvmax = 9)


summary_model_4<- summary(model_4)
mse_4 <- summary_model_4$sigma^2
r_sq_4 <- summary_model_4$r.squared
adj_r_sq_4 <- summary_model_4$adj.r.squared

#MSE = 1646.41
#R-Squared = 0.446
#Adjusted Squared = 0.4462

#Lasso Regression for Variable Selection 
lasso_fit <- glmnet(x = as.matrix(train[, -c(which(colnames(train) %in% c("room_type", "neighbourhood_group","price", "name", "host_name", 
                                                                                        "neighbourhood", "last_review", "host_id", "id")))]), 
                    y = train$price, alpha = 0.5)

coef(lasso_fit,s = lasso_fit$lambda.min)

#The covariates given out by this fit are:
#latitude
#longitude
#minimum_nights
#number_of_reviews
#reviews_per_month
#calculated_host_listings_count
#availability_365

model_5 <- lm(price ~ latitude + longitude + minimum_nights + number_of_reviews + reviews_per_month + calculated_host_listings_count + availability_365,
                     data = train, nbest = 2, nvmax = 9)


summary_model_5<- summary(model_5)
#mse_6 <- mean(summary_model_6$sigma^2)
mse_5 <- mean(summary_model_5$residuals^2)
r_sq_5 <- summary_model_5$r.squared
adj_r_sq_5 <- summary_model_5$adj.r.squared


#MSE = 2677.54
#R-Squared = 0.099
#Adjusted Squared = 0.099


#Cross Validation of Lasso-Fit

cv_lasso_fit = cv.glmnet(x = as.matrix(train[, -c(which(colnames(train) %in% c("room_type", "neighbourhood_group","price" , "name", "host_name",
                                                                                             "neighbourhood", "last_review")))]), 
                         y = train$price, alpha = 1, nfolds = 5)
plot(cv_lasso_fit)


###------Model Selection-------###
library(readxl)
library(kableExtra)
model_comp <- read_excel("Model_Comparison.xlsx")

kable(model_comp) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, fixed_thead = T, )

#The model built using Lasso Regression is the 
#least desirable one as it has the lowest adjusted R-squared and highest MSE value.
#-----OBSERVATION------##
#The model built using Lasso Regression is the least 
#desirable one as it has the lowest adjusted R-squared and highest MSE value.
#The linear model built using stepwise regression using both AIC and BIC have the
#best combination of MSE and Adjusted R-squared and these values are same for both of them. However, we 
#are selecting the **Model 4** (stepwise regression using BIC) as 
#BIC is considered to be more Conservative

summary_model_4

#--Making Predictions of the Final Model-----##

pred_model<- predict(model_4, test)
summary(pred_model)
summary(test$price)
#mean square error
mean((pred_model - test$price)^2)
#1650.605

#Mean Aboslute Error
mean(abs(pred_model - test$price))
#31.03889
library(boot)
#Cross Validation of the final model

model_glm_1 = glm(price~neighbourhood_group + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
                    reviews_per_month +calculated_host_listings_count + availability_365,  data = airbnb_data)

cv.glm(data =airbnb_data, glmfit = model_glm_1, K = 3)$delta[2]
#1645.601

#Comparing the MSE of Test Dataset which is equal to 1650.6,
#and the MSPE of the Full Data which is 1645.6, 
#we can see that the values are almost similar. 
#Hence the variables that we have selected for our model 
#are good estimators of our dependent variable.


