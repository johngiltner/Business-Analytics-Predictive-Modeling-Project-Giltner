### Data and Packages Download ###
df<-Business_Analytics_Data_File_1 # Data file downloaded and set to "df"
df$GDP2<-df$`GDP dollars  per capita`^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
df$GDP3<-df$`GDP dollars  per capita`^3 #CUBIC TRANSFORMATION (3rd ORDER)
attach(Business_Analytics_Data_File_1)
attach(df)
library(ggplot2) # g gplot2 package downloaded

### Data Partitioning ###
p<-0.7 # value set for partitioning
obs_count<-dim(df)[1] # Number of observations in data set recorded
training_size <- floor(p * obs_count) # Number of observations to be used in training data set
set.seed(1234) # seed set to be reproducible
training_shuffle <- sample(obs_count, size = training_size) # randomizes observations in training sample
### Separating the randomized samples into training and testing data ###
Training <- df[training_shuffle, ] # all observations from "training_shuffle" vector have assigned to variable "Training"
Testing <- df[-training_shuffle, ] # the remaining observations in the dataset are designated as testing data

### Verifies that Training and Testing data have been split properly ###
dim(Training) # Dimensions for Training data results in 156 rows (69.6%)
dim(Testing) # Dimensions for Testing data results in 68 rows (30.3%)

#PLOTTING THE TRAINING AND TESTING PARTITIONS
plot(`Infant mortality per 1000 births` ~ `GDP dollars  per capita`, df) # all of data comparing GDP per capita and Infant Mortality Rate has been plotted
ggplot(df, aes(x = `GDP dollars  per capita`, y = `Infant mortality per 1000 births`)) + # Running smoothed geom line over points between GDP dollars per capita and Infant mortality rate
  geom_smooth()
plot(`Infant mortality per 1000 births` ~ `GDP dollars  per capita`, Training, col ='green') # plots GDP per capita and Infant Mortality only for the Training data
plot(`Infant mortality per 1000 births` ~ `GDP dollars  per capita`, Testing, col ='red') # plots the GDP per capita and Infant Mortality Rate only for the Testing data

### Creating the Models and Model Predictions###
Model_1<-lm(`Infant mortality per 1000 births`~`GDP dollars  per capita`,Training) # Model 1 created with Infant Mortality Rate (y) and GDP per capita (x)
summary(Model_1) # Summary statistics given for Model_1
View(Model_1$fitted.values) # Views the fitted values of the linear model (in sample data)
Mod1_Pred_TRN <- predict(Model_1, Training) # Creates prediction values for training data
Mod1_Pred_TST<-predict(Model_1,Testing) # Creates prediction values for the testing data

### Measuring Error ###
RMSE_Mod1_TRN<-sqrt(sum((Mod1_Pred_TRN - Training $ `Infant mortality per 1000 births`)^2)/length(Mod1_Pred_TRN))  # Root mean squared error calculated for in sample data
RMSE_Mod1_TST<-sqrt(sum((Mod1_Pred_TST - Testing $ `Infant mortality per 1000 births`)^2)/length(Mod1_Pred_TST)) # Root mean squared error calculated for out of sample data
RMSE_Mod1_TRN  # Displays in sample RMSE
RMSE_Mod1_TST # Displays out of sample RMSE

### Plotting predicted values against out of sample data ###
x_grid <- seq(0,8,.1) # creates a grid of x-axis values
predictions_1 <- predict(Model_1, list(`GDP dollars  per capita`=x_grid)) # Predictions for Model_1 created and assigned to "predictions_1"
plot(Training$`Infant mortality per 1000 births` ~ Training$`GDP dollars  per capita`, col='blue') # plots predicted values against actual values
lines(x_grid, predictions_1, col='green', lwd=3) # Predictions_1 plotted and colored green
points(Testing$`Infant mortality per 1000 births` ~ Testing$`GDP dollars  per capita`, col='red', pch=3) # Testing data plotted and colored red

### Creating second linear model ###
Model_2<- lm(`Infant mortality per 1000 births`~ `GDP dollars  per capita` + `Pop. Density per sq. mi.`, Training) # model 2 created with the additional variable of population density per square mile
summary(Model_2) #summary statistics for Model_2
View(Model_2$fitted.values) # Displays fitted values for Model_2
Mod2_Pred_TRN<- predict(Model_2, Training) # Predictions for Model_2 training data created
Mod2_Pred_TST<- predict(Model_2,Testing) # Predictions for Model_2 testing data created

### Measuring Error ###
RMSE_Mod2_TRN<-sqrt(sum((Mod2_Pred_TRN - Training $ `Infant mortality per 1000 births`)^2)/length(Mod2_Pred_TRN))  # Root mean squared error calculated for in sample data with linear Model_2
RMSE_Mod2_TST<-sqrt(sum((Mod2_Pred_TST - Testing $ `Infant mortality per 1000 births`)^2)/length(Mod2_Pred_TST)) # Root mean squared error calculated for out of sample data with linear Model_2
RMSE_Mod2_TRN  # Displays in sample RMSE
RMSE_Mod2_TST # Displays out of sample RMSE

### Plotting predicted values against out of sample data for Model_2 ###
x_grid <- seq(0,8,.1) # creates a grid of x-axis values
predictions_2 <- predict(Model_2, list(`GDP dollars  per capita`=x_grid, `Pop. Density per sq. mi.`=x_grid)) # Predictions for Model_2 created and assigned to "predictions_2"
plot(Training$`Infant mortality per 1000 births` ~ Training$`GDP dollars  per capita`, col='blue') # plots predicted values against actual values
lines(x_grid, predictions_2, col='green', lwd=3) # Predictions_1 plotted and colored green
points(Testing$`Infant mortality per 1000 births` ~ Testing$`GDP dollars  per capita`, col='red', pch=3) # Testing data plotted and colored red


### Creating a 3rd Linear Model  ###
Model_3<- lm(`Infant mortality per 1000 births` ~ `GDP dollars  per capita` + GDP2, Training) # Quadratic model created with GDP_2 (GDP dollars per capita squared) added to right side
summary(Model_3) #summary statistics for Model_3
Mod3_Pred_TRN<- predict(Model_3, Training) # Predictions for Model_3 training data created
View(Mod3_Pred_TRN)
View(Model_3$fitted.values) # Displays fitted values for Model_3
Mod3_Pred_TST<- predict(Model_3,Testing) # Predictions for Model_3 testing data created

### Measuring Error for Model_3 ###
RMSE_Mod3_TRN<-sqrt(sum((Mod3_Pred_TRN - Training $ `Infant mortality per 1000 births`)^2)/length(Mod3_Pred_TRN))  # Root mean squared error calculated for in sample data with linear Model_2
RMSE_Mod3_TST<-sqrt(sum((Mod3_Pred_TST - Testing $ `Infant mortality per 1000 births`)^2)/length(Mod3_Pred_TST)) # Root mean squared error calculated for out of sample data with linear Model_2
RMSE_Mod3_TRN  # Displays in sample RMSE
RMSE_Mod3_TST # Displays out of sample RMSE

### Plotting predicted values against out of sample data for model_3 ###
x_grid <- seq(0,8,.1) # creates a grid of x-axis values
predictions_3 <- predict(Model_3, list(`GDP dollars  per capita`=x_grid, GDP2=x_grid^2)) # Predictions for Model_2 created and assigned to "predictions_2"
plot(Training$`Infant mortality per 1000 births` ~ Training$`GDP dollars  per capita`, col='blue') # plots predicted values against actual values
lines(x_grid, predictions_3, col='green', lwd=3) # Predictions_1 plotted and colored green
points(Testing$`Infant mortality per 1000 births` ~ Testing$`GDP dollars  per capita`, col='red', pch=3) # Testing data plotted and colored red

### Creating a 4th Linear Model  ###
Model_4<- lm(`Infant mortality per 1000 births` ~ `GDP dollars  per capita` + GDP2 + GDP3 , Training) # Quadratic model created with GDP_2 (GDP dollars per capita squared) added to right side
summary(Model_4) #summary statistics for Model_3
Mod4_Pred_TRN<- predict(Model_3, Training) # Predictions for Model_3 training data created
View(Mod4_Pred_TRN)
View(Model_4$fitted.values) # Displays fitted values for Model_3
Mod4_Pred_TST<- predict(Model_4,Testing) # Predictions for Model_3 testing data created

### Measuring Error for Model_4 ###
RMSE_Mod4_TRN<-sqrt(sum((Mod4_Pred_TRN - Training $ `Infant mortality per 1000 births`)^2)/length(Mod4_Pred_TRN))  # Root mean squared error calculated for in sample data with linear Model_2
RMSE_Mod4_TST<-sqrt(sum((Mod4_Pred_TST - Testing $ `Infant mortality per 1000 births`)^2)/length(Mod4_Pred_TST)) # Root mean squared error calculated for out of sample data with linear Model_2
RMSE_Mod4_TRN  # Displays in sample RMSE
RMSE_Mod4_TST # Displays out of sample RMSE

### Plotting predicted values against out of sample data for model_4 ###
x_grid <- seq(0,8,.1) # creates a grid of x-axis values
predictions_4 <- predict(Model_4, list(`GDP dollars  per capita`=x_grid, GDP2=x_grid^2, GDP3=x_grid^3)) # Predictions for Model_2 created and assigned to "predictions_2"
plot(Training$`Infant mortality per 1000 births` ~ Training$`GDP dollars  per capita`, col='blue') # plots predicted values against actual values
lines(x_grid, predictions_4, col='green', lwd=3) # Predictions_1 plotted and colored green
points(Testing$`Infant mortality per 1000 births` ~ Testing$`GDP dollars  per capita`, col='red', pch=3) # Testing data plotted and colored red

### Comparing in sample model performance using RMSE ###
RMSE_Mod1_TRN # model 1 in sample error
RMSE_Mod2_TRN # model 2 in sample error
RMSE_Mod3_TRN # model 3 in sample error
RMSE_Mod4_TRN # model 4 in sample error


### Comparing in sample model performance using RMSE ###
RMSE_Mod1_TST # model 1 out of sample error
RMSE_Mod2_TST # model 2 out of sample error
RMSE_Mod3_TST # model 3 out of sample error
RMSE_Mod4_TST # model 4 out of sample error

### Comparing prediction models against each other ###
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`Infant mortality per 1000 births` ~ Training$`GDP dollars  per capita`, col='blue')
lines(x_grid, predictions_1, col='Black', lwd=3) #PLOTS M1
lines(x_grid, predictions_2, col='green', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='orange', lwd=3) #PLOTS M3
lines(x_grid, predictions_4, col='yellow', lwd=3) #PLOTS M4
points(Testing$`Infant mortality per 1000 births` ~ Testing$`GDP dollars  per capita`, col='red', pch=3)


