#Installing and Loading the necessary libraries

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

install.packages("caret")
library(caret)

install.packages("GGally")
library(GGally)

install.packages("minpack.lm")
library(minpack.lm)

install.packages('e1071') 
library(e1071) 

install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

install.packages("randomForest")
library(randomForest)

#Reading data

data=file.choose()
data <- read.csv(data,header=TRUE)
glimpse(data)
str(data)
data <- data.frame(data)
#Exploring the data

colSums(is.na(data))
ggplot(data, aes(x = area)) +geom_histogram()
data$log_area = log10(data$area + 1)
ggplot(data, aes(x = log_area)) + geom_histogram()

scale_x_log10("Burn Area (log10)", breaks = c(1, 10, 100, 1000))
burn_coord = data %>% group_by(X, Y) %>% summarize(area_mean = mean(area))
ggplot(burn_coord, aes(x = factor(X), y = factor(Y),
                       fill = area_mean)) + geom_tile() + scale_fill_gradient2()
data$month = factor(data$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                       "jul", "aug", "sep", "oct", "nov", "dec"))

facet_wrap(~month)
scale_y_log10()
ggpairs(select(data, temp, RH,rain, wind,DMC,FFMC,ISI,DC, log_area))

data1 = filter(data, log_area > 0)

#Classifying records into weekend-records and otherwise

data$is_weekend = ifelse(data$day %in% c("sat","sun"), 1, 0)
data$is_weekend = factor(data$is_weekend)
ggplot(data, aes(x = is_weekend, y = log_area))+
geom_point()+
geom_boxplot()


print(nrow(data%>%filter((is_weekend==1))))
print(nrow(data%>%filter(is_weekend==0)))

IS_WEEKENDEQUALTOZERO=(data%>%filter(is_weekend==0))
print(max(IS_WEEKENDEQUALTOZERO$log_area))
print(min(IS_WEEKENDEQUALTOZERO$log_area))

IS_WEEKENDEQUALTOONE=(data%>%filter(is_weekend==1))
print(max(IS_WEEKENDEQUALTOONE$log_area))
print(min(IS_WEEKENDEQUALTOONE$log_area))

print(quantile(IS_WEEKENDEQUALTOZERO$log_area,probs=0.5))
print(data$log_area)


print(quantile(IS_WEEKENDEQUALTOONE$log_area,probs=0.5))

#Extracting records with log_area>0

print(data$log_area)
max=500

head(data%>% filter(log_area==0) )

d_areagreaterthanzero=data%>%filter(area>0)
head(d_areagreaterthanzero)

print(nrow(d_areagreaterthanzero%>%filter(wind>6)))

#Plotting attributes against the log_area attribute

ggplot(data, aes(x = wind, y = log_area)) + geom_point() +geom_smooth(method = "lm")

ggplot(d_areagreaterthanzero, aes(x = wind, y = log_area)) + geom_point()+geom_smooth(method = "lm")
ggplot(d_areagreaterthanzero, aes(x = RH, y = area)) + geom_point() + geom_smooth(method = "lm")

ggplot(d_areagreaterthanzero, aes(x = RH, y = log_area)) + geom_point() + geom_smooth(method = "lm")

ggplot(d_areagreaterthanzero, aes(x = temp, y = log_area)) + geom_point() +geom_smooth(method = "lm")


ggplot(d_areagreaterthanzero, aes(x = rain, y = log_area)) + geom_point() +geom_smooth(method = "lm")


ggplot(data, aes(x = FFMC, y = log_area)) + geom_point() +geom_smooth(method = "lm")
ggplot(d_areagreaterthanzero,aes(x=FFMC,y=log_area))+geom_point()+geom_smooth(method="lm")

ggplot(data,aes(x=ISI,y=log_area))+geom_point()+geom_smooth(method="lm")
ggplot(d_areagreaterthanzero,aes(x=ISI,y=log_area))+geom_point()+geom_smooth(method="lm")

ggplot(data,aes(x=DMC,y=log_area))+geom_point()+geom_smooth(method="lm")
ggplot(d_areagreaterthanzero,aes(x=DMC,y=log_area))+geom_point()+geom_smooth(method="lm")

ggplot(data,aes(x=DC,y=log_area))+geom_point()+geom_smooth(method="lm")
ggplot(d_areagreaterthanzero,aes(x=DC,y=log_area))+geom_point()+geom_smooth(method="lm")


ggplot(d_areagreaterthanzero, aes(x = is_weekend, y = log_area))+#+geom_point(aes(col="orange"))+geom_density()
  geom_point()+
  geom_boxplot()
  
#Filtering and selecting is_weekend for d_areagreaterthanzero  

IS_WEEKENDEQUALTOZEROaMORE=(d_areagreaterthanzero%>%filter(is_weekend==0))
print(max(IS_WEEKENDEQUALTOZEROaMORE$log_area))
print(min(IS_WEEKENDEQUALTOZEROaMORE$log_area))
print(nrow(IS_WEEKENDEQUALTOZEROaMORE))

IS_WEEKENDEQUALTOONEaMORE=(d_areagreaterthanzero%>%filter(is_weekend==1))
print(max(IS_WEEKENDEQUALTOONEaMORE$log_area))
print(min(IS_WEEKENDEQUALTOONEaMORE$log_area))
print(nrow(IS_WEEKENDEQUALTOONEaMORE))

print(nrow(data%>% filter(log_area==0)) )
print(length(data$log_area))

#Extracting seasons from the data

data$season = 0
for (i in 1:length(data$month)) {
  if (data$month[i] %in% c("dec", "jan", "feb")) {
    data$season[i] = "winter"  
  } else if (data$month[i] %in% c("mar", "apr", "may")) {
    data$season[i] = "spring" 
  }else if (data$month[i] %in% c("jun", "jul", "aug")) {
    data$season[i] = "summer"   
  }else  data$season[i] = "autumn"
}
data$season = as.factor(data$season)





#Observing and analysing data based on the season

ggplot(d_areagreaterthanzero, aes(x = season, y = log_area)) +
geom_point() +
geom_boxplot()

print(nrow(d_areagreaterthanzero%>%filter(season=="autumn")))
print(nrow(d_areagreaterthanzero%>%filter(season=="spring")))
print(nrow(d_areagreaterthanzero%>%filter(season=="summer")))
print(nrow(d_areagreaterthanzero%>%filter(season=="winter")))

summary(d_areagreaterthanzero$month)
length(d_areagreaterthanzero$month)

#Encoding attributes and taking a subset of the data

month = model.matrix(~month - 1, data = data)
day = model.matrix(~day - 1, data = data)
season_binary = model.matrix(~season - 1, data = data)
data = cbind(data, month, day, season_binary)
data = select(data, -month, -day, -area)
head(data)
in_train = createDataPartition(y = data$log_area, p = 0.8, list = FALSE)
head(in_train)
is(in_train)






data_train = data[in_train, ]
head(data_train)

#Defining a function for calculating R-Squared Error
 
rsquare=function(actual,pred){
	cor(actual,pred)^2
}

#Linear Regression Model

model=lm(log_area~X+Y+temp+rain+RH+wind+FFMC+DMC+DC+ISI,data=data_train)
predictions<-predict(model,data_train)

print(summary(model))
print(mean(summary(model)$residuals^2))

rsw=rsquare(data_train$log_area,predictions)
print(rsw)

#SVMs

classifier = svm(formula = log_area~X+Y+temp+rain+RH+wind+FFMC+DMC+DC+ISI, data=data_train)



pr=predict(classifier,data_train) 
print(summary(classifier))



print(mean(summary(classifier)$residuals^2))

rsw=rsquare(data_train$log_area,pr)
print(rsw)

#Classification And Regression Tree

fit.tree=rpart(log_area~X+Y+temp+rain+RH+wind+FFMC+DMC+DC+ISI,data=data_train,method='anova')

rpart.plot(fit.tree)
pr.tree=predict(fit.tree,data_train) 
print(summary(fit.tree))

MSE=mean((pr.tree-data_train$log_area)^2)
print(MSE)

rsw=rsquare(data_train$log_area,pr.tree)
print(rsw)



fit.tree=rpart(log_area~X+Y+temp+rain+RH+wind+FFMC+DMC+DC+ISI,data_train)
rpart.plot(fit.tree)
pr.tree=predict(fit.tree,data_train) 
print(summary(fit.tree))



MSE=mean((pr.tree-data_train$log_area)^2)
print(MSE)

rsw=rsquare(data_train$log_area,pr.tree)
print(rsw)

rsw=rsquare(data_train$log_area,pr)
print(rsw)

#Random Forest

rFModel=randomForest(log_area~X+Y+temp+rain+RH+wind+FFMC+DMC+DC+ISI,data_train)
predictedvalues=predict(rFModel,data_train)
print(summary(rFModel))

MSE=mean((predictedvalues-data_train$log_area)^2)
print(MSE)

rsw=rsquare(data_train$log_area,predictedvalues)
print(rsw)


#Loess model
ggplot(d_areagreaterthanzero,aes(x=DMC,y=log_area))+geom_point()+geom_smooth(method="loess")

model_loess=loess(formula=log_area~FFMC+DMC+DC+ISI,data=d_areagreaterthanzero)

p=predict(model_loess,d_areagreaterthanzero)

print(summary(model_loess))
print(mean(summary(model_loess)$residuals^2))


#Training the models using all predictor variables (including is_weekend,day and season values)

model=lm(log_area~X+Y+FFMC+DMC+DC+ISI+temp+RH+wind+rain+is_weekend+monthjan+monthfeb+monthmar+monthapr+monthmay+monthjun+monthjul+monthaug+monthsep+monthnov+monthdec+daysun+daymon+daytue+daywed+daythu+dayfri+daysat+seasonautumn+seasonspring+seasonsummer+seasonwinter,data=data_train)
print(model)
print(sample(data,size=20))

predictions <- predict(model,data_train)
print(predictions)

accuracy <- mean(predictions == data_train$log_area)
print(summary(model))
cat("Accuracy:", accuracy, "\n")
print(mean(summary(model)$residuals^2))














