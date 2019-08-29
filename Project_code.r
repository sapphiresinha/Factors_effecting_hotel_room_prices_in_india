---
title: "Hotel room pricing"
author: "Neelam"
date: "29 December 2017"
output:
  word_document: default
  html_document: default
EmailId: Sapphiresinha@gmail.com
College: Shanti Niketan college of engg
---
#Read Data using read.csv and view Hotel room pricing
```{r}
Cities <- read.csv(paste("Cities42.csv", sep=""))
View(Cities)
```
#Summary Statistics - mean, median, standard deviation
```{r}
attach(Cities)
library(psych)
describe(Cities)
```
```{r}
summary(Cities)
```
# Create one-way contingency tables for the categorical variables in your dataset
```{r}
Table <- with(Cities,table(CityName))
    Table
```

```{r}
Table1 <- with(Cities,table(IsMetroCity))
    Table1
```

```{r}
Table2 <- with(Cities,table(IsTouristDestination))
    Table2
```
```{r}
Table3 <- with(Cities,table(IsWeekend))
    Table3
```

```{r}
Table4 <- with(Cities,table(IsNewYearEve))
    Table4
```

```{r}
Table5 <- with(Cities,table(HasSwimmingPool))
    Table5
```

#Create two-way contingency tables for the categorical variables in your dataset.
```{r}
t1 <- xtabs(~ StarRating + IsMetroCity, data=Cities)
t1
```

```{r}
t2 <- xtabs(~ FreeWifi+ FreeBreakfast, data=Cities)
t2
```

```{r}
t3 <- xtabs(~ IsTouristDestination + IsNewYearEve, data=Cities)
t3
```

#Draw a  boxplot of the variables that belong to your study.

```{r}
hist(Cities$Population, main= "Population" ,xlab="Population" ,col = "blue")
```
```{r}
hist(Cities$StarRating, main= "Star rating" ,xlab="Star rating",col = "blue")
```

```{r}
hist(Cities$Airport, main = "Distance to nearest major airport distribution", xlab = "Distance to nearest major Airport in km",col = "blue")
```

```{r}
hist(Cities$HotelCapacity, main = "Capacity of hotels", xlab = "Hotel Capacity", col = "blue")

```

```{r}
hist(Cities$CityRank, main = "Distribution of rank of cities", xlab = "City rank", col = "blue")

```

```{r}
boxplot(Cities$CityRank , horizontal =TRUE, main="city rank",col = "Red" )
```

```{r}
boxplot(Cities$IsMetroCity, horizontal =TRUE, main="Metro city and Tourist destination",col = "light green" )
```
```{r}
boxplot(Cities$IsTouristDestination, horizontal =TRUE, main="Metro city and Tourist destination",col = "light green" )
```
#Corrgram of Cities
```{r}
library(corrgram)
corrgram(Cities, order = TRUE, upper.panel = panel.pie)
```


#Create a correlation matrix.
```{r}
 x<-Cities[,c("RoomRent","StarRating","Airport","HotelCapacity")]
    
y<-Cities[,c("RoomRent","StarRating","Airport","HotelCapacity")]
    cor(x,y)
```

#Visualize your correlation matrix using corrgram.
```{r}
corrgram(Cities[c("RoomRent","StarRating","Airport","HotelCapacity")], upper.panel = panel.pie)
```

#Create a scatter plot matrix for your data set.
```{r}
library(car)    
scatterplot(RoomRent~StarRating,     data=cities,
            spread=FALSE, smoother.args=list(lty=2),
            main="Scatter plot of Star Rating vs Room rent",
            ylab="Room Rent",
            xlab="Star Rating")
```

```{r}
library(car)    
scatterplot(RoomRent~CityRank,     data=cities,
            spread=FALSE, smoother.args=list(lty=2),
            main="Scatter plot of City rank vs Room rent",
            ylab="Room Rent",
            xlab="Rank of City")
```

```{r}
library(car)    
scatterplot(RoomRent~ HasSwimmingPool, data=Cities,
            spread=FALSE, smoother.args=list(lty=2),
            main="Scatter plot of hotel with Swimming pool vs Room rent",
            ylab="Room Rent",
            xlab="Hotel with swimming pool")
```

```{r}
library(car)
scatterplotMatrix(formula = ~ RoomRent + IsWeekend + IsNewYearEve +Airport , data = Cities, pch = 16)
```

#T-Test
#test1: Null Hypothesis - Their is no Difference between the Room Rent on weekdays and on weekends
```{r}
t.test(Cities$RoomRent ~ Cities$IsWeekend)
```
#As we can see the P-Value = 0.6 (>0.05) , We Fail To reject the Null Hypothesis.
#It Shows that Their is No Significant Difference Between the Room rents on Weekdays and Weekends.

#test2: Null Hypothesis - Their is no Difference between the Room Rent on new year's eve and on other days
```{r}
t.test(Cities$RoomRent ~ Cities$IsNewYearEve)
```
# P-Value = 3.046e-05 (<0.05) Which is small enough for Rejecting the Null Hupothesis.
#Hence there is significant difference between the Room Rent on new year's eve and on other days

#test3: Null Hypothesis - Their is no Difference between the Room Rent of Metro Cities and other cities
```{r}
t.test(Cities$RoomRent ~ Cities$IsMetroCity)

```
# P-Value = 2.2e-16 (<0.05) Which is small enough for Rejecting the Null Hupothesis.
#Hence there is significant difference between the Room Rent of Metro Cities and other cities


##test4: Null Hypothesis - Their is no Difference between the Room Rent where wifi is free and other rooms.

```{r}
t.test(Cities$RoomRent ~ Cities$FreeWifi)

```
#As we can see the P-Value = 0.44 (>0.05) , We Fail To reject the Null Hypothesis.
#It Shows that Their is No Significant Difference Between the Room Rent where wifi is free and other rooms.

#test5: Null Hypothesis: Their is no difference in the means of room Rent where free Breakfast is available or not
```{r}
t.test(Cities$RoomRent ~ Cities$FreeBreakfast)

```
#The difference between The two means is not different as p-value = 0.32 (>0.05) so we fail to reject the Null hypothesis. 
#It Means The Room rents Are same for all room whether free Breakfast is available or not.

##test6: null Hypothesis: The correlation between RoomRent and Star Rating is Zero 
```{r}
cor.test(Cities$RoomRent , Cities$StarRating)
```
#We Can clearly see that the corrrelation between RoomRent and StarRating = 36.9% or approx 37%.and this relation is significant because the p-value = 2.2e-16 (<0.05) which suggests us to reject the Null hypothesis.

##test7: Null hypothesis: The correlation between RoomRent and Distance to the airport is Zero 
```{r}
cor.test(Cities$RoomRent , Cities$Airport)
```
#We can see that the correlation is small 5% But it is significant as the p-value < 0.05.


##test8: null Hypothesis: The correlation between RoomRent and Hotel Capacity is Zero
```{r}
cor.test(Cities$RoomRent , Cities$HotelCapacity)
```
#As the p-value = 2.2e-16 (<0.05) We can reject the Null hypothesis that Means Their exists a significant relation between Room Rent and Hotel Capacity and as we see it is approx 16%


##test9: Null Hypothesis: Their is no difference in the means of room Rent where Swimming Pool is available or not
```{r}
t.test(Cities$RoomRent~ Cities$HasSwimmingPool)
```
#The difference between The two means is different as p-value = 2.2e-16 (<0.05) so we reject the Null hypothesis. It Means The Room rents have effect whether the hotel has the swimming pool or not.


#linear regression
#let us assume that We create a model that shows how the variables effect the Room Rent
#Our model will be like y = B0 + B1x1 + B2x2 + B3x3.. + E
#y - Room Rent (dependent variable). B0 - intercept. B1, B2, B3 .- Beta coefficients for different variables.And x1, x2, x3. x1, x2, x3 - StarRating, Dist to airport, FreeWifi, etc (independent variables).
#E - error term.

```{r}
Model1  <- lm(RoomRent ~ StarRating + Airport + FreeWifi + FreeBreakfast + HotelCapacity + HasSwimmingPool, data = Cities)
summary(Model1)
```

#Final model
#let us assume that We create a model that shows how the variables effect the Room Rent
#Our model will be like y = B0 + B1x1 + B2x2 + B3x3 + E
#y - Room Rent (dependent variable). B0 - intercept. B1, B2, B3 . - Beta coefficients for different variables. x1, x2, x3. x1, x2, x3 - dates, external factors, internal factors (independent variables). E - error term.

```{r}
Model2 <- lm(RoomRent ~ IsNewYearEve + IsMetroCity + IsTouristDestination + StarRating + Airport + HotelCapacity + HasSwimmingPool, data = Cities)
summary(Model2)
```

#Conclusion
#Hotel pricing is mainly dependent on factors like 
#new year events,
#whether the hotel is in metro city or not, 
#whether the hotel is in tourist place or not,
#Star rating of hotel,
#Hotel capacity,
#whether it has swimming pool or not,
#And distance between Hotel and Airport.
#we can summarize it as:
#Room Rent = f(NewYearseve, IsMetroCity, IsTouristDestination, StarRating, Distance from the airport, HotelCapacity and HasSwimmingPool).



#END OF THE PROJECT


