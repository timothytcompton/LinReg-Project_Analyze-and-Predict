#TimothyCompton
#April 8, 2021
#03DSwR
#Project1: Retail Analysis with WalMart Data

#========================================================
#Load and Set Up Data:
#========================================================
#Set Up Environment:
rm(list=ls())
library(lubridate)
library(dplyr)
library(sp)
library(raster)
library(usdm)

#Read Data File:
DF1=read.csv("C:/Users/Tim/Documents/Certs & Tests/DSci 2021/Simplilearn 03062021/Courses/03 Data Science with R/Projects-Assessment/Project1_RetailAnalysisWithWalmartData/Walmart_Store_sales.csv")

#View Data:
View(DF1)

#Check for NAs:
summary(DF1) #No NAs found

#Reformat Initial Data:
DF1$Date=as.Date(DF1$Date,format="%d-%m-%Y")

#Add all new columns to DF1:
DF1$year=year(DF1$Date)
DF1$month=month(DF1$Date)
DF1$quarter=quarter(DF1$Date)
DF1$semester=semester(DF1$Date)

View(DF1)

#========================================================
#Basic Statistics Tasks:
#========================================================
#New Data File for Questions 1-4: Stats1
Stats1=summarize(group_by(DF1,Store), Max_WeeklySales=max(Weekly_Sales), Sum_WeeklySales=sum(Weekly_Sales), Mean_WeeklySales=mean(Weekly_Sales), StdDev_WeeklySales=sd(Weekly_Sales), Sales_2012.Q2=sum(Weekly_Sales[quarter==2&year==2012]), Sales_2012.Q3=sum(Weekly_Sales[quarter==3&year==2012]))
Stats1$CV_WeeklySales=Stats1$StdDev_WeeklySales/Stats1$Mean_WeeklySales*100
Stats1=Stats1[, colnames(Stats1)[c(1:5,8,6,7)]]
Stats1$GrowthRate_Q3.Q2=Stats1$Sales_2012.Q3/Stats1$Sales_2012.Q2*100
View(Stats1)

#New Data File for Question 5: Stats2
Metric=c("TotalSales")
Stats2=data.frame(Metric)
Stats2$Non_Holiday=sum(DF1$Weekly_Sales[DF1$Holiday_Flag==0])/sum(DF1$Holiday_Flag==0)
Stats2$SuperBowl=sum(DF1$Weekly_Sales[DF1$Holiday_Flag==1&DF1$month==2]/sum(DF1$Holiday_Flag==1&DF1$month==2))
Stats2$LabourDay=sum(DF1$Weekly_Sales[DF1$Holiday_Flag==1&DF1$month==9]/sum(DF1$Holiday_Flag==1&DF1$month==9))
Stats2$Thanksgiving=sum(DF1$Weekly_Sales[DF1$Holiday_Flag==1&DF1$month==11]/sum(DF1$Holiday_Flag==1&DF1$month==11))
Stats2$Christmas=sum(DF1$Weekly_Sales[DF1$Holiday_Flag==1&DF1$month==12]/sum(DF1$Holiday_Flag==1&DF1$month==12))
View(Stats2)

#New Data File for Question 6: Stats3
Stats3=summarize(group_by(DF1,month),MonthlySales=sum(Weekly_Sales))
Stats3$MonthlySales.Millions=Stats3$MonthlySales/1000000
Stats3$SalesJanNovDec=Stats3$MonthlySales #Add new column to calculate sales for months missing
Stats3[1,"SalesJanNovDec"]=Stats3[1,"MonthlySales"]+Stats3[1,"MonthlySales"]/2 #Account for January 2010 sales missing. Add average of 2011 and 2012 sales
Stats3[11,"SalesJanNovDec"]=Stats3[11,"MonthlySales"]+Stats3[11,"MonthlySales"]/2 #Account for November 2012 sales missing. Add average of 2010 and 2011 sales
Stats3[12,"SalesJanNovDec"]=Stats3[12,"MonthlySales"]+Stats3[12,"MonthlySales"]/2 #Account for December 2012 sales missing. Add average of 2010 and 2011 sales
Stats3$MonthlySalesJanNovDec.Millions=Stats3$SalesJanNovDec/1000000
View(Stats3)

#New Data File for Question 6: Stats4
Stats4=summarize(group_by(DF1,semester),SemesterSales=sum(Weekly_Sales))
Stats4$SemesterSales.Billions=Stats4$SemesterSales/1000000000
Stats4$SemesterSalesJanNovDec=Stats4$SemesterSales
Stats4[1,"SemesterSalesJanNovDec"]=Stats4[1,"SemesterSales"]+Stats3[1,"MonthlySales"]/2
Stats4[2,"SemesterSalesJanNovDec"]=Stats4[2,"SemesterSales"]+Stats3[11,"MonthlySales"]/2+Stats3[12,"MonthlySales"]/2
Stats4$SemesterSalesJanNovDec.Billions=Stats4$SemesterSalesJanNovDec/1000000000
View(Stats4)

#1.  Which Store Has Maximum Sales?
     #14 had the highest week of sales.
     print(Stats1[which.max(Stats1$Max_WeeklySales),1])


#2.  Which Store has Maximum Standard Deviation?
     #14 has the highest standard deviation among its Weekly Sales.
     print(Stats1[which.max(Stats1$StdDev_WeeklySales),1])

#3.  Which Store has the Greatest CV (Coefficient of Mean to Standard Deviation)?
     #35 has the greatest CV for its Weekly Sales.
     print(Stats1[which.max(Stats1$CV_WeeklySales),1])

#4   Which store/s has good quarterly growth rate in Q3'2021?
     #7 has the best quarterly growth rate from Q2 to Q3 2021.
     #Store #16 had the second best quarterly growth rate.
     print(Stats1[which.max(Stats1$GrowthRate_Q3.Q2),1])
     
#5   Which holidays have higher sales than the mean sales in non-holiday season (all stores)
     #Super Bowl, Labor Day, and Thanksgiving all have higher sales than average non-holiday periods
     #Christmas has lower sales than non-holiday periods
     View(Stats2)
     
#6   Monthly and Semester View of Sales
     #Monthly
        plot(Stats3$month,Stats3$MonthlySales.Millions,type="o",main="Sales by Month: Totals of 45 Stores in U.S. from February 2010 through October 2012",xlab="Month",ylab="Sales in Millions of Dollars")
        plot(Stats3$month,Stats3$MonthlySalesJanNovDec.Millions,type="o",main="Sales by Month: Average Sales of 45 Stores in U.S.",xlab="Month",ylab="Sales in Millions of Dollars")
        #Insights
             #This is very interesting. I have a few thoughts:
             #   - If little is going on and the weather is warm, people seem to spend more money (May is a crunch to complete the school year)
             #   - Christmas seems to create dismal January sales (people try to spend less in general because of high spendings on Christmas gifts/activities).
             #   - Although the week of Christmas yields low sales, sales in the month of Christmas (December) benefit from Christmas.
        
     #Semester
        plot(Stats4$semester,Stats4$SemesterSales.Billions,type="o",xaxt="none",main="Sales by Semester: Totals of 45 Stores in U.S. from February 2010 through October 2012",xlab="Semester",ylab="Sales in Billions of Dollars")
        axis(1,at=0:2)
        plot(Stats4$semester,Stats4$SemesterSalesJanNovDec.Billions,type="o",xaxt="none",main="Sales by Semester: Average Sales of 45 Stores in U.S.",xlab="Semester",ylab="Sales in Billions of Dollars")
        axis(1,at=0:2)
        #Insights:
             #Not too much insight from me here.
             #   - There is a nearly 11% difference between Semester 1 and Semester 2 across all stores and years.
             #   - The presence of Christmas in Semester 2 is a likely culprit for this difference.

        
        
#========================================================
#Statistical Model Section:
#========================================================
#For Store 1: Build Prediction Models to Forecast Demand, and
#             Select the Linear Regression model which gives best accuracy

#Create new DF for Store 1
Store1=filter(DF1,Store==1)
View(Store1)

#Restructure Dates as 1 for 5 Feb 2010:
Store1$Day=as.numeric(Store1$Date)-14645+1 #Convert all Dates into Numeric values, where Day1 (5 Feb 2010) has value of 1465
View(Store1)

Store1LR=Store1[,-c(1,2)] #Removes Store and Date columns
View(Store1LR)

#My Hypotheses (before looking at graphs, data, etc.):
#     CPI: Will not impact sales
#     Unemployment: Will impact sales
#     Fuel Price: will impact sales, but not in a linear manner


#---------------------------
#Linear Regression Model 1:
#---------------------------
Store1aLR=Store1LR

#Check for NAs: none
summary(Store1aLR)

#Check for Correlations with DV (Weekly_Sales): Poor correlation among all variables
cor(Store1aLR)

#Check for Multicollinearity: Removes year, Day, quarter
vifstep(Store1aLR[,-1],th=10)

#Generate Best Model, minimize AIC
Fit1=step(lm(Weekly_Sales~Temperature+CPI+month+Holiday_Flag+Fuel_Price+Unemployment+semester,data=Store1aLR))
summary(Fit1)

#month(***),Temperature+CPI+semester(**),Holiday_Flag( )
#Multiple R-squared 0.2431

#MAPE
Fit1$residuals

Fit1$residuals/Store1LR$Weekly_Sales #PE

abs(Fit1$residuals/Store1LR$Weekly_Sales) #APE

mean(abs(Fit1$residuals/Store1LR$Weekly_Sales)) #MAPE
#Returns 0.06042918

#Predict
a1=as.data.frame(predict(Fit1,newdata = Store1aLR))
names(a1)[1]="Predicted"
a1$Actual=Store1LR$Weekly_Sales
a1$Difference=a1$Predicted-a1$Actual
a1$absDifference=abs(a1$Difference)
a1$Percent_Error=a1$Difference/a1$Actual*100
a1$absPercent_Error=abs(a1$Percent_Error)


#---------------------------
#Linear Regression Model 2:
#---------------------------
#Create Second DF for Store1, this time considering Holidays to be outliers
#Replace all values for Weekly_Sales on Holidays with the mean of Weekly_Sales
Store1bLR=Store1aLR
Store1bLR$Weekly_Sales=ifelse(Store1bLR$Holiday_Flag>0,mean(Store1bLR$Weekly_Sales),Store1bLR$Weekly_Sales)
View(Store1bLR)

summary(Store1bLR) #Check for NAs: none

cor(Store1bLR) #Check for Correlations with DV (Weekly_Sales): Poor correlation among all variables

vifstep(Store1bLR[,-1],th=10) #Check for Multicollinearity: Removes year, Day, quarter

#Generate Best Model, minimize AIC. year, Day, quarter excluded
Fit2=step(lm(Weekly_Sales~Temperature+CPI+month+Holiday_Flag+Fuel_Price+Unemployment+semester,data=Store1bLR))
summary(Fit2)

#CPI+month(***),Temperature+semester(**)
#Multiple R-squared 0.2368

#MAPE
Fit2$residuals

Fit2$residuals/Store1LR$Weekly_Sales #PE

abs(Fit2$residuals/Store1LR$Weekly_Sales) #APE

mean(abs(Fit2$residuals/Store1LR$Weekly_Sales)) #MAPE
#Returns 0.05594411

#Predict
a2=as.data.frame(predict(Fit2,newdata = Store1bLR))
names(a2)[1]="Predicted"
a2$Actual=Store1LR$Weekly_Sales
a2$Difference=a2$Predicted-a2$Actual
a2$absDifference=abs(a2$Difference)
a2$Percent_Error=a2$Difference/a2$Actual*100
a2$absPercent_Error=abs(a2$Percent_Error)


#---------------------------
#Linear Regression Model 3:
#---------------------------
#Create Third DF for Store1, this time considering all sales in December to be outliers
#Replace all values for Weekly_Sales in December with the mean of Weekly_Sales
Store1cLR=Store1bLR
Store1cLR$Weekly_Sales=ifelse(Store1cLR$month==12,mean(Store1cLR$Weekly_Sales),Store1cLR$Weekly_Sales)
View(Store1cLR)

summary(Store1cLR) #Check for NAs: none

cor(Store1cLR) #Check for Correlations with DV (Weekly_Sales): Poor correlation among all variables

vifstep(Store1cLR[,-1],th=10) #Check for Multicollinearity: Removes year, Day, quarter

#Generate Best Model, minimize AIC. year, Day, quarter excluded
Fit3=step(lm(Weekly_Sales~Temperature+CPI+month+Holiday_Flag+Fuel_Price+Unemployment+semester,data=Store1cLR))
summary(Fit3)

#CPI(***),semester(**),month(*)
#Multiple R-squared 0.201

#MAPE
Fit3$residuals

Fit3$residuals/Store1LR$Weekly_Sales #PE

abs(Fit3$residuals/Store1LR$Weekly_Sales) #APE

mean(abs(Fit3$residuals/Store1LR$Weekly_Sales)) #MAPE
#Returns 0.04471027

#Predict
a3=as.data.frame(predict(Fit3,newdata = Store1cLR))
names(a3)[1]="Predicted"
a3$Actual=Store1LR$Weekly_Sales
a3$Difference=a3$Predicted-a3$Actual
a3$absDifference=abs(a3$Difference)
a3$Percent_Error=a3$Difference/a3$Actual*100
a3$absPercent_Error=abs(a3$Percent_Error)


#---------------------------
#Visualization of Results: Models 1, 2, 3
#---------------------------
plot(a3$Actual,a3$absDifference,type="p",main="abs(Difference) vs. Actual $",xlab="Actual ($)",ylab="abs(Difference) ($)")
points(a1$Actual,a1$absDifference,type="p",col="red")
points(a2$Actual,a2$absDifference,type="p",col="blue")

plot(a3$Actual,a3$absPercent_Error,type="p",main="abs(Error) vs. Actual $",xlab="Actual ($)",ylab="abs(Error) (Percent)")
points(a1$Actual,a1$absPercent_Error,type="p",col="red")
points(a2$Actual,a2$absPercent_Error,type="p",col="blue")
