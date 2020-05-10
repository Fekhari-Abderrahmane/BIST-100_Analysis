library(dplyr)
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
library(tseries)
library(forecast)

#install.packages('dplyr')
#require(dplyr)

install.packages('forecast')
require(forecast)

BIST100 <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
#BIST100 <- read.csv("C:\\Users\\Anonymous\\Downloads\\BIST 100 Gemi Verileri.csv")
head(BIST100)

class(BIST100)

class(BIST100$Date)
class(BIST100$Price)


BIST100$Date<- as.Date(BIST100$Date, format = "%d/%m/%Y")

class(BIST100$Date)


#BIST100["Date"]
#BIST100["Price"]
str(BIST100)

summary(BIST100$Price)
##representation
(time_plot <- ggplot(BIST100, aes(x = Date, y = Price)) +
    geom_line() +
    scale_x_date(date_labels = "%d-%m", date_breaks = "1 day") +
    theme_classic())

barplot
##better representation
(time_plot <- ggplot(BIST100, aes(x = Date, y = Price)) +
    geom_line() +
    scale_x_date(date_labels = "%Y") +
    theme_classic())

barplot

#decomposition


#span sets the number of points used to plot each local regression in the curve: the smaller the number, the more points are used and the more closely the curve will fit the original data.

(time_plot <- ggplot(BIST100, aes(x = Date, y = Price)) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, span = 0.6) +
    theme_classic())

barplot


# Transform to `ts` class
BIST100_ts <- ts(BIST100$Price)#, start = c(03,02),frequency=365) 
# Specify start and end year, measurement frequency (monthly = 12),end=c(2020,4,12)
BIST100_ts 
BIST100_ts [3]
# Decompose using `stl()`
BIST100_stl <- stl(BIST100_ts, s.window = "season")

# Generate plots
plot(BIST100_stl)  
# top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation
monthplot(BIST100_ts, choice = "remainder") 
monthplot(BIST100_ts, choice = "trend") 
monthplot(BIST100_ts, choice = "data") 
# variation in milk production for each month
seasonplot(BIST100_ts)

## Duckey fuller

#alternative hypothesis: stationary
adf.test(BIST100_ts)

#autocorrelogramme
acf(BIST100_ts,
    main = "autocorrelations")

plot(BIST100_ts)
## plot decemposition
plot(decompose(BIST100_ts),xlab="BIST100")
#############################################################
##LOG

BIST100_LOG=diff(log(BIST100_ts)) #BIST100_LOG
plot(BIST100_LOG)

adf.test(BIST100_LOG)


acf(BIST100_LOG,
    main = "autocorrelations log")
## saisonalite de la serei LOG
BIST100_LOG_stl <- stl(BIST100_LOG, s.window = "period")
# Generate plots
plot(BIST100_LOG_stl)
## plot decemposition
plot(decompose(BIST100_LOG),xlab="BIST100_LOG")


##Modelisation
BIST100_LOG_ts <- ts(BIST100_LOG)#, start = c(03,02),frequency=365)
BIST100_LOG_ts
plot(BIST100_LOG_ts)
Train <- window(x = BIST100_ts, start = 150, end =320)
Test <- window(x = BIST100_ts, start = 320,end=403)
head(Train)
class(Train)
Model[1]
 
plot(BIST100_ts)
plot(Train)
plot(Test)

class(Test)
Test

##Moving-Average Smoothing
plot(Train)
lines(ma(Train,order=3),col="red")

##prevision
ets1<- ets(Train,model="ANN")
prediction<-predict(ets1,12)
plot(prediction)

fcast<- forecast(ets1,60)
plot(fcast)
lines(Test,col="red")
accuracy(fcast,Test)

ets1<- ets(Train,model="MMN")
fcast<- forecast(ets1,h=60)
plot(fcast)
lines(Test,col="orange")
accuracy(fcast,Test)

ets1<- ets(Train,model="MMM")
fcast<- forecast(ets1,h=60)
plot(fcast)
lines(Test,col="pink")
accuracy(fcast,Test)

ets1<- ets(Train,model="N")
fcast<- forecast(ets1)
plot(fcast)
lines(Test,col="brown")
accuracy(fcast,Test)



fcast %>%
  filter(Month == "Aug 2010") %>%
  select(Month, Point_Forecast)