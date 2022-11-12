# 1 READ DAILY WEATHER DATA IN 2022
dat <- NULL
current.month <- 9
for (i in 1:(current.month - 1)){
  i0 <- ifelse(i<10, paste("0", i, sep=""), i)
  mth <- paste("2022", i0, sep="")
  bom <- paste("IDCJDW2801.", mth, ".csv", sep="")
  dat.i <- read.csv(bom, skip=6, check.names=FALSE,
                    na.strings = c("NA", "", " "), stringsAsFactors = FALSE)
  dat.i[, 1] <-toupper(month.abb[i])
  # USE month.name() TO GET FULL MONTH NAMES dat <- rbind(dat,
  #dat.i)
}
dat <- rbind(dat,dat.i)
dim(dat)#The data has the dimension with 31 rows and 22 columns
dat


# 2 a #####
ms_values<-apply(dat,2,FUN=function(x){table(x,useNA = "ifany")})
lapply(ms_values, sort)# the useNA="ifany" option allows us to
#see missing values
#The inspect for suspicious or problematic records in the data shows that the variable
#"9am wind speed (km/h)" has entry  coded as "calm" which has to be change to 0 contain numeric values. 
#This shows there is an issue while entering the data

dat$`9am wind speed (km/h)`[dat$`9am wind speed (km/h)`== "Calm"] <-0 #changing
# the value to Zero 0 instead of calm
table(dat$`9am wind speed (km/h)`) # To show the new table with vale 0

# 2 b ######
# we remove the data set ???Time of maximum wind gust??? 
dat<-dat[ ,-c(10)]
names(dat)# show he table after column "Time of maximum wind gust removed"
 
# 2 c ########
names(dat) <- c("Month", "Date", "MinTemp", "MaxTemp", "Rainfall",
                "Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed",
                "Temp9am", "Humidity9am", "Cloud9am", "WindDir9am",
                "WindSpeed9am", "Pressure9am", "Temp3pm", "Humidity3pm",
                "Cloud3pm", "WindDir3pm", "WindSpeed3pm", "Pressure3pm")
dim(dat)
names(dat)#The variable names are too long. Rename the data set as follows
#The data has 31 rows and 21 columns after the removal of "Time of max wind gust"


#### 2 d #####

#first change their Calm values
#as 0 and then change their types into ???numerical???
#using the function as.numeric().

WindSpeed9am<-as.vector(dat$WindSpeed9am) 
WindSpeed9am[WindSpeed9am=="Calm"] <-0 
dat$"WindSpeed9am"<-WindSpeed9am 
dat$"WindSpeed9am"<- as.numeric(dat$"WindSpeed9am")#change to numeric 
table(dat$WindSpeed9am) #to see the data in Windspeed

### 2 e #####
dat$RainToday<-ifelse(dat$Rainfall>1, 1, 0)#Define a variable called RainToday
print(dat$RainToday)#print the values
table(dat$RainToday)#report the number of 0s and 1s

dat$RainTomorrow <- c(dat$RainToday[2:nrow(dat)], NA)#define a variable called RainTomorrow
print(dat$RainTomorrow)#print the values
table(dat$RainTomorrow)#report the number of 0s and 1s

##### 2 f #######
#save(dat,filename = "PROJECT_1")#Save a Rdata copy


#### 3.  Exploratory Data Analysis #########

str(dat)
dat<-dat[, -c(6,7)]# We first remove remove Sunshine and Evaporation column 6 & 7 respectively
#since their column is empty
names(dat)#The new data column is presented below

rn_today<-table(dat$RainToday)
pie(rn_today, col=c("cyan1", "darkgoldenrod"), main="Pie plot of Rain Today", 
    names.arg = c("No rain", "It rained"))#pie plot of rain today
class(dat$RainToday) #The result shows that there is high possibility of NOT raining today compare to raining today
rn_tmrrw<-table(dat$RainTomorrow)
barplot(rn_tmrrw, col=c("aquamarine", "chartreuse4"), main="Bar plot of Raining Tomorrow")#bar plot of rain tomorrow
hist(dat$WindSpeed3pm, col = "Red", xlab= "Windspeed 3am", 
     main="Histogram of Windspeed at 3pm")#windspeed histogram at 3pm
boxplot(dat$WindSpeed9am,dat$Month,col="Yellow", xlab= "Month",ylab="windspeed 9am",
        main="Boxplot of Windspeed at 9am") #boxplot winspeed at 9am
draw_table <- table(dat$Month, dat$RainToday, useNA="no"); draw_table
colnames(draw_table)<-c("No Rain", "Rain")
draw_table
chisq.test(draw_table)#determine the chi_square 
#of raining today
fitted_value <- glm(dat$RainToday ~ dat$Temp9am+dat$WindSpeed9am, data=dat, family=binomial)
summary(fitted_value)#obtain the summary of data if its rain today 
#with temperature and windspeed by 9am 
#The result obtain shows there no significant relationship between rain today and 
# the temperature at 9am, also the windspeed

#which gives the mean, min, max, mode of the plotted data
wilcox.test(Humidity9am~ RainTomorrow, data=dat)#perform wilcox test between humidity 9am and raining tomorrow
test_b<-data.frame(dat$MinTemp, dat$MaxTemp, dat$Rainfall, dat$WindSpeed9am, dat$Humidity3pm, dat$Humidity9am,
                     dat$Pressure3pm, dat$Pressure9am, dat$WindSpeed9am, dat$Temp3pm, dat$WindGustSpeed )
apply(test_b, 2, summary)#we summarize or describe the data  of the variables after extracting empty column as given below 
chisq.test(dat$RainTomorrow, dat$Temp9am)#chi_square test to determine if
#there is independent relationship between raining tomorrow and 9am Temperature
#The result obtained revealed no possibility of raining tomorrow with the Temperature at 9am
#since the P_value is greater than 0.05