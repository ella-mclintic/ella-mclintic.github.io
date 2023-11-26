library(tidyr)
library(tidyverse)

## set the working directory
setwd("C:/Users/ellam/OneDrive/Documents/exeter/Programming for Social Data Science/Assessments/summative1")

##COVID CASES

#read cleaned data, stacked in python
ds <- read.csv("cleancovid19_global.csv")

#mutate dates
ds2 <- mutate(ds, date= as.Date(date, format= "%d/%m/%Y"))

#subset 2021 into ds3
DATE1 <- as.Date("2021-01-01")
DATE2 <- as.Date("2021-12-31")

ds3 <- ds2[ds2$date >= DATE1 & ds2$date <= DATE2,]

#population data
UID <- read.csv("UID_table.csv")

UID1 <- UID[,c('country','population')]

total <- merge(ds3,UID1, by="country",all.x=T)

#calculate rate (value/population)*1000
total <- within(total, covid_rate <- ((value*1000) / population))

#aggregate covid_rate by month

data_new <- total                                   # Duplicate data
data_new$year <- strftime(data_new$date, "%Y")    # Create year column
data_new$month <- strftime(data_new$date, "%m")   # Create month column
head(data_new)

ds5 <- aggregate(covid_rate ~ month + year + country,       # Aggregate data
                        data_new,
                        FUN = mean)


#add month-year column
library(zoo)
ds5$date <- as.yearmon(paste(ds5$year, ds5$month), "%Y %m")
#ds5 <- mutate(ds5, date= as.Date(date, format= "%m%Y"))


#graph covid_rate
#library(ggplot2)
#ggplot(normal3, aes(x = date, y = value, color = country)) +
#geom_line()

#rename
colnames(ds5)[4] <- "value"
ds5$country[ds5$country=="US"]<-"United States"


#log value
#ds5 <- within(ds5, log_value <- (log(value)))

#drop outliers
ds7<-ds5[!(ds5$country=="United Kingdom" | ds5$country=="United States" | ds5$country=="India" | ds5$country=="France" | ds5$country=="Colombia" | ds5$country=="Russia" | ds5$country=="Spain"| ds5$country=="Moldova" | ds5$country=="Korea, North" | ds5$country=="Kiribati" | ds5$country=="Nauru"| ds5$country=="Palau"| ds5$country=="Tonga"| ds5$country=="Tuvalu" ),]

#normalise value
#print(max(ds7$value))
#print(min(ds7$value))
#ds7$max <- 7403.875
#ds7$min <- 0


# Select Rows by column value
ds8<-ds5[ds5$month == '01',]
#change name ds7
colnames(ds8)[4] <- "min"


#merge initial value
normal1<-merge(ds7, ds8, by.x = c("country"), by.y = c("country"))


library(dplyr)
normal2 <- normal1[ c('country','date.x','month.x','year.x','value','min') ]

#change name ds7
colnames(normal2)[2] <- "date"
colnames(normal2)[3] <- "month"
colnames(normal2)[4] <- "year"

normal2 <- within(normal2, based_value <- ((value / min)-1))

write.csv(normal2, "C:/Users/ellam/OneDrive/Documents/exeter/Programming for Social Data Science/Assessments/summative1/case_rate_nooutliers_baseyear.csv", row.names=FALSE)



##COVID DEATHS1

#read cleaned data, stacked in python
deaths1 <- read.csv("cleancovid19_deaths.csv")

#mutate dates
deaths2 <- mutate(deaths1, date= as.Date(date, format= "%d/%m/%Y"))

#subset 2021 into ds3
DATE1 <- as.Date("2021-01-01")
DATE2 <- as.Date("2021-12-31")

deaths3 <- deaths2[deaths2$date >= DATE1 & deaths2$date <= DATE2,]

#population data

totaldeaths <- merge(deaths3,UID1, by="country",all.x=T)

#calculate rate (value/population)*1000
totaldeaths <- within(totaldeaths, death_rate <- ((value*1000) / population))

#aggregate covid_rate by month

deaths_new <- totaldeaths                                   # Duplicate data
deaths_new$year <- strftime(deaths_new$date, "%Y")    # Create year column
deaths_new$month <- strftime(deaths_new$date, "%m")   # Create month column
head(deaths_new)

deaths5 <- aggregate(death_rate ~ month + year + country,       # Aggregate data
                 deaths_new,
                 FUN = mean)


#add month-year column
library(zoo)
deaths5$date <- as.yearmon(paste(deaths5$year, deaths5$month), "%Y %m")
#ds5 <- mutate(ds5, date= as.Date(date, format= "%m%Y"))


#graph covid_rate
#library(ggplot2)
#ggplot(normal3, aes(x = date, y = value, color = country)) +
#geom_line()

#rename
deaths5$country[deaths5$country=="US"]<-"United States"


#log value
#deaths5 <- within(deaths5, log_value <- (log(death_rate)))

#drop outliers
deaths7<-deaths5[!(deaths5$country=="United Kingdom" | deaths5$country=="United States" | deaths5$country=="Solomon Islands" | deaths5$country=="Samoa" | deaths5$country=="Nauru" | deaths5$country=="Micronesia" | deaths5$country=="Marshall Islands"| deaths5$country=="Holy See" | deaths5$country=="Korea, North" | deaths5$country=="Kiribati" | deaths5$country=="Nauru"| deaths5$country=="Palau"| deaths5$country=="Tonga"| deaths5$country=="Tuvalu" ),]

#normalise value
#print(max(deaths7$value))
#print(min(deaths7$value))
#ds7$max <- 7403.875
#ds7$min <- 0


# Select Rows by column value
deaths8<-deaths7[deaths7$month == '01',]
#change name ds7
colnames(deaths8)[4] <- "min"


#merge initial value
deaths9<-merge(deaths7, deaths8, by.x = c("country"), by.y = c("country"))


library(dplyr)
deaths10 <- deaths9[ c('country','date.x','month.x','year.x','death_rate','min') ]

#change name ds7
colnames(deaths10)[2] <- "date"
colnames(deaths10)[3] <- "month"
colnames(deaths10)[4] <- "year"

deaths10 <- within(deaths10, based_dvalue <- ((death_rate / min)-1))

write.csv(deaths10, "C:/Users/ellam/OneDrive/Documents/exeter/Programming for Social Data Science/Assessments/summative1/death_rate_nooutliers_baseyear.csv", row.names=FALSE)

##MERGE DS5 AND DEATHS6 DATAFRAMES

#merge deaths and covid rates
library(dplyr)
merged_df<-merge(normal2, deaths10, by.x = c("country", "date"), by.y = c("country", "date"))

#scatter
with(merged_df, plot(x=value, y=death_rate,
                     xlab="COVID-19 case rate",
                     ylab="COVID-19 deaths rate", main="COVID-19 Case and Death Rates (2021), By Country, Monthly"))


##COVID DEATHS2

#read cleaned data, stacked in python
deaths1 <- read.csv("cleancovid19_deaths.csv")

#mutate dates
deaths2 <- mutate(deaths1, date= as.Date(date, format= "%d/%m/%Y"))

#subset 2021 into ds3
DATE1 <- as.Date("2021-01-01")
DATE2 <- as.Date("2021-12-31")

deaths3 <- deaths2[deaths2$date >= DATE1 & deaths2$date <= DATE2,]

#population data
totaldeaths <- merge(deaths3,UID1, by="country",all.x=T)

#calculate rate (value/population)*1000
totaldeaths <- within(totaldeaths, death_rate <- (value / population))

#aggregate death_rate by month

deaths_new <- totaldeaths                                   # Duplicate data
deaths_new$year <- strftime(deaths_new$date, "%Y")    # Create year column
deaths_new$month <- strftime(deaths_new$date, "%m")   # Create month column
head(deaths_new)

deaths4 <- aggregate(death_rate ~ month + year + country,       # Aggregate data
                 deaths_new,
                 FUN = mean)

#round to 2 dp
library(dplyr)
deaths5 <- deaths4 %>% mutate(across(c('death_rate'), round, 3))

#add month-year column
library(zoo)
deaths5$date <- as.yearmon(paste(deaths5$year, deaths5$month), "%Y %m")
#ds5 <- mutate(ds5, date= as.Date(date, format= "%m%Y"))

#subset deaths, date, country
deaths6 <- deaths5[,c('country','date','death_rate')]

##MERGE DS5 AND DEATHS6 DATAFRAMES

#merge deaths and covid rates
library(dplyr)
merged_df<-merge(ds5, deaths6, by.x = c("country", "date"), by.y = c("country", "date"))

write.csv(merged_df, "C:/Users/ellam/OneDrive/Documents/exeter/Programming for Social Data Science/Assessments/summative1/merged_covid2.csv", row.names=FALSE)


write.csv(countrydf, "C:/Users/ellam/OneDrive/Documents/exeter/Programming for Social Data Science/Assessments/summative1/countrydf.csv", row.names=FALSE)
countrydf <- ds6$country
#scatter
with(merged_df, plot(x=covid_rate, y=death_rate,
               xlab="COVID-19 case rate",
               ylab="COVID-19 deaths rate 2021", main="COVID-19 Case and Death Rates, By Country, Monthly"))
