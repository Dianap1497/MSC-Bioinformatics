##import data
library(vroom)
library(tidyverse)
library(lubridate)
library(wbstats)
covid_data <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/time_series_covid19_deaths_global.csv")
##change the first two names of our data frame
names(covid_data)[1:2] <- c("Province.State", "Country.Region")
##Take the covid data 
covid_long <- covid_data %>%
  ##and then apply this function 
  pivot_longer(cols = -c(Province.State:Long),
               names_to = "Date",
               values_to = "Deaths")
##see the unique dates
unique(covid_long$Date)

##tell lubridate what the current format is - month, day, year
covid_long$Date<-mdy(covid_long$Date)

##then convert it to day, month, year to make it easy to read
covid_long$Date <- format(covid_long$Date, "%d-%m-%Y")

####see the unique dates
unique(covid_long$Date)

##extract the population data for all countries
pop_data <- wb_data(indicator = "SP.POP.TOTL", 
                    start_date = 2002, 
                    end_date = 2020)


##convert it to a tibble
pop_data <- as_tibble(pop_data)

##the maximum value of the years in the date column
max(pop_data$date)

## filter the data to include data from the year 2020 only:
pop_2020 <- pop_data %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date == 2020)

##look at the data
pop_2020

##the covid data
covid_long
##the population data
pop_2020
##the first 10 and last 10 unique values in the country column
## the ; operature acts as a new line - meaning you can run two bits of code which don't interact on the same line
head(unique(pop_2020$country), 10); tail(unique(pop_2020$country), 10)

##the covid data
head(covid_data, 10)

## just look at the data from Australia:
covid_data %>% filter(Country.Region == "Australia")
pop_2020 %>% filter(country == "Australia")

## make a new data.frame from the old covid_long data.frame
covid_country <- covid_long %>% 
  ## we want to calculate the number of 
  ##deaths in each country and at each date:
  group_by(Country.Region, Date) %>% 
  ## and we want the sum of the "Death" column in these groups:
  summarise(Deaths = sum(Deaths))

## have a look at the data.frame that is produced:
covid_country

##look at the first row of the WB data:
tail(pop_2020)

## add a column to covid data containing the 
covid_country$code <- countrycode(covid_country$Country.Region, 
                                  origin = "country.name", 
                                  destination = "iso3c")
##look at the new column we have added to the data set:
head(covid_country, 1)

##compare that to the values in the WB data
pop_2020 %>% filter(iso3c == "AFG")
##rename the 5th column so it works with the following code
names(pop_2020)[5] <- "value"

##demonstration of what select does:
head(pop_2020 %>% select(iso3c, value))
## join the two data sets:
covid_w_pop <- left_join(covid_country, 
                         pop_2020 %>% select(iso3c, value),
                         by = c("code" = "iso3c"))

##look at the new data set
covid_w_pop
## column names
names(covid_w_pop)
##the ones which are equal to "value"
names(covid_w_pop) == "value"
##the position in the vector of the "TRUE" statements
which(names(covid_w_pop) == "value")
##change the name
names(covid_w_pop)[which(names(covid_w_pop) == "value")] <- "Population"
## quick visual check
covid_w_pop %>% filter(Country.Region=="Afghanistan" & Date == "1/22/2020") 
pop_2020 %>% filter(country=="Afghanistan")
##filter to leave the most recent data
most_recent <- covid_country %>% 
  filter(Date == max(covid_country$Date))

##sum the deaths
sum(most_recent$Deaths)

## make a new data frame of the global deaths using group_by() and summarise()
global_deaths_day <- covid_country %>% 
  group_by(Date) %>%
  summarise("Global.deaths" = sum(Deaths))
## make a new data frame of the global deaths using group_by() and summarise()
global_deaths_day <- covid_country %>% 
  group_by(Date) %>%
  summarise("Global.deaths" = sum(Deaths, na.rm=T))
##calculate deaths per million people and add it to the data.frame
covid_w_pop$Deaths.p.m <- (covid_w_pop$Deaths / covid_w_pop$Population) * 1000000

##look at the data
tail(covid_w_pop)

## make a ggplot object
ggplot(data = global_deaths_day, aes(x = Date, y = Global.deaths))

## make a ggplot object
ggplot(data = global_deaths_day, aes(x = Date, y = Global.deaths)) + 
  ##add a geom to that object (in this case geom_point)
  ## notice the + after the ggplot() argument which allows us to 
  ##split this over multiple lines
  geom_point()

global_deaths_day

## tell R that the data is a date. 
##We need to specify the format the date data are given in using "%m-%d-%y" 
##(see ?as_date for help on what this means)
global_deaths_day$Date.corrected <- as_date(global_deaths_day$Date,
                                            format = "%d-%m-%y")

##make a ggplot with the corrected dates
ggplot(data = global_deaths_day, aes(x=Date.corrected, y= Global.deaths)) + geom_point()

##a line plot
ggplot(data = global_deaths_day, aes(x = Date.corrected, y= Global.deaths)) + geom_line()

##a scatter and line plot
ggplot(data = global_deaths_day, aes(x=Date.corrected, y= Global.deaths)) +
  #points
  geom_point(col="darkgrey") +
  #and lines!
  geom_line(col= "red")

##you can save the plots as objects in R without overwriting it
##make the ggplot object 
p1 <- ggplot(data = global_deaths_day, aes(x = Date.corrected, y = Global.deaths)) 

## add the graphic (in this case lines)
p1 <- p1 + geom_line()

##plot it
p1

##try p1 with points too
p1 + geom_point()

##you can create 2 different versions and compare how they look
##make the ggplot object
p1<-ggplot(data = global_deaths_day, aes(x=Date.corrected, y=Global.deaths)) 
## add the graphic (in this case lines)
p1<-p1+geom_line()
##make a second plot where you have points too, saved to a new object so you dont overwrite p1:
p2<-p1+geom_point()
##compare the two
p1
p2

##view covid_w_pop
covid_w_pop
##new column which contains the date in the  correct format
covid_w_pop$Date.corrected <- as_date(covid_w_pop$Date, format = "%d-%m-%y")

##make ggplot
by_country <- ggplot( data = covid_w_pop, aes(x = Date.corrected, y= Deaths))

##make the ggplot object
by_country + geom_point(aes(col = Country.Region))
##make the ggplot object
by_country + geom_point(aes(col = Country.Region)) + theme(legend.position = "none")

##make a vector of countries we want to look at:
selec_countries <- c("United Kingdom", "China", "US", "Italy", "France", "Germany")

##use this to filter by for our plot. here using the %in% operature:
sel_country_plot <- ggplot(data = covid_w_pop %>% 
                             filter(Country.Region %in% selec_countries), 
                           aes(x = Date.corrected, y = Deaths)) 

##add a line geom specifying that the colours are dependant on the groups in `Country.Region`
sel_country_plot + geom_line(aes(col=Country.Region))

##set line type by country
sel_country_plot + geom_line(aes(linetype = Country.Region))
##set line type by country
sel_country_plot + geom_point(aes(shape = Country.Region))

##facet the data by country:
sel_country_plot + geom_line() + facet_wrap(. ~ Country.Region)

#facet the data by country:
sel_country_plot + geom_line(aes(col = Country.Region)) + facet_wrap(. ~ Country.Region)

##specify the directory and name of the pdf, and the width and height
pdf("C:/Users/diana/OneDrive/Documentos/R/git/MSC-Bioinformatics/Code/Week 4/Plots/Deaths by country.pdf", width = 6, height = 4)

##run your code to print your plot
sel_country_plot + 
  ##add lines
  geom_line(aes(col = Country.Region)) + 
  ##add facets
  facet_wrap(. ~ Country.Region)

##stop the pdf function and finish the .pdf file
dev.off()

##make a vector of countries we want to look at:
selec_countries <- c("United Kingdom", "China", "US", "Italy", "France", "Germany")

##use this to filter by for our plot. here using the %in% operature:
sel_country_plot <- ggplot(data = covid_w_pop %>% 
                             filter(Country.Region %in% selec_countries), 
                           aes(x = Date.corrected, y = Deaths)) 

##add a line geom specifying that the colours are dependant on the groups in `Country.Region`
sel_country_plot + geom_line(aes(col=Country.Region))