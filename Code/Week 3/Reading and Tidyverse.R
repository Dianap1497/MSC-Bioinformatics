library(vroom)
covid_data <- vroom("C:/Users/diana/OneDrive/Documentos/R/git/MSC-Bioinformatics/Data/Bioinformatics_data/Workshop 3/time_series_covid19_deaths_global.csv")

install.packages("tidyverse")
library("tidyverse")

#what class is the object
class(covid_data)

#look at data
covid_data

#change the first two names of our data frame
names(covid_data)[1:2] <- c("Providence.State" , "Country.Region")

#apply pivot_longer function to our data frame
covid_long <- covid_data %>%
  pivot_longer(cols= -c(Providence.State, Country.Region, Lat,Long))
##it assumed that the values/observations we want to put into the long format and that the
## names of those columns will form a new colum call name

#look at new covid data
covid_long

#covid longer with names and values 
## specifyign the name of the two columns using names_ and values_to

covid_longer <- covid_data %>%
  pivot_longer(cols= -c(Providence.State:Long),
                        names_to="Date",
                        values_to= "Deaths")
##look at new data frame
covid_longer

#change long to wide
covid_longer %>%
  pivot_wider(names_from = Date,
              values_from = Deaths)
covid_longer