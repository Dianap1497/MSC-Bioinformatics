library(vroom)
library(wbstats)
library(countrycode)
library(tidyverse)

#import data
tokyo <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%205/Tokyo%202021%20medals.csv")

##extract the GDP data for all countries
GDP_data <- wb_data(indicator = "NY.GDP.PCAP.KD", 
                    start= 2019, 
                    end= 2019)

## add a column to tokyo data containing the code
tokyo$code <- countrycode(tokyo$Country, 
                                  origin = "country.name", 
                                  destination = "iso3c")
##rename the 7th column so it works with the following code
names(GDP_data)[5] <- "GDP"


##correct CHINA
tokyo$code[2] <- "CHN"

## join the two data sets:
tokyo_GDP <- left_join(tokyo, 
                         GDP_data %>% select(iso3c, GDP),
                         by = c("code" = "iso3c"))
##Add position column
tokyo_GDP$position <- 1:nrow(tokyo_GDP)
tokyo_GDP

##Visualizing data
ggplot(tokyo_GDP, aes(x=GDP, y=position)) +
  geom_point() +
  theme_bw() +
  ylab("position") + 
  xlab("GDP") 

##fit a glm()
mod1 <- glm(position ~ GDP,
            data = tokyo_GDP,
            family = "gaussian")
plot(mod1)

##Not a good model to represent the data
##The residuals vs fitted show a clear trend
## the normal Q-Q has some values that do not follow the dotted line
##The residuals do not appear randomly distributed
## There is a point outside de 0.5 line
##Explore other models
##fit a glm with a poisson distribution
mod2 <- glm(position ~ GDP,
            data = tokyo_GDP,
            family = "poisson")
plot(mod2)

##fit a glm with a gaussian distribution with a log link
mod3 <- glm(position ~ GDP,
            data = tokyo_GDP,
            family = gaussian(link= "log"))
plot(mod3)

##guassian model with an inverse link
mod4 <- glm(position ~ GDP,
            data = tokyo_GDP,
            family = gaussian(link = "inverse"))
plot(mod4)

## inverse guassian model with an inverse link
mod5 <- glm(position ~ GDP,
            data = tokyo_GDP,
            family = inverse.gaussian)
plot(mod5)

##transform data with log
ggplot(tokyo_GDP, aes(x=GDP, y=position)) +
  geom_point() +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')
  theme_bw() +
  ggtitle("Logged Data")
  
##transform data with sqrt
  ggplot(tokyo_GDP, aes(x=GDP, y=position)) +
    geom_point() +
    scale_x_sqrt() +
    scale_y_sqrt()
  theme_bw() +
    ggtitle("Sqrt Data")
   
##mod were x and y are sqrt
mod6 <- glm(sqrt(position) ~ sqrt(GDP), data=tokyo_GDP)
plot(mod6)

##mod7 - normal data
mod7 <- glm(position ~ GDP, data=tokyo_GDP)
plot(mod7)

##Logged data
## fit a model where both the x and y are logged:
mod8 <- glm(log10(position) ~ log10(GDP), data=tokyo_GDP)
plot(mod8)

##compare them
AIC_mods <- AIC(mod1, mod2, mod3,mod4, mod5, mod6,mod7, mod8)

##rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AIC),]

summary(mod8)
summary(mod6)
