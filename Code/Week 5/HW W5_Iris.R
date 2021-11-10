library(tidyverse)
library(multcomp)
##View data
data("iris")
iris

#Plot Petal.width split by pethal width so we can visualise if there are effects
##and if this differs amongst the three species
petal_width <- ggplot(data=iris,
                      aes(x= Petal.Length, y=Petal.Width)) +
  geom_point(aes(col=Species)) + 
  theme_bw() 

##Linear regression to visualise effects
##plotting a different LM for species

petal_width + geom_smooth(aes(col=Species), method="lm")


##fit a glm()
mod_pet_width <- glm(Petal.Width ~ Petal.Length*Species,
                ##specify the data
                data = iris,
                ##specify the error structure
                family = "gaussian")
plot(mod_pet_width)
##log10() single mod
mod_log <- glm(log(Petal.Width) ~ Petal.Length*Species,
                     ##specify the data
                     data = iris)
plot(mod_log)

##sqrt() mod
mod_sqrt <- glm(sqrt(Petal.Width) ~ Petal.Length*Species,
               ##specify the data
               data = iris)
plot(mod_sqrt) 
##Residuals vs Fitted 
##graph shows a better distribution than mod log and gaussian log
##graph does not shows a clear trend
##Normal Q-Q: Values are close to the dotted line
##Scale-Location: Better dsitribution than the other models, no heteroskedasticity
##red line is roughly horizontal better line than the other methods.
##Residuals vs Leverage: No points outside the 0.5 lines


##compare the models
AIC_mods <- AIC(mod_pet_width, mod_log, mod_sqrt)

##rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AIC),]
##summary
summary(mod_sqrt)

##whether there is an overall effect of Petal.Length on Petal.Width
##whether there is an overall effect of Species on Petal.Width
##whether there is a difference in the magnitude of the Petal.Length on Petal.Width between different species
##i.e. does the slope of the effect of Petal.Length on Petal.Width change between different species.