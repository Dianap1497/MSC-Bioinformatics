library(vroom)
library(tidyverse)
##read in the data
wide_spp.1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
wide_spp.2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv") 

## code to reshape data
## first join the data using full join - this will keep all of the columns
long_spp <- full_join(wide_spp.1, wide_spp.2) %>%
  ## pivot the joined data frame, using species, primary_threat, secondary_threat, tertiary_threat as ID columns
  ## and using names-pattern to pull out the population number 
  ## and make a new column (called population) to store them in. 
  ##Drop the NAs. 
  pivot_longer(cols = -c(species, 
                         primary_threat, 
                         secondary_threat, 
                         tertiary_threat), 
               names_to = c("population", "date"),
               names_pattern = "(.*)_(.*)",
               values_drop_na = T, 
               values_to = "abundance")

# convert date info in format 'mm/dd/yyyy'
long_spp$date <- as.Date(long_spp$date, format = "%Y-%m-%d")
print(long_spp)

#Filter Trichocolea tomentella
single_spp <- long_spp %>%
                filter(species == "Trichocolea tomentella")
single_spp

##visualise the data by creating a plot
p1 <- ggplot(single_spp, aes(x=date, y=abundance)) +
      geom_point() +
      geom_line() +
      ylab("Abundance") + 
      xlab("Year") 
##add the loess smoothing:
p1 + geom_smooth(method="loess")

## calculate a new column (`standardised_time`) which is the difference between the
## starting date of the time series and each other date in weeks (see ?difftime)
## we will set this to a numeric vector

single_spp <- single_spp %>%
  mutate(standardised_time = as.numeric(difftime(as.Date(date),
                                                 min(as.Date(date)),
                                                 units = "weeks")))

print(single_spp[,c("abundance", "date", "standardised_time")], 30)

##glm
##fit a glm()
mod1 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "gaussian")

##return the predicted ( response) values from the model
##add them to the single species tibble:
single_spp$pred_gaussian <- predict(mod1, type= "response")

##add the residuals
single_spp$resid_gaussian <- resid(mod1)

##plot the abundances through time
p2 <- ggplot(single_spp, aes(x=standardised_time, y= abundance)) +
      geom_point() + 
      geom_line() +
      theme_bw() +
      ylab("Abundance") + 
      xlab("Standardised Time")

##add a line of the predicted values
p2 <- p2 + geom_line(aes(x= standardised_time, y= pred_gaussian),
                     col = "dodgerblue",
                     size = 1)

## we can also add in vertical blue lines which show the redsidual error of the model
## (how far the observed points are from the predicted values).
## in geom_segement we specify where we want the start and end of the segments (lines)
## to be. Without any prompting ggplot assumes that we want the start of the lines to
## be taken from the x and y values we are plotting using the ggplot() function
## i.e. standardised_time and abundance, so we just need to specify the end points of
## the lines:
p2 <- p2 + geom_segment(aes(xend = standardised_time,
                            yend = pred_gaussian),
                        col="lightblue")
##add title
p2 <- p2 + ggtitle("Fitted model (gaussian with identity link)")

p2

##Check residuals from the fitted values
##plot a histogram of theresiduals from the model using geom_histogram()
p3 <- ggplot(single_spp, aes(x=resid_gaussian)) +
      geom_histogram(fill= "goldenrod") +
      theme_minimal() +
      ggtitle("Histogram of residuals (gaussian with identity link")
p3

##plot predicted vs residuals
pred_res <-  ggplot(single_spp, aes(x=pred_gaussian, y= resid_gaussian)) +
  geom_point() + 
  theme_minimal() +
  ylab("Predicted Values") + 
  xlab("Residuals")+
  ggtitle("Predicted vs Residual (Gaussian with identity link") +
##using geom_smooth without specifying the method (see later) means geom_smooth()
##will try a smoothing function with a formula y~x and will try to use a loess smoothing
##or a GAM (generalised additive model) smoothing depending on the number of data points
  geom_smooth(fill="lightblue", col="dodgerblue")
pred_res

##plot the qq plot for the residuals from the model assuming a normal distribution,
## and add the straight line the points should fall along:
qqnorm(single_spp$resid_gaussian); qqline(single_spp$resid_gaussian)

##Explore other models
##fit a glm with a poisson distribution
mod2 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = "poisson")
mod2

##fit a glm with a gaussian distribution with a log link
mod3 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link= "log"))
mod3

##guassian model with an inverse link
mod4 <- glm(abundance ~ standardised_time,
            data = single_spp,
            family = gaussian(link = "inverse"))
mod4

##compare the models
AIC_mods <- AIC(mod1, mod2, mod3,mod4)

##rank them by AIC using the order() function
AIC_mods[order(AIC_mods$AIC),]

##mod3

##return the predicted ( response) values from the model 3
##add them to the single species tibble:
single_spp$pred_log <- predict(mod3, type= "response")

##add the residuals
single_spp$resid_log <- resid(mod3)

##plot predicted vs residuals mod3
pred_res <-  ggplot(single_spp, aes(x=pred_log, y= resid_log)) +
  geom_point() + 
  theme_minimal() +
  ylab("Predicted Values") + 
  xlab("Residuals")+
  ggtitle("Predicted vs Residual (Gaussian with identity log link") +
  ##using geom_smooth without specifying the method (see later) means geom_smooth()
  ##will try a smoothing function with a formula y~x and will try to use a loess smoothing
  ##or a GAM (generalised additive model) smoothing depending on the number of data points
  geom_smooth(fill="lightblue", col="dodgerblue")
pred_res

##plot the abundances through time
stan_timemod3 <- ggplot(single_spp, aes(x=standardised_time, y= abundance)) +
  geom_point() + 
  geom_line() +
  theme_bw() +
  ylab("Abundance") + 
  xlab("Standardised Time")

##add a line of the predicted values
stan_timemod3 <- stan_timemod3 + geom_line(aes(x= standardised_time, y= pred_log),
                     col = "dodgerblue",
                     size = 1)

## we can also add in vertical blue lines which show the redsidual error of the model
## (how far the observed points are from the predicted values).
## in geom_segement we specify where we want the start and end of the segments (lines)
## to be. Without any prompting ggplot assumes that we want the start of the lines to
## be taken from the x and y values we are plotting using the ggplot() function
## i.e. standardised_time and abundance, so we just need to specify the end points of
## the lines:
stan_timemod3 <- stan_timemod3 + geom_segment(aes(xend = standardised_time,
                            yend = pred_log),
                        col="lightblue")
##add title
stan_timemod3 <- stan_timemod3 + ggtitle("Fitted model (gaussian with identity log link)")

stan_timemod3

##plot the diagnostics graphics for model 3
plot(mod3)

##summarise the model outputs
summary(mod3)

##plot model 3 
##plot data again
p6 <- ggplot(single_spp, aes(x=standardised_time, y=abundance)) +
      geom_point() + 
      geom_line() +
      theme_bw() + 
      ylab("Abundance") +
      xlab("Standardised Time")
## use the geom_smooth() function to add the regression to the plot.
## unlike earlier here we are specifying the model type (glm), the formula,
## and the error structure and link
p6 <- p6 + geom_smooth(data=single_spp,
                       method="glm",
                       method.args = list(family = gaussian(link = "log")),
                       formula = y ~ x,
                       col= "dodgerblue",
                       fill = "lightblue")
p6