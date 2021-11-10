##Homework - Proff solution
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

## make a new column in long_spp which is the number of threats recorded for each population
##this is the length of the non-NA values in each row of the colunns primary_threat, secondary_threat, and tertiary_threat:
long_spp <- long_spp %>%
           #mean of the columns
            rowwise %>%
           #mutate adds a new column
            mutate(n.threats = length(which(!is.na(c(primary_threat, secondary_threat, tertiary_threat)))))

##load in the scales package for the psudo_log transformation
library(scales)       

##build the ggplot
ggplot(long_spp, aes(x=date, y=abundance,
                     col=n.threats)) +
  ##set the theme to minimal
  theme_minimal() + 
  ##position the legend at the top on the left
  theme(legend.position = "top",
        legend.justification = "left" ) +
          ## change the automatic label for the legend to the following
          ## note it is done twice (once for fill and once for col) as
          ## we change the colour of the lines and the fill of the smooth according to the number of threats
        labs(col="Number of threats" , fill = "Number of threats") +
  
  ## add in the lines of the population, using the group and interaction functions
  ## to specify how the data are grouped and get 1 line per population
  geom_line(aes(group = interaction(population,
                                    species,
                                    n.threats,
                                    population)),
            alpha=0.2,
            size=1) +
  
  
  ## add in a smoothed line across all of the population time series for each number of threats
  ## this visualises the mean change across the populations
  geom_smooth(aes(group = n.threats,
                  fill = n.threats), alpha=0.3, size=1) +
  ##change the colours to reflect the number of threats, using a gradient
  scale_color_gradient(low = "#745745",
                       high = "#d14124") +
  scale_fill_gradient(low = "#745745",
                      high = "#d14124") +
  
  ## transform the y axis using a psudo_log transformation
  ## (as we have some 0 counts and you can't log 0)
  scale_y_continuous(trans="pseudo_log") +
  ##change the x and y axis labels
  ylab("Population abundance") +
  xlab("Year")
  