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
               values_drop_na = F, 
               values_to = "abundance")
long_spp
selec_threat <- c("Habitat loss", "Habitat destruction", "Habitat fragmentation")

## filter the data to include data from the year 2010 to the most recent year which is 2019:
pop_2010 <- long_spp %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date >= 2010)
pop_2010

##if we try to create a plot with useful information, we need to explore the data a little
##deeper since this plot does not says much
by_threat <- ggplot(data=pop_2010, aes(x=date, y=abundance))
by_threat + geom_point(aes(col=primary_threat)) + theme(legend.position = "none")

##Make a selection of species we want to visualize

select_species <- c("Anastrophyllum minutum", "Bryum rubens", "Bryum weigelii",
                    "Bartramia halleriana", "Brachythecium velutinum", "Bryum bornholmense","Bryum gemmiferum",
                    "Bryum klingraeffii", "Buxbaumia viridis", "Calymperes erosum",
                    "Cephalozia lunulifolia")

##use this to filter by for our plot. here using the %in% operature:
sel_species <- ggplot(data = pop_2010 %>% 
                             filter(species %in% select_species), 
                           aes(x =abundance , y =primary_threat )) 

##add a point geom specifying that the colours are dependant on the groups in `Species`
##the plot only shows the species that form part of the select_species vector and in the data frame
## of 2010-2019. The plot makes a relantioship of the primary threat of the species and the abundance of it
sel_species + geom_point(mapping = aes(col=species)) 

##Another year to visualizes if the threats were different
## filter the data to include data from the year 2000:
pop_2000 <- long_spp %>% 
  ##only return data where the date is equal to the maximum value in the column "date"
  filter(date >= 2000)
pop_2000

by_threat <- ggplot(data=pop_2000, aes(x=date, y=abundance))
by_threat + geom_point(aes(col=primary_threat)) + theme(legend.position = "none")

select_species2 <- c("Anastrophyllum minutum", "Bryum rubens", "Bryum weigelii",
                    "Bartramia halleriana", "Brachythecium velutinum", "Bryum bornholmense","Bryum gemmiferum",
                    "Bryum klingraeffii", "Buxbaumia viridis", "Calymperes erosum",
                    "Cephalozia lunulifolia")

##use this to filter by for our plot. here using the %in% operature:
sel_species2 <- ggplot(data = pop_2000 %>% 
                        filter(species %in% select_species), 
                      aes(x =abundance , y =primary_threat )) 

##add a line geom specifying that the colours are dependant on the groups in `Species`
##With this plot in a different years we  can see the relation of the primary threat and the impact 
##that has in the species
sel_species2 + geom_point(mapping = aes(col=species)) 

##boxplot
sel_species2 <- ggplot(data = pop_2000 %>% 
                       filter(species %in% select_species), 
                       mapping= aes(x =abundance, y =primary_threat )) 

sel_species2 + geom_boxplot() + labs(x="Abundance", y= "Primary Threat") + theme_bw() 

##Histogram

date_abundance <- ggplot(data = pop_2010 %>% 
      filter(species %in% select_species), 
       mapping = aes(x = abundance, fill = date))
date_abundance + geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "Abundance", y = "Threat",
       title = "Abundance by Date facet by Primary Threat") +
  facet_grid(. ~ primary_threat) +
  theme_bw()
##Facet_grip
##The data was facet by the primary threat 

three <- ggplot(data = pop_2010 %>% 
                filter(species %in% select_species), 
                mapping = aes(x = abundance, y =date)) 

three + geom_point() +  theme_bw() +
       facet_grid(primary_threat ~ species)

##it creates only a visual plot about the species and its abundance
four <- ggplot(data = pop_2010 %>% 
                 filter(species %in% select_species),
               mapping = aes(x = abundance, y = species)) 
four + geom_point(position = "jitter")


##it separates the species by population
ggplot(data = pop_2000,
       mapping = aes(x = species, y = abundance, fill = population)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "Species", y = "Abundance", fill = "Population") +
  theme_bw()

####it creates a plot with all the species from 2010-2019 and it shows you the abundance per year
ggplot(data = pop_2000,
       mapping = aes(x = species, y = abundance, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "Species", y = "Abundance", fill = "date") +
  theme_bw()

##it creates a plot with all the species from 2010-2019 and it shows you the abundance per year
ggplot(data = pop_2010,
       mapping = aes(x = species, y = abundance, fill = date)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "Species", y = "Abundance", fill = "date") +
  theme_bw()