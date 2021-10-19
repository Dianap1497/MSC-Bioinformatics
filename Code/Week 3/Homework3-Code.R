library(vroom)
sort_pop1 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_1.csv")
sort_pop2 <- vroom("https://raw.githubusercontent.com/chrit88/Bioinformatics_data/master/Workshop%203/to_sort_pop_2.csv")

#visualizing information about the datas
dim(sort_pop1)
dim(sort_pop2)

#full_join 
sort_pop1 %>%
  full_join(sort_pop2, by="species")
#re-shaping wide to long format
##our data frame
long <- sort_pop1 %>%
  ##and then apply this function 
  pivot_longer(cols = -c(species:tertiary_threat),
               names_to = "population date",
               values_to = "abundance")
long

#View the whole dataframe
view(long)