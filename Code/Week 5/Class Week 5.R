##Statistics in R
library(tidyverse)
install.packages("multcomp")
library(multcomp)
##View data
data("iris")
iris

#Plot Sepal.width split by species, so we can visualise if there are differences
##between the different species
sepal_width <- ggplot(data=iris,
                      aes(x= Species, y=Sepal.Width)) +
                geom_jitter(aes(col=Species)) + 
                theme_bw()
sepal_width

##Histogram
ggplot(iris, aes(x=Sepal.Width, fill = Species)) +
  ## bin width determines how course the histogram is
  ## the alpha determines the transparency of the bars
  ## position allows you to determine what kind of histogram you plot (e.g. stacked vs overlapping). try changing to position="stack"
  geom_histogram(binwidth = .1, alpha = .5, position="identity")

##fit a glm()
mod_iris <- glm(Sepal.Width ~ Species,
                ##specify the data
                data = iris,
                ##specify the error structure
                family = "gaussian")
mod_iris

class(mod_iris)
plot(mod_iris)
summary(mod_iris)
 ## run the multiple comparisons, and look at the summary output
summary(glht(mod_iris, mcp(Species="Tukey")))

