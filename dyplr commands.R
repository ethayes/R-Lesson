
gapminder <- read.csv("data/gapminder_data.csv")
head(gapminder)

# Basic IF-ELSE syntax
#if (condition) { do something }
#if (condition) { do something } else { do something else }
#if (condition) { do something } else if ( other condition ) { do other thing } else { do something else }


if (nrow(gapminder)>100) {print("it's big!")}

#Note about dataframe sizes: nrow(), ncol(), dim()
#Note about dataframe labels: rownames(), colnames()

if ("birthrate" %in% colnames(gapminder)) {
  lm(birthRate ~ pop, data=gapminder)
  } else if ("avgHeight" %in% colnames(gapminder)) { 
  lm(avgHeight ~ pop, data=gapminder) 
    } else {
  lm(lifeExp ~ pop, data=gapminder)
}
  
#Use an if() statement to test whether gapminder has any records from 2012

if (2012 %in% gapminder$year) { 
  print("gapminder has data from 2012")
} else {
    print ("no data from 2012")
  }

#For loop (basic syntax)
#for ( var in collection ) { do something with variable }

for (i in 1:3) {
  print(gapminder[1:i,])
}
result <- c()
for (i in c(10, 25, 50)) {
  current_result <- mean(gapminder[1:i, "lifeExp"])
  result <- c(result, current_result)
}
new_result <- list()
for (i in 1:3) {
  for (j in c('a', 'b', 'c', 'd')) {
    current_name <- paste(i,j)
    new_result[current_name] <- rnorm(1)
  }
}

#Loop through gapminder data by continent, print whether mean LifeExp < or > 50 
#mean()
#unique()

for (current_continent in unique(gapminder$continent)) {
  data_subset <- gapminder[gapminder$continent == current_continent,"lifeExp"]
  mean_life_exp <- mean(data_subset)
    if (mean_life_exp < 50) {
    adjective <- "less than"
      } else if (mean_life_exp > 50) {
        adjective <- "more than"
      } else {
        adjective <- "equal to"
      }
  print(paste("Mean life expectancy in", current_continent, "is", adjective, "50" ))
}

#Writing our own functions

fahr_to_kelvin <- function(fahr_temperature) {
  #main content of the function goes here
  kelvin_temperature <- (fahr_temperature - 32) * 5/9 + 273.15
  return (kelvin_temperature)
}

kelvin_to_celsius <- function(kelvin_temperature) {
  celsius_temperature <- (kelvin_temperature - 273.15)
  return (celsius_temperature)
}

kelvin_to_celsius(fahr_to_kelvin(32))

fahr_to_kelvin(1:10)

fahr_to_kelvin(c(20, 30, 40))

calc_gdp <- function(dataframe) {
  gdp <- dataframe$pop * dataframe$gdpPercap
  return(gdp)
}
all_the_gdps <- calc_gdp(gapminder)
cbind(gapminder, all_the_gdps)

##############
# DPLYR

library(dplyr)

install.packages("dplyr")
Y
#pipe %>% inserts its left side argument as the first argument of the function on its right
gapminder %>% calc_gdp()

gapminder %>% 
  mutate(GDP = gdpPercap * pop) ->
  gapminder_plus

gapminder_plus %>%
  group_by(continent) %>%
  summarize( meanGDP = mean(GDP))

gapminder_plus %>%
  select( country, lifeExp )

gapminder_plus %>%
  filter(year == 2002, continent == "Europe", lifeExp > 78) %>%
  head()

gapminder_plus %>%
  group_by(continent) %>%
  count()

gapminder_plus %>%
  group_by(continent) %>%
  sample_n(20) %>%
  count()

#add a column to gapminder_plus where it contains the average GDP for 
#that continent in that year 

gapminder_plus %>%
  group_by(continent, year) %>%
  mutate(meanGDPofContinentThisYear = mean(GDP)) ->
  gapminder_plus
 
# DPLYR pipelines with ggplot2

gapminder_plus %>% 
  filter(gdpPercap < 0.5 * mean(gdpPercap)) %>%
  ggplot(mapping=aes(x=year, y=lifeExp, by=country, color=continent)) +
  geom_line()










