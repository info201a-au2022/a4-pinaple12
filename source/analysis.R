library(tidyverse)
library(ggplot2)
options(scipen = 999)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
prison_pop_updated <- select(incarceration_df, ends_with("prison_pop")) * 100
#dataframe of prison population and total population by state in 2000, removed
#states without relevant data
data_2000 <- incarceration_df %>%
  filter(year == 2000) %>%
  group_by(state) %>%
  summarise(
    total_pop_15to64 = sum(total_pop_15to64),
    white_pop_15to64 = sum(white_pop_15to64, na.rm = TRUE),
    black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE),
    total_prison_pop = sum(total_prison_pop, na.rm = TRUE) * 10,
    black_prison_pop = sum(black_prison_pop, na.rm = TRUE) * 10,
    white_prison_pop = sum(white_prison_pop, na.rm = TRUE) * 10
  ) %>%
  filter(black_prison_pop != 0)
#black proportion in prison
black_prison_prop <- sum(data_2000$black_prison_pop) / sum(data_2000$total_prison_pop)
#white proportion in prison
white_prison_prop <- sum(data_2000$white_prison_pop) / sum(data_2000$total_prison_pop)
#total proportions in the general population
black_pop_per <- sum(data_2000$black_pop_15to64) / sum(data_2000$total_pop_15to64) * 100
white_pop_per <- sum(data_2000$white_pop_15to64) / sum(data_2000$total_pop_15to64) * 100
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

#This function creates a dataframe with total jail population by year. 
get_year_jail_pop <- function() {
  year_jail_df <- incarceration_df %>%
    group_by(year) %>%
    summarize(
      total_jail_pop = sum(total_jail_pop, na.rm = TRUE)
    )
return(year_jail_df)   
}

# This function plots jail population by year in a bar chart.
plot_jail_pop_for_us <- function()  {
  year_jail_df <- get_year_jail_pop()
  year_jail_plot <- ggplot(year_jail_df, aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity") + 
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", caption = "This barchart shows the growth in total jail population (national) over time (year).") + 
    xlab("Year") + 
    ylab("Total Jail Population")
  return(year_jail_plot)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

#takes a vector of 5 states, and returns dataframe with prison population count per year for each state
get_jail_pop_by_states <- function(states) {
  state_jail_df <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(year) %>%
    summarize(
      total_jail_population = sum(total_jail_pop, na.rm = TRUE),
      state1_data = sum(total_jail_pop[state == states[1]], na.rm = TRUE),
      state2_data = sum(total_jail_pop[state == states[2]], na.rm = TRUE),
      state3_data = sum(total_jail_pop[state == states[3]], na.rm = TRUE),
      state4_data = sum(total_jail_pop[state == states[4]], na.rm = TRUE),
      state5_data = sum(total_jail_pop[state == states[5]], na.rm = TRUE)
      )
  return(state_jail_df)
}
#vector of 5 states
states_plotted <- c("WA", "CA", "NY", "AL", "OR")

#creates a line graph of prison population per year in 5 states
plot_jail_pop_by_states <- function(states)  {
  state_jail_df <- get_jail_pop_by_states(states)
  state_jail_plot <- ggplot(state_jail_df, aes(x = year)) +
    geom_line(aes(y = state1_data, color = "WA")) +
    geom_line(aes(y = state2_data, color = "CA")) +
    geom_line(aes(y = state3_data, color = "NY")) +
    geom_line(aes(y = state4_data, color = "AL")) +
    geom_line(aes(y = state5_data, color = "OR")) +
    xlab("Year") +
    ylab("Total Jail Population (State)") +
    labs(title = "Jail Population by State", caption = "This chart displays growth in total jail population by specific states over time.")  +
    scale_colour_manual(name = "Key", values = c("WA" = "red", "CA" = "blue", "NY" = "orange", "AL" = "purple", "OR" = "green"))
  return(state_jail_plot)   
} 

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

#This function returns a data frame with information about population proportions
#and inmate population proportions.
get_demographic_ratios <- function() {
  demographic_df <- incarceration_df %>%
    group_by(year) %>%
    summarize(
      black_jail_prop = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
      white_jail_prop = sum(white_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
      black_population_prop = sum(black_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE),
      white_population_prop = sum(white_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE),
      black_inmate_population_diff = black_jail_prop - black_population_prop,
      white_inmate_population_diff = white_jail_prop - white_population_prop
    ) %>%
    filter(year > 1989)
  return(demographic_df)
}

#This function returns a plot that shows white and black inmate and population proportions 
#as separate lines on one plot
plot_demographic_ratios <- function() {
  demographic_df <- get_demographic_ratios()
  demographic_plot <- ggplot(demographic_df, aes(x = year)) + 
    geom_line(aes(y = black_jail_prop, color = "Black Inmates Proportion")) + 
    geom_line(aes(y = white_jail_prop, color = "White Inmates Proportion")) + 
    geom_line(aes(y = black_population_prop, color = "Black Population Proportion")) +
    geom_line(aes(y = white_population_prop, color = "White Population Proporton")) +
    xlab("Year") +
    ylab("Population Proportion") + 
    labs(title = "Population Proportion by Race") +
    scale_colour_manual(name = "Key", values = c("Black Inmates Proportion" = "Red", "White Inmates Proportion" = "Blue", "Black Population Proportion" = "Pink", "White Population Proporton" = "Cyan"))
}

#This function returns a plot that shows the difference between inmate population proportion
#and general population proportion for white and black people as two separate lines on one plot.
plot_difference_demographic <- function() {
  demographic_df <- get_demographic_ratios()
  difference_plot <- ggplot(demographic_df, aes(x = year)) + 
    geom_line(aes(y = black_inmate_population_diff, color = "African American")) + 
    geom_line(aes(y = white_inmate_population_diff, color = "White")) +
    scale_colour_manual(name = "Key", values = c("Black Proportional Difference" = "Black", "White Proportional Difference" = "Grey")) +
    xlab("Year") + 
    ylab("Proportional Difference") +
    labs(title = "Difference between Inmate Proportion and Population Proportion",
    caption = "This chart displays the difference between inmate proportion 
         and population proportion for white and black people.")
  return(difference_plot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
library(usmap)
library(maps)

#This function creates graphable dataframe that contains information on the difference between 
#racial proportional gap in 2018 compared to racial proportional gap in 2000 for every state. 
get_state_data <- function() {
  proportion_by_state <- incarceration_df %>%
    group_by(state) %>%
    summarize(
      black_jail_prop_2018 = sum(black_jail_pop[year == 2018], na.rm = TRUE) / sum(total_jail_pop[year == 2018], na.rm = TRUE),
      black_population_prop_2018 = sum(black_pop_15to64[year == 2018], na.rm = TRUE) / sum(total_pop_15to64[year ==2018], na.rm = TRUE),
      black_inmate_population_diff_2018 = black_jail_prop_2018 - black_population_prop_2018,
      black_jail_prop_2000 = sum(black_jail_pop[year == 2000], na.rm = TRUE) / sum(total_jail_pop[year == 2000], na.rm = TRUE),
      black_population_prop_2000 = sum(black_pop_15to64[year == 2000], na.rm = TRUE) / sum(total_pop_15to64[year ==2000], na.rm = TRUE),
      black_inmate_population_diff_2000 = black_jail_prop_2000 - black_population_prop_2000,
      diff_2018_2000 = black_inmate_population_diff_2018 - black_inmate_population_diff_2000
    )
  return(proportion_by_state)
}

#This function creates a heatmap based on increase or decrease amount in the proportional gap for imprisonment rate
#and population size based on race
get_heat_map <- function() {
  proportion_by_state <- get_state_data()
  usmap <- plot_usmap(data = proportion_by_state, values = "diff_2018_2000", color = "black") +
    scale_fill_continuous(
      low = "white", high = "red", name = "2000 to 2008 Proportional Difference"
    ) +
    theme(legend.position = "right") +
    labs (title = "Change in Difference between Black Inmate Proportion and Black Population Proportion
          from 2000 to 2018",
          caption = "This map shows the increase or decrease in difference between black inmate proportion
          and black population proportion")
  return(usmap)
}



