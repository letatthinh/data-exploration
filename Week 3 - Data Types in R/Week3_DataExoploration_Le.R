# Your name here 
# 9/19/2024
# Data Exploration - Fall 2024
# Week 3 Practice - Data Types in R - Melissa Laurino & Dr. Clif Baldwin

library(tidyverse)

# Get your working directory. Your working directory is the path to this file. This may look different for PC users/Mac users.
getwd()

# Set your working directory:
setwd("G:/My Drive/Schools & Universities/Stockton university/Classes/DSSA-5101-001 - Data exploration/Repository/data-exploration/Week 3 - Data Types in R")

# Read in the data from the .csv file:
# Use TAB to auto complete
# "file name", - Case and space sensitive!
# header = TRUE - because our data has headers (Column names)
stranding <- read.csv("2000-2017_StrandingData.csv", header = TRUE)

# If you are continuing your R script from last week, you may not have to reload the data or set your working environment.

# Use glimpse() to view the data:
glimpse(stranding)

# Manually create a vector of professor last names:
professors <- c("Baldwin", "Dunn", "Laurino", "Olsen", "Sokol", "Trout")

# Print the new professors Vector
professors

# Remove the practice vector, professors.
rm(professors)

# Create a vector from the Species column:
species_vector <- stranding$Species

# If we print the vector, you'll see a vector of the Species column, similar to what we saw with the professor vector.
species_vector

# Convert the vector to a factor using factor()
species_factor <- factor(species_vector)

# Add the factor as a NEW column in the data frame, stranding:
stranding$SpeciesFactor <- species_factor

# Check the structure of the new Species_factor column
str(stranding$SpeciesFactor)

# Summary statistics for the Latitude column
latitude_as_number <- as.numeric(stranding$Latitude)
latitude_summary <- summary(latitude_as_number)

# Summary statistics for the Longitude column
longitude_as_number <- as.numeric(as.numeric(stranding$Longitude))
longitude_summary <- summary(longitude_as_number)

# Create a list() with the above values, plus the species factor we created before:
my_list <- list(
  species_vector,
  species_factor,
  str(stranding$SpeciesFactor),
  latitude_summary,
  longitude_summary
)

# Print the list
my_list

# Assign names to our new list of data
names(my_list) <- c(
  "Species vector", 
  "Species factor", 
  "Species factor's structure", 
  "Latitude summary", 
  "Longitude summary"
)

# Manually create a matrix using the values from the summary of the latitudes and longitudes:
matrix <- matrix(c(36.77, 39.02, 39.41, 39.50, 39.98, 53.86, -80.23, -74.85, -74.37,  -74.48, -74.07, -72.32), nrow = 2, byrow = TRUE)
matrix

# Name the rows of the matrix
rownames(matrix) <- c("Latitude", "Longitude")

# Name the columns of the matrix
colnames(matrix) <- as.character(1:6)

# What is the median of the Latitude in our matrix?
median(matrix["Latitude", ])

# What is the maximum value of the Longitude in our matrix?
max(matrix["Longitude", ])

