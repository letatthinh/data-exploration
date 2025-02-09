# Your name here 
# 9/19/2024
# Data Exploration - Fall 2024
# Week 3 Practice - Data Types in R - Melissa Laurino & Dr. Clif Baldwin

# Get your working directory. Your working directory is the path to this file. This may look different for PC users/Mac users.


# Set your working directory:


# Read in the data from the .csv file:
# Use TAB to auto complete
# "file name", - Case and space sensitive!
# header = TRUE - because our data has headers (Column names)
stranding <- read.csv("2000-2017_StrandingData.csv", header = TRUE)

# If you are continuing your R script from last week, you may not have to reload the data or set your working environment.

# Use glimpse() to view the data:

?c()

# Manually create a vector of professor last names:
professors <- c("Baldwin", "Dunn", "Laurino", "Olsen", "Sokol", "Trout")


# Print the new professors Vector


# Remove the practice vector, professors.


# Create a vector from the Species column:


# If we print the vector, you'll see a vector of the Species column, similar to what we saw with the professor vector.


# Convert the vector to a factor using factor()

# Add the factor as a NEW column in the data frame, stranding:


# Check the structure of the new Species_factor column

# Summary statistics for the Latitude column

# Summary statistics for the Longitude column


# Create a list() with the above values, plus the species factor we created before:

# Print the list

# Assign names to our new list of data

# Manually create a matrix using the values from the summary of the latitudes and longitudes:
matrix <- matrix(c(36.77, 39.02, 39.41, 39.50, 39.98, 53.86, -80.23, -74.85, -74.37,  -74.48, -74.07, -72.32), nrow = 2, byrow = TRUE)

matrix

# Name the rows of the matrix

# Name the columns of the matrix

# What is the median of the Latitude in our matrix?

# What is the maximum value of the Longitude in our matrix?

