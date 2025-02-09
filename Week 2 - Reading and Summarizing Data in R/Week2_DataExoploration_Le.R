# Thinh Le
# 9/12/2024
# Data Exploration - Fall 2024
# Week 2 Practice - Reading and Summarizing Data in R - Melissa Laurino & Dr. Clif Baldwin

# Load the libraries

# Get your working directory. Your working directory is the path to this file. This may look different for PC users/Mac users.
getwd()

# Set your working directory:
setwd("G:/My Drive/Schools & Universities/Stockton university/Classes/DSSA-5101-001 - Data exploration/Repository/data-exploration/Week 2 - Reading and Summarizing Data in R")

# Read in the data from the .csv file:
# Use TAB to auto complete
# "file name", - Case and space sensitive!
# header = TRUE - because our data has headers (Column names)
stranding <- read.csv("2000-2017_StrandingData.csv")

# Check the dimensions of the data
dim(stranding)

# Summarize your entire data
summary(stranding)

# View only the first 6 rows of data
head(stranding)

# List column names within your data
colnames(stranding)

# Summarize only a specific column by name using $ in your syntax
summary(stranding$ObservationDate)

# View the structure of your data 
str(stranding)

# Install the package first:
# Comment your code with a hashtag to avoid running a line a second time. 
# install.packages("tidyverse") 
library(tidyverse)

# Use glimpse() to view the data:
glimpse(stranding)

# Select a specific column of your data:
observation_date <- stranding %>% select(ObservationDate)

# Use filter to select for a specific species, truncatus, within the column Species
truncatus <- stranding %>% filter(Species == "truncatus")

# Install package readxl as recommended by R for Data Science
# https://r4ds.hadley.nz/spreadsheets
# install.packages("readxl")

#Load the package
library(readxl)

#Load the data
stranding_xl <- read_excel("2000-2017_StrandingData.xls")