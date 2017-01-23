# Introduction to R workshop
# Spring 2017
# UVa StatLab
# Clay Ford



# Welcome to R! -----------------------------------------------------------

# This R script is intended to get you started doing data analysis with R.



# Set your working directory ----------------------------------------------

# The first thing we often do in an R script is set our working directory. You 
# usually set your working directory to where your data files are located. In
# this script, we want to set our working directory to where you downloaded the
# workshop files.

# To set working directory via point-and-click:
# (1) Session...Set Working Directory...Choose Directory
# Or Ctrl + Shift + H

# (2) Use the Files tab. Naviagte to folder and select "Set As Working
# Directory" under More


# To set working directory with R code:
# use setwd() function; path must be in quotes

# In RStudio, you can use the TAB key in the quotes to autocomplete the path.
# Try it! 

# Start with "/" to start from your root directory. 
# Start with "~/" to start from your home directory.


# Loading data ------------------------------------------------------------


# You can import just about any kind of data into R: Excel, Stata, SPSS, SAS, 
# CSV, JSON, fixed-width, TXT, DAT, shape files, and on and on. You can even 
# connect to databases. The best way to figure out how: Google "how to import 
# <type> files into R." This will usually involve installing and loading a
# special R package, which we'll talk about later.

# Today we'll import a CSV file.

# Data: Forbes 2000 list from 2004 
# File name: Forbes2000.csv
# source: Handbook of Statistical Analysis Using R (Everitt & Hothorn)
# amounts are in billion US dollars
    
# Import a CSV file and create a "data.frame"; here are three ways to do it:

# (1) read file from working directory on computer. If we've already set our 
# working directory to the location where the CSV file is located, we just need
# to put the name of the file in quotes.
forbes <- read.csv("Forbes2000.csv")

# (2) read from a web site. If the file is on the web, simply copy-and-paste the
# URL and enclose in quotes:
forbes <- read.csv("http://people.virginia.edu/~jcf2d/data/Forbes2000.csv")

# (3) use the RStudio "Import Dataset" button in the Environment window. Click
# on "Import Dataset", select "From Local File...", and navigate to file.

# REQUEST: Please don't do (3) during the workshop. It imports csv files 
# slightly differently than the read.csv function. I explain in the next
# section.


# Inspecting Data ---------------------------------------------------------

# Click on "forbes" in the environment window to browse the data. You can only
# browse the data. You cannot edit.

# view structure of data
str(forbes)

# Can also click the blue arrow next to the name in the Environment window. To
# see the data click "forbes".

# Notice the non-numeric data is "Factor". This basically means a categorical 
# variable. "Factor w/ 57 levels" for country means there are 57 unique 
# countries in this data. But notice the numbers. Factors are actually stored as
# integers, and the integers have associated "labels".

# If you plan to do any statistical modeling (eg, regression or ANOVA) with 
# non-numeric (text/categorical) data, you'll want it stored as a Factor.

# If we don't want non-numeric data stored as factors upon import, we
# can set stringsAsFactors = FALSE, like this:

forbes2 <- read.csv("Forbes2000.csv", stringsAsFactors = FALSE)
str(forbes2)

# Notice the non-numeric columns are simply character now. This is what the 
# Import Dataset button does that we discussed in the previous section.

# Let's call the summary function on both data frames. When called on data
# frames, the summary function returns a summary for each column.
summary(forbes) 
summary(forbes2) 

# In the second summary call, notice character data is not summarized like
# Factors are in the first summary call.

# When do you use factor and when do you use character?

# If you plan to run ANOVA or linear models that involves your text data, you
# need to store your text data as Factors.

# If you plan to manipulate or use patterns of character data, for data cleaning
# or text mining, you want to store your text data as character.



# More inspection ---------------------------------------------------------

# Some other functions for inspecting data frames. Try them out. What do they
# do?

head(forbes)
head(forbes, n = 3)
tail(forbes)
tail(forbes, n = 1)

names(forbes)
nrow(forbes)
ncol(forbes)
dim(forbes)

# To access or view just the column of a data frame, use a dollar sign after the
# name of the data frame
forbes$sales

# Try typing forbes$ below and see what RStudio does for you.


# Subsetting data ---------------------------------------------------------

# We often want to see parts of our data that meet a certain condition. Which 
# companies had sales over 100 billion? Which companies are from Canada? Which
# Banking companies are from the United States?

# We can subset our data using the subset() function. 
# The basic syntax is subset(data, condition). 

# Show companies with sales greater than 100 billion
subset(forbes, sales > 100)

# wrapping in View() results in a nicer display
View(subset(forbes, sales > 100))

# Show all Canadian countries: (notice we use two equal signs)
subset(forbes, country == "Canada")

# Show Banking companies in the United States: (use & for AND, use | for OR)
subset(forbes, country == "United States" & category == "Banking")

# Show multiple equalities using %in% operator:
subset(forbes, country %in% c("Mexico","Ireland","Denmark"))
subset(forbes, country %in% c("Mexico","Ireland","Denmark") & category == "Banking")


# We can also select certain columns. 
# subset(data, condition, columns)
# For example:
subset(forbes, sales > 100, c(name, sales))

# columns next to one another can be spanned with a ":"
subset(forbes, sales > 100 & country != "United States", sales:marketvalue)

# We can always save our subsetted data as a new object:
USbanks <- subset(forbes, country == "United States" & category == "Banking")

# YOUR TURN! Select all non-United States companies in the Utilities category.


# Some basic data manipulation --------------------------------------------

# Four common data manipulation tasks:


# (1) Deriving new columns (or variables) based on calculations

# Notice below we just add whatever column name we want to create after the
# dollar sign and perform the calculation.

# Subtract profits from sales to create a new column called totalcosts:
forbes$totalcosts <- forbes$sales - forbes$profits

# Add a new column for log-transformed sales
forbes$logsales <- log(forbes$sales)


# (2) creating an indicator variable

# An indicator variable takes two values, such as 0 and 1. "Is company
# profitable? 1 if yes, 0 otherwise." We can use the ifelse() function for this.

# create an indicator for profits. If greater than 0, set to 1, otherwise 0. 
# syntax: ifelse(condition, action if TRUE, action if FALSE)
forbes$profitable <- ifelse(forbes$profits > 0, 1, 0)

# create an indicator for US or Foreign company using ifelse function
# syntax: ifelse(condition, action if TRUE, action if FALSE)
forbes$US <- ifelse(forbes$country=="United States", "US", "Foreign")


# (3) changing a variable from Factor to character

# You have a column that is Factor and you want to be character

# Convert name column to character using as.character
forbes$name <- as.character(forbes$name)

# Likewise, as.factor() will convert a character column to Factor.


# (4) dropping columns (variables)

forbes$totalcosts <- NULL
forbes$logsales <- NULL
forbes$profitable <- NULL



# YOUR TURN! Add a column called salesM that is for sales in millions instead of
# billions. (ie, multiply by 1000)



# Basic summary stats -----------------------------------------------------

# The summary function is nice for quickly generating summaries of all columns,
# but we often want to generate specific summaries.

# to calculate frequencies of a factor or character variable use the table
# function:
table(forbes$category) # factor
table(forbes2$category) # character


# summarize numeric columns
mean(forbes$sales)
mean(forbes$profits) # NA?

# By default R returns NA (not available) for certain calculations if there is
# any missing data. Use na.rm=TRUE to override:
mean(forbes$profits, na.rm = TRUE)

# How many missing?
summary(forbes$profits)

# Which companies have missing?
subset(forbes, is.na(profits))

# A few other summary functions
median(forbes$sales)
sd(forbes$sales) # standard deviation
range(forbes$sales) # returns min and max values; see also min() and max()

# Counting number of conditions satisfied. 

# For example, how many companies had sales over 5 billion?

# The following generates a vector of TRUE/FALSE values:
forbes$sales > 5

# In R, TRUE = 1 and FALSE = 0, so we can do math with TRUE/FALSE values. How
# many TRUES?
sum(forbes$sales > 5)

# Percent of Forbes 2000 with sales over 5 billion:
# Taking the mean of 0,1 data returns percent of 1's
mean(forbes$sales > 5)

# How many companies had negative profits?
sum(forbes$profits < 0)

# We have NA in the profits column, so we need to tell R to ignore them:
sum(forbes$profits < 0, na.rm = TRUE)
mean(forbes$profits < 0, na.rm = TRUE)


# YOUR TURN! 
# What proportion of the Forbes 2000 list is from the United States?


# Aggregating and Summarizing Data ----------------------------------------

# Descriptive Statistics such as contingency tables and summary stats

# for contingency tables (or cross tabs) use the table function
# synatx: table(row variable, column variable)

# cross tab of categories vs. US/Foreign countries
table(forbes$category, forbes$US)

# calculate percents with the prop.table() function.

# First we save the table object as CatTable
CatTable <- table(forbes$category, forbes$US)
CatTable

prop.table(CatTable, margin = 1) # rows proportions sum to 1
prop.table(CatTable, margin = 2) # columns proportions sum to 1

# For basic numeric summary stats we can use the aggregate() function.
# syntax: aggregate(numeric ~ group, data, statistic)
# NOTE: aggregate ignores missing data by default

# median profits by category
aggregate(profits ~ category, data=forbes, median)

# mean sales by country
aggregate(sales ~ country, forbes, mean)

# mean sales by US/Foreign and category
aggregate(sales ~ US + category, forbes, mean)


# aggregate also provides a subset argument that allows us to subset data before
# aggregation.

# total profits by country for banking companies
aggregate(profits ~ country, forbes, sum, subset = category == "Banking")


# YOUR TURN!
# Find total assets by category for US companies


# Simple Graphics ---------------------------------------------------------

# scatter plots
plot(x = forbes$assets, y = forbes$marketvalue)

# same with formula interface: y ~ x
plot(marketvalue ~ assets, data=forbes)

# plot log transformed data
plot(log(marketvalue) ~ log(assets), data=forbes)

# Customize the labels
plot(log(marketvalue) ~ log(assets), data=forbes, 
     main = "Market Value vs. Assets",
     ylab = "Log Market Value",
     xlab = "Log Assets")

# use the Zoom button to see bigger image
# Use the Export button to save image as JPG, TIF, PDF, etc.

# histograms
# Histogram of market value
hist(forbes$marketvalue) # skew
hist(log(forbes$marketvalue)) # more symmetric
hist(log(forbes$marketvalue), prob=TRUE) # show probability densities
hist(log(forbes$marketvalue), prob=TRUE, breaks=20) # more bins


# boxplots
# boxplot(numeric ~ group)
boxplot(log(marketvalue) ~ salesCat, data=forbes, 
        main="Log Market Value by Sales Category")

# See Past and Upcoming Workshops for more on R Graphics:
# http://data.library.virginia.edu/training/
# http://data.library.virginia.edu/workshops/past-workshops/



# Packages ----------------------------------------------------------------

# Packages are collections of functions and/or data created by other R users. 
# You will certainly want to install some packages! 

# What packages are available?:
# https://cran.r-project.org/web/packages/

# To see what packages you have installed, click the Packages tab in RStudio.

# To install a package in RStudio:

# 1. click the Install button on the Packages tab
# 2. type in the name of the package
# 3. click install.

# or use the install.packages function

# Packages only need to be installed once, though they occasionally need to be 
# updated. Click the Update button on the Packages tab to see which packages 
# have available updates.

# Note: Packages often have dependencies. This means installing one package
# will also install other packages it depends on. 

# The ggplot2 package 

# a package that allows you to create plots using the Grammar of Graphics.

# Only install once. It will likely install several other packages it depends on.
install.packages("ggplot2")

# Load the package. Need to do once per R session if you want to use it.
library(ggplot2)


# ggplot2 examples - scatterplots

ggplot(forbes, aes(x = log(assets), y = log(marketvalue))) + 
  geom_point()

ggplot(forbes, aes(x = log(assets), y = log(marketvalue), color = US)) + 
  geom_point()

ggplot(forbes, aes(x = log(assets), y = log(marketvalue), color = US)) + 
  geom_point() +
  geom_smooth()

ggplot(forbes, aes(x = log(assets), y = log(marketvalue), color = US)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se=FALSE)

# See Help...Cheatsheets...Data Visualization with ggplot2 for a cheatsheet of
# ggplot2 commands.

# A few other packages to know about:
# - haven (Import 'SPSS', 'Stata' and 'SAS' Files)
# - readxl (Read Excel files)
# - reshape2 (reshape data)
# - tidyr (another package to reshape data)
# - stringr (for working with character data)
# - lubridate (for working with time and dates)
# - data.table (Fast aggregation of large data)
# - dplyr (a grammar of data manipulation)



# Basic statistics examples -----------------------------------------------

# basic linear regression

# let's look at marketvalue vs profits
ggplot(forbes, aes(x = profits, y = marketvalue)) + 
  geom_point() +
  geom_smooth()

# look at just positive profits
ggplot(subset(forbes, profits > 0), aes(x = profits, y = marketvalue)) + 
  geom_point(alpha = 1/5) +
  geom_smooth() 

# Can we summarize the relationship between profits and market value with a
# straight line?

# We can fit a linear model using the lm function (ie, regression)
# lm(response ~ independent variables)
# regress marketvalue on profits, but only where profits > 0, and save to mod
mod <- lm(marketvalue ~ profits, data=forbes, subset = profits > 0) 
# summary of the model
summary(mod) 

# Naive interpretation: Every additional 1 billion in profits is associated with
# an increase of about 17 billion in additional marketvalue (among those Forbes
# 2000 companies that turned a profit)

# This is just a simple demonstration. We should check model assumptions, check
# for influential observations, perhaps transform the data, and maybe consult
# with an economist.

# To learn more, see my past workshop Linear Modeling with R 
# http://data.library.virginia.edu/statlab/past-workshops/



# chi-square test of independence

# 1991 General Social Survey
# An Introduction to Categorical Data Analysis (Agresti, 1996), p. 31
# Table 2.5

# manually enter data from Table 2.5 into a matrix
# The matrix function fills by column
table_2.5 <- matrix(data = c(279,165,73,47,225,191), ncol=3)
table_2.5
colnames(table_2.5) <- c("Dem","Ind","Rep")
rownames(table_2.5) <- c("Females","Males")
table_2.5

# Null Hypothesis: no association between gender and political affiliation
chisq.test(table_2.5)
# save test into an object called "results"
results <- chisq.test(table_2.5)
str(results) # A list object
results$statistic
results$p.value
results$stdres # standardized residuals

# To learn more, see my past workshop
# Introductory Categorical Data Analysis in R
# http://data.library.virginia.edu/statlab/past-workshops/



# Two-sample t-test 

# Are means sales different between US and non-US companies in the Forbes 2000
# list?
aggregate(sales ~ US, data = forbes, mean)

# An assumption is the populations from which the samples have been drawn should
# be normal. They don't look normal:
ggplot(forbes, aes(x = sales, fill = US)) + geom_histogram() 

# We may want to work with the log transformed data
# stacked histograms
ggplot(forbes, aes(x = log(sales), fill = US)) + geom_histogram() 
# overlayed histograms
ggplot(forbes, aes(x = log(sales), fill = US)) + 
  geom_histogram(position = "identity", alpha = 0.7) 

# Perform the t-test on the log transformed data
t.test(log(sales) ~ US, data = forbes)
ggplot(forbes, aes(x = US, y = log(sales))) + 
  geom_boxplot() 


# Analysis of Variance (ANOVA)

# Let's use a data set that comes with R: iris.

# Are the means between Sepal.Length different between the three species of
# iris?
boxplot(Sepal.Length ~ Species, data = iris)
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Length ~ Species, data = iris, sd)

# use the aov() function to run an ANOVA
aov.out <- aov(Sepal.Length ~ Species, data = iris)
summary(aov.out)



# Bonus material! ---------------------------------------------------------

# Stuff I'm not sure we'll have time for but you might like to review in your
# free time.



# Saving R objects --------------------------------------------------------

# Let's say we plan to do extensive work with the Forbes 2000 data. It would be 
# nice to not have to re-run the read.csv() function (plus any other data 
# cleaning or analysis code) every time we want to work with the data in R. We
# can do that by saving R objects as .Rda (or .Rdata) files using the save()
# function. These are binary files that can be read into R using the load()
# function. The nice thing about .Rda files is that they can store multiple data
# objects.

# For example, here's how we can save the forbes, USbanks, CatTable and mod
# object in one .Rda file called "forbes_work.Rda":
save(forbes, USbanks, CatTable, mod, file = "forbes_work.Rda")

# Now remove those objects from memory:
rm(forbes, USbanks, CatTable, mod)

# Now load those objects into memory by simply loading "forbes_work.Rda":
load("forbes_work.Rda")

# Indexing brackets -------------------------------------------------------

# R keeps data out of sight. If we want to see or use our data, we have to tell 
# it what parts we want to see or use. While this can seem counterintuitive at 
# first, it allows us focus on the data we're interested in and cut down on
# distractions.

# We can use indexing brackets to select portions of data frame:
# [row number(s),column number(s)/name(s)]

# show first 6 records of first 2 columns
# 1:6 = 1,2,3,4,5,6
forbes[1:6,1:2] 

# first six rows; nothing after the comma means "show all columns"
forbes[1:6,] 

# columns 2 and 3; nothing before the comma means "show all rows"
forbes[,2:3] 

# can also use column names; they need to be entered as a "vector"
forbes[1:6,c("name","category")]

# c("name","category") creates a "vector"; the c() function means "combine"

# We can access columns of a data frame using $ as follows
forbes$name

# First 10 cells of the name column
forbes$name[1:10]

# Notice we didn't need a comma because the column only has one dimension.


# Missing data ------------------------------------------------------------

# Missing data is represented with "NA"

# summary will identify NAs
summary(forbes) # see the summary for "profits

# function for identifying missing data: is.na()
# answers T/F to question: is the value NA?
is.na(forbes$profits)
sum(is.na(forbes$profits)) # number missing
which(is.na(forbes$profits)) # which row numbers missing
miss <- which(is.na(forbes$profits))
forbes[miss,]

# the complete.cases function is also useful for identifying missing data
complete.cases(forbes) # returns logical vector (TRUE = no missing data in row)
any(complete.cases(forbes)) # any records missing data?
all(complete.cases(forbes)) # all records missing data?
table(complete.cases(forbes)) # how many records have missing data
which(complete.cases(forbes)==FALSE) # which records (indices)
which(!complete.cases(forbes)) # another way using '!' (means 'not')
miss <- which(complete.cases(forbes)==FALSE) # save indices (row numbers)
forbes[miss,] # subset data using indices and see the companies

# na.omit() - drop any records with missing data
forbesComplete <- na.omit(forbes) 
# would probably want to investigate before blindly dropping records
nrow(forbes)
nrow(forbesComplete)

rm(forbesComplete)




# Recode a continuous variable into categories ----------------------------


# Here we recode sales into four categories: (0,5], (5,10], (10,100], (100,500]
# using the cut() function
forbes$salesCat <- cut(forbes$sales, breaks = c(0,5,10,100,500))
summary(forbes$salesCat)

# We can add labels:
forbes$salesCat <- cut(forbes$sales, breaks = c(0,5,10,100,500), 
                       labels = c("small","mid-small","mid-large","large"))
summary(forbes$salesCat)



# hypothesis test and confidence interval ---------------------------------


# Probability and Statistical Inference (Hogg & Tanis, 2006), p. 492
# problem 8.2-6
# A coach claims FVC (forced vital capacity) of players is greater than 3.4 liters;
# nine players randomly sampled
fvc <- c(3.4, 3.6, 3.8, 3.3, 3.4, 3.5, 3.7, 3.6, 3.7)
# Null: mean <= 3.4
# Alternative: mean > 3.4
t.test(fvc, mu = 3.4, alternative = "greater")
tout <- t.test(fvc, mu = 3.4, alternative = "greater")
str(tout) # a list object
tout$estimate
tout$p.value
rm(fvc, tout)


# simulation example ------------------------------------------------------

# generate 10,000 samples from a Normal distribution with mean=140 and sd=15
pop <- rnorm(10000, mean = 140, sd = 15)
hist(pop)
hist(pop, prob=TRUE, ylim=c(0,0.03))
x <- 80:200
lines(x, dnorm(x, mean=140, sd=15)) # add normal curve for N(140,15)

# take a 1000 samples of size 20 and calculate mean of each sample
m <- replicate(n = 1000, expr = mean(sample(pop,20,replace=TRUE)))

# compare means
mean(pop); mean(m)
# compare standard deviations
sd(pop); sd(m)
# theoretical SD of sampling distribution
15/sqrt(20) 

# compare histograms
par(mfrow=c(2,1)) # split plot area into 2 rows
# pop'n histogram
hist(pop, freq=FALSE, xlim=c(80,200), ylim=c(0,0.15))
lines(x, dnorm(x, mean=140, sd=15))
# sampling distribution histogram
hist(m, freq=FALSE, xlim=c(80,200), ylim=c(0,0.15))
lines(x, dnorm(x, mean=140, sd=(15/sqrt(20))))
par(mfrow=c(1,1))
rm(m,pop,x)


# function and programming example ----------------------------------------

# use the function function to write your own functions!

# Simple function: calculate the area of a circle:
# A = pi*r^2
# pi is a keyword in R that returns a good approximation for pi

# The arguments to the funtion() function are the arguments we want our function
# to have. Below we want an argument for radius, which we call r.

# submit the function code:
areaCirc <- function(r) pi * r^2

# Now the function is ready to use:
areaCirc(r = 5)
areaCirc(r = 2)

# calculate area circles with radii 2 - 9
areaCirc(r = 2:9)

# Another example: coefficient of variation (CV)
# The ratio of the standard deviation to the mean
# It shows the extent of variability in relation to the mean of the sample

cv <- function(x){
  ratio <- sd(x)/mean(x)
  cat("The coefficient of variation is", ratio)
  }

# use on the forbes data
cv(forbes$marketvalue)
cv(log(forbes$marketvalue))
