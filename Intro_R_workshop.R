# Introduction to R workshop
# Fall 2016
# UVa StatLab
# Clay Ford



# Welcome to R! -----------------------------------------------------------

# This R script is intended to get you started doing data analysis with R.

# R comes with built-in data sets that we can use for learning purposes. The 
# "hello world" data set of R is the "iris" data set. Put your cursor on the 
# line below that says data(iris) and hit Ctrl + Enter (Windows/Linux) or Command +
# Enter (Mac). This loads the built-in iris data set.

data(iris)

# The str() function will tell us about the structure of the data
str(iris)

# Notice the output in the console says iris is a "data.frame". Most data you'll
# work with R will be a data.frame.

# Click on "iris" in the Environment window to browse the data. 

# Calling summary() on a data frame summarizes the columns of the data frame:
summary(iris)

# Calling pairs() on a data frame produces pair-wise scatter plots:
pairs(iris)

# Obviously this only works well for data.frames with a few columns.

# other handy functions for investigating data.frames
head(iris)
head(iris, n=3)

tail(iris)
tail(iris, n=1)

names(iris)
nrow(iris)
ncol(iris)
dim(iris)


# Loading our own data ----------------------------------------------------

# The iris data is nice for playing with R, but eventually we need to load our 
# own data. Also it's worth noting that R really isn't for data entry. You can 
# do it, but it's easier to use something like Excel for data collection and 
# data entry. R is for manipulating and analyzing data you have already
# collected.

# You can import just about any kind of data into R: Excel, Stata, SPSS, SAS, 
# CSV, JSON, fixed-width, TXT, DAT, shape files, and on and on. You can even 
# connect to databases. The best way to figure out how: Google "how to import 
# <type> files into R." This will usually involve installing and loading a
# special R package, which we'll talk about later.

# Today we'll import a CSV file.

# Before we do that let's set our working directory. Your working directory is 
# where R looks for files if you don't provide a path to the file. You typically
# set your working directory to where your research or project files are
# located. The working directory is displayed in the header of the console.

# Good to know:
# "~/" is your home directory
# "/" is your root directory

# To set working directory via point-and-click:
# Session...Set Working Directory...Choose Directory

# To set working directory with R code:
# use setwd() function; path must be in quotes

# TRY NOW! 
# Type "setwd" and open parens. Then in the parens type a double-quote. Then hit
# Tab. A list of directories should pop up. Keep selecting directories and
# hitting TAB until you arrive at the directory where you downloaded the 
# workshop files.
setwd("~/_workshops/IntroR/")


# Loading CSV data --------------------------------------------------------

# Forbes 2000 list from 2004 
# source: Handbook of Statistical Analysis Using R (Everitt & Hothorn)
# amounts in billion US dollars
    
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



# Inspecting Data ---------------------------------------------------------

# view structure of data
str(forbes)

# Can also click the blue arrow next to the name in the Environment window. To
# see the data click "forbes".

# Notice the non-numeric data is "Factor". This basically means a categorical 
# variable. "Factor w/ 57 levels" for country means there are 57 unique 
# countries in this data. But notice the numbers. Factors are actually stored as
# integers, and the integers have associated "levels".

# If you plan to do any statistical modeling with categorical data, you'll want
# it stored as a Factor.

# If we don't want non-numeric data stored as factors upon import, we
# can set stringsAsFactors = FALSE, like this:
forbes2 <- read.csv("Forbes2000.csv", stringsAsFactors = FALSE)
str(forbes2)
# Notice the non-numeric columns are simply character now.

# Let's call the summary function on both data frames
summary(forbes) 
summary(forbes2) 

# In the second summary call, notice character data is not summarized like
# Factors are in the first summary call.

# "Which is better: character or Factor?" That's the wrong question. We should
# ask "what's the difference?"

# If you plan to run ANOVA or linear models, you need to store your text data as
# Factors.

# If you plan to manipulate or use patterns of character data, for say text
# mining, you want to store your text data as character.

# YOUR TURN!
# Try the pairs function on the forbes data. What do you think?


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

# columns 2 and 3
forbes[,2:3] 

# rows 10 and 11, columns 2, 4, and 6
forbes[c(10,11),c(2,4,6)]

# Note: c(10,11) and c(2,4,6) are "vectors"; The c() function means "combine"

# can also use column names
forbes[1:6,c("name","category")]

# first 5 records and all but first column (rank)
forbes[1:5,-1]

# first record and all but last two columns (assets and marketvalue)
forbes[1,-c(7,8)]

# We can access columns of a data frame using $ as follows
forbes$name

# First 10 cells of the name column
forbes$name[1:10]

# Notice we didn't need a comma because a vector only has one dimension.

# YOUR TURN!
# use the pairs() function on columns 5 - 8 of the forbes data:



# Subsetting data ---------------------------------------------------------

# We often want to see parts of our data that meet a certain condition. Which 
# companies had sales over 100 billion? Which companies are from Canada? Which
# Banking companies are from the United States?

# We can define conditions in our indexing brackets such that only rows where
# the condition is TRUE are shown.

# TASK: Show companies with sales greater than 100 billion.

# This comparison returns TRUE/FALSE
forbes$sales > 100

# Using the comparison inside brackets before the comma will return only rows
# that are TRUE
forbes[forbes$sales > 100,]

# We can select certain columns if we like:
forbes[forbes$sales > 100, c("name","sales")]

# More examples:


# Show all Canadian countries:

# Show rows where country is equal to Canada. Notice the double equals ("==")
forbes[forbes$country=="Canada",]


# Show all Banking companies in the United States

# Show rows where country equals United States & industry equals Banking.
forbes[forbes$country=="United States" & forbes$category=="Banking",]

# We can save these subsets of data for future analysis:
us.banks <- forbes[forbes$country=="United States" & forbes$category=="Banking",]

# Works with vectors too. Show all US companies with marketvalue greater than 200
forbes$name[forbes$marketvalue > 200]

# We can also subset our data using the subset() function. The syntax is 
# subset(data, condition). Notice we don't have to keep typing "forbes" before
# each column name.
subset(forbes, sales > 100)
subset(forbes, country == "Canada")
subset(forbes, country == "United States" & category == "Banking")

# We can also select certain columns. For example:
subset(forbes, sales > 100, c(name, sales))
subset(forbes, sales > 100 & country != "United States", sales:marketvalue)
subset(forbes, sales > 100 & country != "United States", c(name, category))

# YOUR TURN! Select all non-United States companies in the Utilities category.



# Some basic data manipulation --------------------------------------------

# (1) Deriving new columns (or variables)

# Subtract profits from sales to create a new column caled totalcosts:
forbes$totalcosts <- forbes$sales - forbes$profits

# Add a new column for log-transformed sales
forbes$logsales <- log(forbes$sales)

# create an indicator for US or non-US company using ifelse function
# syntax: ifelse(condition, action if TRUE, action if FALSE)
forbes$US <- ifelse(forbes$country=="United States", "US", "Not US")

# Create a column of TRUE/FALSE based on whether profits are positive
forbes$profitsI <- forbes$profits > 0


# (2) dropping columns (variables)

forbes$totalcosts <- NULL
forbes$logsales <- NULL
forbes$profitsI <- NULL

# (3) Recode a continuous variable into categories. 

# Here we recode sales into four categories: (0,5], (5,10], (10,100], (100,500]
# using the cut() function
forbes$salesCat <- cut(forbes$sales, breaks = c(0,5,10,100,500))
summary(forbes$salesCat)

# We can add labels:
forbes$salesCat <- cut(forbes$sales, breaks = c(0,5,10,100,500), 
                       labels = c("small","mid-small","mid-large","large"))
summary(forbes$salesCat)

# YOUR TURN! Add a column called salesM that is for sales in millions instead of
# billions. (ie, multiply by 1000)


# Basic summary stats -----------------------------------------------------

# The summary function is nice for quickly generating summaries of all columns,
# but we often want to generate specific summaries.

# to calculate frequencies of a factor or character variable use the table
# function:
table(forbes$category) # factor
table(forbes2$category) # character

# sort category count in increasing or decreasing order
sort(table(forbes$category))
sort(table(forbes$category), decreasing = TRUE)
# Top 5
sort(table(forbes$category), decreasing = TRUE)[1:5]

# summarize numeric columns
mean(forbes$sales)
mean(forbes$profits) # NA?

# By default R returns NA (not available) for certain calculations if there is
# any missing data. Usa na.rm=TRUE to override:
mean(forbes$profits, na.rm = TRUE)

# How many missing?
summary(forbes$profits)

# A few other summary functions
median(forbes$sales)
sd(forbes$sales) # standard deviation
range(forbes$sales) # returns min and max values
quantile(forbes$sales) # 25th, 50th, 75th quantiles (percentiles)
quantile(forbes$sales, probs=c(0.1,0.9)) # 10th and 90th quantiles
summary(forbes$sales)

# Counting number of conditions satisfied. 

# For example, how many companies had sales over 5 billion?

# The following generates a vector of TRUE/FALSE values:
forbes$sales > 5

# In R, TRUE = 1 and FALSE = 0, so we can do math with TRUE/FALSE values. How
# many TRUES?
sum(forbes$sales > 5)

# Percent of Forbes 2000 with sales over 5 billion:
mean(forbes$sales > 5)

# How many companies had negative profits?
sum(forbes$profits < 0)

# We have NA in the profits column, so we need to tell R to ignore them:
sum(forbes$profits < 0, na.rm = TRUE)
mean(forbes$profits < 0, na.rm = TRUE)

# We can also count up TRUE and FALSE with table
table(forbes$sales > 5)
table(forbes$profits < 0)

# Notice table() ignores NAs by default.

# YOUR TURN! 
# What proportion of the Forbes 2000 list is from the United States?



# Aggregating and Summarizing Data ----------------------------------------

# Descriptive Statistics such as contingency tables and summary stats

# for contingency tables (or cross tabs) use the table function
# synatx: table(row variable, column variable)

# cross tab of categories vs. US/Non-US countries
table(forbes$category, forbes$US)

# can use with() function to temporarily "attach" data frame, which allows you
# to reference column names directly without using the $.
with(forbes, table(category,US))

# calculate percents with prop.table

# First we save the table object as CatTable
CatTable <- with(forbes, table(category, US))
CatTable

prop.table(CatTable, margin = 1) # rows proportions sum to 1
prop.table(CatTable, margin = 2) # columns proportions sum to 1

# For basic numeric summary stats use the aggregate() function.
# syntax: aggregate(numeric ~ category, data, statistic)
# NOTE: aggregate ignores missing data by default

# median profits by category
aggregate(profits ~ category, data=forbes, median)

# mean sales by country
aggregate(sales ~ country, forbes, mean)

# total profits by country
aggregate(profits ~ country, forbes, sum)

# mean sales by US/Not US and category
aggregate(sales ~ US + category, forbes, mean)


# aggregate also provides a subset argument that allows us to subset data before
# aggregation.

# total profits by country for companies with profits
aggregate(profits ~ country, forbes, sum, subset= profits > 0)



# Simple Graphics ---------------------------------------------------------

# scatter plots
plot(x = forbes$assets, y = forbes$marketvalue)
with(forbes, plot(assets,marketvalue))

# same with formula interface: y ~ x
plot(marketvalue ~ assets, data=forbes)

# plot log transformed data
plot(log(marketvalue) ~ log(assets), data=forbes)

# Customize the labels
plot(log(marketvalue) ~ log(assets), data=forbes, 
     main="Market Value vs. Assets",
     ylab = "Log Market Value",
     xlab = "Log Assets")

# use the Zoom button to see bigger image
# Use the Export button to save image as JPG, TIF, PDF, etc.

# histograms
# Histogram of market value
hist(forbes$marketvalue) # skew
hist(log(forbes$marketvalue)) # more symmetric
hist(log(forbes$marketvalue),prob=TRUE) # show probability densities
hist(log(forbes$marketvalue),prob=TRUE, breaks=20) # more bins

# boxplots
# boxplot(numeric ~ category)
boxplot(log(marketvalue) ~ salesCat, data=forbes, main="Log Market Value by Sales Category")

# See Past StatLab Workshops for an Intro to R Graphics:
# http://data.library.virginia.edu/workshops/past-workshops/



# Packages ----------------------------------------------------------------

# Come to R for the price, stay for the packages.

# What packages are available?:
# https://cran.r-project.org/web/packages/available_packages_by_name.html

# To install a package in RStudio:

# 1. click the Install button on the Packages tab
# 2. type in the name of the package
# 3. click install.

# or use the install.packages function

# packages only need to be installed once

# corrplot: Visualization of a correlation matrix
install.packages("corrplot")

# load the package; need to do this every time you want to use it
library(corrplot)

# compute a correlation matrix of numeric forbes values;
# The cor() function does this for us.
# they are in columns 5 - 8, so use indexing notation to select;
# need use="complete.obs" because of missing values in profit
M <- cor(forbes[,5:8], use="complete.obs")
M


# now use corrplot function to visualize
corrplot(M)
corrplot(M, diag=FALSE, addCoef.col="black")
corrplot(M, diag=FALSE, addCoef.col="black", type="lower")

# See ?corrplot for many more examples

# A few packages to know about:
# - haven (Import 'SPSS', 'Stata' and 'SAS' Files)
# - readxl (Read Excel files)
# - ggplot2 (data visualization)
# - reshape2 (reshape data)
# - dplyr (data manipulation for data frames)
# - lme4 (multilevel modeling)
# - car (companion to applied regression)
# - stringr (for working with character data)
# - lubridate (for working with time and dates)

# Note: Packages often have dependencies. This means installing one package
# will also install other packages it depends on. Example: installing
# ggplot2 package will install 9 other packages it uses.



# statisical analysis examples --------------------------------------------

# basic linear regression

# recall this plot
plot(log(marketvalue) ~ log(assets), data=forbes)

# Is there a relationship between assets and market value? Can we summarize it
# with a straight line? 

# We can fit a linear model using the lm function (ie, regression)
# lm(response ~ independent variables)
lm(log(marketvalue) ~ log(assets), data=forbes) 

# save the model (ie, create a model object) and view a summary:
mod <- lm(log(marketvalue) ~ log(assets), data=forbes) 
# summary of the model
summary(mod) 

# interpreting slope when both variables log transformed: a 1% increase in
# assets yields about a 0.4% in marketvalue (among the Forbes 2000)

# add fitted line using abline()
abline(mod, col="blue")

# Another way to add a line:
# add fitted line using lines() function; just like plotting x-y values in
# algebra class. The lines() function adds lines to an existing plot.
plot(log(marketvalue) ~ log(assets), data=forbes, col="gray")
lines(x = sort(log(forbes$assets)), y = sort(fitted(mod)), col = "blue")


# Want to learn more? See the Linear Modeling in R workshop:
# http://static.lib.virginia.edu/statlab/materials/workshops/LinearModelingR.zip
rm(mod)


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

# Null Hypothesis: no association between gender and political party
chisq.test(table_2.5)
results <- chisq.test(table_2.5)
str(results) # more than meets the eye! A list object.
results$statistic
results$p.value
results$residuals

rm(table_2.5, results)


# Two-sample t-test 

# Are means sales different between US and non-US companies in the Forbes 2000
# list?
aggregate(sales ~ US, data = forbes, mean)

# An assumption is the populations from which the samples have been drawn should
# be normal. They don't look normal:
hist(forbes$sales[forbes$US=="US"])
hist(forbes$sales[forbes$US!="US"])

# We may want to work with the log transformed data
hist(log(forbes$sales[forbes$US=="US"]))
hist(log(forbes$sales[forbes$US!="US"]))

# Now perform the t-test on the log transformed data
t.test(log(sales) ~ US, data = forbes)
boxplot(log(sales) ~ US, data = forbes)


# Analysis of Variance (ANOVA)

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

# function that calls str(), summary() and head() on data frame
# and lists row numbers with missing data (if any)
# syntax: function(argument(s))
mydata <- function(x){
  print("### STRUCTURE OF DATA FRAME ###")
  print(str(x))
  print("### SUMMARY OF DATA FRAME ###")
  print(summary(x))
  print("### FIRST 6 RECORDS ###")
  print(head(x))
  print("### RECORDS WITH MISSING DATA ###")
  if(all(complete.cases(x))==TRUE) print("No records with missing data")
  else print(which(complete.cases(x)==FALSE))
}
mydata(forbes) # try function on the forbes data frame

# a function that tells you if x is evenly divisible by y
# It takes two arguments: x and y
divisible <- function(x,y){
  
  # error check; return a special message
  if (is.numeric(c(x,y))==FALSE) 
    stop("please enter numbers")
  
  if (x %% y == 0){
    print("yes")
  } else {
    print("no")
  }
}

# test function
divisible(245,23)
divisible(245,"r")

n1 <- 1605
n2 <- 3
divisible(n1,n2)
rm(n1,n2)

