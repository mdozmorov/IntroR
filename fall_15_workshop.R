# Introduction to R workshop
# Fall 2015
# StatLab@UVa Library
# Clay Ford

# Preface comments with '#'

# RStudio layout ---------------------------------------------------------

# top left: R script (text file with R code) 
# top right: objects in memory bottom
# left: console (output and interactive work) 
# bottom right: plots, packages and help


# If you want to follow along, please download the workshop files: 
  
# 1. Go to http://data.library.virginia.edu/statlab/workshops/ 

# 2. Click Download Workshop Materials link to download zip file containing materials

# 3. Unzip the files to a convenient folder on your computer. You should have a
# CSV file, a PDF file and an R file.



# R can be a calculator ---------------------------------------------------

# move your cursor to the next line and hit Ctrl + Enter (or Command + Enter on
# Mac)
2 + 2
22/7
2 + 7 * 7 - (3^2)
5 %% 2 # modulo (ie, remainder of division)
5 %/% 2 # integer division

# can also use the console. Try it.


# Typing and submitting functions -----------------------------------------

# Example: the log function (default is natural log)
log(27)
# functions often have arguments (here, 'base=')
log(1000, base=10)
log(27) + 4

# on the blank line below, or in the console, type "log" and hit Tab:

# functions that begin with "log" appear along with help.
# The word next to the function in curly braces is the package it's in.


# assign results of calculation to x using the assignment operator: <-

x <- log(27) + 4 
y <- log(1000, base=10) 

# view x and y
x
y 

# use x and y
x + y
x * y
log(x*y)
log(x) + log(y)

# Your Turn: assign the value of x/y to z
# RStudio shortcut: Alt + - inserts "<-", Try it!


# the rm function removes objects from memory
rm(x, y, z)


# get help on a function with the help function
help(log)
# or
?log
# or use the search field in the RStudio help section


# The working directory ---------------------------------------------------

# Your working directory is where R will save output or look for input unless
# told otherwise.

# See your working directory with getwd function
# notice the parentheses! 
getwd() 

# What happens without parentheses?

# Set working directory in RStudio: Session...Set Working Directory.
# Or use setwd() function; path must be in quotes;
# I recommend the latter.
# hit Tab and R will show you what directories are available. 
# "/" is your root directory
# "~/" is your home directory

# Try setting your working directory using setwd():


# Loading/Importing Data --------------------------------------------------

# Forbes 2000 list from 2004 
# source: Handbook of Statistical Analysis Using R (Everitt & Hothorn)
# amounts in billion US dollars
    
# Import a CSV file and create a "dataframe"; here are three ways to do it:

# (1) read file from working directory on computer
forbes <- read.csv("Forbes2000.csv")

# (2) read from web site 
forbes <- read.csv("https://github.com/clayford/IntroR/raw/master/Forbes2000.csv")

# (3) use the RStudio "Import Dataset" button in the Environment window

# another common function: read.table()
# (read.csv is actually read.table with different defaults.)

# R can import ASCII files, fixed-width format, Stata, SPSS, Minitab, XML, XLS,
# JSON, etc. It can also read relational databases. See "R Data Import/Export"
# on the help main menu. Or better yet just Google how to import whatever format
# you're dealing with.

# Inspecting Data ---------------------------------------------------------

# Quick way: click the name in the Environment window

# Another way: enter the name in the console and hit Enter.
# Try it and see what happens.

# Good time to introduce this keyboard shortcut:
# Ctrl + L/Command + L to clear console


# view structure of data; very useful function!
str(forbes)

# int = integer
# num = number
# Factor = categorical variable

head(forbes) # first 6 records
tail(forbes) # last 6 records
summary(forbes) # summary of variables
dim(forbes) # dimensions of data frame

# Try the following functions on the forbes data frame in the console:
# names
# nrow
# ncol
# what do they return?


# Working with columns/rows of data ---------------------------------------

# Use indexing brackets to select portions of data frame
# [row number(s),column number(s)/name(s)]

# show first 6 records of first 2 columns
# 1:6 = 1,2,3,4,5,6
forbes[1:6,1:2] 
# first six rows
forbes[1:6,] 
# columns 2 and 3
forbes[,2:3] 
# columns 2, 4, and 6
forbes[,c(2,4,6)]

# can also use column names
forbes[1:6,c("name","category")]

# first 10 records and all but first column (rank)
forbes[1:10,-1]

# last row without typing the row number:
forbes[nrow(forbes),]

# We can access columns of a data frame using $ as follows
forbes$name
forbes$name[1:10] # first 10

# On the next line type forbes$ and hit Tab. What happens?


# recall forbes$category was a factor (ie, categorical variable)
# to see the levels, use the levels function
levels(forbes$category)

# to calculate frequencies of a factor
table(forbes$category)
summary(forbes$category)

# summarize numeric columns
mean(forbes$sales)
mean(forbes$profits) # NA?

# By default R returns NA (not available) for certain calculations if there is
# any missing data. Usa na.rm=TRUE to override:
mean(forbes$profits, na.rm = TRUE)

median(forbes$sales)
sd(forbes$sales) # standard deviation
range(forbes$sales) # returns min and max values
quantile(forbes$sales)
quantile(forbes$sales, probs=c(0.1,0.9)) # 10th and 90th quantiles
summary(forbes$sales)


# Data Manipulation -------------------------------------------------------

# Here's how to change column names:
names(forbes) # view column names
names(forbes)[2] # view 2nd column name
names(forbes)[2] <- "company"  # assign "company" as 2nd column name
names(forbes)

# extract two columns and make a new data frame called forbes.sales;
# nothing before comma means select all rows
forbes.sales <- forbes[,c("company","sales")]
head(forbes.sales)

# using logical (true/false) operators to select data

# R evalutes the following and returns TRUE or FALSE
forbes.sales$sales >= 50
forbes$country == "Japan"
forbes$country != "Japan"
forbes$profits < 0

# the above results have limited use unless we combine them with functions.
# In R TRUE = 1, FALSE = 0
sum(forbes$sales > 100)
sum(forbes$country == "Japan")
mean(forbes$country == "United States") # proportion of forbes list that are in US
sum(forbes$profits < 0) # ??? Some values are missing, so R returns NA
sum(forbes$profits < 0, na.rm=TRUE) # use na.rm=TRUE to remove missing values

# summarize results
summary(forbes.sales$sales > 100)
table(forbes.sales$sales > 100)

# can use results of logical operations to select data;
# TRUE shows row; FALSE does not show row;
# In words: "extract rows of forbes.sales such that sales > 100"
forbes.sales[forbes.sales$sales > 100,] 

# create new data frame for companies with sales > $100B
top.sales <- forbes.sales[forbes.sales$sales > 100,] 
top.sales

# new data frame still has row numbers from previous data frame
rownames(top.sales) <- NULL # reset row numbers (or row "names")
top.sales

# sorting data

# Use the sort() function to sort vectors
sort(forbes$assets)[1:10] # top 10 smallest assets
sort(forbes$profits,decreasing = TRUE)[1:10] # top 10 largest profits
sort(table(forbes$category), decreasing = TRUE)[1:5] # top 5 industries

# Question: what's the difference between the following two lines of code?
sort(forbes$assets)[1:10] 
sort(forbes$assets[1:10]) 

# sorting data frames in R takes some getting used to;
# use the order function; order top.sales by sales
order(top.sales$sales) # returns indices of ordered elements (ascending)

# we can use the results of order() to sort a data frame:
# sort the top.sales data frame by sales:
top.sales[order(top.sales$sales),] 
top.sales[order(top.sales$sales, decreasing=TRUE),] # in descending order


# subsetting data using subset function
# syntax: subset(data, selection conditions, which columns to keep)

# see the Japanese companies
subset(forbes, country == "Japan", company)

# see the Japanese companies with sales > 50 billion
subset(forbes, country == "Japan" & sales > 50, c(company, sales))

# create data frame of just Japanese companies and drop country column
JapanComp <- subset(forbes, country == "Japan", -country)
head(JapanComp)

# How would you select all non-United States companies in the Utilities category?

# deriving new variables
# adding columns (variables)
forbes$totalcosts <- forbes$sales - forbes$profits
head(forbes[,c("company","sales","profits","totalcosts")])

# create categories (factors) from continuous variables with the cut function 
# divide the range of sales into 4 intervals of equal length
forbes$salesP <- cut(forbes$sales,breaks=4)
summary(forbes$salesP)

# create an indicator for US or non-US company using ifelse function
# ifelse(condition, action if TRUE, action if FALSE)
forbes$US <- ifelse(forbes$country=="United States", "US", "Not US")
table(forbes$US)

# dropping columns (variables)
forbes$totalcosts <- NULL


# Missing data ------------------------------------------------------------

# Missing data is represented with "NA"

# summary will identify NAs
summary(forbes) # see the summary for "profits

# function for identifying missing data: is.na()
# answers T/F to question: is the value NA?
is.na(forbes$profits)
sum(is.na(forbes$profits)) # number missing
which(is.na(forbes$profits))
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

# Saving Data -------------------------------------------------------------

# R objects can be saved; use an .Rda or .Rdata extension

# save multiple objects for later use
save(forbes, forbes.sales, top.sales, file="forbes.Rda")
rm(forbes, forbes.sales, top.sales)
load("forbes.Rda")

# I typically write an R script for data preparation and manipulation that ends 
# with saving the analysis-ready data as a .Rda file. Then I write another 
# analysis script that begins with loading the .Rda file. This way I don't have
# to re-run my data prep code everytime I come back to work on my analysis.


# Aggregating and Summarizing Data ----------------------------------------

# Descriptive Statistics such as contingency tables and summary stats

# for contingency tables (or cross tabs) use the table function
# synatx: table(row variable, column variable)

# cross tab of categories vs. sales percentile categories
table(forbes$category, forbes$salesP)

# cross tab of categories vs. US/Non-US countries
table(forbes$category, forbes$US)

# can use with() function to temporarily "attach" data frame, which allows you
# to reference column names directly without using the $.
with(forbes, table(category,US))

# calculate percents with prop.table
CatTable <- with(forbes, table(category, US))
prop.table(CatTable, margin = 1) # rows proportions sum to 1
prop.table(CatTable, margin = 2) # columns proportions sum to 1

# for basic aggregation use aggregate
# aggregate(numeric ~ category, data, statistic)

# median profits by category
aggregate(profits ~ category, data=forbes, median)

# mean sales by country
aggregate(sales ~ country, forbes, mean)

# total profits by country
aggregate(profits ~ country, forbes, sum)

# total profits by country for companies with profits
aggregate(profits ~ country, forbes, sum, subset= profits > 0)

# mean sales by category and US/Not US
aggregate(sales ~ category + US, forbes, mean)


# Simple Graphics ---------------------------------------------------------

# scatter plots
plot(x = forbes$assets, y = forbes$marketvalue)
# same with formula interface: y ~ x
plot(marketvalue ~ assets, data=forbes)
plot(log(marketvalue) ~ log(assets), data=forbes)
plot(log(marketvalue) ~ log(assets), data=forbes, 
     main="Market Value vs. Assets")

# Use Ctrl + Shift + Page Up/Page Down to flip between plots
# use the Zoom button to see bigger image
# Use the Export button to save image as JPG, TIF, PDF, etc.

# histograms
hist(forbes$marketvalue) # skew
hist(log(forbes$marketvalue)) # more symmetric
hist(log(forbes$marketvalue),prob=TRUE) # show probability densities
hist(log(forbes$marketvalue),prob=TRUE, breaks=20) # more bins

# multiple graphs in one plot
# par = graphical parameters; mfrow = multi-frame row
par(mfrow=c(1,2)) # divide graphics window into 2 "frames"

hist(forbes$marketvalue, main="Market Value", col="red")
hist(log(forbes$marketvalue), main="Log Market Value", col="blue")

# reset graphics window to 1x1 frame
par(mfrow=c(1,1))

# boxplots
# boxplot(numeric ~ category)
boxplot(log(marketvalue) ~ US, data=forbes, main="Market Value")

# dotchart
# good for visualizing counts
summary(forbes$category)
dotchart(summary(forbes$category))
dotchart(sort(summary(forbes$category))) # sorted

# See StatLab Past Workshops for an Intro to R Graphics:
# http://data.library.virginia.edu/workshops/past-workshops/


# statisical analysis -----------------------------------------------------

# basic linear regression

# recall this plot; this time we make it gray
plot(log(marketvalue) ~ log(assets), data=forbes, col="gray")

# Is there a relationship between assets and market value? can we summarize it
# with a straight line? We can use the scatter.smooth function to make scatter plot
# and add a fitted smooth line
scatter.smooth(x=log(forbes$assets),
               y=log(forbes$marketvalue),
               col="gray")
# We can fit a linear model using the lm function (ie, regression)
# lm(response ~ independent variables)
lm(log(marketvalue) ~ log(assets), data=forbes) 

# save the model (ie, create a model object) and view a summary:
mod <- lm(log(marketvalue) ~ log(assets), data=forbes) 
summary(mod) 
confint(mod) # 95% confidence interval for model coefficients
abline(mod, col="blue") # add the regression line to the plot
# interpreting slope when both variables log transformed: a 1% increase in
# assets yields about a 0.4% in marketvalue (among the Forbes 2000)

# Four diagnostic plots
plot(mod) 

# Want to learn more? See the Linear Modeling in R workshop from October 2014:
# http://static.lib.virginia.edu/statlab/materials/workshops/LinearModelingR.zip
rm(mod)

# chi-square test of independence
# 1991 General Social Survey
# An Introduction to Categorical Data Analysis (Agresti, 1996), p. 31
# Table 2.5

# manually enter data from Table 2.5 into a matrix
# The matrix function fills by column
table_2.5 <- matrix(c(279,165,73,47,225,191),ncol=3)
table_2.5
colnames(table_2.5) <- c("Dem","Ind","Rep")
rownames(table_2.5) <- c("Females","Males")
table_2.5

# Null Hypothesis: no association between gender and political party
chisq.test(table_2.5)
results <- chisq.test(table_2.5)
str(results) # more than meets the eye
results$statistic
results$p.value

rm(table_2.5, results)

# hypothesis test and confidence interval
# Probability and Statistical Inference (Hogg & Tanis, 2006), p. 492
# problem 8.2-6
# a coach claims FVC (forced vital capacity) of players is greater than 3.4 liters;
# nine players randomly sampled
fvc <- c(3.4, 3.6, 3.8, 3.3, 3.4, 3.5, 3.7, 3.6, 3.7)
# Null: mean <= 3.4
# Alternative: mean > 3.4
t.test(fvc, mu = 3.4, alternative = "greater")


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

# a lovely simple package: corrplot
# Visualization of a correlation matrix
install.packages("corrplot")

# load the package; need to do this every time you want to use it
library(corrplot)

# which packages are currently loaded? Use the search function
search()

# compute a correlation matrix of numeric forbes values;
# they are in columns 5 - 8, so use indexing notation to select;
# need use="complete.obs" because of missing values in profit

M <- cor(forbes[,5:8], use="complete.obs")

# now use corrplot function to visualize

corrplot(M)
corrplot(M, diag=FALSE)
corrplot(M, type="upper", diag=FALSE)


# A few packages to know about:
# - ggplot2 (data visualization)
# - lme4 (multilevel modeling)
# - reshape2 (reshape data)
# - dplyr (data manipulation for data frames)
# - plyr (data manipulation for all objects)

# Note: Packages often have dependencies. This means installing one package
# will also install other packages it depends on. Example: installing
# ggplot2 package will install 9 other packages it uses.


# Back to presentation


# Bonus material! ---------------------------------------------------------

# Stuff I'm pretty sure we won't have time for but you might like to review in
# your free time.

# two-sample t-test -------------------------------------------------------

# t test using data that comes with R (see datsets package)
# Student's sleep data
# Data which show the effect of two soporific drugs 
# (increase in hours of sleep compared to control) on 10 patients.
sleep
plot(extra ~ group, data = sleep)
# is there a difference in mean extra sleep between groups?
t.test(extra ~ group, data = sleep)
t.test(extra ~ group, data = sleep, var.equal = TRUE) # assume equal variance


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

