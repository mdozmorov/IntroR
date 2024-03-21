# Introduction to R workshop
# Modified after https://github.com/clayford/IntroR

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
head(iris, n = 3)

tail(iris)
tail(iris, n = 1)

names(iris)
nrow(iris)
ncol(iris)
dim(iris)


# Loading our own data ----------------------------------------------------

# The iris data is nice for playing with R, but eventually we need to load our
# own data. Also it's worth noting that R really isn't for data entry. You can
# do it, but it's easier to use something like Excel for data collection and
# data entry. R is for cleaning and analyzing data you have already collected.

# You can import just about any kind of data into R: Excel, Stata, SPSS, SAS,
# CSV, JSON, fixed-width, TXT, DAT, shape files, and on and on. You can even
# connect to data bases. The best way to figure out how: Google "how to import
# <type> files into R." This will usually involve installing and loading a
# special R package, which we'll talk about later.

# Today we'll import a CSV file.

# Before we do that let's set our working directory. Your working directory is
# where R looks for files if you don't provide a path to the file. The working
# directory is displayed in the header of the console. You typically set your
# working directory to where your research or project files are located.

# Good to know:
# "/" is your root directory
# "~/" is your home directory

# To set working directory via point-and-click:
# Session...Set Working Directory...Choose Directory

# To set working directory with R code:
# use setwd() function; path must be in quotes

# TRY NOW!
# Type "setwd" and open parens. Then in the parens type a double-quote. Then hit
# Tab. A list of directories should pop up. Keep selecting directories and
# hitting TAB until you arrive at the directory where you downloaded the
# workshop files.


# Loading/Importing Data --------------------------------------------------

# Forbes 2000 list from 2004
# source: Handbook of Statistical Analysis Using R (Everitt & Hothorn)
# amounts in billion US dollars

# Import a CSV file and create a "dataframe"; here are three ways to do it:

# (1) read file from working directory on computer. If we've already set our
# working directory to the location where the CSV file is located, we just need
# to put the name of the file in quotes.
forbes <- read.csv("Forbes2000.csv")

# (2) read from a web site. If the file is on the web, simply copy-and-paste the
# URL and enclose in quotes:
forbes <- read.csv("https://raw.githubusercontent.com/curran/data/gh-pages/Rdatasets/csv/HSAUR/Forbes2000.csv")

# (3) use the RStudio "Import Dataset" button in the Environment window. Click
# on "Import Dataset", select "From Local File...", and navigate to file.

# another common function: read.table()
# (read.csv is actually read.table with different defaults.)


# Inspecting Data ---------------------------------------------------------

# view structure of data
str(forbes)

# Can also click the name in the Environment window

# We can also just enter forbes in the console and hit Enter. That's not very
# useful. Try it and see what happens.

# It blew away our console! And it didn't even display all the data. R has a
# default limit for printing information to the console.

# Good time to tell you about this keyboard shortcut:
# Ctrl + L (Win) or Command + L (Mac) to clear console


head(forbes)
tail(forbes)
dim(forbes)

names(forbes)
summary(forbes)

# Indexing brackets -------------------------------------------------------

# R keeps data out of sight. If we want to see or use our data, we have to tell
# it what parts we want to see or use. While this can seem counterintuitive at
# first, it allows us focus on the data we're interested in and cut down on
# distractions.

# We can use indexing brackets to select portions of data frame:
# [row number(s),column number(s)/name(s)]

# show first 6 records of first 2 columns
# 1:6 = 1,2,3,4,5,6
forbes[1:6, 1:2]

# first six rows
forbes[1:6, ]

# columns 2 and 3
forbes[, 2:3]

# rows 10 and 11, columns 2, 4, and 6
forbes[c(10, 11), c(2, 4, 6)]

# Note: c(10,11) and c(2,4,6) are "vectors"; The c() function means "combine" or
# "concatenate"

# can also use column names
forbes[1:6, c("name", "category")]

# first 5 records and all but first column (rank)
forbes[1:5, -1]

# first record and all but last two columns (assets and marketvalue)
forbes[1, -c(7, 8)]

# We can access columns of a data frame using $ as follows
forbes$name
forbes$name[1:10] # first 10

# On the next line type forbes$. What happens?



# Subsetting data ---------------------------------------------------------

# We often want to see parts of our data that meet a certain condition. Which
# companies had sales over 100 billion? Which companies from Canada are in the
# list? Which Banking companies from the United States are in the list?

# We can define conditions in our indexing brackets.

# Show rows with sales greater than 100 billion
forbes[forbes$sales > 100, ]

# Notice the construction "forbes$sales". That $ allows us to extract just the
# sales column.

# Show rows where country is equal to Canada
forbes[forbes$country == "Canada", ]

# Show rows where country equals United States and industry equals Banking.
forbes[forbes$country == "United States" & forbes$category == "Banking", ]

# We can save these subsets of data for future analysis:
us.banking <- forbes[forbes$country == "United States" & forbes$category == "Banking", ]


# We can also subset our data using the subset() function. The syntax is
# subset(data, condition)
subset(forbes, sales > 100)
subset(forbes, country == "Canada")
subset(forbes, country == "United States" & category == "Banking")

# We can also select certain columns. For example:
subset(forbes, sales > 100 & country != "United States", c("name", "category"))

# YOUR TURN! Select all non-United States companies in the Utilities category.


# Basic summary stats -----------------------------------------------------

# The summary function is nice for quickly generating summaries of all columns,
# but we often want to generate specific summaries.

# to calculate frequencies of a factor, or categorical variable
table(forbes$category)
summary(forbes$category)

# sort category count in increasing or decreasing order
sort(table(forbes$category))
sort(table(forbes$category), decreasing = TRUE)

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
quantile(forbes$sales)
quantile(forbes$sales, probs = c(0.1, 0.9)) # 10th and 90th quantiles
summary(forbes$sales)

# Counting number of conditions satisfied. For example, how many companies had
# sales over 5 billion?

# The following generates a vector of TRUE/FALSE values (and some NA where
# profit is missing)
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

# YOUR TURN! What percent of the Forbes 2000 list is from the United States?



# Some basic data manipulation --------------------------------------------

# Here's how to change column names:
names(forbes) # view column names
names(forbes)[2] # view 2nd column name
names(forbes)[2] <- "company" # assign "company" as 2nd column name
names(forbes)


# We can derive new columns. Here we subtract profits from sales to create a new
# column caled totalcosts:
forbes$totalcosts <- forbes$sales - forbes$profits

# Add a new column for log-transformed sales
forbes$logsales <- log(forbes$sales)

# create an indicator for US or non-US company using ifelse function
# syntax: ifelse(condition, action if TRUE, action if FALSE)
forbes$US <- ifelse(forbes$country == "United States", "US", "Not US")
table(forbes$US)

# Could also just do this; generates TRUE/FALSE:
# forbes$US <- ifelse(forbes$country=="United States")


# dropping columns (variables)
forbes$totalcosts <- NULL
forbes$logsales <- NULL

# YOUR TURN! Add a column called salesM that is for sales in millions instead of
# billions. (ie, multiply by 1000)
forbes$salesM <- forbes$sales * 1000


# Aggregating and Summarizing Data ----------------------------------------

# Descriptive Statistics such as contingency tables and summary stats

# for contingency tables (or cross tabs) use the table function
# synatx: table(row variable, column variable)

# cross tab of categories vs. US/Non-US countries
table(forbes$category, forbes$US)

# can use with() function to temporarily "attach" data frame, which allows you
# to reference column names directly without using the $.
with(forbes, table(category, US))

# calculate percents with prop.table
CatTable <- with(forbes, table(category, US))
prop.table(CatTable, margin = 1) # rows proportions sum to 1
prop.table(CatTable, margin = 2) # columns proportions sum to 1

# for basic summary stats use aggregate
# syntax: aggregate(numeric ~ category, data, statistic)

# median profits by category
aggregate(profits ~ category, data = forbes, median)

# mean sales by country
aggregate(sales ~ country, forbes, mean)

# total profits by country
aggregate(profits ~ country, forbes, sum)

# total profits by country for companies with profits
aggregate(profits ~ country, forbes, sum, subset = profits > 0)

# mean sales by category and US/Not US
aggregate(sales ~ category + US, forbes, mean)

# YOUR TURN! What is the mean marketvalue by category for companies not in the
# US?
