# -- 
# flood risk seminar in Summer Semester 
# R project language concepts and fundamentals
# 
# Created: 22.05.2022
# Last updated: 14
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)
# ---


### ------ Introduction to R ------

### ------ Data types and structures in R ------
## ---- Data types ----

x <- 10.1 # numeric: floating point number
x <- 0  # numeric: integer

x1 <- "name" # character
x2 <- TRUE   # logical / boolean

# rules for variable names: 
# 1. A variable name must start with a letter and can be a combination of letters, digits, period(.)
#    and underscore(_). If it starts with period(.), it cannot be followed by a digit.
# 2. A variable name cannot start with a number or underscore (_)
# 3. Variable names are case-sensitive (age, Age and AGE are three different variables)
# 4. Reserved words cannot be used as variable names (like TRUE, FALSE, NULL, if...)


is.numeric(x)
is.integer(x)  # is it a integer number?
is.character(x)
is.logical(x)


# type conversion
as.character(10)
as.numeric('1.52') # convert from character to number
as.numeric('xs')   # failed
as.integer(10.54)  # convert to integer
as.logical(5)      # TRUE: as long as it is a non-zero numerical value 

# convert boolean / logical value to integer 

as.numeric(TRUE)  # 1
as.numeric(FALSE) # 0

1 + TRUE
2 - TRUE * 2
2 - FALSE

## ----- Data structures in R --------

# Vector ------

v1 <- c(1, 2, 5, 3, 6, -2, 4)  # numeric vector, with 7 elements

v2 <- c(TRUE, FALSE, TRUE)     # logical vector, with 3 elements
v3 <- c("Name1", "Name2")      # character vector, with 2 elements

v4 <- 1:5  # a numeric sequence from 1 to 5 with a step of 1
v4 <- seq(1, 10, by = 2)


v5 <- 1:20
v5 <- seq(from = 1, to = 20, by = 2) # sequence


# data.frame ----
# A data frame is more general than a matrix in 
# that different columns (vectors) can contain different modes of data

# create data.frame from a couple of vectors, with the same length
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

# refer to the individual column in the data.frame
patientdata$age   # access specific column with $ sign
patientdata$diabetes



#### ---- getting data into R ----------
# data input and output
# from keyboard ------
data_example <- c(6,38, 30) # data.frame

# from text file ------
# import data as a data frame from a text file
df <- read.table(
  # file path and name; we use slash sign / here 
  file = 'D:/FloodRiskSeminar/data/Example_data.csv', 
  header = TRUE, sep = ','
)

?read.table  # find more about the function and parameters

write.table(
  df, 
  file = 'D:/test.csv',
  col.names = TRUE, row.names = FALSE, quote = F, append = F, sep = ','
  # col.names: whether the column names added to the first row in the output file?
  # row.names: should the row names be printed as the first column in the output file?
  # quote: should quotation sign be added to enclose character variables?
  # append: output mode
  # sep: separator
)


#### ------ Functions --------

# basic functions ----
length(c(1,2,3,4))  # Gives the number of elements/components.

dim(df)  # Gives the dimensions of an object.
str(df)  # Gives the structure of an object.

# mathematical (operations / functions)----
1 + 1
3 - 2
1 * 9
9 / 3
2 ^ 3         # Exponentiation

abs(-0.1253)  # Absolute value
sqrt(4)       # square root
x = 12.23134
cos(x)
sin(x)
tan(x)        # Trigonometric functions: cosine, sine, and tangent

log(x)        # the natural logarithm
exp(x)        # Exponential function


c(1, 2, 3, 4, 5) + 1  # the operations are performed in a vectorized manner
c(1, 2, 3, 4, 5) * 2
c(1, 2, 3, 4, 5) / 2
sqrt(c(1, 2, 3, 4, 5))

c(1, 2, 3, 4, 5) + c(5, 4, 3, 2, 1)  # with the same element numbers
c(1, 2, 3, 4, 5) * c(5, 4, 3, 2, 1)


# statistical ----
x <- c(0.1, 1, 1.2, 0.25, 0.56, 5)
mean(x)    # Mean
median(x)  # Median
sd(x)      # Standard deviation
var(x)     # Variance

range(x)   # Range, min-max
sum(x)
max(x)
min(x)

sort(x, decreasing = F)
sort(x, decreasing = T)

# functions for objects 

length(x)  # length of vector object
str(x)     # structure of the object

head(df, 6)   # first 6 rows in the dataframe df
tail(df, 7)   # last 7 rows in the dataframe df
dim(df)       # dimensions of the data frame
str(df)       # structure of the dataframe
colnames(df)  # column names of data frame
rownames(df)  # row names of data frame
summary(df)   # a brief summary of a numeric data frame


# User-defined functions ----
myFunc <- function(parameter_list) {
  # the operations in this function are performed on or based on
  #    the parameters provided.
  
  # command chunk here
  out = ...
  
  # return an object to the environment where this function is called
  # the out object can be of any type of data structure: vector, data frame, ...
  return(out) 
}

myfunc <- function(x1, x2) {
  # give two numeric values x1, x2
  y = (x1 + x2) * (x1 - x2)
  return(y)
}

myfunc(1.0, 2.0)  # call the defined function 


# --- control structure ----

## if-else structure ----
if (condition1) {
  # condition1 == TRUE, then
  
  # command chunk 1 here
  
} else if (condition2) {
  # condition2 == TRUE, then
  
  # command chunk 2 here
  
} else {
  # command chunk 3 here
  
}

# if-else example
condition = TRUE
if (condition) 
{
  print(1)
} else {
  print(0)
}

## Comparison operators ---- 
# operations with logical results
1 < 3      # less than
1.2 <= 2.0 # less than or equal to

2 > 3      # greater than 
1.25 >= 2  # greater than or equal to

x = 1
x == 1     # equal
x != 1     # not equal 


c(1, 2, 1, 3, 2.5) > 1.5 # perform vectorized comparison 
c(1, 2, 1, 3, 2.5) == c(1, 0, 1, 0, 0)

is.numeric()
is.character()
is.logical()

## logical operator ---- 
x1 <- 1.5  
x2 <- 2.5
y1 <- 1.6
y2 <- 0.6

#  Logical Operators
# chain multiple test expressions
x1 > x2  & y1 < y2   # &: and
x1 <= x2 & y1 <= y2  
x1 > x2  | y1 <= y2  # |: or 
x1 <= x2 | y1 <= y2  

# vectorized operation also applies here, but you need also pay extra attention. 

x1 <= x2
!(x1 <= x2)         # !: exclamation mark: not 

# if-else examples ----
x <- -3
if (x > 0) {
  x = x
} else if (x < 0) {
  x = -x
} else {
  x = 0
}


# ---- loops -------
Ids <- c(1, 2, 3, 4, 5)

for (i in Ids) {
  out = i * (-1)
  print(out)
}

id = 1
while (id <= 5) {
  out = id * (-1)
  print(out)
  id = id + 1
}

id <- 1

while (id <= 5) {
  out = id * (-1)
  print(out)
}

#### ---- Data manipulation -----


# data creating ------
x <- NULL
x <- 0
x <- c(1,2,3,4,5,6)  # numeric vectors

# creating logical vectors
x <- c(TRUE, FALSE, FALSE)  # direct creation 

nv <- c(0.1, 1, 1.2, 0.25, 0.56, 5)
x <- nv < 0.5  # create from comparison expression 
x <- nv > 0.5

# creating character vectors
x <- c('a', 'b', 'c', 'f')

as.character(c(1,2,3))

years = 1999:2020  # convert numeric to string
paste0('Y', years)


# creating data frame (from multiple vectors)
# import from text files
# create by keyboard
patientID <- c(1, 2, 3, 4, 5, 6)
age <- c(25, 34, 28, 52, 35, 40)
diabetes <- c("Type1", "Type2", "Type1", "Type1", "Type1", "Type2")
status <- c("Poor", "Improved", "Excellent", "Poor", "Poor", "Improved")
df <- data.frame(patientID, age, diabetes, status)
df

# data rename -----

colnames(df)  # access the column names
colnames(df) <- c("c1", "c2", "c3", "c4")  # rename the columns
colnames(df) <- c("ID", "age", "diabetes", "status")  # rename the columns

# data altering ------
x[1]
x[2] <- 0
x[5] <- 11

# data adding ------
# for vector objects
x <- c(1, 2, 3)
x[4] <- 4

x[10] <- 10

x1 <- c(2, 4, 6)

x <- c(x, x1)  # combine with vectors 

x <- NULL  # null vector, it is empty after declaration 
x[1] <- 1  # fill values
x[5] <- 5
x

# data sorting -----
x <- c(1.0, 0, -2.2, 2.5, -5.1, 6, 4.15, -1.5, 3.2)
sort(x)           # sort the numeric vector in increasing order
sort(x, decreasing = T)

df[order(df$age), ]
df[order(df$age, decreasing = TRUE), ]

# filtering and sub-setting ------

x1 <- c(1.0, 0, -2.2, 2.5, -5.1, 6, 4.15, -1.5, 3.2)

# select (subset) by index (or position)
x1[1]
x1[6]

# subset by an index (position) vector
x1[1:6]
x1[c(1, 3, 5)]

# subset by logical expression
x1[c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)]

x1[x1 > 0] # select all the elements in x1 greater than 0 (positive values)

x1[x1 > mean(x1)]  # select all the elements in x1 greater than the average
x1[x1 > 0 & x1 < 5] 


# filter a data frame with two dimension
# select by a index
df[1, 1]

# select one row of data (observation) by index
df[1, ]

# select on column (field) by index
df[, 1]

# select by name
colnames(df)
df[, "age"]
df[1, "age"]

# multiple selecting (sub-setting)
df[1:2, 1]
df[1, 2:3]

df[1:2, 2:3]
df[2:4, c('ID', 'age')]

# select column(s) by name(s)

df$age   # then all select methods for vectors are also valid

df$age[1]  # just address df$age as a general vector
df$age[2:3]
df$age[df$age > 18]

df$age[df$age > 18 & df$age < 50]

y = df$age
y[y > 18 & y < 50]

df$age[df$status != "Poor"]  # filter one column based on another column

max(df$age[df$status == "Poor"])
mean(df$age[df$status == "Poor"])


# data frame merging ------

gender <- c("M", "F", "F", "M", "M", "F")
cbind(df, gender) # bind by column, number of df rows == length of vector

rbind(
  df, 
  c(7, 30, "Type2", "Excellent")
) # bind by row, number of df columns == length of vector


df1 <- data.frame(
  'c1' = 1:3,
  'c2' = c('a', 'b', 'c')
)
df2 <- data.frame(
  'c1' = 4:6,
  'c2' = c('d', 'e', 'f')
)

cbind(df1, df2)
rbind(df1, df2)

df2 = data.frame(
  ID = c(2,3,5), 
  gender = c("M", "F", "F")
)

merge(
  df, df2,
  by.x = "ID", by.y = "ID", all.x = TRUE
)  # identity column: ID


# data frame group and aggregation ------

aggregate(age~diabetes, df, FUN = mean)
aggregate(age~diabetes, df, FUN = max)
aggregate(age~status, df, FUN = mean)


### ------- Exercise with R - R basics -----------

# exercise 1. ------
# getting data into R and find data file here: 
#     data/Example_data.csv, use read.table() function
df <- read.table(
  # 
  # fill the parameters here
  "D:/FloodRiskSeminar/data/Example_data.csv",
  header = T, sep = ","
)


# exercise 2. ------
# How many days are there with discharge exceeding (>=) 8000? 

# solution 1: 
sum(df$discharge >= 8000)  # TRUE: 1; FALSE: 0

# solution 2:
df_subset <- df[df$discharge >= 8000, ] # filter the data frame based on discharge column 
dim(df_subset)   # result the dimension of the data frame, number of rows and cols

# solution 3:

discharge <- df$discharge  # access the discharge column

number = 0    # define a variable to count
for (dis in discharge) {  # iterate over the elements in discharge vector
  if (dis >= 8000) {
    number = number + 1 # increment number as long as one discharge value exceeding 8000
  }
}
number


# exercise 3. ------
# Which month has the most discharge days exceeding 8000? 

# solution 1: 
exceed8000 <- function(x) {
  # define a function here, 
  # x: the only parameter: a numeric vector
  # this function is designed to count the number of values exceeding 8000
  number = sum(x >= 8000)
  return(number)  # return the count
}

# data aggregation: group the data frame based on month variable, and then apply the function exceed8000
# to discharge column for each group and return a data frame
df_out <- aggregate(discharge ~ month, df, FUN = exceed8000)
df_out[order(df_out$discharge, decreasing = T),] # sort the data frame

# solution 2:
# use for loops

numbers <- NULL  # define a null vector, used to store the counts for 12 months
for (i in 1:12) { # iterate over each month (in total 12 months)
  df_subset <- df[df$month == i, ] # filter the data frame, with month only equal to month i
  numbers[i] <- sum(df_subset$discharge >= 8000) # adding value to the null vector;
}

# create a data frame from month vector and numbers vector
out <- data.frame(
  month = 1:12,
  numbers
)
out[order(out$numbers, decreasing = T), ]  # sort the data frame based on numbers column

# exercise 4. ------
# Derive the annual maximum discharge series 

aggregate(discharge~year, df, FUN = max)  # aggregation, group variable: year; operating variable: discharge



# exercise 5. ------
# Figure out in which day (date) the discharge reaches the maxima for each year 

# solution 1
range(df$year)
years <- seq(
  min(df$year),
  max(df$year),
  1
)

years <- seq(
  head(df$year, 1),
  tail(df$year, 1),
  1
)

years <- unique(df$year)


out <- data.frame(
  year = NULL,
  month = NULL,
  day = NULL,
  discharge = NULL
)

# split the data frame into subset dfs based on year column
# and the sort the subset df based on discharge column in decreasing order, 
# then retrieve the first row in the sorted subset df

for (i in 1:length(years)) {
  df_temp <- df[df$year == years[i], ]  # subset data frame for one specific year
  df_temp <- df_temp[order(df_temp$discharge, decreasing = T), ] # sorting the data frame based on discharge
  out <- rbind(out, head(df_temp, 1))   # get the first row of the sorted data frame 
}


# solution 2

df_max <- aggregate(discharge~year, df, FUN = which.max)
df_max$first_day <- as.Date(paste0(df_max$year, "-01-01"))
df_max$date_max <- df_max$first_day + df_max$discharge - 1


x <- c(5, 1, 3, 2, 1)
which.max(x)

x <- c(12, 9.0, 11, 20)
which.max(x)


as.Date("2024-05-24")  # convert character to date type
as.Date("2024-05-24") - 1  # get the date of yesterday 
as.Date("2024-05-24") + 5  # get the date 5 days after today

as.Date("1845-01-01") + 5  

as.Date("2024-05-24") - as.Date("2024-01-01")

# know more about date in R:
# https://bookdown.org/rdpeng/rprogdatascience/dates-and-times.html

