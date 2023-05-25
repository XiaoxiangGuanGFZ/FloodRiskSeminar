# -- 
# flood risk seminar in Summer Semester 
# R project language concepts and fundamentals
# 
# Created: 22.05.2022
# Last updated: 25.05.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)
# ---


### ------ Introduction to R ------
#
# work space --------

getwd() # List/get/print the current working directory.

setwd('D:/FloodRiskSeminar/')  #Changes the current working directory
ls()   # Lists the objects in the current work space
rm(objectlist)  # Removes (deletes) one or more objects.

# packages -------
install.packages('ggplot2') # install packages(s) into R environment
install.packages("extRemes")

library('ggplot2') # Load a package into this working environment

help(package="ggplot2") 
help(package="extRemes") 

# provides a brief description of the package and an
# index of the functions and datasets included

# getting help in R --------

help("iris") #Help on function foo (quotation marks optional)
?iris
# Lists all available example datasets contained in currently loaded packages
data('iris') 


### ----- Data structures in R --------

# Vector ------
x <- 0.2  # a scalar is one-element vector
x <- TRUE
x <- "pass"

a <- c(1, 2, 5, 3, 6, -2, 4)  # <- assignment sign; numeric 
a

b <- c("one", "two", "three") # character
b

c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE) # Boolean or logical
c

d <- 1:20
d

e <- seq(from = 1, to = 20, by = 1)
e

# Matrices ------
# A matrix is a two-dimensional array 
# in which each element has the same mode (numeric, character, or logical)

y <- matrix(data = 1:20, nrow=5, ncol=4, byrow = T) # produce matrix from a vector
y

# Array ------
# Arrays are similar to matrices but can have more than two dimensions
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(data = 1:24, 
           dim = c(2, 3, 4), 
           dimnames = list(dim1, dim2, dim3)
           ) # array(vector, dimensions, dimnames)
z

# data.frame ----
# A data frame is more general than a matrix in 
# that different columns (vectors) can contain different modes of data

df <- data.frame(
  'column1' = c(1, 2, 3),
  'column2' = c('a', 'b', 'c'),
  'column3' = c(TRUE, FALSE, TRUE)
)  # create a data.frame from scratch

# create data.frame from a couple of vectors
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

# refer to the individual column in the data.frame
patientdata$age
patientdata$diabetes

patientdata[, 1]  # first column
patientdata[1, ]  # first row
patientdata[1, 1] # element located at 

# some features about df
dim(patientdata)  # dimensions 
dim(df)

colnames(patientdata)  # column names
colnames(df)

row.names(patientdata)  # row names
row.names(df)

iris  # the built-in data set from R project
colnames(iris)
dim(iris)

# list --------
# a list is an ordered collection of objects (components)
list_example <- list(
  # here the list has 4 components
  'l1' = 1.25,
  'l2' = c(1,2,3,4,5),
  'l3' = data.frame('c1' = c(1,3,5), 'c2' = c(2,4,6)),
  'l4' = list(0)
)

# data types and conversion ----
x = 10.1
is.numeric(x)
is.integer(x)
is.character(x)
is.logical(x)
is.double(x)
is.vector(x)
is.matrix(x)
is.data.frame(x)

as.character(10)
as.numeric(c('10', '1.52', '4.6')) 
as.numeric(c('xs', 'c0', ''))  # failed
as.integer(10.54)
as.logical(c(1, 0, 4, -1, 1.2, 0))  

x1 <- c(1,2,3)
x2 <- c('a', 'b', 'c')
x1 + x2  # fail

as.numeric(c(TRUE, TRUE, FALSE))
sum(c(TRUE, TRUE, FALSE))
1 + c(TRUE, TRUE, FALSE)

#### ---- getting data into R ----------
# data input and output
# from keyboard ------
data_example <- c() # data.frame

# from text file ------
# import data as a data frame from a text file
df <- read.table(
  file = 'D:/FloodRiskSeminar/data/Example_data.csv', # filepath + file name
  header = TRUE, sep = ','
)
?read.table
# the parameters in read.table
head(df, 6)
tail(df, 6)

# export / save a data frame into a text file
write.table(
  df, 
  file = '',
  col.names = TRUE, row.names = FALSE, quote = F, append = F, sep = ','
)

# from Excel --------
install.packages('xlsx')
library(xlsx)
workbook <- "D:/myworkbook.xlsx"
mydataframe <- read.xlsx(workbook, 1)


#### ------ Function and loops --------

# basic functions ----
length()  # Gives the number of elements/components.

dim(df)  # Gives the dimensions of an object.
str(df)  # Gives the structure of an object.

# mathematical ----
1 + 1
c(1,2,3,4,5) + 1  # the operations are performed vectorizedly
c(1,2,3,4,5) + c(1,2,3,4,5)

3 - 2 
1 * 9 
c(1,2,3,4,5) * 2
9 / 3 
9 / c(3, 2, 1)
2 ^ 3  # Exponentiation

2 ^ c(1,2,3,4)
9 %% 4  # Modulus (x mod y)
5 %/% 2  # Integer division


abs()  # Absolute value
sqrt() # square root 
ceiling(x)  # Smallest integer not less than x
floor(x) # Largest integer not greater than x
n = 0
round(x, digits=n) # Rounds x to the specified number (n) of decimal places
cos(x); sin(x); tan(x)  # Cosine, sine, and tangent

log(x,base=n)  # Logarithm of x to the base n
log(x) # the natural logarithm.
log10(x) # the common logarithm.
exp(x)  # Exponential function

# statistical ----
x <- c(0.1, 1, 1.2, 0.25, 0.56, 5)
mean(x) # Mean
median(x) # Median
sd(x) # Standard deviation
var(x) # Variance

range(x) # Range
sum(x)
max(x)
min(x)

sort(x, decreasing = F)
sort(x, decreasing = T)

# character functions ----
x <- "xjhadnc_102"
nchar(x) # Counts the number of characters of x.
substr(x, start, stop) # Extracts or replaces substrings in a character vector.
substr(x, 1, 4)  # the first 4 letters

strsplit(x, split) # Splits the elements of character vector x at split.
strsplit('sd_sd1_214', '_')

paste('...', 'hxgb', '125', 1425, sep="")  # string concatenation

# Customized functions ----
myFunc <- function(parameters) {
  # calculation formula or processes
  # command chunk
  out = ...
  
  return(out)
}

myfunc <- function(x1,x2) {
  # give two numeric values x1, x2
  y = (x1 + x2) * (x1 - x2)
  return(y)
}
myfunc(1.0, 2.0)

# --- control structure ----
# Logical operators
1 < 3
1.2 <= 2.0
2 > 3
1.25 >= 2

1.1 != 1

c(1,2,1,3,2.5) > 1.5

1 %in% c(1,2,3,1.2)
'xs' %in% c('XS', 'X')

x1 <- 1.5
x2 <- 2.5
x3 <- 1.6
x4 <- 0.6

#  Logical Operators
x1 > x2 & x3 < x4   # &: and
x1 <= x2 & x3 <= x4 # &: and
x1 > x2 | x3 <= x4  # |: or 
x1 <= x2 | x3 <= x4 # |: or

x1 <= x2
!(x1 <= x2)  # !: exclamation mark: not 

# ---- if-else structure ----
if (x1 >= x2) {
  y = x1 - x2
} else {
  y = x2 - x1
}


# ---- loops -------
Ids <- c(1, 2, 3, 4, 5)
for (id in Ids) {
  print(id)
}

id = 1
while (id <= 5) {
  print(id)
  id = id + 1
}

#### ---- Data manipulation -----

# variable creation, adding, altering ----
# creating numeric vectors
x <- NULL
x <- 0
x <- c(1,2,3,4,5,6)
x[1]
x[2] <- 0
x[5] <- 11

# creating logical vectors
x <- c(TRUE, FALSE, FALSE)
x[2] <- TRUE
c(x, c(TRUE, FALSE))

x1 <- c(0.1, 1, 1.2, 0.25, 0.56, 5)

x1 < 0.5
x1 > 0.5

# creating character vectors
x <- c('a', 'b', 'c')
as.character(c(1,2,3))

x[1]<- 'd'
c(x, 'e', 'd')  # combine
paste0('s', 'd')  # paste, or concatenate together

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

colnames(df)
colnames(df) <- c("c1", "c2", "c3", "c4")  # rename the columns
colnames(df) <- c("patientID", "age", "diabetes", "status")  # rename the columns

# filtering and sub-setting ----

x1 <- c(1.0, 0, -2.2, 2.5, -5.1, 6, 4.15, -1.5, 3.2)

# select by index
x1[1]
x1[6]

# select by an index vector
x1[1:6]
x1[c(1, 3, 5, 7, 9)]

# select by logical expression
x1[c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,FALSE, FALSE, FALSE)]

x1[x1 > 0]
x1[x1 > mean(x1)]
x1[x1 > 0 & x1 < 5]

x1[-2]  # exclude the 2nd value, 

# filter a data frame with two dimension
# select by a index
df[1, 1]
df[3, 2]

# select one row of data (observation) by index
df[1, ]

# select on columm (field) by index
df[, 1]

# select by name
colnames(df)
row.names(df)
df[1, 'age']
df['1', 'status']

# multiple selecting
df[1:2, 1]
df[1, 2:3]

df[1:2, 2:3]
df[2:4, c('patientID', 'age')]

# select column(s) by name(s)

df$age   # then all select methods for vectors are also valid

df$age[1]
df$age[2:3]
df$age[df$age > 18]  # all rainy days
df$age[df$age > 18 & df$age < 50]  # all rainy days in 2000

df$age[df$status == "Poor"]

length(df$age[df$status == "Poor"])
max(df$age[df$status == "Poor"])
mean(df$age[df$status == "Poor"])

# merging data ----
df1 <- data.frame(
  'c1' = 1:3,
  'c2' = c('a', 'b', 'c')
)
df2 <- data.frame(
  'c1' = 4:6,
  'c2' = c('d', 'e', 'f')
)

cbind(df1, df2)  # bind by column
rbind(df1, df2)  # bind by row

cbind(df1, c(4,5,6)) # bind a data frame with a vector (with the same dimension)
rbind(df1, c(4,'d')) 


### ------- Exercise with R - R basics -----------

# exercise 1. ------
# getting data into R and find data file here: 
#     data/Example_data.csv, use read.table() function
df <- read.table(
  #
  # fill the parameters here
)


# exercise 2. ------
# Check the dimensions of the imported data




# exercise 3. ------
# How many years this data set covers? Starting year? End year?




# exercise 4. ------
# Is there any missing data? (-99.9 indicates the missing value). 
#     If there is, how many missing values?



# exercise 5. ------
# Derive the annual maximum discharge series 




# exercise 6. ------
# Estimate the mean value, standard deviation of annual maximum discharge series




# exercise 7. ------
# Export the annual maximum discharge series into text file (.csv or .txt format) 
#     with write.table() function





