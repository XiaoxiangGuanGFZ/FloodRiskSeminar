# -- 
# flood risk seminar in Summer Semester 
# R project language concepts and fundamentals
# 
# Created: 22.05.2022
# Last updated: 09.05.2024
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)
# ---


### ------ Introduction to R ------


### ------ Data types and structures in R ------
## ---- Data types ----
x = 10.1
x

x <- 10.1
x

10.1 -> x
x
  
is.numeric(x)
is.integer(x)  # is it a integer number?
is.character(x)
is.logical(x)

# test: x = 10

# type conversion
as.character(10)
as.numeric('1.52') # convert from character to number
as.numeric('xs')   # failed
as.integer(10.54)  # convert to integer
as.logical(5)      # TRUE: as long as it is a non-zero numerical value 

# convert boolean to integer 

as.numeric(TRUE)
as.numeric(FALSE)

1 + TRUE

2 - TRUE * 2

2 - FALSE

## ----- Data structures in R --------

# Vector ------
x <- 0.2  # a scalar is one-element vector
x <- TRUE
x <- "pass"

a <- c(1, 2, 5, 3, 6, -2, 4)  # <- assignment sign; numeric 
a
b <- c(TRUE, FALSE, TRUE)
c <- c("Name1", "Name2")

d <- 1:5  # a numeric sequence from 1 to 5 with a step of 1
d <- seq(1, 10, by = 2)

# element indexing: access specific elements in the vector
a[1]  # access the first element in vector a
a[1:3]  # access the first 3 elements in a
a[c(1, 3, 5, 7)]  # access multiple elements in the vector with position vector c(1, 3, 5, 7)

b <- c("one", "two", "three") # character
b
b[c(1, 3)]

c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE) # Boolean or logical
c

d <- 1:20
d

e <- seq(from = 1, to = 20, by = 2) # sequence
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
patientdata$age   # access specific column with $ sign
patientdata$diabetes

patientdata$diabetes[1:2]  # first 2 elements in "diabetes" column

# access data (elements) with [] operator
patientdata[, 1]  # first column
patientdata[1, ]  # first row
patientdata[1, 1] # element located at 1,1 
patientdata[1:2, 2] # the first 2 elements in the 2nd column
patientdata[1, 1:2] # the first 2 elements in the 1st row
patientdata[1:1, 1:2] # subset 


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

?read.table
# the parameters in read.table
head(df, 6)
tail(df, 6)

# export / save a data frame into a text file
write.table(
  df, 
  file = 'D:/test.csv')

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


#### ------ Function and loops --------

# basic functions ----
length(c(1,2,3,4))  # Gives the number of elements/components.

dim(df)  # Gives the dimensions of an object.
str(df)  # Gives the structure of an object.

# mathematical ----
1 + 1
c(1,2,3,4,5) + 1  # the operations are performed in a vectorized manner
c(1,2,3,4,5) + c(1,2,3,4,5)  # with the same element numbers

c(1,2,3,4) + c(1,2) # it is valid, but dangerous; vectors with different length

3 - 2 
1 * 9 
c(1,2,3,4,5) * 2
9 / 3 
9 / c(3, 2, 1)
2 ^ 3  # Exponentiation

2 ^ c(1,2,3,4)

abs(-0.1253)  # Absolute value
sqrt(4) # square root 

x = 12.23134
n = 2
round(x, digits=n) # Rounds x to the specified number (n) of decimal places

cos(x); sin(x); tan(x)  # Trigonometric functions: cosine, sine, and tangent

log(x)  # the natural logarithm
exp(x)  # Exponential function

# statistical ----
x <- c(0.1, 1, 1.2, 0.25, 0.56, 5)
mean(x) # Mean
median(x) # Median
sd(x) # Standard deviation
var(x) # Variance

range(x) # Range, min-max
sum(x)
max(x)
min(x)

max(patientdata$age)
mean(patientdata$age)
min(patientdata$age)


sort(x, decreasing = F)
sort(x, decreasing = T)

# character functions ----
x <- "xjhadnc_102"
nchar(x) # Counts the number of characters of x.

paste('hjjx', ';', '125', 1425, sep = "")  # string concatenation
paste('hjjx', ';', '125', 1425, sep = "-") 


### ------- Exercise with R - R basics -----------

# exercise 1. ------
# getting data into R properly and find data file here: 
#     data/Example_data.csv, use read.table() function;
#     hint: take care of the parameters. first row as column name and the columns are separated by comma ","
df <- read.table(
  # 
  # fill the parameters here
)


# exercise 2. ------
# Check the dimensions (or shape) of the imported data
#   number of rows and columns




# exercise 3. ------
# How many years this data set covers? Starting year? End year?






# exercise 4. ------
# Derive the maximum, minimum, mean value, standard deviation of the discharge column




# exercise 5. ------
# Derive the maximum, minimum, mean value, standard deviation 
#     of the first 365 elements in discharge column
#hint: here we would have to extract the first 365 elements (first year) in discharge column




# exercise 6. ------
# Export the first 365 rows of df into text file
#     with write.table() function
#     requirement: first row as the column names; 
#     rownames are not needed, columns are separated by ";" sign 
#     give the output file name as "output"
#     don't forget the file extension: .csv or .txt




