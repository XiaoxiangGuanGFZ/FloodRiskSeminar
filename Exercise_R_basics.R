# -- 
# flood risk seminar in 2022 Summer Semester 
# R project language concepts and fundamentals
# 
# Created: 22.05.2022
# Last updated: 
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)

### Introduction to R ----

# --- work space ----

getwd() # List/get/print the current working directory.

setwd('D:/FloodRiskSeminar/')  #Changes the current working directory
ls()   # Lists the objects in the current work space
rm(objectlist)  # Removes (deletes) one or more objects.
# --- packages
.libPaths()  # get where your library is located in the machine
library()  # shows you what packages you’ve saved in your library
search() # which packages are loaded and ready to use

install.packages('ggplot2') # install packages(s) into R environment
install.packages("extRemes")

library('ggplot2') # Load a package into this working environment

help(package="ggplot2") 
help(package="extRemes") 

# provides a brief description of the package and an
# index of the functions and datasets included

# --- getting help in R ----

help("iris") #Help on function foo (quotation marks optional)
?iris
example('max') # Examples of function max (quotation marks optional)

# Lists all available example datasets contained in currently loaded packages
data('iris') 

apropos("str", mode="function") #Lists all available functions with str in their name


### Data and variable ----
# --- data structure ----
# ---- Vector ----
a <- c(1, 2, 5, 3, 6, -2, 4)  # <- assignment sign; numeric 
b <- c("one", "two", "three") # character
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE) # Boolean or logical
c(1,2,4,5,8,7,4,10) -> d  # -> assignment sign, not suggested

e <- 1.25  # a scalar is one-element vector
f <- 1:20
g <- seq(from = 1, to = 20, by = 1)

# ---- Matrices ----
# A matrix is a two-dimensional array 
# in which each element has the same mode (numeric, character, or logical)

y <- matrix(data = 1:20, nrow=5, ncol=4, byrow = T) # produce matrix from a vector
?matrix 

# ---- Array ----
# Arrays are similar to matrices but can have more than two dimensions
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(data = 1:24, 
           dim = c(2, 3, 4), 
           dimnames = list(dim1, dim2, dim3)
           ) # array(vector, dimensions, dimnames)
z

# ---- data frame ----
# A data frame is more general than a matrix in 
# that different columns (vectors) can contain different modes of data

df <- data.frame(
  'column1' = c(1, 2, 3),
  'column2' = c('a', 'b', 'c'),
  'column3' = c(TRUE, FALSE, TRUE)
)

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata
patientdata$age
patientdata$diabetes

dim(patientdata)
dim(df)

colnames(patientdata)
colnames(df)

row.names(patientdata)
row.names(df)

iris
str(iris)
colnames(iris)
dim(iris)

# ---- list ----
# a list is an ordered collection of objects (components)
list_example <- list(
  'l1' = 1.25,
  'l2' = c(1,2,3,4,5),
  'l3' = data.frame('c1' = c(1,3,5), 'c2' = c(2,4,6)),
  'l4' = list(0)
)

# --- data type and mode conversion ----
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
as.numeric(c('xs', 'c0', ''))
as.integer(10.54)
as.logical(c(1, 0, 4, -1, 1.2, 0))

x1 <- c(1,2,3)
x2 <- c('a', 'b', 'c')
x1 + x2

c(x1, x2) # c(as.character(x1), x2)
as.numeric(c(TRUE, TRUE, FALSE))
sum(c(TRUE, TRUE, FALSE))
x1 + c(TRUE, TRUE, FALSE)

# --- data input and output ----
# ---- from keyboard ----
data_example <- c() # data.frame
# ---- from text file ----

# import data as a data frame from a text file
df <- read.table(
  file = 'D:/FloodRiskSeminar/FloodRisk/data/Chitan-data-PEQ.csv', # filepath + file name
  header = TRUE,
  sep = ','
)
head(df, 6)
tail(df, 6)

# export / save a data frame into a text file
write.table(
  df, 
  file = '',
  col.names = TRUE, row.names = FALSE, quote = F, append = F, sep = ','
)
# ---- from Rdata (built-in data respiratory ) ----
save(df, file = 'D:/exercises.Rdata')
remove(df)

load(file = 'D:/exercises.Rdata')
# ---- from Excel ----
install.packages('xlsx')
library(xlsx)
workbook <- "D:/myworkbook.xlsx"
mydataframe <- read.xlsx(workbook, 1)

# ---- from others ----
# from netCDF, grib2,
# specifically designed packages required

# --- functions ----

length()  # Gives the number of elements/components.

dim(df)  # Gives the dimensions of an object.
str(df)  # Gives the structure of an object.
mode(df)  # Determines how an object is stored.
df_new <- edit(df)  # Edits object and saves it as newobject.
fix(df)   # Edits an object in place

### Calculation and computation ----

# --- mathematical ----
1 + 1
c(1,2,3,4,5) + 1  # the operations are performed vectorized
c(1,2,3,4,5) + c(1,2,3,4,5)
c(1,2,3,4) + c(1,2)

3 - 2 
1 * 9 
c(1,2,3,4,5) * 2
9 / 3 
9 / c(3, 2, 1)
2 ^ 3  # Exponentiation.
2 ** 3 # Exponentiation.

2 ^ c(1,2,3,4)
9 %% 4  # Modulus (x mod y)
5 %/% 2  # Integer division


abs()  # Absolute value
sqrt() # square root 
ceiling(x)  # Smallest integer not less than x
floor(x) # Largest integer not greater than x
round(x, digits=n) # Rounds x to the specified number of decimal places
cos(x); sin(x); tan(x)  # Cosine, sine, and tangent
acos(x); asin(x); atan(x) # Arc-cosine, arc-sine, and arc-tangent
log(x,base=n)  # Logarithm of x to the base n
log(x) # the natural logarithm.
log10(x) # the common logarithm.
exp(x)  # Exponential function

# --- statistical ----
x <- runif(10, 0, 2)
mean(x) # Mean
median(x) #Median
sd(x) # Standard deviation
var(x) # Variance
quantile(x, probs) 
quantile(x, 0.5)
quantile(x, 0.9)

# Quantiles where x is the numeric vector, where quantiles are desired and
# probs is a numeric vector with probabilities in [0,1]

range(x) # Range
sum(x)
max(x)
min(x)

sort(x, decreasing = F)
sort(x, decreasing = T)
rank(x)

# --- Probability functions ----
# d = density
# p = probability distribution function
# q = quantile function
# r = random generation (random deviates)
# Distribution Abbreviation
# Beta - beta 
# Logistic - logis
# Binomial - binom 
# Multinomial - multinom
# Cauchy - cauchy 
# Negative binomial - nbinom
# Chi-squared - chisq 
# Normal - norm
# Exponential - exp 
# Poisson - pois
# Uniform - unif

x <- seq(-3, 3, 0.01)
y <- dnorm(x)
plot(x, y)

pnorm(1.96)
qnorm(.9, mean=500, sd=100)

rnorm(50, mean=50, sd=10)

runif(20, min = 0, max = 2)

# --- character functions ----
nchar(x) # Counts the number of characters of x.
substr(x, start, stop) # Extracts or replaces substrings in a character vector.

sub(pattern, replacement,
    x, ignore.case=FALSE )
# Finds pattern in x and substitutes the replacement text.
sub('h', 'H', 'h1245')

strsplit(x, split) # Splits the elements of character vector x at split.
strsplit('sd_sd1_214', '_')

paste('...', 'hxgb', '125', 1425, sep="")

# --- Customized functions ----
myFunc <- function(parameters) {
  # calculation formula or processes
  out = ...
  return(out)
}

myfunc <- function(x1,x2) {
  y = (x1 + x2) * (x1 - x2)
  return(y)
}


### Data management ----

# --- control show ----

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

x1 > x2 & x3 < x4  # &: and
x1 <= x2 & x3 <= x4
x1 > x2 | x3 <= x4  # |: or 
x1 <= x2 | x3 <= x4

x1 <= x2
!(x1 <= x2)  # !: exclamation mark: not 
# ---- conditional execution ----
if (x1 >= x2) {
  y = x1 - x2
} else {
  y = x2 - x1
}

# ifelse(cond, statement1, statement2)
# The ifelse construct is a compact and vectorized version of the if-else construct
score = 75
ifelse(score > 60, "Passed", "Failed")

score = c(55, 60, 75, 80)
ifelse(score >= 60, "Passed", "Failed")

# ---- repetition and looping ----
Ids <- c(1,2,3,4,5)
for (id in Ids) {
  print(id)
}

# --- variable creation, naming ----
# creating numeric vectors
x <- NULL
x <- 0
x <- c(1,2,3,4,5,6)
x <- runif(10)
x[1]
x[2] <- 0
x[11] <- 11
x[13] <- 13
c(x, c(1.2,2.3,5.4,0.3) )
# creating logical vectors
x <- c(TRUE, FALSE, FALSE)
c(x, TRUE)
x[4] <- TRUE
c(x, c(TRUE, FALSE))

x1 <- runif(10, min = 0 ,max = 1)
x1 < 0.5
x1 > 0.5

# creating character vectors
x <- c('a', 'b', 'c')
as.character(c(1,2,3))

x[4]<- 'd'
c(x, 'e', 'd')
paste0('s', 'd')

years = 1999:2020
paste0('Y', years)

# creating data frame (from multiple vectors)
# import from text files
# create by keyboard

colnames(df)
colnames(df) <- c('y', 'm', 'd', 'P', 'Ep', 'Q')
fix(df)


# --- missing value ----
# NA : not available
x <- c(1, 2, NA, 3, 4, 5, 6, -99.9)
is.na(x)
x[8] <- NA

sum(x)
mean(x)

sum(x, na.rm = TRUE)
mean(x, na.rm = TRUE)

1 / 0
log(-10)  # NaN: not a number

is.infinite( 1 / 0)
is.nan( log(-10) )

na.omit(x)


# ---- filtering and sub-setting ----

x1 <- c(1.0, 0, -2.2, 2.5, -5.1, 6, 4.15, -1.5, 3.2)
length(x1) 
# select by index
x1[1]
x1[6]
# select by an index vector
x1[1:6]
x1[c(1, 3, 5, 7, 9)]
# select by logical expression
x1[x1 > 0]
x1[x1 > mean(x1)]
x1[x1 > 0 & x1 < 5]

x1[-2]  # exclude the 2nd value
# filter a data frame with two dimension
# select by a index
df[1, 1]
df[6, 5]
# select one row of data (observation) by index
df[1, ]
# select on columm (field) by index
df[, 1]

# select by name
colnames(df)
row.names(df)
df[1, 'rainfall']
df['1', 'rainfall']

df[1, 'evaporation']
df['1', 'evaporation']

# multiple selecting
df[1:6, 4]
df[1, 4:6]

df[1:6, 4:6]
df[1:6, c('rainfall', 'evaporation', 'discharge')]
df[c('1', '2', '3'), c('year', 'rainfall')]

# select column(s) by name(s)
df$rainfall  # get a vector; 
df$year   # then all select methods for vectors are also valid
df$discharge

df$rainfall[1]
df$rainfall[1:6]
df$rainfall[df$rainfall > 0]  # all rainy days
df$rainfall[df$rainfall > 0 & df$year == 2000]  # all rainy days in 2000
df$rainfall[df$rainfall > 0 & df$rainfall < 10]

# filtering a data frame by column-based conditions

df[df$rainfall > 0 & df$year == 2000, ]
df[df$rainfall > 0, ]
df[df$rainfall > 0 & df$rainfall < 1, ]
df[df$rainfall > 30 & df$discharge > mean(df$discharge), ]

# ---- creating new columns or remove column(s)----

df$new_column <- seq(1, dim(df)[1])
df$ratio_P_Ep <- df$rainfall / df$evaporation
df$ratio_P_Q <- df$rainfall / df$discharge

df[, -7]
df[, !(colnames(df) %in% c('new_column'))]

# ---- merging data ----
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

# ---- aggregating data ----
aggregate(rainfall ~ year, data = df, FUN = sum)
aggregate(discharge ~ year, data = df, FUN = mean)

rainydays <- function(x) {
  return(sum(x>0))
}

aggregate(rainfall ~ year, data = df, FUN = rainydays)
aggregate(rainfall ~ month + year, data = df, FUN = rainydays)


### graph drawing ----
# --- basic graphs ---

# histogram
hist(iris$Sepal.Length )
hist(iris$Sepal.Width )

hist(iris$Sepal.Length,
     xlab = 'Sepal length', 
     ylab = 'Frequency', 
     main = 'Histogram of Sepal length')


# boxplot
boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length,
        col = 'red',
        lty = 3,
        ylab = 'Sepal length')

# general plot function
?plot
# scatter plot
plot(iris$Sepal.Length)
plot(iris$Sepal.Length, cex = 2)

plot(iris$Sepal.Length, type = 'p')
plot(iris$Sepal.Length, type = 'p', col = 'red')
plot(iris$Sepal.Length, iris$Sepal.Width,
     type = 'p', 
     pch = 1,
     col = 'red',
     xlab = 'Sepal Length',
     ylab = 'Sepal width')

plot(iris$Sepal.Length, iris$Petal.Length,
     type = 'p', col = 'red',
     xlab = 'Sepal Length',
     ylab = '')

points(iris$Sepal.Length, iris$Petal.Width,
       type = 'p', col = 'blue')

legend(x = 4.5, y = 6, c('Petal length', 'Petal width'),
       col = c('red', 'blue'),
       pch = c(1,1), title = 'legend')

# line plot

x <- seq(-3, 3, 0.01)
y <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 1, sd = 1)
plot(x, y, type = 'l')
plot(x, y2, type = 'l')

plot(x, y, type = 'l', 
     lty = 1,
     col = 'red',
     ylab = 'PDF')
points(x,y2, type ='l',
       lty = 2,
       col = 'blue')
legend(x = -2.5, y = 0.3,
       c('mean=0, sd=1', 'mean=1, sd=1'),
       col = c('red', 'blue'),
       lty = c(1,2),
       title = 'parameters'
       )
### Exercises ----

# import data into R environment from text file
df <- read.table(
  # fill the parameters
)

# --- 1. what is the maximum daily discharge in 2000
# tip: select the data of 2000; find the maximum



# --- 2. find all daily discharge values larger than 2000 (or 1500)



# --- 3. calculate the total rainfall volume for each month (12 months) in 2000
# tip: filter the data set to 2000, compute the summation for each month



# --- 4. find the annual maximum daily discharge for each year in the data set
# tip: how many years does the data set cover? 



# --- 5. inter-year distribution pattern of rainfall volume 
# tip: mean annual accumulated rainfall volume for each month


