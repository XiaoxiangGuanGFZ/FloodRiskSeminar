# -- 
# flood risk seminar in Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 22.05.2022
# Last updated: 05.06.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)


# ------ built-in graphic tool -------------

# ------- basic graphs -----
# general plot function
?plot

iris  # built-in data frame object
head(iris)


# histogram 
hist(iris$Sepal.Length) # just give a numeric vector
?hist # see details on the parameters

# boxplot
boxplot(iris$Sepal.Length)
?boxplot

# scatter plot
plot(iris$Sepal.Length)

#line plot
# one vector for plotting, the position of the vector mapping to x-axis
plot(iris$Sepal.Length, type = "l")  

plot(x = iris$Sepal.Length,
     y = iris$Sepal.Width)

plot(iris$Sepal.Length,
     iris$Sepal.Width,
     type = "p",  # p: points; l: line plot, o: line and points
     col = 'red',
     xlab = "Sepal length",
     ylab = "Sepal width",
     main = "title of the plot here"
)

plot(iris$Sepal.Length,
     iris$Sepal.Width,
     type = "b",  # p: points; l: line plot, b: both lines and points
     col = 'blue',
     pch = 2,     # point character: 2 - void triangle
     xlab = "Sepal length",
     ylab = "Sepal width",
     main = "title of the plot here"
)

# multiple layers
range_x <- range(iris$Sepal.Length)  # the range of the column: min and max
range_y <- range(iris$Sepal.Width)

df1 <- iris[iris$Species == "virginica", ]
df2 <- iris[iris$Species == "versicolor", ]
df3 <- iris[iris$Species == "setosa", ]

plot(
  df1$Sepal.Length,
  df1$Sepal.Width,
  type = "p",  # plot type
  pch = 1,     # point character
  col = "blue",# color
  xlab = "Sepal length",  # label of x-axis
  ylab = "Sepal width",   # label of y-axis
  xlim = range_x,   # specify the x-axis and y-axis limits of a graph
  ylim = range_y
)

# points() function in R is used to 
#   add a group of points of specified shapes, 
#   size and color to an existing plot
points(
  df2$Sepal.Length,
  df2$Sepal.Width,
  type = "p",
  pch = 1,
  col = "red",
  xlab = "Sepal length",
  ylab = "Sepal width",
)

points(
  df3$Sepal.Length,
  df3$Sepal.Width,
  type = "p",
  pch = 1,
  col = "black",
  xlab = "Sepal length",
  ylab = "Sepal width",
)

legend(
  x = 6.5, y = 4.2,  # position to put the legend in the panel
  legend = c("virginica", "versicolor", "setosa"),
  pch = c(1, 1, 1),   ## Plot character or pch
  col = c("blue", "red", "black")
)


# ------ Probability functions -------
# d = density
# p = probability distribution function
# q = quantile function
# r = random number generation

# Distribution Abbreviation: 
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

x <- seq(-3, 3, 0.01)  # sequence function, 

y <- dnorm(x, mean = 0, sd = 1)
y2 <- pnorm(x, mean = 0, sd = 1)

plot(x, y2, type = "l", col = "red", xlab = "random variable", ylab = "PDF/CDF")
points(x, y, type = "l")

# pnorm gives the distribution function, 
pnorm(1.96, mean = 0, sd = 1)
pnorm(c(-1, -0.5, 0, 0.5, 1), mean = 0, sd = 1)

# qnorm gives the quantile function,
qnorm(.9, mean = 500, sd = 100)
qnorm(.9, mean = 0, sd = 1)

# rnorm generates random deviates.
# parameter: the number of random value you want to generate
rnorm(10, mean = 0, sd = 1)  # generate 10 normally distributed random values  
rnorm(50, mean = 0, sd = 1)
rnorm(50, mean = 50, sd = 10)

runif(20, min = 0, max = 1)  # generate uniformly distributed random values  
runif(20, min = 1, max = 2)  



# ------------- flood frequency analysis -------------

# install packages required in this project
# install.packages("extRemes")  # package for extreme value statistics
library(extRemes)  # load the package (objects,functions and codes)
# to R environment to be applied.

# -- read in data
df <- read.table(
  # fill the parameters
  "D:/FloodRiskSeminar/data/Example_data.csv",
  header = T, sep = ','
)

# extract annual maximum discharge with aggregate()
AMS = aggregate(discharge ~ year, data = df, FUN = max)

# GEV fitting --------

# fit the GEV by using fevd() function in `extRemes` package
GEV_mle = fevd(AMS$discharge, # extreme variable
               method = "MLE", # parameter estimation method
               type = "GEV"  # probability distribution type
               )
# the result GEV_mle is a list
GEV_mle  # check the results of GEV fitting 
str(GEV_mle)
length(GEV_mle)  # the number of elements in this list
GEV_mle$method   # access elements in the list with $ operator
GEV_mle$x

GEV_mle$results$par  # the estimated 3 GEV parameters

paras <- GEV_mle$results$par  # the three parameters in GEV
str(paras)    # check the structure of this object, named numeric vector
names(paras)  # name attribute of this vector
paras["location"]
paras[1]

paras <- `names<-`(paras, NULL)  # rename the vector


revd(100, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

pevd(8000, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

qevd(0.5, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

devd(6000, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")


# ----------------- Exercise ------------------
# 
# 1. get data into R: ./data/Example_data.csv, use read.table() function

df <- read.table(
  # fill the parameters
  "D:/FloodRiskSeminar/data/Example_data.csv",
  header = T, sep = ','
)


# 2. Derive the annual maximum discharge (AMS) series: aggregate() 


# extract annual maximum discharge with aggregate()
AMS = aggregate(discharge ~ year, data = df, FUN = max)


# 3. Estimate the GEV parameters for AMS: fevd()


# GEV fitting --------

# fit the GEV by using fevd() function in `extRemes` package
GEV_mle = fevd(AMS$discharge, # extreme variable
               method = "MLE", # parameter estimation method
               type = "GEV"  # probability distribution type
)

paras = GEV_mle$results$par

# 4. Derive the 100-year discharge

return_period <- 100
probability <- 1 - 1 / rp  ## CDF

qevd(probability, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")


# 5. What is the return period of the discharge 10000 m3/s?


probability = pevd(10000, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

probability
1 / ( 1- probability)   # return period



# 6. Plot the empirical frequency curve and estimated GEV curve together: 
#    using plot() and points()

AMS$m_rank = rank(AMS$discharge) # rank of the values
AMS$P = round(ams$m_rank / (1 + dim(AMS)[1]), 4) # empirical probability
AMS$ReturnPeriod_T = round(1 / (1 - AMS$P), 1) # # empirical return period
AMS

plot(x = AMS$ReturnPeriod_T, y = AMS$discharge)

gene_rp <- seq(1.1, 500, 0.5)
gene_p <- 1 - 1/ gene_rp

gene_p <- seq(0.00001, 0.99, 0.00001)
gene_rp <- 1/ (1 - gene_p)
gene_q <- qevd(gene_p, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

plot(
  x = AMS$ReturnPeriod_T, y = AMS$discharge,
  xlab = "return period [year]",
  ylab = "discharge [m3/s]"
)
points(x = gene_rp, y = gene_q, col = "red", type = "l")




# 7. Split the AMS series into two parts: a) 1845-1925 and b)1926-2004: 
#   estimate the GEV parameters for this two periods respectively
#   estimate the 100-year discharges for these two different historical periods, compare them  
#   plot the estimated (two) GEV curves for these two different historical periods 





