# -- 
# flood risk seminar in Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 22.05.2022
# Last updated: 25.02.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)

# typical data analysis procedure in R project 
# 1. data import
# 2. data check and data clean
# 3. data summary and management
# 4. model the data
# 5. display the data
# 6. presentation

# first create project folder and save there the data file in subfolder "Data"
# then File --> New Project --> Existing directory

# getwd() should give now the project directory
wd <- 'D:/FloodRiskSeminar/data/'
setwd(wd)

# install packages required in this project
# install.packages("extRemes")  # package for extreme value statistics
library(extRemes)  # load the package (objects,functions and codes)
# to R environment to be applied.

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

#### data.frame manipulation -------
# filtering a data frame by column-based conditions

df[df$rainfall > 0 & df$year == 2000, ]
df[df$rainfall > 0, ]
df[df$rainfall > 0 & df$rainfall < 1, ]
df[df$rainfall > 30 & df$discharge > mean(df$discharge), ]

# creating new columns or remove column(s)----

df$new_column <- seq(1, dim(df)[1])
df$ratio_P_Ep <- df$rainfall / df$evaporation
df$ratio_P_Q <- df$rainfall / df$discharge

df[, -7]
df[, !(colnames(df) %in% c('new_column'))]

# aggregating data ----
aggregate(rainfall ~ year, data = df, FUN = sum)
aggregate(discharge ~ year, data = df, FUN = mean)

rainydays <- function(x) {
  return(sum(x>0))
}

aggregate(rainfall ~ year, data = df, FUN = rainydays)
aggregate(rainfall ~ month + year, data = df, FUN = rainydays)


### ------ graphic tool ----

# basic graphs -----
# histogram -----
hist(iris$Sepal.Length )
hist(iris$Sepal.Width )

hist(iris$Sepal.Length,
     xlab = 'Sepal length', 
     ylab = 'Frequency', 
     main = 'Histogram of Sepal length')


# boxplot -------
boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length,
        col = 'red',
        lty = 3,
        ylab = 'Sepal length')

# general plot function
?plot
# scatter plot ------
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

# line plot -----

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


################################ Data preparation ##############################

# read (import) data from file
DailyDischarge = read.table(file = "EXAMPLE_Koeln_Q_daily.txt", 
                            header = TRUE)
head(DailyDischarge)  # check the data
# View(DailyDischarge)

# extract annual maximum discharge
DailyDischarge$year = as.numeric(substr(DailyDischarge$date, 1,4))
AMS = aggregate(value~year, data = DailyDischarge, FUN = max)
head(AMS)  # check the results
# write.table(
#   t(AMS),
#   file = 'D:/AMS.csv',
#   col.names = F, row.names = F, quote = F, sep = ','
# )
# then how to extract all the values over a specified threshold? (peak-over-threshold)

################################~Data preparation~##################################

colnames(AMS) <- c('year', 'value')
AMS$m_rank = rank(AMS$value) # rank of the values
AMS$P = round(AMS$m_rank / (1 + dim(AMS)[1]), 4) # empirical probability
# dim(AMS)[1]: the first dimension of data frame AMS, 
#   namely the number of rows (the number of years, the sample size)

# round(x, n): round() function rounds the values to 
#   the specified number of decimal places (default 0)

AMS$ReturnPeriod_T = round(1 / (1 - AMS$P), 1) # # empirical return period

plot(log10(AMS$ReturnPeriod_T), AMS$value, 
     # first two parameters: mapping to x and y axis respectively
     main = 'Return level plot',  # title of the plot
     xlab = 'return period: years',  # the label of x axis
     ylab = 'Return level (Discharge: m3/s)',  # the label of y axis
     type = 'p',  # the type of the plot
     xaxt = "n"  # hide the tick labels of x axis
     )
axis(1, # identity No. of x axis
     labels = c(1, 10,100,1000), # customized labels
     at = log10(c(1, 10,100,1000))  # locations the labels to be put
     )  # customize the tick labels in x axis

grid(nx = 20, # number of x grid lines
     ny = NULL,  # no grid line at y axis
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 0.5)      # Grid line width

############################ Exploratory data analysis ############################

nrow(AMS) # time series length

plot(AMS$year, AMS$value, 
     type = "l", # plot type: line plot
     main = 'Annual maximum discharge at Koeln',
     ylab = "Q_max (m^3/s)", 
     xlab = NA
     )
plot(DailyDischarge$value, # variable for x axis is neglected, 
     
     type = "l",
     col = 'lightblue',
     main = 'Daily discharge'
     )

max(AMS$value)
min(AMS$value)
mean(AMS$value)  
median(AMS$value) # median < mean : positively skewed distribution

# basic statistic diagrams
hist(AMS$value)  # histogram
boxplot(AMS$value) # boxplot
abline(h = mean(AMS$value), # add one horizontal line
       col = "red")

############################~Exploratory data analysis~############################



############################ Flood frequency analysis ##############################

#### 1. Fit the Generalized Extreme Value distribution (GEV) to the annual maxima ###

# load needed package
library(extRemes)

# fit the GEV
GEV_mle = fevd(AMS$value, # extreme variable
               method = "Lmoments", # parameter estimation method
               type = "GEV"  # probability distribution type
               )

GEV_mle = fevd(AMS$value, 
               method = "MLE", 
               type = "GEV"
               )
GEV_mle  # check the results

# The package extRemes offers the possibility to plot diagnostic plots

plot.fevd(GEV_mle, type = "probprob") # Probability-probability plot
# the Model vs. Empirical probabilities; main = 'Probability plot'

plot.fevd(GEV_mle, type = "qq") # Quantile-Quantile (QQ) plot; main = 'Quantile plot'

plot.fevd(GEV_mle, type = "density") # Compare modeled and empirical density function

plot.fevd(GEV_mle, type = "rl")   # Return level plot with confidence intervals (5%, 95%)
# 'Return level plot'


plot.fevd(GEV_mle, type = "primary")
dev.off()   # return to normal plotting without subplots

#### 2.  Compare GEV with Gumbel distribution ###

# Gumbel is a subcase of GEV with zero shape parameter, which is impossible or very unlikely 
# to occur when one fits the GEV. 

Gumb_mle = fevd(AMS$value, method = "MLE", type = "Gumbel") # Gumbel has shape = 0, 
# this means the method uses only two fitting parameters (location and scale)

# compare GEV - Gumbel diagnostic plots
par(mfcol = c(2, 3))  # enable subplots: 2 rows, 3 columns
plot.fevd(GEV_mle, type = "density", main = "GEV")
plot.fevd(Gumb_mle, type = "density", main = "Gumbel, shape == 0")

plot.fevd(GEV_mle, type = "qq", main = NA)
plot.fevd(Gumb_mle, type = "qq", main = NA)

plot.fevd(GEV_mle, type = "rl", main = NA)
plot.fevd(Gumb_mle, type = "rl", main = NA)
dev.off()

#### Exercises

# (1) do the GEV distribution fitting to annual maximum daily  
#   discharge in data file 'EXAMPLE_Chitan_Q_daily.txt'. 
df <- read.table(
  # fill the parameters
  'EXAMPLE_Chitan_Q_daily.txt', header = T
)


AMS_rainfall <- aggregate()


# (2) do the GEV distribution fitting to seasonal extreme daily discharge
# two seasons in a calendar year: 
# summer: from May to October
# Winter: November, December, and January to April




