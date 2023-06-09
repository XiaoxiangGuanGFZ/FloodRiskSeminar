# -- 
# flood risk seminar in Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 22.05.2022
# Last updated: 05.06.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)


# ------ Probability functions -------
# d = density
# p = probability distribution function
# q = quantile function
# r = random generation (random deviates)
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
# dnorm gives the density, 
y <- dnorm(x, mean = 0, sd = 1)
plot(x, y)

# pnorm gives the distribution function, 
pnorm(1.96, mean = 0, sd = 1)
pnorm(c(-1, -0.5, 0, 0.5, 1), mean = 0, sd = 1)

# qnorm gives the quantile function,
qnorm(.9, mean = 500, sd = 100)
qnorm(.9, mean = 0, sd = 1)

# rnorm generates random deviates.
rnorm(10, mean = 0, sd = 1)
rnorm(50, mean = 0, sd = 1)
rnorm(50, mean = 50, sd = 10)

runif(20, min = 0, max = 1)  # generate uniformly distributed random numbers  
runif(20, min = 1, max = 2)  

#------ data.frame manipulation -------
# read data into data.frame
df <- read.table(
  file = "D:/FloodRiskSeminar/data/EXAMPLE_Area4766_1982-2002.csv",
  header = TRUE, sep = ','
)

# description (basic info) of data.frame
head(df, 6)
View(df)
colnames(df)
dim(df)

df$y  # access the columns (vectors)
df$rainfall

unique(df$y)

range(df$y)
range(df$rainfall)
range(df$discharge)


df$rainfall[1]
df$discharge[1:100]
# manipulation on data.frame columns (as vector)
# apply functions
mean(df$rainfall)
sd(df$rainfall)
# sorting
sort(df$rainfall)
# filtering
df$rainfall[df$rainfall > 0]
df$rainfall[df$rainfall > mean(df$rainfall)]
df$rainfall[df$rainfall > median(df$rainfall)]

# filtering (sub-setting) a data frame by column-based conditions
# filtering based on one column
df[df$rainfall > 0, ]
df[df$rainfall > mean(df$rainfall), ]

# multiple test expressions
df[df$rainfall > 0 & df$year == 2000, ]
df[df$rainfall > 0 & df$rainfall < 1, ]

# filtering based on multiple columns
df[df$rainfall > 30 & df$discharge > mean(df$discharge), ]
df[df$rainfall > 30 & df$discharge > mean(df$discharge) & df$y == 2000, ]

# creating new columns or remove column(s)----
df$new_column <- seq(1, dim(df)[1])  # just like vector assignment
df$ratio_P_Ep <- df$rainfall / df$evaporation  # from existing columns arithmetic
df$ratio_P_Q <- df$rainfall / df$discharge

# remove the 2nd column
df[, -2]  

# aggregating data ----
# Splits the data into subsets, 
#   computes summary statistics for each, 
#   and returns the result in a convenient form.

aggregate(rainfall ~ y, data = df, FUN = sum)  # aggregate function: sum
aggregate(rainfall ~ y + m, data = df, FUN = sum)  # aggregate function: sum
aggregate(discharge ~ y, data = df, FUN = mean) # aggregate function: mean
aggregate(discharge ~ y, data = df, FUN = max)  # aggregate function: max


rainydays <- function(x) {
  # define a function: 
  # given a vector x, return the number of elements in x greater than 0
  # Parameter:
  # x: a numeric vector
  
  x = x[x > 0]  # sub-set the x vector, select the elements in x greater than 0
  n = length(x)  # estimate the length (size) of vector x, how many elements in vector x.
  
  return(n)  # return the output
}

aggregate(rainfall ~ year, data = df, FUN = rainydays)
aggregate(rainfall ~ month + year, data = df, FUN = rainydays)


### ------ built-in graphic tool -------------

# basic graphs -----
# histogram 
hist(df$rainfall )
hist(df$discharge )

hist(df$rainfall,
     xlab = 'rainfall (mm)', 
     ylab = 'Frequency', 
     main = 'Histogram of rainfall')


# boxplot
boxplot(df$rainfall)
boxplot(df$rainfall,
        ylab = 'rainfall (mm)')

# general plot function
?plot
# scatter plot
plot(df$rainfall )

plot(df$rainfall, col = 'red')
plot(df$rainfall, df$discharge,
     type = 'p', 
     pch = 1,
     col = 'blue',
     xlab = 'rainfall',
     ylab = 'discharge',
     main = "plot title")

# line plot

x <- seq(-3, 3, 0.01)
y <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 1, sd = 1)
plot(x, y, type = 'l')  # plot type is line
plot(x, y2, type = 'l')

plot(x, y, type = 'l', 
     lty = 1,
     col = 'red',
     ylab = 'PDF')
points(x,y2, type ='l',  # add second plot by using points()
       lty = 2,
       col = 'blue')
legend(x = -2.5, y = 0.3,
       c('mean=0, sd=1', 'mean=1, sd=1'),
       col = c('red', 'blue'),
       lty = c(1,2),
       title = 'parameters'
)

# ------- typical data analysis procedure in R project --------
# 1. data import
# 2. data check and data clean
# 3. data summary and management
# 4. model the data
# 5. display the data
# 6. presentation


####################### flood frequency analysis ####################
wd <- 'D:/FloodRiskSeminar/data/'

# read (import) data from file
df_discharge = read.table(file = paste0(wd, "Example_data.csv"), 
                            header = TRUE, sep = ',')
head(df_discharge)  # check the data


# extract annual maximum discharge
AMS = aggregate(discharge ~ year, data = df_discharge, FUN = max)
head(AMS)  # check the results
# write.table(
#   AMS,
#   file = 'D:/AMS.csv',
#   col.names = F, row.names = F, quote = F, sep = ','
# )

#------ Exploratory data analysis -------
plot(df_discharge$discharge, # variable for x axis is neglected, 
     type = "l",
     col = 'lightblue',
     main = 'Daily discharge'
)

plot(AMS$year, AMS$discharge, 
     type = "l", # plot type: line plot
     main = 'Annual maximum discharge at Koeln',
     ylab = "Q_max (m^3/s)", 
     xlab = NA
     )

max(AMS$discharge)
min(AMS$discharge)
mean(AMS$discharge)  
median(AMS$discharge) # median < mean : positively skewed distribution

# basic statistic diagrams
hist(AMS$discharge)  # histogram
boxplot(AMS$discharge) # boxplot


#------- GEV fitting --------

#### 1. Fit the Generalized Extreme Value distribution (GEV) to the annual maxima ###

# install packages required in this project
# install.packages("extRemes")  # package for extreme value statistics
library(extRemes)  # load the package (objects,functions and codes)
# to R environment to be applied.


# fit the GEV by using fevd() function in `extRemes` package
GEV_mle = fevd(AMS$discharge, # extreme variable
               method = "MLE", # parameter estimation method
               type = "GEV"  # probability distribution type
               )

GEV_mle  # check the results of GEV fitting 
GEV_mle$results$par  # the estimated 3 GEV parameters
GEV_mle$results$par[1]  # location parameter
GEV_mle$results$par[2]  # scale parameter
GEV_mle$results$par[3]  # shape parameter


# The package extRemes offers the possibility to plot diagnostic plots

plot.fevd(GEV_mle, type = "probprob") # Probability-probability plot
# the Model vs. Empirical probabilities; main = 'Probability plot'

plot.fevd(GEV_mle, type = "qq") # Quantile-Quantile (QQ) plot; main = 'Quantile plot'

plot.fevd(GEV_mle, type = "density") # Compare modeled and empirical density function

plot.fevd(GEV_mle, type = "rl")   # Return level plot with confidence intervals (5%, 95%)
# 'Return level plot'

plot.fevd(GEV_mle, type = "primary")
dev.off()   # return to normal plotting without subplots


# -------------- Exercises 1 ------------------

# (1) get data into R: ./data/Example_data.csv, use read.table() function
df <- read.table(
  # fill the parameters
)


# (2) Derive the annual maximum discharge series 
# hint: AMS <- aggregate(discharge~year, data = df, FUN = max)


# (3) Estimate the GEV parameters for annual maximum discharge
# hint: fevd()

# (4) Generate 100 random numbers based on the estimated GEV parameters
# hint: revd(100, loc = ,scale = ,shape = , type = "GEV")



# ----------------- Exercise 2 ------------------
# (1) In case, the flood defense water level is 3.8 m, 
#    then a flood happens, with the discharge of 11000 m3/s, 
#    what is the expected economic loss?


   
# (2) In case, the flood defense water level is 3.8 m, 
#   generate 100 years of GEV-distributed annual maximum discharge series, 
#   calculate the mean annual expected economic loss. 



# (3) Design the H_defense which could maintain the 
#   mean annual expected economic loss 
#   no greater than 1.5 million $ for the following 100 years.

x = seq(0, 1.2, 0.01)
y = NULL
for (i in 1:length(x)) {
  if (x[i] <= 0.15){
    y[i] = 0
  } else if (x[i] <= 1) {
    y[i] = (x[i] - 0.15) * 8
  } else {
    y[i] = (1 - 0.15) * 8
  }
}
plot(x, y, type = 'l',
     xlab = 'Exceeding level Hf-Hd (m)',
     ylab = 'Economic loss (million $)',
     main = "Vulnerability")
