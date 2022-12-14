# -- 
# flood risk seminar in 2022 Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 22.05.2022
# Last updated: 
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)

# typical data analysis procedure in R project 
# 1. data import
# 2. data check and data clean
# 3. data summary and management
# 4. model the data
# 5. display the data
# 6. presentation


# install packages required in this project
install.packages("extRemes")  # package for extreme value statistics
library(extRemes)  # load the package (objects,functions and codes)
# to R environment to be applied.

################################ Data preparation ##############################

# read (import) data from file
DailyDischarge = read.table(file = "Koeln-data-Q.csv", 
                            header = TRUE, sep = ',')
head(DailyDischarge)  # check the data
View(DailyDischarge)

# extract annual maximum discharge
AMS = aggregate(Q~year, data = DailyDischarge, FUN = max)
head(AMS)  # check the results

# then how to extract all the values over a specified threshold? (peak-over-threshold)

################################~Data preparation~##################################

############################ Exploratory data analysis ############################

colnames(AMS) <- c('year', 'Qmax')
AMS$m_rank = rank(AMS$Qmax) # rank of the values
AMS$P = round(AMS$m_rank / (1 + dim(AMS)[1]), 4) # empirical probability
# dim(AMS)[1]: the first dimension of data frame AMS, 
#   namely the number of rows (the number of years, the sample size)

# round(x, n): round() function rounds the values to 
#   the specified number of decimal places (default 0)

AMS$ReturnPeriod_T = round(1 / (1 - AMS$P), 1) # # empirical return period

plot(log10(AMS$ReturnPeriod_T), AMS$Qmax, 
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

plot(AMS$year, AMS$Qmax, 
     type = "l", # plot type: line plot
     main = 'Annual maximum discharge at Koeln',
     ylab = "Q_max (m^3/s)", 
     xlab = NA
     )
plot(DailyDischarge$Q, # variable for x axis is neglected, 
     
     type = "l",
     col = 'lightblue',
     main = 'Daily discharge'
     )

max(AMS$Qmax)
min(AMS$Qmax)
mean(AMS$Qmax)  
median(AMS$Qmax) # median < mean : positively skewed distribution

# basic statistic diagrams
hist(AMS$Qmax)  # histogram
boxplot(AMS$Qmax) # boxplot
abline(h = mean(AMS$Qmax), # add one horizontal line
       col = "red")

############################~Exploratory data analysis~############################



############################ Flood frequency analysis ##############################

#### 1. Fit the Generalized Extreme Value distribution (GEV) to the annual maxima ###

# load needed package
library(extRemes)

# fit the GEV
GEV_mle = fevd(AMS$Qmax, # extreme variable
               method = "Lmoments", # parameter estimation method
               type = "GEV"  # probability distribution type
               )

GEV_mle = fevd(AMS$Qmax, 
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

#### 2.  Gumbel distribution ###

# Gumbel is a subcase of GEV with zero shape parameter, which is impossible or very unlikely 
# to occur when one fits the GEV. 

Gumb_mle = fevd(AMS$Qmax, method = "MLE", type = "Gumbel") # Gumbel has shape = 0, 
# this means the method uses only two fitting parameters (location and scale)
plot.fevd(Gumb_mle, type = "density", main = "Gumbel, shape == 0")
plot.fevd(Gumb_mle, type = "qq", main = NA)
plot.fevd(Gumb_mle, type = "rl", main = NA)

#### Exercises

# (1) do the GEV distribution fitting to annual maximum daily rainfall 
#   and discharge in data file 'Chitan-data-PEQ.csv' respectively. 
df <- read.table(
  # fill the parameters
  'Chitan-data-PEQ.csv'
)


AMS_rainfall <- aggregate()


# (2) do the GEV distribution fitting to seasonal extreme daily discharge
# two seasons in a calendar year: 
# summer: from May to October
# Winter: November, December, and January to April




