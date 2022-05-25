# -- 
# flood risk seminar in 2022 Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 22.05.2022
# Last updated: 
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)

# first create project folder and save there the data file in subfolder "Data"
# then File --> New Project --> Existing directory
wd <- getwd()
# getwd() should give now the project directory
wd <- 'D:/FloodRiskSeminar/Materials from Luzie/'
setwd(wd)

# example how to install packages
# install.packages("extRemes")
library(extRemes)


################################ Data preparation ###########################################

# read data from file
DailyDischarge = read.table(file = "Data/Koeln_1.txt", header = TRUE, sep = "\t", 
                            col.names = c("date", "Q"), stringsAsFactors = FALSE)

# adjust date format
DailyDischarge$date = as.Date(DailyDischarge$date, format = "%d.%m.%Y")

# extract year from date
DailyDischarge$year = as.numeric(strftime(DailyDischarge$date, format = "%Y"))


# POT & AMS


# extract annual maximum discharge
AMS = aggregate(list(Qmax = DailyDischarge$Q), by = list(year = DailyDischarge$year), max)

################################~Data preparation~##################################
ams = AMS
colnames(ams) <- c('year', 'value')
ams$m_rank = rank(ams$value) # rank of the values
ams$P = round(ams$m_rank / (1+dim(ams)[1]), 4) # empirical probability
ams$ReturnPeriod_T = round(1 / (1-ams$P), 1) # # empirical return period
df = ams
plot(log10(df$ReturnPeriod_T), df$value, 
     #main = 'Return level plot',
     xlab = 'return period: years',
     ylab = 'Return level (Discharge: m3/s)',
     type = 'p',
     xaxt = "n")
axis(1, labels = c(1, 10,100,1000),
     at = log10(c(1, 10,100,1000)))
grid(nx = 20, 
     ny = NULL,
     lty = 2,      # Grid line type
     col = "gray", # Grid line color
     lwd = 0.5)      # Grid line width


############################ Exploratory data analysis ############################

nrow(AMS) # time series length

plot(AMS$year, AMS$Qmax, type = "l", ylab = "Q_max (m^3/s)", xlab = NA)
plot(DailyDischarge$Q, type = "l")

max(AMS$Qmax)
min(AMS$Qmax)
mean(AMS$Qmax)  
median(AMS$Qmax) # median < mean : positively skewed distribution
hist(AMS$Qmax)

boxplot(AMS$Qmax)
abline(h = mean(AMS$Qmax), col = "red")

############################~Exploratory data analysis~############################



############################ Flood frequency analysis ##############################

#### 1. Fit the Generalised Extreme Value distribution (GEV) to the annual maxima ###

# load needed package
library(extRemes)

# fit the GEV
GEV_mle = fevd(AMS$Qmax, method = "MLE", type = "GEV")
GEV_mle

GEV_mle = fevd(AMS$Qmax, method = "MLE", type = "Exponential")

GEV_mle = fevd(AMS$Qmax, method = "Lmoments", type = "GEV")

# The package extRemes offers the possibility to plot diagnostic plots
plot.fevd(GEV_mle, type = "primary")
dev.off()   # return to normal plotting without subplots

plot.fevd(GEV_mle, type = "probprob") # Probability-probability plot
# the Model vs. Empirical probabilities 
main = 'Probability plot'
plot.fevd(GEV_mle, type = "qq") # Quantile-Quantile (QQ) plot
main = 'Quantile plot'
plot.fevd(GEV_mle, type = "density") # Compare modeled and empirical density function

plot.fevd(GEV_mle, type = "rl")   # Return level plot with confidence intervals (5%, 95%)
main = 'Return level plot'

#### 2.  Compare GEV with Gumbel distribution ###

# Gumbel is a subcase of GEV with zero shape parameter, which is impossible or very unlikely 
# to occur when one fits the GEV. 

Gumb_mle = fevd(AMS$Qmax, method = "MLE", type = "Gumbel") # Gumbel has shape = 0, 
# this means the method uses only two fitting parameters (location and scale)

# compare GEV - Gumbel diagnostic plots
par(mfcol = c(2, 3))  # enable subplots: 2 rows, 3 columns
plot.fevd(GEV_mle, type = "density", main = "GEV")
plot.fevd(Gumb_mle, type = "density", main = "Gumbel")

plot.fevd(GEV_mle, type = "qq", main = NA)
plot.fevd(Gumb_mle, type = "qq", main = NA)

plot.fevd(GEV_mle, type = "rl", main = NA)
plot.fevd(Gumb_mle, type = "rl", main = NA)

