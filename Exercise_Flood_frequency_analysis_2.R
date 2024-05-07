# -- 
# flood risk seminar in Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 12.06.2022
# Last updated: 12.06.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)


### ---- control structure 
# ---- if-else structure ----
x1 = -1.25

if (x1 >= 0 ) { #if it is non-negative, print it
  print( x1 )
} else {
  print( -x1 )  # if it is negative, print its opposite
}


# ---- for-loop ----
for (i in 1:5) { 
  print(i) 
} 

# -- while-loop -----
i <- 0 
while (i <= 4) 
{ 
  i <- i + 1 
  print(i) 
} 


#------ data.frame manipulation -------
# read data into data.frame
df <- read.table(
  file = "D:/FloodRiskSeminar/data/EXAMPLE_Area4766_1982-2002.csv",
  header = TRUE, sep = ','
)

### ---- description (basic info) of data.frame -----
head(df, 6)
View(df)
colnames(df)
dim(df)

### ----- access and describe the columns (vectors) -----
df$y  
df$rainfall

unique(df$y)

range(df$y)

### ----- apply functions ----

# each column in the data.frame is a vector, we can apply any functions 
#   that work with vectors to specific column of data.frame.
mean(df$rainfall)
sd(df$rainfall)


df$rainfall[1]  # access elements like vector with [] operator
df$discharge[1:100]  # access a sequence of elements

### ---- sorting the data ----

sort(df$rainfall)  # sort specific column like vectors
df[order(df$rainfall), ]  # sort the entire data.frame based on one column "rainfall"

### ------------- filtering the column in data.frame ----------
# columns are manipulated as vectors
df$rainfall[df$rainfall > 0]
df$rainfall[df$rainfall > mean(df$rainfall)]
df$rainfall[df$rainfall > median(df$rainfall)]

### ---- filtering (sub-setting) a data frame by column-based conditions -----
# ---- filtering based on one column----
df[df$rainfall > 0, ]
df[df$rainfall > mean(df$rainfall), ]

# ---- multiple test expressions ----
#    combine by logical operator
df[df$rainfall > 0 & df$y == 1999, ]
df[df$rainfall > 0 & df$rainfall < 1, ]

# ---- filtering based on multiple columns ----
df[df$rainfall > 30 & df$discharge > mean(df$discharge), ]
df[df$rainfall > 30 & df$discharge > mean(df$discharge) & df$y == 2000, ]

### ---- creating new columns ----
df$new_column <- seq(1, dim(df)[1])  # just like vector assignment
df$ratio_P_Ep <- df$rainfall / df$evaporation  # from existing columns arithmetic
df$ratio_P_Q <- df$rainfall / df$discharge

### ---- remove columns from the data.frame ----
df <- df[, -2]  # remove the 2nd column from df

-c(1,2,3)

### ---- aggregating data -----
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

aggregate(rainfall ~ y, data = df, FUN = rainydays)
aggregate(rainfall ~ m + y, data = df, FUN = rainydays)


####################### flood frequency analysis ####################
setwd('D:/FloodRiskSeminar/data/')  # set the work space

# read (import) data from file
df_discharge = read.table(file = "Example_data.csv", 
                          header = TRUE, sep = ',')
head(df_discharge)  # check the data

colnames(df_discharge) <- c()

# extract annual maximum discharge
AMS = aggregate(discharge ~ year, data = df_discharge, FUN = max)
head(AMS)  # check the results
# write.table(
#   AMS,
#   file = 'D:/AMS.csv',
#   col.names = F, row.names = F, quote = F, sep = ','
# )

#------ Exploratory data analysis -------

max(AMS$discharge)
min(AMS$discharge)
mean(AMS$discharge)  
median(AMS$discharge) # median < mean : positively skewed distribution


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


# -------------- Exercise 1 ------------------

# (1) get data into R: ./data/Example_data.csv, use read.table() function
setwd()  # set the work space
df <- read.table(
  # fill the parameters
)


# (2) Derive the annual maximum discharge series 
# hint: 
AMS <- aggregate(discharge~year, data = df, FUN = max)


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



# (3) Design the H_defense which could keep the 
#   mean annual expected economic loss 
#   no greater than 1.5 million $ for the following 100 years.


