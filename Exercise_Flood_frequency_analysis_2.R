# -- 
# flood risk seminar in Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 12.06.2022
# Last updated: 12.06.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)


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



# ----------------- Exercise ------------------
# (1) In case, the flood defense water level is 3.8 m, 
#    then a flood happens, with the discharge of 11000 m3/s, 
#    what is the expected economic loss?

Flood_WaterLevel = 0.478 * 11000 ^(0.23)
Flood_WaterLevel

E_loss = 0
if (Flood_WaterLevel <= 3.8) {
  print("no flooding")
  E_loss = 0
} else {
  WaterLevel_exceed = Flood_WaterLevel - 3.8
  if (WaterLevel_exceed <= 0.15) {
    E_loss = 0
  } else if (WaterLevel_exceed <= 1) {
    E_loss = (WaterLevel_exceed - 0.15) * 8
  } else {
    E_loss = 6.8
  }
}


# (2) In case, the flood defense water level is 3.8 m, 
#   generate 100 years of GEV-distributed annual maximum discharge series, 
#   calculate the mean annual expected economic loss. 
WaterLevel_defense = 3.8

WL <- function(Q) {
  ## define a function to estimate the water level from discharge
  # one parameter: Q discharge
  waterlevel = 0.478 * Q ^ (0.23)
  
  return(waterlevel)
}

WL(12000)
WL(22000)
WL(19000)

EconomicLoss <- function(WL_exceed){
  ## define a function to estimate the economic loss from exceeding water level
  ## unit: million $
  if (WL_exceed <= 0.15) {
    loss = 0
  } else if (WL_exceed <= 1) {
    loss = (WL_exceed - 0.15) * 8
  } else {
    loss = 6.8
  }
  return(loss)
}

EconomicLoss(-0.2)
EconomicLoss(0.1)
EconomicLoss(0.48)
EconomicLoss(1.5)

AMS_run <- revd(100, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

loss_100 <- NULL  # define a null vector to store the 100 estimated losses
WL_defense <- 3.8
for (i in 1:100) {   # a for-loop, 
  
  flood_discharge = AMS_run[i]  # iterate each element in AMS_run
  
  # estimate the water level from discharge
  flood_wl = WL(flood_discharge) 
  
  # estimate the economic loss from exceeding water level
  loss_100[i] = EconomicLoss(flood_wl - WL_defense) 
}

mean(loss_100)


# (3) Design the H_defense which could maintain the 
#   mean annual expected economic loss 
#   no greater than 1.5 million $ for the following 100 years.

AMS_run <- revd(100, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

loss_100 <- NULL  

## change the defense water level for several times and 
## see the response from mean annual economic loss
WL_defense <- 3.33


for (i in 1:100) {   # a for-loop, 
  
  flood_discharge = AMS_run[i]  
  
  flood_wl = WL(flood_discharge) 
  
  loss_100[i] = EconomicLoss(flood_wl - WL_defense) 
}

mean(loss_100)

