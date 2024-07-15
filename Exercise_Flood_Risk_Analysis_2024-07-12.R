# -- 
# flood risk seminar in Summer Semester 
# Flood frequency analysis
# Generalized Extreme Value distribution
# Created: 12.06.2022
# Last updated: 12.06.2023
# Xiaoxiang Guan (guan.xiaoxiang@gfz-potsdam.de)


#----------- Exercise: flood frequency analysis --------

# Exercise 1.1 ------
# get data into R: ./data/Example_data.csv, 
# use read.table() function, 
# then derive the annual maximum discharge (AMS) series: aggregate() 

# -- read in data
df <- read.table(
  # fill the parameters
  "D:/FloodRiskSeminar/data/Example_data.csv",
  header = T, sep = ','
)

# extract annual maximum discharge with aggregate()
AMS = aggregate(discharge ~ year, data = df, FUN = max)

# Exercise 1.2 ------
# Estimate the GEV parameters for AMS: fevd()
library(extRemes) 
# fit the GEV by using fevd() function in `extRemes` package
GEV_mle = fevd(AMS$discharge, # extreme variable
               method = "MLE", # parameter estimation method
               type = "GEV"  # probability distribution type
)

paras <- as.numeric(GEV_mle$results$par)  # the estimated 3 GEV parameters


# Exercise 1.3 ------
# Derive the 100-year discharge
# hint: 
# the return period is 100, then estimate the probability,
# then use qevd() function
rp = 100
p = 1 - 1 / rp
qevd(p, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")


# Exercise 1.4 ------
# What is the return period of the discharge 10000 m3/s?
# hint:
# estimate the return period of random variable taking on 
# the value of 10000, using qevd() function
Q = 10000
p = pevd(Q, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")

rp = 1 / (1 - p)
rp


# Exercise 1.5 ------
# Plot the empirical frequency curve and estimated GEV curve together: 
# plot() and points()
# Hint:
# Weilbull formula to calculate the empirical probability - plot()
# Use the 3 GEV parameters to plot the estimated GEV curve - points()


AMS$m_rank = rank(AMS$discharge) # rank of the values
AMS$P = round(AMS$m_rank / (1 + dim(AMS)[1]), 4) # empirical probability
AMS$ReturnPeriod_T = round(1 / (1 - AMS$P), 1) # # empirical return period
AMS

plot(x = AMS$ReturnPeriod_T, y = AMS$discharge)

gene_rp <- seq(1.1, 500, 0.5)  ## artifical return periods
gene_p <- 1 - 1/ gene_rp       ## corresponding probability

gene_q <- qevd(gene_p, loc = paras[1], scale = paras[2], shape = paras[3], type = "GEV")
# gene_q: the estimated discharges


plot(
  x = AMS$ReturnPeriod_T, y = AMS$discharge,
  xlab = "return period [year]",
  ylab = "discharge [m3/s]"
)  # generate a plot
points(x = gene_rp, y = gene_q, col = "red", type = "l") # add one layer





#----------- Exercise: flood risk analysis --------
# Hint:
# write a function for water level - discharge relationship
# write a function for economic loss - exceeding water level relationship 
# revd() can be used to generate any number of random values




# Exercise 2.1 ------
# In case, the flood defense water level is 3.8 m, 
#    then a flood happens, with the discharge of 11000 m3/s, 
#    what is the expected economic loss?



# Exercise 2.2 ------
# In case, the flood defense water level is 3.8 m, 
#   generate 100 years of GEV-distributed annual maximum discharge series, 
#   calculate the mean annual expected economic loss. 



# Exercise 2.3 ------
# Design the H_defense which could keep the 
#   mean annual expected economic loss 
#   no greater than 1.5 million $ for the following 100 years.



