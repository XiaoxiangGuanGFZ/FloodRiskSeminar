# ------- Flood risk seminar in 2022 summer semester ------
# in Institute of Environmental Science and Geography, University of Potsdam
# web-based interactive application for presentation of fundamental knowledge
# on extreme statistics and application in time-varying
# flood risk analysis.

# Created data: 20.04.2022
# Last updated: 13.10.2022
# Developer: Xiaoxiang Guan
# E-mail: guan.xiaoxiang@gfz-potsdam.de
# web application: https://guanxiaoxiang-gfz.shinyapps.io/FloodRisk/

# ---- load required packages ---
require(shiny)
require(shinythemes)
require(DT)
require(Rlab)
require(tidyverse)
require(evd)
require(Metrics)
require(extRemes)
require(extraDistr)
require(ggplot2)

#
# 1. yearly damage plot (represented with present value of the loss) 
#    modulate the exposure features, give more distinguishments
# 2. ** default AMS data set or imported data set
# 3. ** exposure: area and unit value
# 4. ** give more descriptions on the arguments
#    make the GUI more friendly
# 5. add the panel filled with exercises and questions
# 6. prepare the materials supporting the software.
# 7. multi-scenario simulation and get the sensitivity 
#    the sensitivity of EAD to time-varying components;
#    FF(hazard) - mu;
#    exposure: area and unit value
#    flood protect: river channel training
# 8. ** TMFR -> data presentation ->
#    re-organize the layout of this panel
#    individual time-varying effects

# --------- defined global variables -----------
####### default annual maxima series ########
# in the example data import panel, as
# an alternative to import data;
# which can be easy for new-hands to manipulate  
AMS_builtin <- data.frame(
  date = 1:160,
  year = c(1845,1846,1847,1848,1849,1850,1851,1852,1853,1854,1855,1856,1857,1858,1859,1860,1861,1862,1863,1864,1865,1866,1867,1868,1869,1870,1871,1872,1873,1874,1875,1876,1877,1878,1879,1880,1881,1882,1883,1884,1885,1886,1887,1888,1889,1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004),
  value = c(9800,8170,7220,6290,6290,9710,6920,6910,5190,5800,7010,5410,2970,2720,4040,5970,5490,8550,3510,3990,4800,5150,8000,6610,6330,6260,5460,5190,4800,2680,5740,9070,6450,4440,6870,8070,6380,10220,9350,4020,5310,4430,5550,6400,4100,6010,4090,6020,6220,4090,7300,7160,7270,4020,5810,5810,5340,4890,5270,5390,3620,6050,5420,4550,4900,6430,4240,4910,4490,6250,6080,6120,6650,6940,8780,10700,2330,5070,5630,9380,9580,10900,5120,6040,4240,8380,5560,5380,3710,2760,5340,5550,6980,4360,7230,5980,7890,8050,3240,7520,7430,7080,9170,9890,2650,6170,6490,7610,4220,5250,9460,7270,6750,8600,5100,3990,5250,5460,5480,3140,7410,7290,6600,7160,5270,9690,3520,5380,3600,5750,5060,3270,6680,6340,6730,8800,6680,7967,9801,8433,4360,5890,7130,9550,4900,7600,6530,4870,10700,7830,10800,4000,7440,8856.375,8010.479,5180,8750,7560,9150,6780)
)

####### discharge-water table relationship curve #####
QH_table <- data.frame(
  # suppose a typical water table - discharge relationship
  # H = a*Q^b, a and b are the two parameters
  Discharge <- runif(500,1100,12000),
  Water_level <- 0.748 * Discharge ^ 0.23 + 
    c(runif(100, 0, 0.05), runif(300, 0, 0.03), runif(100, 0, 0.05))
)
colnames(QH_table) <- c('Discharge', 'Water_level')
# plot(QH_table)

####### exposure data set (land use types) ######
Table_exposure <- data.frame(
  # Height: m.a.p, height over the reference surface 
  # Unitvalue: €/m^2 
  # Area: m^2
  # Objecttype: land use cover type
  # 
  # 1=Residential, single storey without basement
  # 2=Residential, single storey with basement
  # 3=Residential, multi-storey without basement
  # 4=Commercial, single storey 
  # 5=Commercial, multi-storey 
  # 6=Other buildings
  # 
  "Height" = round(c(3.89,4.14,4.31,4.51,4.75,
               3.58,3.88,4.08,4.29,4.4,4.53,4.76,4.91,
               3.68,4.13,4.39,4.7,4.86,3.89,
               4.19,4.38,4.39,4.86,
               3.64,3.87,4.27,4.64,4.88,
               3.5,3.52,3.71,3.83,4.08,
               4.26,4.4,4.55,4.74,4.91) - 3.4, 2),
  "Unitvalue" = c(4500,4500,4500,4500,4500,4500,4500,4500,4500,
                  4500,4500,4500,4500,3000,3000,3000,3000,3000,
                  2000,2000,2000,2000,2000,2000,2000,2000,2000,
                  2000,1000,1000,1000,1000,1000,1000,1000,1000,
                  1000,1000) * 2,
  "Area" = c(290,1005,936,290,834,92,725,3395,5743,1988,
             3493,2968,4631,270,880,1147,482,319,5912,
             5113,2789,5514,6797,2160,2108,13179,17733,
             8490,46,227,81,440,1399,2619,1270,1122,634,1551) * 2,
  "Object_type" = c(1,1,1,1,1,
                    2,2,2,2,2,2,2,2,
                    3,3,3,3,3,
                    4,4,4,4,4,
                    5,5,5,5,5,
                    6,6,6,6,6,6,6,6,6,6)
)

###### venerability (damage susceptibility) #######
Table_susceptibility <- data.frame(
  # Inundation_depth: mm
  # Relative_loss: %
  # Object_type: land use type
  'Inundation_depth'=c(0,0.15,0.3,1,2.7,3.2,3.7,6,15,
                       0,0.15,0.3,1,2.7,3.2,3.7,6,15,
                       0,0.15,0.3,1,2.7,3.2,3.7,6,15,
                       0,0.05,0.1,0.6,1.5,3,3.5,6,15,
                       0,0.05,0.1,0.6,1.5,3,3.5,6,15,
                       0,0.05,0.1,0.6,1.5,3,3.5,6,15),
  'Relative_loss'=c(0,0.0015,3,6,8.25,9,12,14.9985,15,
                    1.5,1.5015,3,7.05,9,9.75,12.45,14.9985,15,
                    1.05,1.0515,2.85,4.8,6,6.75,9,12,15,
                    0,0.0015,3,6,9,13.5,14.25,14.85,15,
                    0,0.0015,1.5,3,4.5,6.75,7.5,14.25,15,
                    0,0.0015,1.5,4.5,6,9,13.5,14.85,15),
  'Object_type' = c(1,1,1,1,1,1,1,1,1,
                    2,2,2,2,2,2,2,2,2,
                    3,3,3,3,3,3,3,3,3,
                    4,4,4,4,4,4,4,4,4,
                    5,5,5,5,5,5,5,5,5,
                    6,6,6,6,6,6,6,6,6)
)
Table_susceptibility_functions <- data.frame(
  # y = a*log(b*x+1)
  # exposure susceptibility is calculated by using 
  #    logarithm function
  a = c(
    rep(0.09, 5),
    rep(0.06, 8),
    rep(0.1, 5),
    rep(0.05, 5),
    rep(0.05, 5),
    rep(0.3, 10)
  ),
  b = c(
    rep(0.15, 5),
    rep(0.5, 8),
    rep(0.1, 5),
    rep(1, 5),
    rep(1.2, 5),
    rep(0.015, 10)
  ),
  "Object_type" = c(1,1,1,1,1,   # y4
                    2,2,2,2,2,2,2,2, #y3
                    3,3,3,3,3,  #y5
                    4,4,4,4,4,   #y1
                    5,5,5,5,5,   #y2
                    6,6,6,6,6,6,6,6,6,6)  # y6
)

# --------- define function (global) -----------
n_gev_random <- function(n_mu, scale, shape) {
  # generate the GEV-distributed random variables (samples) with different
  # loc (location parameter)
  n = length(n_mu)
  out = NULL
  for (i in 1:n) {
    out[i] = evd::rgev(1, loc = n_mu[i], scale = scale, shape = shape)
  }
  out[out <= 0] = mean(out[out > 0])
  return(out)
}

Multi_n_gev_random <- function(
  mu, sigma, xi, years, runs, CR_mu
) {
  # mu, sigma, xi are GEV parameters, scale values
  # years: number of simulations
  # runs: number of running times
  # CR_mu: change rate of parameter mu, (%/a)
  # output:
  # a vector (runs ==1) or a data.frame with the number of columns as runs
  index = seq(1, years)
  seq_mu = mu + (index-1) * CR_mu /100 * mu # a sequence of nonstationary mu
  sample_value = n_gev_random(seq_mu, scale = sigma, shape = xi)
  if (runs == 1) { 
    # only one run
    out = sample_value
  } else {
    # several sampling runs
    out = sample_value
    for (i in 2:runs) {
      sample_value = n_gev_random(seq_mu, scale = sigma, shape = xi)
      out = cbind(out, sample_value)
    }
    out
  }
  return(out)
}

InundationDepth <- function(depth, Table_exposure) {
  # depth: simulated inundation water table,
  #        the depth deviation between 
  #        sample discharge water table and 
  #        flood defense system return level -water table
  # calculate the inundation depth and extent,
  # we suppose that the exceeded water table is the inundation depth,
  #       although this is by no means the case in reality. 
  out = Table_exposure %>%
    mutate(inundationdepth = depth - Height)
  out$inundationdepth[out$inundationdepth <= 0] = 0
  
  return(out)
}


# simplified version, in aspect of vulnerability
EAD <- function(
  Table_Exposure_Inund, 
  Inflation_co, 
  Table_susceptibility_functions
) {
  # Table_Exposure_Inund: a data frame, filtered by depth and asset heights
  # Inflation_co: (1 + rate)^n, where n is the number of years
  Inund = Table_Exposure_Inund$inundationdepth
  if (sum(Inund) == 0) {
    return(0)
  } else {
    a = Table_susceptibility_functions$a
    b = Table_susceptibility_functions$b
    lossrate = a * log(b * Inund + 1)
    EAD_f = sum(lossrate * Table_Exposure_Inund$Area * Table_Exposure_Inund$Unitvalue)
    EAD_p = EAD_f / Inflation_co # compute the present value 
    return(EAD_p)
  }
  
}


EAD_years <- function(
  samples,
  QH_paras,
  H_defense,
  Inflation_rate,
  Table_exposure,
  Table_susceptibility_functions
) {
  # samples: sampled annual maximum discharge from
  #          fitted GEV distribution
  # QH_paras: H = a * Q^b; a vectors of two values
  # H_defense: the water table of flood defense system
  #           exceed which flood (inundation) happens
  # Inflation_rate: unit - %
  # Table_exposure: steady or time-varying exposure table
  # Table_susceptibility: stationary or time-varying susceptibility
  ######
  # output the calculated Expected Annual Damage (EAD) at the present value
  #    with the same size as samples
  
  n = length(samples)
  ead = NULL
  for (i in 1:n) {
    Q = samples[i]
    H = QH_paras[1] * Q ^ (QH_paras[2])
    depth_deviation = H - H_defense
    if (depth_deviation <= 0) {
      # no inundation
      ead[i] = 0
    } else {
      # flood occurs 
      Table_Exposure_Inund = InundationDepth(H, Table_exposure)
      Inflation_co = (1 + Inflation_rate / 100) ^ i
      
      ead[i] = EAD(Table_Exposure_Inund, 
                   Inflation_co, 
                   Table_susceptibility_functions)
    }
  }
  return(ead) # a vector, with the length as the number of simulation years
}


Theme_my <- theme_bw() + 
  theme(legend.position = 'bottom',
        strip.background = element_rect(fill='white', color = 'white'),
        panel.border = element_blank()
  )

POT_func <- function(dat, threshold) {
  # sample the time series using Peak-Over-Threshold
  # dat: a data.frame with two columns: date and value
  # threshold
  
  n = dim(dat)[1]
  index = rep(0, n)
  for (i in 2:(n-1)) {
    if (dat$value[i] >= dat$value[i-1] & dat$value[i] >= dat$value[i+1]) {
      index[i] = 1
    }
  }
  return(
    dat[dat$value >= threshold & index == 1, ]
  )
}
rmse <- function(x,y) {
  # root mean square error
  # x and y should share the same data length
  out = sqrt(
    sum((x-y)^2) / length(x)
  )
  return(
    out
  )
}
mae <- function(x, y) {
  # mean absolute error
  return(
    sum(abs(x-y)) / length(x)
  )
}

#  ----------  UI (User interface) -----------------
ui <- navbarPage(
  'Flood risk',
  # ---------------UI - Introduction------------------
  tabPanel("Overview",
           shinythemes::themeSelector(),
           fluidRow(column(12,
                           wellPanel(
                             h3("Flood risk seminar",align = "center")       
                           ))
           ),
           fluidRow(column(12,
                           wellPanel(
                             p("This is a Web-based interactive application 
                               developed as a teaching assistance for flood risk 
                               seminar in Institute of Environmental Science and Geography, 
                               University of Potsdam. This seminar is led by Professor Dr. Bruno Merz. 
                               The main contents of this seminar cover the fundamental 
                               knowledge of statistics and probability, extreme value 
                               theory and application, flood risk and its sensitivity analysis. 
                               The app is aimed to help the college students enter into the world 
                               of hydrologic engineering design, specifically on flood 
                               frequency analysis and design flood derivation. The original 
                               developer is Xiaoxiang Guan (guan@gfz-potsdam.de), 
                               with whom you can get contact and get support, 
                               in case of problems in app utility. "),
                             # br(),
                             # p("Developers:Xiaoxiang Guan (guan@gfz-potsdam.de)",align = "right"),
                             # p(em("Institute of Environmental Science and Geography, University of Potsdam"), align = "center"),
                             # p(em("Helmholtz centre potsdam, GFZ German Research Centre for Geosciences"), align = "center")
                           )
           )
           ),
           p(em("@Institute of Environmental Science and Geography, University of Potsdam"), align = "right")
  ),
  # --------------- UI - Fundamental statistics --------------------
  tabPanel('Fundamental statistics',
           navlistPanel(
             tabPanel(
               "Random variable",
               fluidPage(
                 p('A random variable, usually written X, 
                   is a variable whose possible values are numerical 
                   outcomes of a random phenomenon. 
                   There are two types of random variables, 
                   discrete and continuous. '),
                 tabsetPanel(
                   tabPanel(
                     'Discrete',
                     p(em('A discrete random variable is one which may take on 
                          only a countable or finite number of distinct values')),
                     selectInput("discrete_type", 
                                 "Choose a type:",
                                 choices = c("Bernoulli", "Binomial", "Poisson"),
                                 selected = 'Bernoulli'),
                     conditionalPanel(
                       "input.discrete_type == 'Bernoulli'",
                       helpText('The Bernoulli distribution is the discrete 
                                probability distribution of a random variable 
                                which takes the value 1 with probability \\(p\\) 
                                and the value 0 with probability \\(1-p\\). 
                                Less formally, it can be thought of as a model 
                                for the set of possible outcomes of any single 
                                experiment that asks a yes–no question.'),
                       numericInput('Dis_para_bernoulli_p', 
                                    '\\(p (0 \\leq p \\leq 1)\\)',
                                    value = 0.5, 
                                    min = 0.1, max=0.99, step = 0.1)
                     ),
                     conditionalPanel(
                       "input.discrete_type == 'Binomial'",
                       helpText('Binomial distribution with parameters 
                                \\(N\\) and \\(p\\) is the discrete probability distribution 
                                of the number of successes in a sequence of 
                                \\(N\\) independent experiments, 
                                each asking a yes–no question, and 
                                each with its own Boolean-valued outcome: 
                                success (with probability \\(p\\)) or 
                                failure (with probability \\(1 − p\\)).'),
                       fluidRow(
                         column(6, numericInput('Dis_para_binomial_N',
                                                '\\(N\\)',
                                                value = 10, min = 1, step = 1)),
                         column(6, numericInput('Dis_para_binomial_p',
                                                '\\(p\\)',
                                                value = 0.5, max=0.99, min = 0.01,
                                                step = 0.1))
                       )
                     ),
                     conditionalPanel(
                       "input.discrete_type == 'Poisson'",
                       helpText('Poisson distribution expresses the 
                                probability of a given number of events 
                                occurring in a fixed interval of 
                                time or space if these events occur 
                                with a known constant mean rate and 
                                independently of the time since the last event.
                                \\(X\\) is the number of times an event occurs 
                                in an interval and \\(x\\) can take values 0, 1, 2, ….'),
                       numericInput('Dis_para_poisson_lambda',
                                    '\\(\\lambda (>0)\\)',
                                    value = 1,
                                    min = 0.1, step = 1)
                     ),
                     br(),
                     fluidRow(
                       column(6,wellPanel(h5("Probabily mass function (PMF)"),
                                          plotOutput("Dis_pmf",height = "300px"),
                                          uiOutput('Dis_equation_pmf'),
                       )),
                       column(6,wellPanel(h5("Cumulative distribution function (CDF)"),
                                          plotOutput("Dis_cdf",height = "300px"),
                                          uiOutput('Dis_equation_cdf'),
                       ))
                     )
                   ),
                   tabPanel(
                     'Continuous',
                     p(em('A continuous random variable is a 
                          random variable where the data can 
                          take infinitely many values.')),
                     selectInput("continuous_type", 
                                 "Choose a type:",
                                 choices = c("Uniform", "Normal", "Exponential", 'Gamma'),
                                 selected = 'Uniform'),
                     conditionalPanel(
                       "input.continuous_type == 'Uniform'",
                       helpText("The continuous uniform distribution or rectangular distribution 
                                is a family of symmetric probability distributions, 
                                which describes an experiment where there is an arbitrary 
                                outcome that lies between certain bounds. 
                                All intervals of the same length on the 
                                distribution's support are equally probable. 
                                \\(X\\) is defined between \\(a\\) and \\(b\\)"),
                       fluidRow(
                         column(6, numericInput('Con_para_unif_a', '\\(a\\)',
                                                value = 1, step = 1)),
                         column(6, numericInput('Con_para_unif_b', '\\(b\\)',
                                                value = 6, step = 1))
                       )
                     ),
                     conditionalPanel(
                       "input.continuous_type == 'Normal'",
                       helpText("In statistics, a normal distribution 
                                (also known as Gaussian, Gauss, or Laplace–Gauss distribution) 
                                is a type of continuous probability distribution 
                                for a real-valued random variable.
                                central limit theorem: under some conditions, 
                                the average of many samples (observations) of 
                                a random variable with finite mean and 
                                variance is itself a random 
                                variable—whose distribution converges 
                                to a normal distribution as the number 
                                of samples increases."),
                       fluidRow(
                         column(6, numericInput('Con_para_normal_mu', '\\(\\mu\\)',
                                                value = 0, step = 1)),
                         column(6, numericInput('Con_para_normal_sigma', 
                                                '\\(\\sigma (>0)\\)',
                                                value = 1, min = 0.1, step = 1))
                       )
                     ),
                     conditionalPanel(
                       "input.continuous_type == 'Exponential'",
                       helpText('The exponential distribution is the probability 
                                distribution of the time between events in 
                                a Poisson point process, i.e., a process 
                                in which events occur continuously and 
                                independently at a constant average rate. 
                                It is a particular case of the gamma 
                                distribution.'),
                       
                       numericInput('Con_para_expo_lambda', 
                                    '\\(\\lambda (>0)\\)', 
                                    value = 1, min = 0.1, step = 0.1)
                       
                     ),
                     conditionalPanel(
                       "input.continuous_type == 'Gamma'",
                       helpText('The gamma distribution is a two-parameter 
                                family of continuous probability distributions. 
                                The exponential distribution, Erlang distribution, 
                                and chi-square distribution are special 
                                cases of the gamma distribution.'),
                       fluidRow(
                         column(6, numericInput('Con_para_gamma_alpha', 
                                                '\\(\\alpha (>0)\\)', value = 1,
                                                min = .1, step = 0.2
                         )),
                         column(6, numericInput('Con_para_gamma_beta', 
                                                '\\(\\beta (>0)\\)',
                                                value = 1, min = 0.1, 
                                                step = 0.2))
                       )
                     ),
                     br(),
                     fluidRow(
                       column(6,wellPanel(h5("Probabily density function (PDF)"),
                                          plotOutput("Con_pdf",height = "300px"),
                                          uiOutput('Con_equation_pdf'),
                       )),
                       column(6,wellPanel(h5("Cumulative distribution function (CDF)"),
                                          plotOutput("Con_cdf",height = "300px"),
                                          uiOutput('Con_equation_cdf'),
                       ))
                     )
                   ),
                   tabPanel(
                     'Hydrologic variables',
                     h4('Rainfall'),
                     tags$ul(
                       tags$li(tags$b("wet-or-dry status of one day"), " - rainy (1) or not rainy (0), a Bernoulli process."),
                       tags$li(tags$b("the rainy days in a year (365 or 366 days)"), " - discrete RV, a Binomial process"),
                       tags$li(tags$b("daily rainfall amount"), " - continuous RV, lower bounded"),
                       tags$li(tags$b("annual maximum daily rainfall amount"), " - continuous RV"),
                     ),
                     
                     h4('Air temperature'),
                     tags$ul(
                       tags$li(tags$b("number of continuous days with air temperature over 30 Celsius degree"), 
                               " - discrete RV, an indicator for hot event (heatwave)")
                     ),
                     h4('River discharge'),
                     tags$ul(
                       tags$li(tags$b("annual maximum daily discharge"), " - continuous RV, Extreme value"),
                       tags$li(tags$b("number of continuous days with daily discharge lower than 30m3/s (for example)"), 
                               " - discrete RV, reflecting the drought potential")
                     )
                   )
                 )
               )
             ), 
             tabPanel(
               'Descriptive measures',
               tabsetPanel(
                 tabPanel(
                   'Specific moments',
                   p(em("In mathematics, the moments of a function are 
                        quantitative measures related to the shape of 
                        the function's graph.")),
                   withMathJax(),
                   h4('Central moments:'),
                   p('a moment of a probability distribution of a random variable about the mean of random variable'),
                   helpText('Discrete RV: \\(n\\) th moment about the mean 
                    $$\\mu_n = E[(X-E[X])^n]= \\sum_{\\mathrm{all} \\ x_i} (x_i-\\mu)^n f_X(x_i) $$'),
                   helpText('Continuous RV: \\(n\\) th moment about the mean 
                    $$\\mu_n = E[(X-E[X])^n]= \\int_{-\\infty}^{\\infty} (x_i-\\mu)^n f_X(x_i) \\mathrm{d} x$$'),
                   h4('Moments about origin:'),
                   p('Raw sample moments'),
                   helpText('Discrete RV: \\(n\\) th moment about the origin 
                    $$\\mu_n^{\'} = E[X^n]= \\sum_{\\mathrm{all} \\ x_i} x_i^n f_X(x_i) $$'),
                   helpText('Continuous RV: \\(n\\) th moment about the origin
                    $$\\mu_n^{\'} = E[X^n]= \\int_{-\\infty}^{\\infty} x_i^n f_X(x_i) \\mathrm{d} x $$'),
                   
                   h4('Special moments:'),
                   helpText('Expected value (mean): 1st moment of origin \\(\\mu_1^{\'}=E[X], \\ n=1\\)'),
                   helpText('Variance: 2nd central moment \\(Var[X]=\\mu_2=E[(X-E[X])^2], \\ n=2\\)'),
                   helpText('Skewness: 3rd central moment \\(\\gamma=\\mu_3=E[(X-E[X])^3], \\ n=3\\); 
                            Any symmetric distribution will have a third central moment, if defined, of zero. ')
                 ),
                 tabPanel(
                   'Example',
                   h4('Binomial:'),
                   helpText('Expected value: \\( E[X] = Np\\)'),
                   helpText('Variance: \\( Var[X] = Np(1-p)\\)'),
                   helpText('Skewness: \\( \\gamma = \\frac{1-2p}{\\sqrt{Np(1-p)}}\\)'),
                   h4('Possion:'),
                   helpText('Expected value: \\( E[X] = \\lambda\\)'),
                   helpText('Variance: \\( Var[X] = \\lambda\\)'),
                   helpText('Skewness: \\( \\gamma = \\frac{1}{\\sqrt{\\lambda}}\\)'),
                   h4('Normal:'),
                   helpText('Expected value: \\( E[X] = \\mu\\)'),
                   helpText('Variance: \\( Var[X] = \\sigma^2\\)'),
                   helpText('Skewness: \\( \\gamma = 0\\)'),
                   
                   h4('Gamma:'),
                   helpText('Expected value: \\( E[X] = \\frac{\\alpha}{\\beta} \\)'),
                   helpText('Variance: \\( Var[X] = \\frac{\\alpha}{\\beta^2}\\)'),
                   helpText('Skewness: \\( \\gamma = \\frac{2}{\\sqrt{\\alpha}}\\)'),
                 )
               )
             ),
             tabPanel(
               'Parameter estimation',
               h4('Graphical estimation method'),
               p('The graphical estimation method is an old-fashioned one, 
               but is still useful and helpful in Statistical Hydrology'),
               p(em('this procedure to estimate the parameters will be shown in 
                    Generalized Extreme Value distribution fitting.'), style="color:red"),
               h4('Least-Squares Estimation (LSE)'),
               p('The least-squares estimation method aims to estimate the 
                 parameters of any theoretical function by minimizing 
                 the sum of its squared differences, with respect to a 
                 given empirical curve or sample data. In effect, the 
                 least-squares method is more suitable for regression analysis.'),
               h4('Method of moments (MOM)'),
               p('The method of moments (MOM) is the simplest and 
                  perhaps the most intuitive.
                  MOM estimators are consistent, but are often biased 
                  and less efficient than MLE
                  estimators, especially for distributions with 
                  more than two parameters, which require estimation o
                  f higher order moments.'),
               h4('Maximum Likelihood Method'),
               p('The Maximum Likelihood Estimation (MLE) method is 
               the most efficient among the methods currently used 
               in Statistical Hydrology, as it generally yields 
               parameters and quantiles with the smallest sampling 
               variances. However, in many cases, the MLE’s highest 
               efficiency is only asymptotic and estimation from a small 
               sample may result in estimators of relatively inferior quality'), 
               h4('Method of L-moments'),
               h4('Method of Entropy'),
               p('')
               # tags$img(src='Para_esti.png', width = "600px", height = "800px", deleteFile = FALSE)
             )
           )
  ),
  # --------------- UI - Extreme value statistics --------------------
  tabPanel('Extreme value statistics',
           navlistPanel(
             tabPanel(
               'Sampling method',
               radioButtons('datasource', 'Example data',
                            choices = c(default = 'default',
                                        import = 'import'),
                            selected = 'default'
               ),
               conditionalPanel(
                 condition = "input.datasource == 'import'",
                 
                 tabsetPanel(
                   
                   tabPanel(
                     "Data input",
                     br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                radioButtons("Sequencetype", "Temporal resolution",
                                             choices = c(
                                               yearly = "yearly",
                                               daily = "daily"),
                                             selected = "daily"),
                                selectInput("Null", 
                                            "Null value:",
                                            choices = c(-99.9,-99,NA),
                                            selected = -99.9),
                                tags$hr(),
                                checkboxInput("header", "Header", TRUE),
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = "\t"),
                                radioButtons("quote", "Quote",
                                             choices = c(None = "",
                                                         "Double Quote" = '"',
                                                         "Single Quote" = "'"),
                                             selected = "")
                                
                              )),
                       column(9, wellPanel(
                         fileInput("Dataset", "Choose dataset File",
                                   multiple = TRUE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         h5("Data sequence range:",align = "left"),
                         tableOutput("range"),
                         tabsetPanel(
                           tabPanel(
                             'Tabular form',
                             DT::dataTableOutput("dataset_table")
                           ),
                           tabPanel(
                             'Hydrograph',
                             plotOutput("Dataset_chart", height = "300px")
                           ),
                           tabPanel(
                             'Statistic chart',
                             plotOutput("Dataset_bar", height = "300px")
                           )
                         )
                         
                       ))
                     )
                   ),
                   tabPanel(
                     'Block maxima',
                     br(),
                     p('The block maxima (BM) approach in extreme value analysis 
                     fits a sample of block maxima to the 
                     Generalized Extreme Value (GEV) distribution.'),
                     plotOutput('plot_Maxima_daily', height = "400px")
                   ),
                   tabPanel(
                     'Peak-over-threshold',
                     br(),
                     p('The Peak Over Threshold-method (POT-method) is one way to
                     model extreme values. The main concept of the method 
                     is to use a threshold to seclude values considered 
                     extreme to the rest of the data and create a model for 
                     the extreme values by modeling the tail of all the 
                     values the exceeds this threshold.'),
                     numericInput('threshold', 'threshold', value = 8000, step = 100),
                     #textOutput('POT_summary'),
                     verbatimTextOutput('POT_summary', placeholder = FALSE),
                     plotOutput('plot_POT_daily', height = "400px")
                   )
                 )
               ),
             ),
             tabPanel(
               'Generalized Extreme Value (GEV)',
               tabsetPanel(
                 tabPanel(
                   'Overview',
                   br(),
                   p('In probability theory and statistics, 
                   the generalized extreme value (GEV) distribution
                   is a family of continuous probability distributions developed
                   within extreme value theory, which is also
                   the only possible asymptotic distribution of properly normalized 
                   maxima of a sequence of independent and identically distributed random variables.
                     '),
                   p('The GEV distribution is widely used in the treatment of "tail risks"'),
                   helpText('Cumulative distribution function (CDF): 
                            $$P(X \\leq x) = F_X(x) = \\mathrm{exp}[-(1+\\xi \\frac{x-\\mu}{\\sigma}) ^ {-1/\\xi}], \\ \\xi \\neq 0$$
                            $$P(X \\leq x) = F_X(x) = \\mathrm{exp}[-\\mathrm{exp}(-\\frac{x-\\mu}{\\sigma})], \\ \\xi=0 $$'),
                   helpText("Three parameters: scale \\(\\xi\\), location \\( \\mu\\), and scale\\(\\sigma\\). "),
                   tags$li(tags$b('Type I - Gumbel distribution:'), 'with \\( \\xi = 0\\), 
                           The GEV becomes the Gumbel, 
                           with scale parameter \\(\\sigma\\) and shape parameter \\( \\mu\\).'), 
                   tags$li(tags$b('Type II - Fréchet distribution:'), 'with \\( \\xi > 0\\), 
                           The GEV becomes the Fréchet, with the domain in \\(x<\\mu+\\sigma / \\xi \\).'), 
                   tags$li(tags$b('Type III - Weibull distribution:'), 'with \\( \\xi < 0\\), 
                           The GEV becomes the Weibull, with the domain in \\(x>\\mu+\\sigma / \\xi \\).'), 
                   
                 ),
                 tabPanel(
                   'Parameters',
                   fluidRow(
                     column(
                       3,
                       br(),
                       h3('GEV Parameters'),
                       sliderInput("GEV_para_mu", "Location \\(\\mu\\)", min = 0, 
                                   max = 60, value = 30, step = 2,
                                   animate = TRUE),
                       sliderInput("GEV_para_sigma", "Scale \\(\\sigma\\)", min = 1, 
                                   max = 10, value = 3, step = 0.2,
                                   animate = TRUE),
                       sliderInput("GEV_para_xi", "Shape \\(\\xi\\)", min = -2, 
                                   max = 2, value = 0, step = 0.1,
                                   animate = TRUE),
                       tags$li(tags$b("Location \\(\\mu\\):"), ': Indicator of center of gravity of distribution;'),
                       tags$li(tags$b('Scale \\(\\sigma\\)'), ': Indicator of dispersion / variability'),
                       tags$li(tags$b('Shape \\(\\xi\\)'), 'Indicator of distribution (tail in particular)'),
                       
                     ),
                     column(
                       9,
                       plotOutput('GEV_plot_pdf', height = '350px'),
                       plotOutput('GEV_plot_cdf', height = '350px')
                     )
                   )
                 ),
                 tabPanel(
                   'Tail feature',
                   fluidRow(
                     column(
                       2, 
                       br(),
                       h4('Gumbel'),
                       p(em('exponential tail'), style="color:red"), 
                       tags$li('\\(\\xi=0\\)'),
                       tags$li('\\(\\mu=30\\)'),
                       tags$li('\\(\\sigma=10\\)'),
                       br(),
                       h4('Frechet'),
                       p(em('fat tail'), style="color:red"), 
                       tags$li('\\(\\xi=0.2\\)'),
                       tags$li('\\(\\mu=30\\)'),
                       tags$li('\\(\\sigma=10\\)'),
                       br(),
                       h4('Weibull'),
                       p(em('upper finite endpoint tail'), style="color:red"), 
                       tags$li('\\(\\xi=-0.2\\)'),
                       tags$li('\\(\\mu=30\\)'),
                       tags$li('\\(\\sigma=10\\)'),
                     ),
                     column(
                       10, 
                       plotOutput('GEV_3_com_pdf', height = '400px'),
                       plotOutput('GEV_3_com_cdf', height = '400px')
                     )
                   )
                 )
               )
             ),
             tabPanel(
               'GEV parameter estimation',
               helpText('Sample data should be imported in "Sampling method" section'),
               
               tabsetPanel(
                 tabPanel(
                   'Premise',
                   h4('Return period'),
                   p('A return period, also known as a recurrence interval 
                     or repeat interval, is an average time 
                     or an estimated average time between 
                     events such as earthquakes, floods, landslides, 
                     or river discharge flows to occur. The return period is 
                     calculated as the inverse of the probability of exceedence: 
                     \\(T=1/P(X>x) = 1/(1-P(X \\leq x)) = 1/(1-F_X(x))\\)'),
                   h4('Return level'),
                   p('Return levels are calculated using the quantile function:
                   inverse of (cumulative) distribution function ( \\(F_X^{-1}(x)\\) ).
                   Namely, return level \\(X_T\\) is the function of probability 
                   of exceedance or return period \\(T\\).
                    '),
                   helpText("$$X_T=\\mu-\\frac{\\sigma}{\\xi} [1-(-\\mathrm{ln}(1-1/T))^{-\\xi}], \\ \\xi \\neq 0$$
                            $$X_T=\\mu-\\sigma \\mathrm{ln}[- \\mathrm{ln}(1-1/T)],\\ \\xi = 0$$"),
                   h4('Empirical probability'),
                   p('Empirical probability, also known as experimental 
                     probability, refers to a probability that is 
                     based on historical data. In other words, 
                     empirical probability illustrates the likelihood of 
                     an event occurring based on historical data, and can be 
                     calculated based on the sorted sequence.'),
                   fluidRow(
                     column(
                       5,
                       tags$li('One first ranks the data, 
                       typically annual extremes or values over a 
                       threshold, in increasing order of magnitude 
                       from the smallest \\(m=1\\) to the largest 
                       \\(m=N\\), where \\(N\\) is the sample size.
                                '),
                       tags$li('A  cumulative probability \\(P\\) is 
                       assigned to the ranked data, usually 
                       with a function of the form: 
                                \\(P=(m+\\alpha)/(N+\\beta)\\)'),
                       tags$li(tags$b('Weibull formula:'), 
                               '\\(P=m/(N+1)\\), as \\( \\alpha=0,\\beta=1\\)'
                       )
                       
                     ),
                     column(
                       7,
                       h4('Empirical probabilities calculation example', 
                          style="color:darkblue"),
                       tabsetPanel(
                         tabPanel(
                           'Table',
                           DT::dataTableOutput("empirical_prob_table")
                         ),
                         tabPanel(
                           'Chart',
                           plotOutput('empirical_prob_chart', height = '350px')
                         )
                       )
                       
                     )
                   )
                 ),
                 tabPanel(
                   'Graphical method',
                   fluidRow(
                     column(
                       2,
                       numericInput('GM_para_mu', 'Location \\(\\mu\\)',
                                    min = 0, value = 5500, step = 10),
                       numericInput('GM_para_sigma', 'Scale \\(\\sigma\\)',
                                    min = 1, value = 2000, step = 10
                       ),
                       numericInput('GM_para_xi', 'Shape \\(\\xi\\)', 
                                    value = -0.2, step=0.01)
                     ),
                     column(
                       10,
                       fluidRow(
                         column(6, plotOutput('GM_returnlevel_chart', height = '300px')),
                         column(6, br(), tableOutput('GM_goodness'))
                       ),
                       fluidRow(
                         column(6, plotOutput('GM_pp_chart', height = '300px')),
                         column(6, plotOutput('GM_qq_chart', height = '300px'))
                       )
                     )
                   )
                 ),
                 tabPanel(
                   'Parametric method',
                   fluidRow(
                     column(6, 
                            selectInput('Dis_type', "select a distribution",
                                        choices = c('GEV', 'Gumbel'),
                                        selected = 'GEV')
                     ), 
                     column(6, 
                            selectInput('Para_methods', 'Select method:',
                                        choices = c('Maximum Likelihood Estimation',
                                                    'Method of L-Moments'),
                                        selected = 'Maximum Likelihood Estimation')
                     ) # c("MLE", "GMLE", "Bayesian", "Lmoments")
                   ),
                   fluidRow(
                     column(6, plotOutput('PM_returnlevel_chart', height = '300px')),
                     column(6, verbatimTextOutput('PM_results'))
                   ),
                   fluidRow(
                     column(6, plotOutput('PM_pp_chart', height = '300px')),
                     column(6, plotOutput('PM_qq_chart', height = '300px'))
                   )
                 ) 
                 
               )
             ),
             tabPanel(
               'Uncertainty analysis',
               p('The sensitivity of return level estimation to three influential 
                 factors: the time period sampled, distribution types to fit and 
                 the parameter estimation methods.'),
               tabsetPanel(
                 tabPanel(
                   'Sample time period',
                   
                   fluidRow(
                     column(5, 
                            sliderInput('period', 'Sample time period', 
                                        min = 1800, max = 2020, 
                                        value = c(1845, 2004), step = 1),
                            helpText('GEV distribution is used and the 
                            parameters estimated by maximum likelihood method'),
                            numericInput('SA_timeperiod_T', 
                                         'Select a return period (T)',
                                         min = 1, max = 1000, 
                                         value = 100, step = 10),
                            verbatimTextOutput('SA_timepriod_returnlevel'),
                     ),
                     column(
                       7, 
                       plotOutput('SA_timeperiod_plot', height = '400px')
                     )
                   )
                 ),
                 tabPanel(
                   'Distribution',
                   helpText('Parameters estimated by Maximum Likelihood method.'),
                   fluidRow(
                     column(6, plotOutput('SA_distribution_plot',height = '400px')),
                     column(6, 
                            br(),
                            h4('Estimated return levels:'),
                            tableOutput('SA_distribution_table'))
                   )
                 ),
                 tabPanel(
                   'Parameter estimation',
                   helpText('GEV distribution is applied.'),
                   fluidRow(
                     column(6, plotOutput('SA_method_plot',height = '400px')),
                     column(6, 
                            br(),
                            h4('Estimated return levels:'),
                            tableOutput('SA_method_table'))
                   )
                 )
               )
             )
           )   
  ),
  # -------------- UI - Flood risk (FR)------------------
  tabPanel('Flood risk (FR)',
           
           navlistPanel(
             tabPanel(
               'Graphical concept',
               h4('Key processes that can cause or 
                prevent disastrous river floods'),
               tags$img(src = "Floodrisk_chart.png",
                        width = "600px", height = "400px", deleteFile = FALSE),
               p('Flood risk is a combination of the probability 
                (likelihood or chance) of an event happening 
                and the consequences (impact) if it occurred. 
                Flood risk is dependent on there being a source 
                of flooding, such as a river, a route for the flood 
                water to take (pathway), and something that is 
                affected by the flood (receptor), 
                such as a housing estate.')
             ),
             
             tabPanel(
               'Hazard',
               h4('Hazard denotes a chance phenomenon capable of 
                 causing harm. Flood hazard in riverine areas can be 
                 characterized by the probability and intensity of 
                 high river flows and resulting inundations, 
                 and depends on the physical processes of 
                 flood generation.'),
               br(),
               
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("Hazard_coms", "Factors: ",
                                  choices = c('Climate change' = 'CC',
                                              'River training' = 'RT',
                                              'Unstructural measures' = "USM"
                                  ),
                                  selected = "CC")
                   ),
                   mainPanel(
                     conditionalPanel(
                       'input.Hazard_coms == "CC"',
                       h4('Climate change influences the extreme characteristics 
                    (frequency, magnitude, and distribution) 
                    of hydrologic variables'),
                       h4('Example: flood discharge return level plot'),
                       helpText('! Note: dischagre sample data required in
                           "Extreme value statistics" section', style = 'color:red'),
                       fluidRow(
                         column(4, numericInput('FR_GEV_mu', '\\(\\mu\\)', 
                                                value = 5490.16)),
                         column(4, numericInput('FR_GEV_sigma', '\\(\\sigma\\)', 
                                                value = 1781.78)),
                         column(4, numericInput('FR_GEV_xi', '\\(\\xi\\)', 
                                                value = -0.1826))
                       ),
                       plotOutput('FR_Hazard_returnlevel', height = '400px'),
                       verbatimTextOutput('FR_Hazard_GEV_paras')
                     ),
                     conditionalPanel(
                       'input.Hazard_coms == "RT"', 
                       h4('Example: '),
                       p('Schematic diagram of river channel cross-section'),
                       tags$img(src = 'Riversection.png',
                                height = '262px',
                                width = '350px', deletefile = FALSE,
                                align = 'center'),
                       br(),
                       p('Discharge and river water table relationship (\\(H=aQ^b\\))'),
                       fluidRow(
                         column(6, 
                                numericInput('FR_QT_a', 'H-Q function (a):',
                                             value = 0.478, step = 0.02,
                                             min = 0.1),
                         ),
                         column(6,
                                numericInput('FR_QT_b', 'H-Q function (b):',
                                             value = 0.23, 
                                             step = 0.02, min = 0.17),
                         )
                       ),
                       plotOutput('FR_QT_chart', width = '350px')
                     )
                   )
                 )
               )
               
             ),
             tabPanel(
               'Vulnerability',
               br(),
               h4('Vulnerability \\(V\\) represents the societal 
                processes and is composed of exposure \\(E\\), 
                susceptibility \\(S\\) and 
                response capacity \\(RC\\).'),
               tags$li(tags$b('Exposure \\(E\\): '), 'the assets that may be affected by floods'),
               helpText('In its broadest sense, assets 
                       comprise the built environment, humans 
                       and their socio-economic systems, 
                       and the natural environment. These need not 
                       be restricted to assets that are located 
                       in inundation areas, since indirect flood 
                       effects may damage assets outside the 
                       flooded area. '),
               tags$li(tags$b('Susceptibility \\(S\\): '), 
                       'the degree to which the system 
                      is damaged by certain floods'),
               helpText('For example, buildings with precautionary measures 
                       implemented would be hit less hard, or 
                       adequate early warning and prepared citizens 
                       would allow emergency measures and, hence, 
                       reduce the damage. '),
               tags$li(tags$b('Response capacity \\(RC\\): '), 
                       'the ability to respond to and to recover from a flood'),
               helpText('For example, building owners with 
                       greater financial or personal resources 
                       or with flood insurance have a greater 
                       availability of means to repair 
                       damage and to recover. '),
               p('Finally, we define adaptive capacity \\(AC\\) 
                as the ability to adjust to observed or 
                expected changes in flood risk. Adaptations 
                may include modifying susceptibility, increasing 
                its response capacity and reducing exposure.'),
               helpText('$$V = f \\{ E(AC), S(AC), RC(AC) \\}$$')
               # helpText('$$V(t) = f \\{ E(AC(t)), S(AC(t)), RC(AC(t)) \\}$$')
             )
             # tabPanel(
             #  'Time-varying',
             
             #)
           )
  ),
  # ------------UI - Time-varying FR------------------
  tabPanel('Time-varying FR',
           tabsetPanel(
             
             tabPanel(
               'Data presentation',
               br(),
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       'Data_show_type', 'Select:',
                       choices = c('Q-T relationship', 
                                   'Exposure', 
                                   'Vulnerability'),
                       selected = 'Q-T relationship'
                     )
                   ),
                   mainPanel(
                     fluidRow(
                       column(6, 
                              DT::dataTableOutput("Data_show_table",
                                                  width = '90%',
                                                  height = '50%')
                       ),
                       column(6, 
                              plotOutput('Data_show_chart', 
                                         width = '400px', height = '400px'),
                              verbatimTextOutput('Data_show_explain'),
                              tags$head(
                                tags$style("#Data_show_explain{
                                             font-size:12px; 
                                             white-space: pre-wrap;}")
                              )
                       )
                     )
                   )
                 )
               )
             ),
             tabPanel(
               'Time-varying analysis',
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     h4('Time-varying parameters:'),
                     
                     fluidRow(
                       column(6, 
                              numericInput('TVFR_para_QT_a', 'H-Q function (a):',
                                           value = 0.748, step = 0.02),
                       ),
                       column(6,
                              numericInput('TVFR_para_QT_b', 'H-Q function (b):',
                                           value = 0.23, step = 0.02),
                       )
                     ),
                     helpText('Water table - discharge relationship: \\(H=aQ^b\\)'),
                     
                     sliderInput('TVFR_para_mu', 'Change ratio of \\(\\mu\\) (%)',
                                 value = 0, min = -5, max = 5, step = 0.1,
                                 animate = TRUE),
                     helpText('Time-varying GEV location parameter \\(\\mu\\)'),
                     sliderInput('TVFR_para_exposure', 
                                 'Change ratio of exposure area (%/a):',
                                 value = 0, min = -10, max = 10, step = 1,
                                 animate = TRUE),
                     helpText('Time-varying asset areas.'),
                     sliderInput('TVFR_para_exposure_unitvalue', 
                                 'Change ratio of exposure unit value (%):',
                                 value = 0, min = -10, max = 10, step = 1,
                                 animate = TRUE),
                     
                     sliderInput('TVFR_para_susceptibility', 
                                 'Change ratio of vulnerability (%):',
                                 value = 0, min = -10, max = 10, step = 1,
                                 animate = TRUE),
                     helpText('Time-varying relative loss to inundation depth.'),
                     sliderInput('TVFR_para_inflation', 'Inflation ratio (%)',
                                 value = 0, min = -5, max = 5, step = 0.2)
                   ),
                   mainPanel(
                     fluidRow(
                       column(6,
                              h4('Experiment setup'),
                              fluidRow(column(6,numericInput('TVFR_para_years', 
                                                             'Simulation years:',
                                                             value = 50, min = 10, step = 5)),
                                       column(6, numericInput('TVFR_para_runs', 
                                                              'Number of runs:',
                                                              value = 40, min = 1, step = 1))),
                              numericInput('TVFR_para_returnperiod', 
                                           'Return period of defense system:',
                                           value = 50, min = 10, step = 10),
                              uiOutput('TVFR_chart_FFA_text', align = 'center'),
                              br(), 
                              verbatimTextOutput('TVFR_EAD_sum'),
                              tags$head(
                                tags$style("#TVFR_EAD_sum{
                                             color:red; 
                                             face: bold;
                                             font-size:12px; 
                                             white-space: pre-wrap;}")
                              )
                              
                       ),
                       column(6, 
                              plotOutput('TVFR_chart_FFA', height = '350px')
                              
                       )
                       
                     ),
                     fluidRow(
                       column(6, plotOutput('TVFR_chart_sampling', height = '350px')),
                       column(6, plotOutput('TVFR_chart_EAD', height = '350px'))
                       
                     )
                   )
                 )
               )
             ),
             # 
             # tabPanel(
             #   'Multi-scenario simulation'
             # ),
             tabPanel(
               'Exercises',
               br(),
               navlistPanel(
                 tabPanel(
                   'Quiz 1',
                   h4('We randomly sample flood events from an existing time series. 
                      In scenario A for 20 years and in scenario B for 100 years (runs = 100), 
                      in both scenarios the flood protection measures are designed 
                      for a 20-year event. In which scenario do we expect greater damage and why?'),
                   actionButton('Hint1', 'Tip'),
                   actionButton('Solution1', 'Solution'),
                   uiOutput('Hint_text_1'),
                   plotOutput('Solution_plot_1')
                 ),
                 
                 tabPanel(
                   'Quiz 2',
                   h4('Randomly sampling flood events for 100 years, 
                      with the flood protection measures designed 
                      for a 20-year event in Scenario A and 
                      100-year event in Scenario B. In which scenario 
                      do we expect greater damage and why? '),
                   actionButton('Hint2', 'Tip'),
                   actionButton('Solution2', 'Solution'),
                   uiOutput('Hint_text_2'),
                   plotOutput('Solution_plot_2')
                 ),
                 
                 tabPanel(
                   'Quiz 3',
                   h4('Investigate the response of EAD to the 
                      changes in the rating curve 
                      (water table-discharge relationship)?'),
                   actionButton('Hint3', 'Tip'),
                   actionButton('Solution3', 'Solution'),
                   uiOutput('Hint_text_3'),
                   plotOutput('Solution_plot_3')
                 ),
                 
                 tabPanel(
                   'Quiz 4',
                   h4('How will the EAD respond to the variations 
                      in exposure components (asset values, 
                      building areas)?'),
                   actionButton('Hint4', 'Tip'),
                   actionButton('Solution4', 'Solution'),
                   uiOutput('Hint_text_4'),
                   plotOutput('Solution_plot_4')
                 ),
                 tabPanel(
                   'Quiz 5',
                   h4('To keep the EAD under 10 M.Euro/a in the 
                      following 50 years under the context that 
                      the location parameter in flood frequency 
                      increases with a rate of 1.5%/a and 
                      exposure areas and unit values experience 
                      an increment of 2%, 
                      how will you design the flood protection measures? 
                      (rating curve and inflation rate are assumed to be steady here)'),
                   actionButton('Hint5', 'Tip'),
                   actionButton('Solution5', 'Solution'),
                   uiOutput('Hint_text_5'),
                   plotOutput('Solution_plot_5')
                 )
               )
             )
           )
  ),
)

# --------- server --------------------------
server <- function(input, output) {
  # ----- discrete RV ---------
  output$Dis_pmf <- renderPlot({
    if (input$discrete_type == 'Bernoulli') {
      req(input$Dis_para_bernoulli_p)
      p = input$Dis_para_bernoulli_p
      x <- seq(0, 1, by = 1)
      # using dbern( ) function to x to obtain corresponding Bernoulli  PDF
      y <- dbern(x, prob = p)  
      # plot dbern values
      barplot(y, names.arg = x, ylab = 'PMF')
      # plot(x, y, type = "p", pch=19, ylab='PMF')
      # mtext('p = 0.7', side=3)
    } else if (input$discrete_type == 'Binomial') {
      req(input$Dis_para_binomial_N)
      req(input$Dis_para_binomial_p)
      N <- input$Dis_para_binomial_N
      x <- seq(0, N)
      p <- input$Dis_para_binomial_p
      y <- dbinom(x, N, p)
      barplot(y, names.arg=x, ylab='PMF') 
      # plot(x, y, type = 'o', ylab='PMF')
      # mtext('N = 10, p = 0.7', side=3)
    } else if (input$discrete_type == 'Poisson') {
      req(input$Dis_para_poisson_lambda)
      lambda <- input$Dis_para_poisson_lambda
      x <- seq(0, round(5*lambda))
      y <- dpois(x, lambda)
      barplot(y, names.arg=x, ylab='PMF')
      # plot(x, y, type = 'o', ylab='PMF')
      # mtext(expression(lambda == 2), side=3)
    }
  })
  output$Dis_cdf <- renderPlot({
    if (input$discrete_type == 'Bernoulli') {
      req(input$Dis_para_bernoulli_p)
      p = input$Dis_para_bernoulli_p
      x <- seq(0, 1, by = 1)
      # using dbern( ) function to x to obtain corresponding Bernoulli  PDF
      y <- pbern(x, prob = p)  
      # plot dbern values
      
      plot(x, y, type = "p", pch=19, ylab='CDF')
      x1 <- seq(0.01, 0.99, by = 0.01)
      y1 <- rep(y[1], length(x1))
      points(x1, y1, type = 'l')
      # mtext('p = 0.7', side=3)
    } else if (input$discrete_type == 'Binomial') {
      req(input$Dis_para_binomial_N)
      req(input$Dis_para_binomial_p)
      N <- input$Dis_para_binomial_N
      x <- seq(0, N)
      p <- input$Dis_para_binomial_p
      y <- pbinom(x, N, p)
      barplot(y, names.arg=x, ylab='CDF')
      # plot(x, y, type = 'o', ylab='CDF')
      # mtext('N = 10, p = 0.7', side=3)
    } else if (input$discrete_type == 'Poisson') {
      req(input$Dis_para_poisson_lambda)
      lambda <- input$Dis_para_poisson_lambda
      x <- seq(0, round(5*lambda))
      
      y <- ppois(x, lambda)
      barplot(y, names.arg = x, ylab='CDF')
      # mtext(expression(lambda == 2), side=3)
      
    }
  })
  
  
  # discrete: pmf equation 
  output$Dis_equation_pmf <- renderUI({
    if (input$discrete_type == 'Bernoulli') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=p^x(1-p)^{(1-x)}, \\ x=0,1 $$')
      )
    } else if (input$discrete_type == 'Binomial') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=\\frac{N!}{x!(N-x)!}p^x(1-p)^{N-x}, \\ x=0,1,...,N $$')
      )
      
    } else if (input$discrete_type == 'Poisson') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=\\frac{\\lambda^x e^{-\\lambda}}{x!}, \\ x=0,1,2..., \\lambda >0$$')
      )
    }
    
  })
  # discrete: cdf equation
  output$Dis_equation_cdf <- renderUI({
    if (input$discrete_type == 'Bernoulli') {
      
    } else if (input$discrete_type == 'Binomial') {
      withMathJax(
        helpText('$$P(X\\leq x)=F_X(x)=\\sum_{i=0}^{N}{N \\choose i}p^i(1-p)^{N-i}, \\ x=0,1,2...,N$$')
      )
    } else if (input$discrete_type == 'Poisson') {
      withMathJax(
        helpText('$$P(X \\leq x)=F_X(x)=\\sum_{i=0}^{x}\\frac{\\lambda^i}{i!}e^{-\\lambda}, \\ x=0,1,2..., \\lambda >0 $$')
      )
    }
  })
  
  # --------- continuous RV -----------
  output$Con_pdf <- renderPlot({
    if (input$continuous_type == 'Uniform') {
      req(input$Con_para_unif_a)
      req(input$Con_para_unif_b)
      a = input$Con_para_unif_a
      b = input$Con_para_unif_b
      if (a < b) {
        x <- seq(a, b, by = 0.1)
        y <- rep(1/(max(x)-min(x)), length(x))
        # plot dbern values
        plot(x, y, type = "l", ylab='PDF')
        # mtext('a = 1, b = 5', side=3)
      }
    } else if (input$continuous_type == 'Normal') {
      req(input$Con_para_normal_mu)
      req(input$Con_para_normal_sigma)
      mu = input$Con_para_normal_mu
      sigma = input$Con_para_normal_sigma
      x <- seq(mu-4.5*sigma, mu+4.5*sigma, by = 0.01)
      y <- dnorm(x, mean = mu, sd = sigma)
      plot(x, y, type = 'l', ylab='PDF')
      # text(0, 0.25, expression(mu == 2)) #
      # text(0, 0.2, expression(sigma == 1))
    } else if (input$continuous_type == 'Exponential') {
      req(input$Con_para_expo_lambda)
      lambda = input$Con_para_expo_lambda
      x <- seq(1, 10, by = 0.1)               
      y <- dexp(x, rate = lambda)
      plot(x, y, type = 'l', ylab='PDF')
      # mtext(expression(lambda == 2), side=3)
    } else if (input$continuous_type == 'Gamma') {
      req(input$Con_para_gamma_alpha)
      req(input$Con_para_gamma_beta)
      alpha = input$Con_para_gamma_alpha
      beta = input$Con_para_gamma_beta
      x <- seq(0, 8, by = 0.04)   
      y <- dgamma(x, shape = alpha, rate = beta) 
      plot(x, y, type = 'l', ylab='PDF')
      # text(7, 0.25, expression(alpha == 2)) #2
      # text(7, 0.2, expression(beta == 1))
    }
  })
  output$Con_cdf <- renderPlot({
    if (input$continuous_type == 'Uniform') {
      req(input$Con_para_unif_a)
      req(input$Con_para_unif_b)
      
      a = input$Con_para_unif_a
      b = input$Con_para_unif_b
      if (a < b) {
        x <- seq(a, b, by = 0.1)
        y <- (x-1)/(5-1)
        plot(x, y, type = "l", ylab='CDF')
        # mtext('a = 1, b = 5', side=3)
      }
      
    } else if (input$continuous_type == 'Normal') {
      req(input$Con_para_normal_mu)
      req(input$Con_para_normal_sigma)
      mu = input$Con_para_normal_mu
      sigma = input$Con_para_normal_sigma
      x <- seq(mu-4.5*sigma, mu+4.5*sigma, by = 0.01)
      y <- pnorm(x, mean = mu, sd = sigma)
      plot(x, y, type = 'l', ylab='CDF')
      # text(0, 0.75, expression(mu == 2)) #
      # text(0, 0.65, expression(sigma == 1))
      
    } else if (input$continuous_type == 'Exponential') {
      req(input$Con_para_expo_lambda)
      lambda = input$Con_para_expo_lambda
      x <- seq(1, 10, by = 0.1)               
      y <- pexp(x, rate = lambda)
      plot(x, y, type = 'l', ylab='CDF')
      # mtext(expression(lambda == 2), side=3)
    } else if (input$continuous_type == 'Gamma') {
      req(input$Con_para_gamma_alpha)
      req(input$Con_para_gamma_beta)
      alpha = input$Con_para_gamma_alpha
      beta = input$Con_para_gamma_beta
      x <- seq(0, 8, by = 0.04)   
      y <- pgamma(x, shape = alpha, rate = beta) 
      plot(x, y, type = 'l', ylab='PDF')
      # text(7, 0.8, expression(alpha == 2)) #2
      # text(7, 0.6, expression(beta == 1))
    }
  })
  
  # continuous: pdf formula
  output$Con_equation_pdf <- renderUI({
    if (input$continuous_type == 'Uniform') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=\\frac{1}{b-a}, \\ a \\leq x \\leq b $$')
      )
    } else if (input$continuous_type == 'Normal') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=\\frac{1}{\\sqrt{2\\pi} \\sigma} \\mathrm{exp} [-\\frac{1}{2}(\\frac{x-\\mu}{\\sigma})^2], \\ -\\infty <x< \\infty $$')
      )
      
    } else if (input$continuous_type == 'Exponential') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=\\lambda e^{-\\lambda x}, \\ x \\geq 0, \\lambda >0$$')
      )
    } else if (input$continuous_type == 'Gamma') {
      withMathJax(
        helpText('$$P(X = x)=f_X(x)=\\frac{x^{\\alpha-1} e ^{-\\beta x} \\beta ^ \\alpha}{\\Gamma (\\alpha)}, \\ x>0, \\alpha, \\beta >0$$')
      )
    }
    
  })
  # continuous: cdf formula
  output$Con_equation_cdf <- renderUI({
    if (input$continuous_type == 'Uniform') {
      withMathJax(
        helpText('$$P(X\\leq x)=F_X(x)=\\frac{x-a}{b-a}, \\ a \\leq x \\leq b$$')
      )
    } else if (input$continuous_type == 'Normal') {
      withMathJax(
        helpText('$$P(X\\leq x)=F_X(x)= \\int_{-\\infty}^x f_X(x) \\mathrm{d} x, \\ -\\infty <x< \\infty$$')
      )
    } else if (input$continuous_type == 'Exponential') {
      withMathJax(
        helpText('$$P(X \\leq x)=F_X(x)=1-e^{- \\lambda x}, \\ x \\geq 0, \\lambda >0$$')
      )
    } else if (input$continuous_type == 'Gamma') {
      withMathJax(
        helpText('$$P(X\\leq x)=F_X(x)= \\int_0^x f_X(x) \\mathrm{d} x, \\ x>0$$')
      )
    }
  })
  
  # ------------ Extreme value statistics ---------------
  
  # data input 
  #--------------data display------------------
  Dataset <- reactive({
    if (input$datasource == 'import') {
      req(input$Dataset)
      df <- read.table(input$Dataset$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote,
                       stringsAsFactors = FALSE)
      colnames(df) = c('date', 'value')
      df
    }
  })
  
  output$range <- renderTable({ 
    req(input$Dataset)
    df1 = Dataset()
    qi = head(df1[,1], 1)
    zhi = tail(df1[,1], 1)
    `colnames<-`(data.frame(qi,zhi), c("From", "To"))
  })
  
  output$dataset_table <- DT::renderDataTable({   #upload data
    req(input$Dataset)
    DT::datatable( Dataset() )
  })
  output$Dataset_chart <- renderPlot({
    req(input$Dataset)
    df = Dataset()
    if (input$Sequencetype == "daily") {
      
      df$date = as.Date(df$date, format = "%Y-%m-%d")
      plot(df$date, df$value, type = "l", xlab='date', ylab='RV')
    } else {
      # yearly data
      plot(df$date, df$value, type = "l", xlab='year', ylab='RV')
    }
    
  })
  output$Dataset_bar <- renderPlot({
    req(input$Dataset)
    df = Dataset()
    hist(df$value,breaks=100, 
         col = "lightblue", main = NULL,
         xlab = 'RV', ylab = 'counts', 
         freq = TRUE)
  })
  # ------ --- AMS (dataset) created-----------
  AMS <- reactive({
    if (input$datasource == 'import') {
      req(input$Dataset)
      df = Dataset()
      if (input$Sequencetype == "daily") {
        # daily data
        df$date = as.Date(df$date, format = "%Y-%m-%d")
        df$year = as.numeric(strftime(df$date, format = "%Y"))
        years = unique(df$year)
        n = length(years)
        ams_value = ams_date = NULL
        for (i in 1:n) {
          df1 = df %>% filter(year == years[i])
          ams_value[i] = max(df1$value)
          ams_date[i] = as.character(strftime(df1$date[which.max(df1$value)], format = "%Y-%m-%d"))
        }
        data.frame(
          'date' = ams_date, 
          'year' = years,
          'value' =  ams_value
        )
      } else {
        # yearly data
        data.frame(
          'date' = df[, 1], 
          'year' = df[, 1],
          'value' =  df[, 2]
        )
      }
      
    } else {
      AMS_builtin
    }
    
  })
  # ----block maxima -----
  output$plot_Maxima_daily <- renderPlot({
    req(input$Dataset)
    ams = AMS()
    if (input$Sequencetype == "daily") {
      df = Dataset()
      df$date = as.Date(df$date, format = "%Y-%m-%d")
      ams$date = as.Date(ams$date, format = "%Y-%m-%d")
      plot(df$date, df$value, type = "l", xlab='date', ylab='RV')
      points(ams$date, ams$value, type = 'p', col = 'red')
    } else {
      # original data: yearly
      plot(ams$date, ams$value, type = 'b', col = 'red')
    }
    
  })
  # ---- POT ----
  output$POT_summary <- renderPrint({
    req(input$Dataset)
    df = Dataset()
    summary(df$value)
  })
  output$plot_POT_daily <- renderPlot({
    req(input$Dataset)
    if (input$Sequencetype == "daily") {
      df = Dataset()
      df$date = as.Date(df$date, format = "%Y-%m-%d")
      df_pot = POT_func(df, input$threshold)
      
      plot(df$date, df$value, type = 'l', 
           main = paste0('Sample size:', dim(df_pot)[1]),
           xlab = 'date', ylab = 'RV')
      
      points(df_pot$date, df_pot$value, type='p', col='red')
      points(df$date, rep(input$threshold, length(df$date)),
             type = 'l', col = 'red', lty = 2)
    }
    
  })
  
  # ------ GEV introduction ------
  output$GEV_plot_pdf <- renderPlot({
    quantiles <- sort(runif(500, 0, 1))
    x <- evd::qgev(quantiles, 
              shape = input$GEV_para_xi, 
              scale = input$GEV_para_sigma, 
              loc = input$GEV_para_mu)
    density <- evd::dgev(x, 
                    shape = input$GEV_para_xi, 
                    scale = input$GEV_para_sigma, 
                    loc = input$GEV_para_mu)
    df <- data.frame(
      'RV' = x,
      'PDF' = density
    )
    plot(df, type='l', xlim=c(0, 120), ylim =c(0, 0.5), main='PDF')
  })
  output$GEV_plot_cdf <- renderPlot({
    quantiles <- sort(runif(500, 0, 1))
    x <- evd::qgev(quantiles, 
              shape = input$GEV_para_xi, 
              scale = input$GEV_para_sigma, 
              loc = input$GEV_para_mu)
    probability <-  evd::pgev(x, 
                         shape = input$GEV_para_xi, 
                         scale = input$GEV_para_sigma, 
                         loc = input$GEV_para_mu)
    df <- data.frame(
      'RV' = x,
      'CDF' = probability
    )
    plot(df, type='l', xlim=c(0, 120), main='CDF' )
  })
  
  output$GEV_3_com_pdf <- renderPlot({
    quantiles <- sort(runif(500, 0, 1))
    x <- evd::qgev(quantiles, 
              shape = 0, 
              scale = 10, 
              loc = 30)
    d_gumbel <-  evd::dgev(x, 
                      shape = 0, 
                      scale = 10, 
                      loc = 30
    )
    d_frechet <- evd::dgev(x, 
                      shape = 0.2, 
                      scale = 10, 
                      loc = 30)
    d_weibull <- evd::dgev(x, 
                      shape = -0.2, 
                      scale = 10, 
                      loc = 30)
    
    plot(x, d_gumbel, type = 'l', lty = 2, col = 'red', 
         main = 'PDF', ylab = 'PDF', xlab = "RV")
    points(x, d_frechet, type = 'l', lty = 1, col = 'blue')
    points(x, d_weibull, type = 'l', lty = 3, col = 'black')
    legend('topright', legend=c("Gumbel", "Frechet", "Weibull"),
           col=c("red", "blue", "black"), lty=c(2,1,3) )
  })
  output$GEV_3_com_cdf <- renderPlot({
    quantiles <- sort(runif(500, 0, 1))
    x <- evd::qgev(quantiles, 
              shape = 0, 
              scale = 10, 
              loc = 30)
    p_gumbel <-  evd::pgev(x, 
                      shape = 0, 
                      scale = 10, 
                      loc = 30
    )
    p_frechet <- evd::pgev(x, 
                      shape = 0.2, 
                      scale = 10, 
                      loc = 30)
    p_weibull <- evd::pgev(x, 
                      shape = -0.2, 
                      scale = 10, 
                      loc = 30)
    
    plot(x, p_gumbel, type = 'l', lty = 2, col = 'red', 
         main = 'CDF', ylab = 'CDF', xlab = "RV")
    points(x, p_frechet, type = 'l', lty = 1, col = 'blue')
    points(x, p_weibull, type = 'l', lty = 3, col = 'black')
    #legend(60, 0.7, legend=c("Gumbel", "Frechet", "Weibull"),
    #       col=c("red", "blue", "black"), lty=c(2,1,3) )
    
  })
  # ------- empirical probability GEV --------
  df_empirical_prob <- reactive({
    # req(input$Dataset)
    ams = AMS()
    ams$m_rank = rank(ams$value) # rank of the values
    ams$P = round(ams$m_rank / (1+dim(ams)[1]), 4) # empirical probability
    ams$ReturnPeriod_T = round(1 / (1-ams$P), 1) # # empirical return period
    ams
  })
  output$empirical_prob_table <- renderDT({
    # req(input$Dataset)
    df_empirical_prob()
  })
  output$empirical_prob_chart <- renderPlot({
    # req(input$Dataset)
    df = df_empirical_prob()
    plot(log10(df$ReturnPeriod_T), df$value, 
         main = 'Return level plot',
         xlab = 'return period: years',
         ylab = 'Return level',
         type = 'p',
         xaxt = "n")
    axis(1, labels = c(1, 10,100,1000),
         at = log10(c(1, 10,100,1000)))
    grid(nx = 20, 
         ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
  })
  # --------- graphical method (parameter calibration) -----------
  
  df_GM <- reactive({
    # req(input$Dataset)
    df_empirical = df_empirical_prob()
    df_empirical$model_prob <- evd::pgev(df_empirical$value, 
                                    shape = input$GM_para_xi, 
                                    scale = input$GM_para_sigma, 
                                    loc = input$GM_para_mu)
    df_empirical$model_T <- round(
      1/ (1-df_empirical$model_prob), 1
    )  # modeled probabilities
    df_empirical$model_value <- round(
      evd::qgev(df_empirical$P, # empirical probability
           shape = input$GM_para_xi, 
           scale = input$GM_para_sigma, 
           loc = input$GM_para_mu)
    ) # modeled quantiles (return levels)
    df_empirical
  })
  output$GM_returnlevel_chart <- renderPlot({
    # req(input$Dataset)
    df = df_empirical_prob()
    
    model_p = seq(min(df$P), max(df$P) + 0.001, 0.001)
    model_T = 1/(1-model_p)
    model_value <- round(
      evd::qgev(model_p, 
           shape = input$GM_para_xi, 
           scale = input$GM_para_sigma, 
           loc = input$GM_para_mu)
    ) # modeled quantiles (return levels)
    
    plot(log10(df$ReturnPeriod_T), df$value, 
         main = 'Return level plot',
         xlab = 'return period: years',
         ylab = 'Return level',
         type = 'p',
         xaxt = "n")
    points(log10(model_T), model_value, type = 'l', col='red')
    axis(1, labels = c(1, 10,100,1000),
         at = log10(c(1, 10,100,1000)))
    grid(nx = 20, 
         ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
  })
  output$GM_goodness <- renderTable({
    # req(input$Dataset)
    df_GM = df_GM()
    rmse_overall = round(rmse(df_GM$value, df_GM$model_value), 2)
    mae_overall = round(mae(df_GM$value, df_GM$model_value), 2)
    R_squared_overall = summary(lm(value~model_value, data=df_GM))$r.squared 
    # --
    df_GM_upper = df_GM %>% filter(P > 0.9)
    rmse_upper = round(rmse(df_GM_upper$value, df_GM_upper$model_value), 2)
    mae_upper = round(mae(df_GM_upper$value, df_GM_upper$model_value), 2)
    R_squared_upper = summary(lm(value~model_value, data=df_GM_upper))$r.squared 
    
    data.frame(
      'Metrics' = c('RMSE', 'MAE', 'R2'),
      'Overall' = c(rmse_overall, mae_overall, R_squared_overall),
      'Upper_P_over_0.9' = c(rmse_upper, mae_upper, R_squared_upper)
    )
  })
  output$GM_pp_chart <- renderPlot({
    # req(input$Dataset) 
    df_GM = df_GM()
    plot(
      df_GM$P, df_GM$model_prob,
      main = 'Probability plot', xlab = 'empirical probability',
      ylab = 'model probability', 
      type='p', col = 'gray'
    )
    abline(0,1)
  })
  output$GM_qq_chart <- renderPlot({
    # req(input$Dataset)
    df_GM = df_GM()
    plot(
      df_GM$value, df_GM$model_value,
      main = 'Quantile plot', 
      xlab = 'empirical quantile',
      ylab = 'model quantile', 
      type='p', col = 'gray'
    )
    abline(0,1)
  })
  
  # ------ parameter estimation: parametric method ------
  GEV_mle <- reactive({
    # req(input$Dataset)
    df = AMS()
    if (input$Para_methods == 'Maximum Likelihood Estimation') {
      Pm = 'MLE'
    } else {
      Pm = 'Lmoments'
    }
    GEV_mle = fevd(df$value, method = Pm, type = input$Dis_type)
  })
  output$PM_returnlevel_chart <- renderPlot({
    
    if (input$Para_methods == 'Method of L-Moments' &
        input$Dis_type == 'Gumbel') {
      
    } else if (
      input$Para_methods == 'Maximum Likelihood Estimation'
    ) {
      # req(input$Dataset)
      GEV_mle = GEV_mle()
      plot.fevd(GEV_mle, type = "rl", 
                main = 'Return level plot')
    } else {
      # req(input$Dataset)
      df = df_empirical_prob()
      GEV_mle = GEV_mle()
      
      model_p = seq(min(df$P), max(df$P) + 0.001, 0.001)
      model_T = 1/(1-model_p)
      model_value <- round(
        evd::qgev(model_p, 
             shape = as.numeric(GEV_mle$results['shape']), 
             scale = as.numeric(GEV_mle$results['scale']), 
             loc = as.numeric(GEV_mle$results['location']))
      ) # modeled quantiles (return levels)
      
      plot(log10(df$ReturnPeriod_T), df$value, 
           main = 'Return level plot',
           xlab = 'return period: years',
           ylab = 'Return level',
           type = 'p',
           xaxt = "n")
      points(log10(model_T), model_value, type = 'l', col='red')
      axis(1, labels = c(1, 10,100,1000),
           at = log10(c(1, 10,100,1000)))
      grid(nx = 20, 
           ny = NULL,
           lty = 2,      # Grid line type
           col = "gray", # Grid line color
           lwd = 0.5)      # Grid line width
    }
    
  })
  output$PM_pp_chart <- renderPlot({
    if (input$Para_methods == 'Method of L-Moments' &
        input$Dis_type == 'Gumbel') {
      
    } else {
      # req(input$Dataset)
      GEV_mle = GEV_mle()
      plot.fevd(
        GEV_mle, 
        type = "probprob",
        main = 'Probability plot'
      ) 
    }
  })
  output$PM_qq_chart <- renderPlot({
    
    if (input$Para_methods == 'Method of L-Moments' &
        input$Dis_type == 'Gumbel') {
      
    } else {
      # req(input$Dataset)
      GEV_mle = GEV_mle()
      plot.fevd(
        GEV_mle, type = "qq",
        main = 'Quantile plot'
      )
    }
  })
  output$PM_results <- renderPrint({
    
    if (input$Para_methods == 'Method of L-Moments' &
        input$Dis_type == 'Gumbel') {
      print('As far, Gumbel parameter estimation with L-moments method is not supported!')
    } else {
      # req(input$Dataset)
      print(GEV_mle())
    }
  })
  # --------  GEV uncertainty analysis -------------
  # ----- sample time periods -------
  df_SA_timepriod <- reactive({
    # req(input$Dataset)
    ams = AMS() %>% filter(
      year >= input$period[1], year <= input$period[2]  
    )
    GEV_mle = fevd(ams$value, method = 'MLE', type = 'GEV')
    GEV_mle$location = as.numeric(GEV_mle$results$par['location'])
    GEV_mle$scale = as.numeric(GEV_mle$results$par['scale'])
    GEV_mle$shape = as.numeric(GEV_mle$results$par['shape'])
    
    return_period=input$SA_timeperiod_T
    return_level = evd::qgev( 
      1 - 1/return_period,
      loc = GEV_mle$location,
      scale = GEV_mle$scale,
      shape = GEV_mle$shape
    )
    GEV_mle$return_period = return_period
    GEV_mle$return_level = return_level
    GEV_mle
  })
  output$SA_timeperiod_plot <- renderPlot({
    # req(input$Dataset)
    GEV_mle = df_SA_timepriod()
    paras = paste0(
      'location = ', round(GEV_mle$location, 1),
      '\nscale = ', round(GEV_mle$scale, 1),
      '\nshape = ', round(GEV_mle$shape, 3)
    )
    plot.fevd(GEV_mle, type = "rl",
              main = 'Return level')
    points(GEV_mle$return_period, GEV_mle$return_level,
           type = 'p', pch = 4, col = 'red', cex = 1.5)
    text(5, quantile(GEV_mle$x, 0.999), paras)
    
  })
  output$SA_timepriod_returnlevel <- renderPrint({
    # req(input$Dataset)
    GEV_mle = df_SA_timepriod()
    print(
      paste0('Return level estimated: ',
             round(GEV_mle$return_level, 1)
      )
    )
  })
  # ------ probability distributions -----
  df_SA_distri_GEV <- reactive({
    # req(input$Dataset)
    ams = AMS()
    GEV_mle = fevd(ams$value, method = 'MLE', type = 'GEV')
    GEV_mle$location = as.numeric(GEV_mle$results$par['location'])
    GEV_mle$scale = as.numeric(GEV_mle$results$par['scale'])
    GEV_mle$shape = as.numeric(GEV_mle$results$par['shape'])
    
    return_period= c(10, 50, 100, 200, 500)
    return_level = evd::qgev( 1 - 1/return_period,
                         loc = GEV_mle$location,
                         scale = GEV_mle$scale,
                         shape = GEV_mle$shape
    )
    GEV_mle$return_period = return_period
    GEV_mle$return_level = return_level
    GEV_mle
  })
  df_SA_distri_Gumbel <- reactive({
    # req(input$Dataset)
    ams = AMS()
    GEV_mle = fevd(ams$value, method = 'MLE', type = 'Gumbel')
    GEV_mle$location = as.numeric(GEV_mle$results$par['location'])
    GEV_mle$scale = as.numeric(GEV_mle$results$par['scale'])
    
    return_period= c(10, 50, 100, 200, 500)
    return_level = evd::qgev( 1 - 1/return_period,
                         loc = GEV_mle$location,
                         scale = GEV_mle$scale,
                         shape = 0
    )
    GEV_mle$return_period = return_period
    GEV_mle$return_level = return_level
    GEV_mle
  })
  
  output$SA_distribution_plot <- renderPlot({
    # req(input$Dataset)
    df_SA_distri_Gumbel = df_SA_distri_Gumbel()
    df_SA_distri_GEV = df_SA_distri_GEV()
    df = df_empirical_prob()
    
    model_p = seq(min(df$P), max(df$P) + 0.001, 0.001)
    model_T = 1/(1-model_p)
    model_value_GEV <- round(
      evd::qgev(
        model_p, 
        shape = df_SA_distri_GEV$shape, 
        scale = df_SA_distri_GEV$scale, 
        loc = df_SA_distri_GEV$location
      )
    ) # modeled quantiles (return levels)
    model_value_Gumbel <- round(
      evd::qgev(model_p, 
           shape = 0, 
           scale = df_SA_distri_Gumbel$scale, 
           loc = df_SA_distri_Gumbel$location
      )
    ) # modeled quantiles (return levels)
    
    
    plot(log10(df$ReturnPeriod_T), df$value, 
         main = 'Return level plot',
         xlab = 'return period: years',
         ylab = 'Return level',
         type = 'p',
         xaxt = "n")
    points(log10(model_T), model_value_GEV, type = 'l', col='red')
    points(log10(model_T), model_value_Gumbel, type = 'l', col='blue')
    
    axis(1, labels = c(1, 10,100,1000),
         at = log10(c(1, 10,100,1000)))
    grid(nx = 20, 
         ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
    legend(
      'bottomright',
      lty = c(1, 1),
      col = c('red', 'blue'), 
      legend = c('GEV', 'Gumbel')
    )
  })
  output$SA_distribution_table <- renderTable({
    # req(input$Dataset)
    df_SA_distri_Gumbel = df_SA_distri_Gumbel()
    df_SA_distri_GEV = df_SA_distri_GEV()
    data.frame(
      'Return_period_T' = as.character(df_SA_distri_GEV$return_period),
      'GEV' = df_SA_distri_GEV$return_level,
      'Gumbel' = df_SA_distri_Gumbel$return_level
    )
  })
  # -------- parameter estimation methods --------
  df_SA_method_GEV <- reactive({
    # req(input$Dataset)
    ams = AMS()
    GEV_mle = fevd(ams$value, method = 'Lmoments', type = 'GEV')
    GEV_mle$location = as.numeric(GEV_mle$results['location'])
    GEV_mle$scale = as.numeric(GEV_mle$results['scale'])
    GEV_mle$shape = as.numeric(GEV_mle$results['shape'])
    
    return_period= c(10, 50, 100, 200, 500)
    return_level = evd::qgev( 1 - 1/return_period,
                         loc = GEV_mle$location,
                         scale = GEV_mle$scale,
                         shape = GEV_mle$shape
    )
    GEV_mle$return_period = return_period
    GEV_mle$return_level = return_level
    GEV_mle
  })
  
  output$SA_method_plot <- renderPlot({
    # req(input$Dataset)
    df_SA_method_GEV = df_SA_method_GEV() # Lmoments
    df_SA_distri_GEV = df_SA_distri_GEV() # mle
    df = df_empirical_prob()
    
    model_p = seq(min(df$P), max(df$P) + 0.001, 0.001)
    model_T = 1/(1-model_p)
    model_value_MLE <- round(
      evd::qgev(model_p, 
           shape = df_SA_distri_GEV$shape, 
           scale = df_SA_distri_GEV$scale, 
           loc = df_SA_distri_GEV$location)
    ) # modeled quantiles (return levels)
    model_value_Lmoments <- round(
      evd::qgev(model_p, 
           scale = df_SA_method_GEV$scale, 
           loc = df_SA_method_GEV$location,
           shape = df_SA_method_GEV$shape
      )
    ) # modeled quantiles (return levels)
    
    
    plot(log10(df$ReturnPeriod_T), df$value, 
         main = 'Return level plot',
         xlab = 'return period: years',
         ylab = 'Return level',
         type = 'p',
         xaxt = "n")
    points(log10(model_T), model_value_MLE, type = 'l', col='red')
    points(log10(model_T), model_value_Lmoments, type = 'l', col='blue')
    
    axis(1, labels = c(1, 10,100,1000),
         at = log10(c(1, 10,100,1000)))
    grid(nx = 20, 
         ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
    legend(
      'bottomright',
      lty = c(1, 1),
      col = c('red', 'blue'), 
      legend = c('MLE', 'Lmoments')
    )
  })
  
  output$SA_method_table <- renderTable({
    # req(input$Dataset)
    df_SA_method_GEV = df_SA_method_GEV()
    df_SA_distri_GEV = df_SA_distri_GEV()
    data.frame(
      'Return_period_T' = as.character(df_SA_distri_GEV$return_period),
      'MLE' = df_SA_distri_GEV$return_level,
      'Lmoments' = df_SA_method_GEV$return_level
    )
  })
  
  # --------------- flood risk ----------------------
  # ------Hazard compartment------
  output$FR_Hazard_returnlevel <- renderPlot({
    # req(input$Dataset)
    df = df_empirical_prob()
    
    model_p = seq(min(df$P), max(df$P) + 0.001, 0.001)
    model_T = 1/(1-model_p)
    model_value = evd::qgev(model_p, 
                       shape = input$FR_GEV_xi, 
                       scale = input$FR_GEV_sigma, 
                       loc = input$FR_GEV_mu
    )
    
    plot(log10(df$ReturnPeriod_T), df$value, 
         xlab = 'return period: years',
         ylab = 'Return level (discharge: m3/s)',
         type = 'p',
         xaxt = "n",
         ylim = c(min(c(model_value, df$value)),
                  max(c(model_value, df$value))
         )
    )
    points(log10(model_T), model_value, type = 'l', col='red')
    axis(1, labels = c(1, 10,100,1000),
         at = log10(c(1, 10,100,1000)))
    grid(nx = 20, 
         ny = NULL,
         lty = 2,      # Grid line type
         col = "gray", # Grid line color
         lwd = 0.5)      # Grid line width
    
  })
  output$FR_Hazard_GEV_paras <- renderPrint({
    # req(input$Dataset)
    ams = AMS()
    GEV_mle = fevd(ams$value, method = 'MLE', type = 'GEV')
    location = as.numeric(GEV_mle$results$par['location'])
    scale = as.numeric(GEV_mle$results$par['scale'])
    shape = as.numeric(GEV_mle$results$par['shape'])
    print(
      paste0('GEV parameters estimated by MLE: location: ',
             round(location, 2), '; scale: ',
             round(scale, 2), "; shape: ",
             round(shape, 4)
      )
    )
  })
  
  # ------ flood risk: river training, H-Q ------
  output$FR_QT_chart <- renderPlot({
    a = input$FR_QT_a
    b = input$FR_QT_b
    
    df = QH_table
    df$Water_level = a*df$Discharge ^ b + 
      c(runif(100, 0, 0.05), runif(300, 0, 0.03), runif(100, 0, 0.05))
    
    x2 = seq(min(df$Discharge), max(df$Discharge), 50)
    y2 = a*x2 ^b
    plot(x = df$Discharge, y = df$Water_level,
         type = 'p',
         main = 'Discharge - water table relationship',
         ylab = 'Water table (H: m.a.p)',
         xlab = 'Discharge (Q: m^3/s)')
    points(x2, y2,
           type = 'l', col = 'red', 
           cex = 2)
    
  })
  
  # ----------- time-varying flood risk -------------
  # ---- data show 
  output$Data_show_table <- renderDT({
    if (input$Data_show_type == 'Q-T relationship') {
      QH_table$Discharge = round(QH_table$Discharge)
      QH_table$Water_level = round(QH_table$Water_level, 2)
      QH_table
    } else if (input$Data_show_type == 'Exposure') {
      
      Table_exposure
    } else {
      Table_susceptibility
    }
  })
  output$Data_show_chart <- renderPlot({
    if (input$Data_show_type == 'Q-T relationship') {
      df = QH_table
      x2 = seq(min(df$Discharge), max(df$Discharge), 50)
      y2 = 0.748*x2 ^0.23
      plot(x = df$Discharge, y = df$Water_level,
           type = 'p',
           main = 'Discharge - water table relationship',
           ylab = 'Water table (H: m.a.p)',
           xlab = 'Discharge (Q: m^3/s)')
      points(x2, y2,
             type = 'l', col = 'red', 
             cex = 2)
      mtext(expression(H == 0.748*Q ^0.23), side=3, col = 'red')
      
    } else if (input$Data_show_type == 'Exposure') {
      
      y1 = Table_exposure$Height
      y2 = Table_exposure$Area
      
      # Draw first plot using axis y1
      par(mar = c(7, 5, 5, 4) + 0.3)         
      plot(y1, type = 'o', col = 'red', 
           xlab = 'index', 
           ylab = 'Height (m.a.p)')
      # set parameter new=True for a new axis
      par(new = TRUE) 
      # Draw second plot using axis y2
      plot(y2, pch = 15, type = 'o',col = 'blue', axes = FALSE, ylab = '')
      axis(side = 4, at = pretty(range(y2)))
      legend('bottomright', 
             legend = c('Height', 'Area'),
             col = c('red', 'blue'),
             pch = c(1,15)
      )
      mtext("Area (m^2)", side = 4, line = 3)
    } else {
      # vulnerability
      df = Table_susceptibility
      df$Object_type = as.factor(df$Object_type )
      ggplot(data = df, aes(x = Inundation_depth,
                            y = Relative_loss, 
                            color = Object_type, 
                            shape = Object_type)) +
        geom_point() + geom_line() + Theme_my + 
        labs(x='Inundation depth (m.a.p)', 
             y = 'Relative loss (%)') + 
        theme(legend.position = c(0.8, 0.25))
    }
    
  })
  output$Data_show_explain <- renderPrint({
    if (input$Data_show_type == 'Q-T relationship') {
      out = paste0(
        'The relationship between the amount of water flowing at a given point in a river or stream (usually at gauging stations) and the corresponding stage is known as stage-discharge relationship or rating curve, which can be algebraically represented with a power function'
      )
      print(out)
    } else if (input$Data_show_type == 'Exposure') {
      out = data.frame(
        object = c(
          "Industral",
          'Residential',
          'Public amenities',
          'Commercial I',
          'Commercial II',
          'Others'
        ),
        Area = aggregate(Area ~ Object_type, Table_exposure, FUN = sum)[,2]
      )
      print(out)
    } else {
      print(
        'Vulnerability functions express the correlations among damage ratios (i.e., the cost of damage as a percentage of the appraised value of a property), and damage states (i.e., inundation depths)'
      )
    }
  })
  # ------ time-varying flood risk sensitivity experiments ---------
  # -- flood frequency analysis and GEV model fitting
  TVFR_df_FFA <- reactive({
    # req(input$Dataset)
    ams = AMS()
    GEV_mle = fevd(ams$value, method = 'MLE', type = 'GEV')
    GEV_mle$location = as.numeric(GEV_mle$results$par['location'])
    GEV_mle$scale = as.numeric(GEV_mle$results$par['scale'])
    GEV_mle$shape = as.numeric(GEV_mle$results$par['shape'])
    
    return_period=input$SA_timeperiod_T
    return_level = evd::qgev( 1 - 1/return_period,
                         loc = GEV_mle$location,
                         scale = GEV_mle$scale,
                         shape = GEV_mle$shape)
    GEV_mle$return_period = return_period
    GEV_mle$return_level = return_level
    GEV_mle
  })
  # ----- threshold for flood occurrence -------
  # --- computed by the return period of defense system
  # --- which is the typical approach in hydraulic engineering design
  TVFR_df_threshold <- reactive({
    # req(input$Dataset)
    GEV_mle = TVFR_df_FFA()
    return_level_Q = evd::qgev(
      1 - 1/input$TVFR_para_returnperiod,
      loc = GEV_mle$location,
      scale = GEV_mle$scale,
      shape = GEV_mle$shape
    )
    return_level_H = (input$TVFR_para_QT_a) * return_level_Q ^ (input$TVFR_para_QT_b)
    data.frame(
      return_level_Q,
      return_level_H
    )
  })
  # plot the empirical probability plot of extreme seires
  # and the fitted GEV curve, 
  # and the flood defense system capacity (return level)
  output$TVFR_chart_FFA <- renderPlot({
    # req(input$Dataset)
    GEV_mle = TVFR_df_FFA()
    threshold = TVFR_df_threshold()
    
    paras = paste0(
      'location = ', round(GEV_mle$location, 1),
      '\nscale = ', round(GEV_mle$scale, 1),
      '\nshape = ', round(GEV_mle$shape, 3)
    )
    plot.fevd(GEV_mle, type = "rl",
              main = 'Return level')
    points(input$TVFR_para_returnperiod,
           threshold$return_level_Q,
           type = 'p', col = 'red') # defense system capacity
    text(5, quantile(GEV_mle$x, 0.999), paras)
  })
  output$TVFR_chart_FFA_text <- renderUI({
    # req(input$Dataset)
    threshold = TVFR_df_threshold()
    helpText(
      paste0('Defense level Q: ', round(threshold$return_level_Q,1),
             '(m3/s); H: ', round(threshold$return_level_H, 2),
             '(m.a.p).'), style = 'color:darkblue'
    )
  })
  # sample based on calibrated GEV distribution
  # number of years and number of runs (uncertainty considered)
  TVFR_df_sampling <- reactive({
    # req(input$Dataset)
    GEV_mle = TVFR_df_FFA()
    mu = GEV_mle$location
    sigma = GEV_mle$scale
    xi = GEV_mle$shape
    CR_mu = input$TVFR_para_mu
    years = input$TVFR_para_years
    runs = input$TVFR_para_runs
    Multi_n_gev_random(
      mu, sigma, xi, years, runs, CR_mu
    )
  })
  output$TVFR_chart_sampling <- renderPlot({
    # req(input$Dataset)
    threshold = TVFR_df_threshold()
    runs = input$TVFR_para_runs
    df = TVFR_df_sampling()
    if (runs == 1) {
      plot(df, xlab = 'year', ylab = 'Discharge (m^3/s)',
           main = 'Sampling outcomes',
           type = 'l', col = 'gray')
      abline(h = threshold$return_level_Q, col = 'red')
    } else {
      plot(df[,1], xlab = 'year', ylab = 'Discharge (m^3/s)',
           main = 'Sampling outcomes',
           type = 'l', col = 'gray', 
           ylim = c(min(df), max(df)))
      
      for (i in 2:runs) {
        points(df[,i], type = 'l', col='gray')
      }
      abline(h = threshold$return_level_Q, col = 'red')
    }
  })
  
  # ------- time-varying components -----------
  # exposure: areas
  # susceptibility: the relative loss 
  
  Table_exposure_TV <- reactive({
    Table_exposure %>% 
      mutate(
        Area = Area * (1 + input$TVFR_para_exposure/100),
        Unitvalue = Unitvalue * (1 + input$TVFR_para_exposure_unitvalue/100)
      )
  })
  
  Table_susceptibility_TV <- reactive({
    Table_susceptibility_functions %>% 
      mutate(
        a = a * (1 + input$TVFR_para_susceptibility/100)
      )
  })
  
  # ------- TVFR functional computation -------
  TVFR_EAD <- reactive({
    # req(input$Dataset)
    df = TVFR_df_sampling()
    Table_exposure_TV = Table_exposure_TV()
    Table_susceptibility_TV = Table_susceptibility_TV()
    threshold = TVFR_df_threshold()
    # when the flood level exceeds the return level of defense system,
    # the flood takes place and causes damages
    # n_years = input$TVFR_para_years
    a = input$TVFR_para_QT_a
    b = input$TVFR_para_QT_b
    n_runs = input$TVFR_para_runs
    inflaionrate = input$TVFR_para_inflation
    if (n_runs == 1) {
      EAD_out = EAD_years(
        df,
        QH_paras = c(a, b),
        H_defense = threshold$return_level_H,
        Inflation_rate = inflaionrate,
        Table_exposure = Table_exposure_TV,
        Table_susceptibility_functions = Table_susceptibility_TV
      )
      
    } else {
      
      EAD_out = df
      for (i in 1:n_runs) {
        EAD_out[, i] = EAD_years(
          df[, i],
          QH_paras = c(a, b),
          H_defense = threshold$return_level_H,
          Inflation_rate = inflaionrate,
          Table_exposure = Table_exposure_TV,
          Table_susceptibility_functions = Table_susceptibility_TV
        )
      }
      
    }
    
    EAD_out
  })
  
  # ------ mean annual expected annual damages(EAD) ------
  output$TVFR_EAD_sum <- renderPrint({
    # req(input$Dataset)
    EAD_out = TVFR_EAD() * 10^(-6)
    n_runs = input$TVFR_para_runs
    if (n_runs == 1) {
      mean_annul_EAD = round(mean(EAD_out), 2)
    } else {
      mean_annul_EAD = round(mean(colMeans(EAD_out)), 2)
      
    }
    print(
      paste('Expected annual damages (EAD): ', 
            mean_annul_EAD, 'M.EUR/a.')
    )
  })
  output$TVFR_chart_EAD <- renderPlot({
    # req(input$Dataset)
    n_runs = input$TVFR_para_runs
    EAD_out = TVFR_EAD()  * 10^(-6)
    # if (n_runs == 1) {
    #   # EAD_out is a vector
    #   plot(EAD_out, 
    #        type = 'l',
    #        xlab = 'year', 
    #        ylab = 'EAD (MEUR)')
    # } else {
      # EAD_out is a dataframe,
    #   plot(EAD_out[,1],
    #        type = 'l', col = 1,
    #        ylim = c(min(EAD_out), max(EAD_out)),
    #        xlab = 'year', 
    #        ylab = 'EAD (MEUR)')
    #   for (i in 1: n_runs) {
    #     points(EAD_out[,i],
    #            type = 'l', col = i)
    #   }
    # }
    ######### calculate the annual average damage
    # attention: the average deals with multiple runs
    if (n_runs == 1) {
      out = EAD_out
    } else {
      out = rowMeans(EAD_out)
    }
    plot(out, 
         type = 'l',
         xlab = 'year', 
         ylab = 'Expected average damages (M.EUR/a)')
  })
  # ------------------ solutions to exercises --------------
  # -----Solution to Quiz 1-----
  Multi_EAD_1 <- eventReactive(input$Solution1, {
    # --- parameter setting
    years <- seq(10, 200, 10)  # **** variable ****
    # years <- seq(10, 200, 10)
    runs <- 50
    GEV_mle = TVFR_df_FFA()
    mu = GEV_mle$location  # mu = 5490.2
    sigma = GEV_mle$scale  # sigma = 1781.8
    xi = GEV_mle$shape     # xi = -0.183
    CR_mu = 0
    a = 0.748
    b = 0.23
    return_level_Q = evd::qgev(
      1 - 1/20,  # return period of defense system
      loc = mu, scale = sigma, shape = xi
    )
    return_level_H = a * return_level_Q ^ b  # defense water stage
    Inflation_rate = 0
    
    #--- computing
    Multi_EAD_out = NULL
    for (i in 1:length(years)) {
      samples <- Multi_n_gev_random(
        mu, sigma, xi, years[i], runs, CR_mu
      )
      ead = NULL
      for (j in 1:runs) {
        ead[j] <- mean(EAD_years(
          samples[, j],
          QH_paras = c(a, b),
          H_defense = return_level_H,
          Inflation_rate = Inflation_rate,
          Table_exposure,
          Table_susceptibility_functions
        ))
      }
      
      Multi_EAD_out[i] <- mean(ead)
    }
    out <- data.frame(
      years,
      EAD = Multi_EAD_out / 1000000
    )
  })
  output$Solution_plot_1 <- renderPlot({
    df = Multi_EAD_1()
    EAD_mean = mean(df$EAD)
    ggplot(data = df, aes(x = years, y = EAD)) + 
      geom_point() + 
      geom_hline(yintercept = EAD_mean, linetype = "dashed", col = 'red')+
      Theme_my + 
      labs(x='number of simulation years', y ='EAD (M. EUR/a)')
  })
  
  Hint_textout_1 <- eventReactive(input$Hint1, {
    'Assign the return period of defense system as 20 years and 
    remain other parameters constant, then modify the simulation years, and examine 
    the response of EAD. multiple runs are suggested, given the uncertainty in sampling.'
  })
  output$Hint_text_1 <- renderUI({
    helpText(Hint_textout_1())
  })
  
  # -----Solution to Quiz 2-----
  Multi_EAD_2 <- eventReactive(input$Solution2, {
    # --- parameter setting
    years <- 100
    runs <- 50
    GEV_mle = TVFR_df_FFA()
    mu = GEV_mle$location
    sigma = GEV_mle$scale
    xi = GEV_mle$shape
    CR_mu = 0
    a = 0.748
    b = 0.23
    returnperiods <- seq(10, 100, 10)  # *****
    return_level_H <- NULL
    for (i in 1:length(returnperiods)) {
      return_level_Q = evd::qgev(
        1 - 1/returnperiods[i],  # return period of defense system
        loc = mu, scale = sigma, shape = xi
      )
      return_level_H[i] = a * return_level_Q ^ b  # defense water stage
    }
    
    Inflation_rate = 0
    
    #--- computing
    samples <- Multi_n_gev_random(
      mu, sigma, xi, years, runs, CR_mu
    )
    Multi_EAD_out = NULL
    for (i in 1:length(return_level_H)) {
      ead = NULL
      for (j in 1:runs) {
        ead[j] <- mean(EAD_years(
          samples[, j],
          QH_paras = c(a, b),
          H_defense = return_level_H[i],
          Inflation_rate=Inflation_rate,
          Table_exposure,
          Table_susceptibility_functions
        ) )
      }
      
      Multi_EAD_out[i] <- mean(ead)
    }
    out <- data.frame(
      returnperiods,
      EAD = Multi_EAD_out / 1000000
    )
  })
  output$Solution_plot_2 <- renderPlot({
    df = Multi_EAD_2()
    EAD_mean = mean(df$EAD)
    ggplot(data = df, aes(x = returnperiods, y = EAD)) + 
      geom_point() + geom_line(col = 'lightblue') + 
      # geom_hline(yintercept = EAD_mean, linetype = "dashed", col = 'red')+
      Theme_my + 
      labs(x='Return periods of defense system', y ='EAD (M. EUR/a)')
  })
  
  Hint_textout_2 <- eventReactive(input$Hint2, {
    'Assign the simulation years as 100 and 
    remain other parameters constant, then modify the return period of defense system,
    and examine the response of EAD.'
  })
  output$Hint_text_2 <- renderUI({
    helpText(Hint_textout_2())
  })
  
  # -----Solution to Quiz 3-----
  Multi_EAD_3 <- eventReactive(input$Solution3, {
    # --- parameter setting
    years <- 100
    runs <- 50
    GEV_mle = TVFR_df_FFA()
    mu = GEV_mle$location
    sigma = GEV_mle$scale
    xi = GEV_mle$shape
    CR_mu = 0
    a = seq(0.6, 0.9, 0.02) # ***** 0.748
    b = 0.23
    
    return_level_Q = evd::qgev(
      1 - 1/20,  # return period of defense system
      loc = mu, scale = sigma, shape = xi
    )
    return_level_H = a * return_level_Q ^ b  # defense water stage
    Inflation_rate = 0
    
    #--- computing
    Multi_EAD_out = NULL
    samples <- Multi_n_gev_random(
      mu, sigma, xi, years, runs, CR_mu
    )
    for (i in 1:length(a)) {
      ead = NULL
      for (j in 1:runs) {
        ead[j] <- mean(EAD_years(
          samples[, j],
          QH_paras = c(a[i], b),
          H_defense = return_level_H[i],
          Inflation_rate=Inflation_rate,
          Table_exposure,
          Table_susceptibility_functions
        ))
      }
      
      Multi_EAD_out[i] <- mean(ead)
    }
    out <- data.frame(
      a,
      EAD = Multi_EAD_out / 1000000
    )
  })
  output$Solution_plot_3 <- renderPlot({
    df = Multi_EAD_3()
    # EAD_mean = mean(df$EAD)
    ggplot(data = df, aes(x = a, y = EAD)) + 
      geom_point() + geom_line(col = 'lightblue') + 
      # geom_hline(yintercept = EAD_mean, linetype = "dashed", col = 'red')+
      Theme_my + 
      labs(x='a in rating curve', y ='EAD (M. EUR/a)')
  })
  
  Hint_textout_3 <- eventReactive(input$Hint3, {
    'Two parameters a and b in rating curve (water table - discharge function),
    examine the response of EAD to changes in a and b individually.'
  })
  output$Hint_text_3 <- renderUI({
    helpText(Hint_textout_3())
  })
  
  # -----Solution to Quiz 4-----
  Multi_EAD_4 <- eventReactive(input$Solution4, {
    # --- parameter setting
    years <- 100
    runs <- 50
    GEV_mle = TVFR_df_FFA()
    mu = GEV_mle$location
    sigma = GEV_mle$scale
    xi = GEV_mle$shape
    CR_mu = 0
    a = 0.748
    b = 0.23
    
    return_level_Q = evd::qgev(
      1 - 1/20,  # return period of defense system
      loc = mu, scale = sigma, shape = xi
    )
    return_level_H = a * return_level_Q ^ b  # defense water stage
    Inflation_rate = 0
    CR_expo <- seq(-5, 5, 1)
    
    #--- computing
    samples <- Multi_n_gev_random(
      mu, sigma, xi, years, runs, CR_mu
    )
    Multi_EAD_out = NULL
    for (i in 1:length(CR_expo)) {
      Exposure_TV <- Table_exposure %>% 
        mutate(
          Area = Area * (1 + CR_expo[i]/100)
        )
      ead = NULL
      for (j in 1:runs) {
        ead[j] <- mean(EAD_years(
          samples[, j],
          QH_paras = c(a, b),
          H_defense = return_level_H,
          Inflation_rate = Inflation_rate,
          Table_exposure = Exposure_TV,
          Table_susceptibility_functions
        ))
      }
      
      Multi_EAD_out[i] <- mean(ead)
    }
    out <- data.frame(
      CR_expo,
      EAD = Multi_EAD_out / 1000000
    )
  })
  output$Solution_plot_4 <- renderPlot({
    df = Multi_EAD_4()
    # EAD_mean = mean(df$EAD)
    ggplot(data = df, aes(x = CR_expo, y = EAD)) + 
      geom_point() + geom_line(col = 'lightblue') + 
      # geom_hline(yintercept = EAD_mean, linetype = "dashed", col = 'red')+
      Theme_my + 
      labs(x='change ratio of exposure area (%)', y ='EAD (M. EUR/a)')
  })
  
  Hint_textout_4 <- eventReactive(input$Hint4, {
    'Keep other parameters constant, and compute the EAD for a set of 
    the Change ratio values of exposure area or unit value, examine the relationship .'
  })
  output$Hint_text_4 <- renderUI({
    helpText(Hint_textout_4())
  })
  
  # -----Solution to Quiz 5-----
  Multi_EAD_5 <- eventReactive(input$Solution5, {
    # --- parameter setting
    years = 50
    runs = 50
    GEV_mle = TVFR_df_FFA()
    mu = GEV_mle$location
    sigma = GEV_mle$scale
    xi = GEV_mle$shape
    CR_mu = 1.5
    a = 0.748
    b = 0.23
    
    returnperiods = seq(10, 200, 10)
    return_level_Q = evd::qgev(
      1 - 1/returnperiods,  # return period of defense system
      loc = mu, scale = sigma, shape = xi
    )
    return_level_H = a * return_level_Q ^ b  # defense water stage
    Inflation_rate = 0
    CR_expo = 2
    
    Exposure_TV = Table_exposure %>% 
      mutate(
        Area = Area * (1 + CR_expo/100),
        Unitvalue = Unitvalue * (1 + CR_expo/100)
      )
    #--- computing
    samples <- Multi_n_gev_random(
      mu, sigma, xi, years, runs, CR_mu
    )
    Multi_EAD_out = NULL
    
    for (i in 1:length(return_level_Q)) {
      
      ead = NULL
      for (j in 1:runs) {
        ead[j] <- mean(EAD_years(
          samples[, j],
          QH_paras = c(a, b),
          H_defense = return_level_H[i],
          Inflation_rate = Inflation_rate,
          Table_exposure = Exposure_TV,
          Table_susceptibility_functions
        ))
      }
      
      Multi_EAD_out[i] <- mean(ead)
    }
    out <- data.frame(
      returnperiods,
      EAD = Multi_EAD_out / 1000000
    )
  })
  output$Solution_plot_5 <- renderPlot({
    df = Multi_EAD_5()
    # EAD_mean = mean(df$EAD)
    ggplot(data = df, aes(x = returnperiods, y = EAD)) + 
      geom_point() + geom_line(col = 'lightblue') + 
      geom_hline(yintercept = 10, linetype = "dashed", col = 'red')+
      Theme_my + 
      labs(x='Return period of defense system', y ='EAD (M. EUR/a)')
  })
  
  Hint_textout_5 <- eventReactive(input$Hint5, {
    'Set the simulation years as 50, change ratio of location parameter as 1.5 %/a;
    and select the variations in exposure features, and compute the EAD for a set of 
    the return periods of defense system with other parameters remaining unchanged'
  })
  output$Hint_text_5 <- renderUI({
    helpText(Hint_textout_5())
  })
  
  ##### ----- Server END ------ #####
}

# Run the application 
shinyApp(ui = ui, server = server)

