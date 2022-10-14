# Web-based Application for Flood Risk Seminar in UP
## Introduction
This web-based application is designed under the context of regular flood risk seminar given by Professor Bruno Merz in Potsdam University. This application is aimed to provide an interactive GUI-based interface to help the college students develop basic concepts about flood frequency analysis and time-varying flood risk.
## Developing tool
R project (4.2.1) and Shiny (1.7.2) package
Attached packages include: 

extraDistr_1.9.1  evd_2.3-6.1       forcats_0.5.2     stringr_1.4.1     dplyr_1.0.10      purrr_0.3.5       readr_2.1.3      tidyr_1.2.1       tibble_3.1.8      ggplot2_3.3.6     tidyverse_1.3.2   DT_0.25           shinythemes_1.2.0 extRemes_2.1-2    distillery_1.2-1  Lmoments_1.3-1 

This application can also be accessed via the link: https://guanxiaoxiang-gfz.shinyapps.io/FloodRisk/ by any browser. 
## Contents
- **Fundamental statistics** covering the basic concepts and knowledge on the statistics and probability, including random variable, descriptive measures and distribution functions. Several common probability distribution functions are introduced as examples.
- **Extreme value statistics** focusing on the Generalized Extreme Value (GEV) distribution to domenstrate the basic concepts in extreme value theorem. This panel also provides interface to play the game of extreme value sampling, distribution fitting and uncertainty analysis
- **Flood Frequency Analysis** introduing the concepts of hazard, exposure and vulnerability. 
- **Time-varying Flood Risk** an interactive application to play the game of time-varying flood risk assessment, where the annual expected loss to flood hazards are computed as the indicator of risk based on assumed scenarios of hazard, exposure and vulnerability. 
## File structure
- **data/** accomodates the example data of daily time series of dicharges
- **rsconnect/**: application deploy info
- **www/**: some illustrations used in this application

## Developer
[Xiaoxiang Guan](https://www.gfz-potsdam.de/staff/guan.xiaoxiang/sec44)

Email: guan@gfz-potsdam.de