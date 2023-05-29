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
## Brief manual
there are overall 5 panels in this application: Overview, Fundamental statistics, Extreme value statistics, Flood risk (FR) and Time-varying FR
### 1. Fundamental statistics
there are 3 sub-panels under the this panel: Random variable, Descriptive measures and Parameter estimation. The last two sub-panles mainly introduce the description and concepts on key moments and the 3 parameter estimation methods.
    
Under Random variable sub-panel, widgets are inserted for distribution pattern illustration for two types of curves: discrete and continuous random variables.  

### 2. Extreme value statistics
this panel covers the main issues on extreme value statistics includeing sampling method, Generalized Extreme Value (GEV), GEV parameter estimation and Uncertainty analysis.

|sub-panel | Contents | functions |
| -----    | ------   | --------- |
| sampling method | block maxima and peak-over-threshold | data options: built-in or import from other sources (text files)
| Generalized Extreme Value (GEV) | main concepts about GEV, features of GEV curves and parameters, and tail behaviours| animate widgets for GEV shape illustration |
| GEV parameter estimation | estimation methods: graphic method and non-parametric methods | graphic method: empirical probability formula and interactive widgets for parameter estimation; non-parametric methods: maximum likelihood estimation and method of L-moments |
|Uncertainty analysis | uncertainty sources: sample data, distribution type and parameter estimation method | widgets for argument adjustment to show the sensitivity of frequency analysis results to the uncertainty sources |

### 3. Flood risk (FR)
there are 3 sub-panels: Graphical concept, Hazard and Vulnerability. The hazard component in flood risk analysis is illustrated from the aspects of climate change and river training. 
| aspect | contents | function |
| ------ | -------- | -------- |
|climate change | extreme value distribution |three GEV parameters: location, scale, and shape |
| river training | changes in river channel cross-section | river water table - discharge relationship |

### 4. Time-varying FR
this panel provides the exercises on time-varying flood risk analysis. The time-varying components include water table - discharge relationship (H-Q relationship, river training), GEV parameters (climate change), exposure area and vulnerability. 

| Component | Content | Contols |
| --------- | ------- | ------- |
| rivering training | H-Q relationship: $H = aQ^b$ | two time-varying parameters: a and b |
| Climate change | GEV distrition | three GEV parameters |
| Exposure | exposure area | time-varying exposure area (%/a) |
| Vulnerability | vulnerability of different assets to flooding | time-varying vulnerability|

There are also 5 quiz to be solved in the exercises panel. The tips and solution are also provided. 

## File structure
- **data/** accomodates the example data of daily time series of dicharges
- **rsconnect/**: application deploy info
- **www/**: some illustrations used in this application
- **app.R**: the source codes for the application
- **vulnerability curve.R**: backup code for vulnerability component in flood risk analysis exercises

## Developer
[Xiaoxiang Guan](https://www.gfz-potsdam.de/staff/guan.xiaoxiang/sec44)