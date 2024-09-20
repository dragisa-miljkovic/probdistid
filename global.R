library(shiny)
library(shinythemes)
library(jsonlite)  # For toJSON function

library(R6)
library(minpack.lm) # for nlsLM
# library(sfsmisc) # for AIC and BIC
library(pracma) # for price function
library(extraDistr) # for pnaka function
library(VGAM) # for rice

library(jsonlite)
library(xml2)
library(magrittr)



# Load R files
# source("model_fitting.R")
source("Distributions.R")
source("functions.R")

# Add the www directory as a resource path
addResourcePath("www", "www")