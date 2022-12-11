#0 Imports

library(knitr)
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('VIM')) install.packages('VIM'); library('VIM')
if (!require('fitdistrplus')) install.packages('fitdistrplus'); library('fitdistrplus')
require(gridExtra)


#******
#1 - Lectura del fitxer
#******

#Load file
df <- read.csv("datSat_Air.csv", sep=",")