#IMPORTS
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')

# Lectura del fitxer i preparació de les dades
cens <- read.csv("CensusIncome_clean.csv", sep=",")
head(cens)

# Edat
## Distribució d'edats
ggplot(cens, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Age Distribution")

age.bp <- boxplot(cens$age, main="Age", xlab = "Age", horizontal=TRUE)

#### https://cran.r-project.org/web/packages/fitdistrplus/fitdistrplus.pdf
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("aursiber/fitdistrplus")
library(fitdistrplus)
if (!require('logspline')) install.packages('logspline'); library('logspline')

# ks.test(cens$age, "pnorm", mean=mean(cens$age), sd=sd(cens$age))
# fits <- list(no = fitdistr(cens$age, "normal"),
#              we = fitdistr(cens$age, "weibull"))
# sapply(fits, function(i) i$loglik)
#### https://rpubs.com/Liam-O/Data604_Wk6
fitdistr(cens$age, "normal")
discrete.age <- cens$age
descdist(discrete.age, discrete = FALSE)
fit.norm <- fitdist(discrete.age, "norm")
denscomp(fit.norm)

#plot(fit.norm)

## Normalitat
#### https://www.ucipfg.com/Repositorio/MGAP/MGAP-05/BLOQUE-ACADEMICO/Unidad-2/complementarias/intervalo_de_confianza_2012.pdf

## Interval de confiança
#### https://www.youtube.com/watch?v=WZimlAIzza0
#### https://www.youtube.com/watch?v=wbs3zQ60DPU
#### https://www.youtube.com/watch?v=6u5DVIy-BmY

#### https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test

IC <- function(x, NC) {
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  z <- abs(qnorm(((1-NC)/2)))
  errorst <- sd/sqrt(n)
  lim_inf <- mean - (z*errorst)
  lim_sup <- mean + (z*errorst)
  output <- data.frame(NC,n,mean,sd,z,errorst, lim_inf, lim_sup)
  return(output)
}

## Càlculs
IC(cens$age, 0.95)
IC(cens$age, 0.90)

t.test(cens$age, conf.level = 0.95)
t.test(cens$age, conf.level = 0.90)
## Interpretació
#### https://www.youtube.com/watch?v=WZimlAIzza0

# Salari
## Pregunta de recerca
## Hipòtesi
## Test a aplicar
## Càlcul
## Conclusió