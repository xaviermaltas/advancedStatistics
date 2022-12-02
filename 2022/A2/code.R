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
df <- read.csv("gpa_clean.csv", sep=",")

#******
#2 - Estadística descriptiva i visualització
#******

##Anàlisi descriptiva
##DF descriptive analysis
#Dimensions
dim(df)
#df structure
str(df)
#Head data sample
head(df)
#Summary
summary(df)
#Class per each var
sapply(df,class)


##Visualització

#1
# sat distribution
ggplot( df, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution")


#colgpa distribution
#discrete var creation
discrete.colgpa <- df$colgpa
#plot
ggplot( df, aes(x=discrete.colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution")


#2
##sat-female
df.female <- df %>% filter(female == TRUE)
df.male <- df %>% filter(female == FALSE)
#female
satfemale.ggplot <- ggplot( df.female, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution Female")
satfemale.ggplot

#male
satmale.ggplot <- ggplot( df.male, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution Male")
satmale.ggplot

#ggplot grid
#grid.arrange(satfemale.ggplot, satmale.ggplot, ncol=2)

##sat-athlete
df.athletes <- df %>% filter(athlete == TRUE)
df.noathletes <- df %>% filter(athlete == FALSE)

#athletes
satAthletes.ggplot <- ggplot( df.athletes, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution Athletes")
satAthletes.ggplot

#no athletes
satNoathletes.ggplot <- ggplot( df.noathletes, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution No Athletes")
satNoathletes.ggplot


##sat-race
df.white <- df %>% filter(white == TRUE)
df.black <- df %>% filter(black == FALSE)
df.noRace <- df %>% filter(black == FALSE & white == FALSE)
count(df.noRace)

#white
satWhite.ggplot <- ggplot( df.white, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution White")
satWhite.ggplot

#black
satBlack.ggplot <- ggplot( df.black, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution Black")
satBlack.ggplot

#no race
satNorace.ggplot <- ggplot( df.noRace, aes(x=sat)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "sat Distribution No Race")
satNorace.ggplot



#3
##colgpa-female
#female
colgpafemale.ggplot <- ggplot( df.female, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution Female")
colgpafemale.ggplot

#male
colgpamale.ggplot <- ggplot( df.male, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution Male")
colgpamale.ggplot

#ggplot grid
#grid.arrange(colgpafemale.ggplot, colgpamale.ggplot, ncol=2)


##colgpa-athlete
#athletes
colgpaAthletes.ggplot <- ggplot( df.athletes, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution Athletes")
colgpaAthletes.ggplot

#no athletes
colgpaNoathletes.ggplot <- ggplot( df.noathletes, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution No Athletes")
colgpaNoathletes.ggplot



##colgpa-race
#white
colgpaWhite.ggplot <- ggplot( df.white, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution White")
colgpaWhite.ggplot

#black
colgpaBlack.ggplot <- ggplot( df.black, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution Black")
colgpaBlack.ggplot

#no race
colgpaNorace.ggplot <- ggplot( df.noRace, aes(x=colgpa)) +
  geom_histogram( aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="indianred3") + 
  labs(title = "colgpa Distribution No Race")
colgpaNorace.ggplot

#******
# Interval de confiança de la mitjana poblacional de la variable _'sat'_ i _'colpga'_
#******

## Supòsits

#summary
summary(df$sat)
summary(df$colgpa)

#metrics table
idx <- c("sat","colgpa")
mean <- as.vector(sapply(df[,idx], mean, na.rm=TRUE))
sd <- as.vector(sapply(df[,idx], sd, na.rm=TRUE))
median <- as.vector(sapply(df[,idx], median, na.rm=TRUE))
IQR <- as.vector(sapply(df[,idx],IQR, na.rm=TRUE))

#table creation
table.satcolgpa <- kable(data.frame(Variables=names(df[idx]),
                                Mean = mean,
                                StandardDeviation = sd,
                                Median = median,
                                IQR = IQR),digits=2, caption="Table")
table.satcolgpa

#metrics
sd(df$sat)

#sat Box plot
boxplot(df$sat, main="Sat Boxplot Distribution")
#colgpa Box plot
boxplot(df$colgpa, main="Colgp Boxplot Distribution")

## Funció de càlcul de l’interval de confiança

IC <- function(x, NC) {
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  z <- abs(qnorm(((1-NC)/2)))
  errorst <- sd/sqrt(n)
  lim_inf <- mean - (z*errorst)
  lim_sup <- mean + (z*errorst)
  output <- data.frame(NC, n, mean, sd, z, errorst, lim_inf, lim_sup)
  return(output)
}


## Interval de confiança de la variable _'sat'_

#Intervals de confinaça
ic95.sat <- IC(df$sat, 0.95)
ic90.sat <- IC(df$sat, 0.90)

#t.test
t.test(df$sat, conf.level = 0.95)
t.test(df$sat, conf.level = 0.90)


## Interval de confiança de la variable _'colgpa'_

#Intervals de confinaça
ic95.colgpa <- IC(df$colgpa, 0.95)
ic90.colgpa <- IC(df$colgpa, 0.90)

#t.test
t.test(df$colgpa, conf.level = 0.95)
t.test(df$colgpa, conf.level = 0.90)

#******
#  Ser atleta influeix a la nota?
#******

## Anàlisi visual

##src: https://stackoverflow.com/questions/46821524/create-a-manual-legend-for-density-plots-in-r-ggplot2
#colgpa ath-noAth density ggplot
colgpaAthNotath.ggplot <- ggplot() +
  geom_density(aes(x = df.athletes$colgpa, color = 'Athletes'), alpha=.5, fill="indianred3") +
  geom_density(aes(x = df.noathletes$colgpa, color = 'No Athletes'), alpha=.5, fill="lightblue") +
  xlab("colgpa") + ylab("Density") + ggtitle('colgpa Distribution Athletes-No athletes') +
  theme(legend.position = 'right') +
  scale_color_manual(values = c('Athletes' = 'red', 'No Athletes' = 'blue'))
colgpaAthNotath.ggplot


## Funció per al contrast de mitjanes

tTest <- function( x1, x2, halternativa="twosided", C=95 ){
  
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  sd1 <- sd(x1)
  sd2 <- sd(x2)
  alfa <- (1-C/100)
  
  #src: https://www.investopedia.com/terms/t/t-test.asp#:~:text=two%20sample%20sets.-,T%2DScore,validity%20of%20the%20null%20hypothesis.
  degreefreedom <- ( (sd1**2/n1 + sd2**2/n2)**2 ) / ( (sd1**2/n1)**2/(n1-1) + (sd2**2/n2)**2/(n2-1))
  t <- (mean1-mean2) / (sqrt( sd1**2/n1 + sd2**2/n2 ))
  
  if (halternativa=="twosided"){
    tcritical <- qt( alfa/2, degreefreedom, lower.tail=FALSE ) 
    pvalue <- pt( abs(t), degreefreedom, lower.tail=FALSE )*2 
  }
  else if (halternativa=="<"){
    tcritical <- qt( alfa, degreefreedom, lower.tail=TRUE )
    pvalue <- pt( t, degreefreedom, lower.tail=TRUE )
  }
  else{ #">"
    tcritical <- qt( alfa, degreefreedom, lower.tail=FALSE )
    pvalue <- pt( t, degreefreedom, lower.tail=FALSE )
  }
  #Resultat en un named vector
  output <- c(alfa, mean1, mean2, t, tcritical, pvalue)
  names(output) <- c("alfa", "mean1", "mean2", "t", "tcritical", "pvalue")
  return (output)
}



## Justificació del test a aplicar
df.athletes.colgpa <- df.athletes$colgpa
df.noathletes.colgpa <-df.noathletes$colgpa

#atheltes

df.athletes.nrow <- nrow(df.athletes)
print(paste("Athletes is a ", (df.athletes.nrow/(nrow(df))) *100, "% of the set"))
pie(table(df$athlete), main="Athletes-No athletes Pie Plot",labels = c("No Athletes","Athletes"))

#athletes norm
fit.norm.athletes.colgpa <- fitdist(df.athletes.colgpa, "norm")
denscomp(fit.norm.athletes.colgpa, main= "Histogram and theoretical densities - Athletes")

#No atheltes
#No athletes norm
fit.norm.noathletes.colgpa <- fitdist(df.noathletes.colgpa, "norm")
denscomp(fit.norm.noathletes.colgpa, main= "Histogram and theoretical densities - No athletes")


var.test(df.athletes.colgpa, df.noathletes.colgpa)


## Càlcul

ttest.athNoAth.colgpa.95<-tTest( df.athletes.colgpa, df.noathletes.colgpa, "<", 95); ttest.athNoAth.colgpa.95
t.test( df.athletes.colgpa, df.noathletes.colgpa, alternative="less", conf.level=0.95)


#******
#  Les dones tenen millor nota que els homes?
#******
## Anàlisi visual

#colgpa female-male density ggplot
colgpaFemaleMale.ggplot <- ggplot() +
  geom_density(aes(x = df.female$colgpa, color = 'Female'), alpha=.5, fill="indianred3") +
  geom_density(aes(x = df.male$colgpa, color = 'Male'), alpha=.5, fill="lightblue") +
  xlab("colgpa") + ylab("Density") + ggtitle('colgpa Distribution Female-Male') +
  theme(legend.position = 'right') +
  scale_color_manual(values = c('Female' = 'red', 'Male' = 'blue'))
colgpaFemaleMale.ggplot

#Densities comparation - real and theoretical
df.female.colgpa <- df.female$colgpa
df.female.nrow <- nrow(df.female); df.female.nrow

df.male.colgpa <-df.male$colgpa
df.male.nrow <- nrow(df.male); df.male.nrow

#Female
fit.norm.female.colgpa <- fitdist(df.female.colgpa, "norm")
denscomp(fit.norm.female.colgpa, main= "Histogram and theoretical densities - Female")

#Male
fit.norm.male.colgpa <- fitdist(df.male.colgpa, "norm")
denscomp(fit.norm.male.colgpa, main= "Histogram and theoretical densities - Male")

## Funció
## Pregunta de recerca
## Hipòtesi nul·la i l'alternativa
## Justificació del test a aplicar
## Càlcul

ttest.femaleMale.colgpa.95<-tTest( df.female.colgpa, df.male.colgpa, ">", 95); ttest.femaleMale.colgpa.95
t.test( df.female.colgpa, df.male.colgpa, alternative="greater", conf.level=0.95)

## Interpretació del test



#******
#  Hi ha diferències a la nota segons la raça?
#******
## Anàlisi visual
#colgpa black-white-noRace density ggplot
colgpaBlackWhiteNorace.ggplot <- ggplot() +
  geom_density(aes(x = df.black$colgpa, color = 'Black'), alpha=.5, fill="indianred3") +
  geom_density(aes(x = df.white$colgpa, color = 'White'), alpha=.5, fill="lightblue") +
  geom_density(aes(x = df.noRace$colgpa, color = 'No race'), alpha=.5, fill="white") +
  xlab("colgpa") + ylab("Density") + ggtitle('colgpa Distribution Black-White-No race') +
  theme(legend.position = 'right') +
  scale_color_manual(values = c('Black' = 'red', 'White' = 'blue', 'No race' = 'green'))
colgpaBlackWhiteNorace.ggplot


#Densities comparation - real and theoretical
df.black.colgpa <- df.black$colgpa
df.black.nrow <- nrow(df.black); df.black.nrow

df.white.colgpa <-df.white$colgpa
df.white.nrow <- nrow(df.white); df.white.nrow

df.noRace.colgpa <-df.noRace$colgpa
df.noRace.nrow <- nrow(df.noRace); df.noRace.nrow

#Black
fit.norm.black.colgpa <- fitdist(df.black.colgpa, "norm")
denscomp(fit.norm.black.colgpa, main= "Histogram and theoretical densities - Black")

#White
fit.norm.white.colgpa <- fitdist(df.white.colgpa, "norm")
denscomp(fit.norm.white.colgpa, main= "Histogram and theoretical densities - White")

#No race
fit.norm.noRace.colgpa <- fitdist(df.noRace.colgpa, "norm")
denscomp(fit.norm.noRace.colgpa, main= "Histogram and theoretical densities - No Race")


## Funció
## Pregunta de recerca
## Hipòtesi nul·la i l'alternativa
## Justificació del test a aplicar
## Càlcul

ttest.blackWhite.colgpa.95<-tTest( df.black.colgpa, df.white.colgpa, ">", 95); ttest.blackWhite.colgpa.95
t.test( df.black.colgpa, df.white.colgpa, alternative="greater", conf.level=0.95)

## Interpretació del test



#******
#  Proporció d’atletes
# ******

## Anàlisi visual

#dfAth and dfNoath nrows
df.athletes.nrow <- nrow(df.athletes); df.athletes.nrow
df.noathletes.nrow <- nrow(df.noathletes); df.noathletes.nrow

#Ath-NoAth pie plot
df.athNoAth <- data.frame( alumnes=c("Athletes","No athletes"), n=c(nrow(df.athletes), nrow(df.noathletes)))
ggplot.pie.athNoAth <-ggplot(df.athNoAth) +
  aes( x="", y=n, fill= alumnes) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  labs(title = "Athletes-No athletes Proportion Pie Plot from sample")
ggplot.pie.athNoAth

## Funció

#proportion function
proportionTest <- function( p, p0, n, alternative="twosided", C=95 ){
  
  z <- (p-p0)/sqrt( (p0*(1-p0)/n))
  alfa <- (1-C/100)
  
  if (alternative=="twosided"){
    zcritical <- qnorm( alfa/2, lower.tail=FALSE )
    pvalue <- pnorm( abs(z), lower.tail=FALSE)*2
  }
  else if (alternative==">"){
    zcritical <- qnorm( alfa, lower.tail=FALSE )
    pvalue <- pnorm( z, lower.tail=FALSE)
  }
  else {
    zcritical <- qnorm( alfa, lower.tail=TRUE )
    pvalue <- pnorm(z, lower.tail=TRUE)
  }
  output <- c(alfa, p, p0, z, zcritical, pvalue)
  names(output) <- c("alfa","p","p0","z", "zcritical", "pvalue")
  return (output)
}


## Pregunta de recerca
## Hipòtesi nul·la i l'alternativa
## Justificació del test a aplicar
## Càlcul

proptest.athNoath.95 <- proportionTest( p=(nrow(df.athletes)/nrow(df)), p0=0.05, n=nrow(df), alternative="<", 95); proptest.athNoath.95
prop.test(x=nrow(df.athletes), n=nrow(df), p=0.05, alternative="less", correct=FALSE)

## Interpretació del test
