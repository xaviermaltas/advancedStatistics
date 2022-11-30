#0 Imports

if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('VIM')) install.packages('VIM'); library('VIM')
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
