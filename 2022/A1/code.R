#0 Imports
library(knitr)
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')

#******
#1 - Carrega de l'arxiu
#******
  
df.raw <- read.csv("gpa_row.csv", sep=",")
dim(df.raw)
str(df.raw)
head(df.raw)
summary(df.raw)
sapply(df.raw,class)

## NA values
colSums(is.na(df.raw))
row.has.na <- apply(df.raw, 1, function(x){any(is.na(x))})
df.with.na <- df.raw[row.has.na,]
df.with.na

## Duplicate df to do the cleaning tasks
df.clean <- df.raw

#Utilities
castToNumeric <- function(data){
  num <- as.numeric(data)
}

castToInteger <- function(data){
  int <- as.integer(data)
}


#******
#2 - Normalització de les variables qualitatives
#******
## Athlete

typeof(df.clean$athlete)
unique(df.clean$athlete)
#Revalue to unique 
df.clean$athlete <- revalue (x = df.clean$athlete, c("true"="TRUE", "false"="FALSE"))
unique(df.clean$athlete)

castToLogical <- function(data){
  data <- as.logical(data)
}

df.clean$athlete <- castToLogical(df.clean$athlete)
typeof(df.clean$athlete)
unique(df.clean$athlete)


factorVectorGeneration <- function(data){
  f <- factor(data)
}
df.clean$athlete <- factorVectorGeneration(df.clean$athlete)

typeof(df.clean$athlete)
unique(df.clean$athlete)
sapply(df.clean,class)


## Female
typeof(df.clean$female)
unique(df.clean$female)

df.clean$female <- factorVectorGeneration(df.clean$female)
typeof(df.clean$female)
levels(df.clean$female)
nlevels(df.clean$female)
sapply(df.clean,class)


## White

typeof(df.clean$white)
unique(df.clean$white)
#Revalue to unique 
df.clean$white <- revalue (x = df.clean$white, c("true"="TRUE","TRUE   "="TRUE", "  TRUE"="TRUE" ,"false"="FALSE"))
unique(df.clean$white)
#Cast to Logical
df.clean$white <- castToLogical(df.clean$white)
typeof(df.clean$white)
unique(df.clean$white)
#Transform to factor
df.clean$white <- factorVectorGeneration(df.clean$white)
#Checking Factor
typeof(df.clean$white)
levels(df.clean$white)
nlevels(df.clean$white)
sapply(df.clean,class)


## Black
#Type and unique checking
typeof(df.clean$black)
unique(df.clean$black)
#Revalue to unique 
df.clean$black <- revalue (x = df.clean$black, c( "TRUE   "="TRUE", "false"="FALSE", "  FALSE"="FALSE", "FALSE   "="FALSE"))
unique(df.clean$black)
#Cast to Logical
df.clean$black <- castToLogical(df.clean$black)
typeof(df.clean$black)
unique(df.clean$black)
#Transform to factor
df.clean$black <- factorVectorGeneration(df.clean$black)
#Checking Factor
typeof(df.clean$black)
levels(df.clean$black)
nlevels(df.clean$black)
sapply(df.clean,class)


#******
#3 - Normalització de les variables quantitatives
#******

  
##sat
#Type checking
sapply(df.clean,class)
#Range values
summary(df.clean$sat)
#There is no NA value
any(is.na(df.clean$sat))
#Cast to numeric
castToNumeric <- function(data){
  num <- as.numeric(data)
}

##tothrs
#Type checking
sapply(df.clean,class)
#There is no NA value
any(is.na(df.clean$sat))
#Transform without units, comma -> dot and numeric datatype
cleanAndExtractNum <- function(row){
  num <- gsub("[a-zA-Z ]", "", row) #Elimiem caracters
  num <- as.numeric(gsub(",", ".",num)) #Comma to dot, to numeric
}
df.clean$tothrs <- cleanAndExtractNum(df.clean$tothrs)
#Checking type
sapply(df.clean,class)


##colpga
#Type checking
sapply(df.clean,class)
#There is no NA value
any(is.na(df.clean$colgpa))
row.has.na <- apply(df.raw, 1, function(x){any(is.na(x))})
df.with.na <- df.raw[row.has.na,]
head(df.with.na)
#Range values
summary(df.clean$colgpa)


##hsize
#Type checking
sapply(df.clean,class)
#Cast to numeric
df.clean$hsize <- cleanAndExtractNum(df.clean$hsize)

sapply(df.clean,class)


##hsperc
#Type checking
sapply(df.clean,class)
#Round 3 digits
df.clean$hsperc <- round(df.clean$hsperc, digits=3)
#Checking type
sapply(df.clean,class)
head(df.clean)
colSums(is.na(df.clean))


#******
#4 - Valors atípics
#******
##sat
#Range values
summary(df.clean$sat)
#Box plot
boxplot(df.clean$sat)
#Density distribution
ggplot(df.clean, aes(x = sat)) +
  geom_density(fill = "indianred3") + 
  labs(title = "sat Distribution")

# #Set NA out of range
df.clean$sat[ 0 > df.clean$sat || df.clean$sat > 1600 ] <- NA
#Check if exist new NAs
sum(is.na(df.clean$sat))




##hsize
#Range values
summary(df.clean$hsize)
#Box plot
boxplot(df.clean$hsize)
hsize.out<-boxplot.stats(df.clean$hsize)$out
length(hsize.out)
sort(hsize.out)[1:169]
sum(df.clean$hsize>7)
#Density distribution
ggplot(df.clean, aes(x = hsize)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Age Distribution")