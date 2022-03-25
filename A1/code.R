#1 - Carrega de l'arxiu

#adult.uci <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"), header=FALSE)
#headers <- c("CS_ID", "age", "workclass", "fnlwgt", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "income")
#colnames(adult.uci) <- headers
#head(adult.uci)

df.full <- read.csv("CensusIncomedataset.csv", sep=";")
colnames(df.full)
sapply(df.full,class)
str(df.full)
summary(df.full)
head(df.full)

#2 - Obtenicio del dataset per fer l'estudi
##Eliminacio del parametres "fnlwgt", "capital_gain", "capital_loss"
df <- df.full[!(names(df.full) %in% c("fnlwgt", "capital_gain", "capital_loss"))]

colSums(is.na(df))
row.has.na <- apply(df, 1, function(x){any(is.na(x))})
df.with.na <- df[row.has.na,]

### https://stackoverflow.com/questions/37801338/count-nas-per-row-in-dataframe
### https://stackoverflow.com/questions/63432836/remove-rows-from-a-data-frame-where-a-cell-is-smaller-or-greater-than-values-in
library('dplyr')
df$naCount <- apply(df, 1, function(x){sum(is.na(x))})
df <- df %>% filter(naCount < 5)
df <- df[!names(df) %in% c("naCount")]

#Instal·lem la següent llibreria en cada de no disposar d'ella
### https://cran.r-project.org/web/packages/mappings/mappings.pdf
if(!require('mappings'))
  install.packages('remotes')
library('remotes')
remotes::install_github("benjaminrich/mappings")
library(mappings)

education_cat_types <- c("primaria", "secundaria", "universitaria", "postuniversitaria")
education_cat_intervals <- c(0, 7, 10, 14, Inf)
m <- cut_mapping(education_cat_intervals, right=FALSE,
                 to=c("primaria", "secundaria", "universitaria", "postuniversitaria"))
print(m)
m(c(5, 8, 9, 10, 12, 16))
df$education_cat<- m(df$education_num)

df$gender <- df$sex
df <- df[!names(df) %in% c("sex")]

#3 - Duplicació de codis
without.duplicated.cs <- df %>% distinct(CS_ID, .keep_all = TRUE)
duplicates <- df[duplicated(df$CS_ID),]
library(stringr)
addZeroCSID <- function(row){
  print(row)
  num <- as.numeric(str_extract(row, "[0-9]+"))
  paste("CS0",num, sep="")
}

duplicates$CS_ID <- addZeroCSID(duplicates$CS_ID)
df <- rbind(without.duplicated.cs, duplicates)

#4 - Normalització de les dades qualitatives
##4.1 Eliminació d'espais en blanc
### https://www.datasciencemadesimple.com/strip-leading-trailing-spaces-of-column-in-r-remove-space-r-2/#:~:text=trimws()%20function%20is%20used,of%20the%20column%20in%20R.
str(df)
df$workclass <- trimws(df$workclass, which = c("both"))
df$marital_status <- trimws(df$marital_status, which = c("both"))
df$relationship <- trimws(df$relationship, which = c("both"))
df$occupation <- trimws(df$occupation, which = c("both"))
df$race <- trimws(df$race, which = c("both"))
df$gender <- trimws(df$gender, which = c("both"))
str(df)

##4.2 Martial-Status
### https://stackoverflow.com/questions/24849699/map-array-of-strings-to-an-array-of-integers
unique(df[c("marital_status")])
library('plyr')
df$marital_status <- revalue (x = df$marital_status, c("Married" = "M", "Single" = "S", "Separated" = "X", "Divorced" = "D", "Widowed" = "W"))
unique(df[c("marital_status")])
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
### https://stackoverflow.com/questions/21639392/make-frequency-histogram-for-factor-variables
martial_status_table <- table(df$marital_status)
barplot(martial_status_table, main="Martial Status Distribution", xlab = "Status")

###4.3 Gènere
unique(df[c("gender")])
df$gender <- revalue (x = df$gender, c("M" = "m", "Fem" = "f", "F" = "f", "male" = "m", "female" = "f", "Male" = "m", "Female" = "f"))
gender_table <- table(df$gender)
barplot(gender_table, main="Gender Distribution", xlab = "Gender")



#5 - Normalització de les dades quantitatives
##5.1 Edat
unique(df[c("age")])
if (!require('naniar')) install.packages('naniar'); library('naniar')
df <- df%>% replace_with_na(replace = list(age = c(650,560)))
colSums(is.na(df))
filter(df, CS_ID == 'CS430')
filter(df, CS_ID == 'CS2754')

### https://rkabacoff.github.io/datavis/Univariate.html
ggplot(df, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Age Distribution")

##5.2 Educacio
unique(df[c("education_num")])
ggplot(df, aes(x = education_num)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Years of education Distribution")


##5.3 Hores per setmana
sapply(df,class)
unique(df[c("hours_per_week")])

extractNum <- function(row){
  num <- gsub("[a-zA-Z ]", "", row)
  num <- as.numeric(gsub(",", ".",num))
}
df$hours_per_week <- extractNum(df$hours_per_week)
sapply(df,class)
unique(df[c("hours_per_week")])

##5.4 Income
extractMoneyUnit <- function(row){
  unit <- gsub('[0-9]+', '', row)
  unit <- gsub(",", "", unit)
  unit <- trimws(unit, which = c("both"))
}

extractMoneyAmount <- function(row){
  num <- gsub("[a-zA-Z ]", "", row)
  num <- gsub("'", "", num)
  num <- as.numeric(gsub(",", ".",num))
  
}

df$moneyUnit <- extractMoneyUnit(df$income)
df$amountIncome <- extractMoneyAmount(df$income)
unique(df[c("moneyUnit")])

### https://bookdown.org/jboscomendoza/r-principiantes4/if-else.html
normalizeAmount <- function(unit, amount){
  num <- ifelse(unit == "euros", amount/1000, amount)
}

df$income <- normalizeAmount(df$moneyUnit, df$amountIncome)
df <- df[!names(df) %in% c("moneyUnit", "amountIncome")]

#6 - Valors atípics

## Age
summary(df$age)

ggplot(df, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Age Distribution")

age.bp <- boxplot(df$age, xlab = "Age", horizontal=TRUE)
ageOutliersAge <- age.bp$out
ageOutliersAge

###Criteri +/-2SD  
age.mean<-mean(df$age, na.rm=TRUE)
age.sd<-sd(df$age, na.rm=TRUE)
age.cutoff<- age.sd*2
print(paste("Cutoff: ",age.cutoff))
age.upperBoundary <- age.mean+age.cutoff
age.lowerBoundary <- age.mean-age.cutoff
print(paste("Range: [", age.lowerBoundary, ",",age.upperBoundary, "]"))

NAAge <- function(lower, upper, age){
  num <- ifelse ( (lower < age ) & ( age < upper ), age, NA)  
}
df$age <- NAAge(age.lowerBoundary,age.upperBoundary,df$age)

## Education Num
summary(df$education_num)

education_num.bp <- boxplot(df$education_num, xlab = "HOurs", horizontal=TRUE)

educationNumOutliersAge <- education_num.bp$out
educationNumOutliersAge #uncomment to see all values


ggplot(df, aes(x = education_num)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Years of education Distribution")

##Hours per week

summary(df$hours_per_week)
hours_per_week.bp <- boxplot(df$hours_per_week, xlab = "Years", horizontal=TRUE)

NAhoursAbove80 <- function(hours){
  num <- ifelse(hours > 80, NA, hours)
}

df$hours_per_week <- NAhoursAbove80(df$hours_per_week)

## Income
summary(df$income)
income.bp <- boxplot(df$income, xlab = "k€", horizontal=TRUE)
incomeOutliersAge <- income.bp$out
incomeOutliersAge

# Imputacio de valors

## Age 

NAtoMeanAge <- function(age, mean){
  num <- ifelse(is.na(age), mean, age)
}
age.mean<-mean(df$age, na.rm=TRUE)
df$age <- NAtoMeanAge(df$age, age.mean)

## Hours per week
if(!require('VIM'))
  install.packages('VIM')
library('VIM')

NAToKNN <- function(hours){
  knnimp <- kNN(hours, k = 11)
  
  num <- ifelse(is.na(hours), knnimp, hours)
}

df$hours_per_week <- NAToKNN(df$hours_per_week)

# Estudi descriptiu

## Funcions de mitjana robustes

###https://www.uv.es/webgid/Descriptiva/25_otras_medias.html#:~:text=Para%20obtener%20la%20Media%20recortada,las%20puntuaciones%20del%20extremo%20inferior.
###https://www.uv.es/mperea/r_ejemplo.htm
x=c(800,6,4,5,5,5,5,6,4,2)
mitjana.retallada <- function(x, perc=0.05){
  v <- mean(x,trim=perc)
}
a <- mitjana.retallada(x)

# https://www.r-bloggers.com/2011/06/winsorization/
library('psych')
mitjana.winsor <- function(x, perc=0.05){
  v <- winsor.mean(x, trim=perc)
}
b <- mitjana.winsor(x)

