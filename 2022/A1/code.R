#0 Imports
library(knitr)
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('VIM')) install.packages('VIM'); library('VIM')
if(!require('mappings'))
  install.packages('remotes')
library('remotes')
remotes::install_github("benjaminrich/mappings")
library(mappings)

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

#******
#5 - Imputació de valors
#******


# row.has.na <- apply(df.clean, 1, function(x){any(is.na(x))})
# df.with.na <- df.raw[row.has.na,]
# df.with.na
# 
colSums(is.na(df.clean))
# 
# #mean way
# idx <- which(is.na(df.clean$colgpa))
# df.clean$colgpa[idx]
# df.clean$colgpa[idx] <- round(mean(df.clean$colgpa, na.rm=TRUE))
# df.clean$colgpa[idx]


#Obtenim les variables quantitatives
quant.variables <- which( colnames(df.clean) %in% c("sat","tothrs","hsize","hsrank","hsperc","colgpa"))
quant.variables
#Obtenim els indexs dels valors NA 
idx <- which( is.na(df.clean$colgpa))
idx

##fem idx
fem.idx <- which(is.na(df.clean$colgpa) & df.clean$female==TRUE); fem.idx
##men idx 
mas.idx <- which(is.na(df.clean$colgpa) & df.clean$female==FALSE); mas.idx

##compute fem new registers
library(Rcpp)
new.fem <- kNN( df.clean[ df.clean$female==TRUE, quant.variables], variable="colgpa", k=11)
df.clean[fem.idx, quant.variables]
new.fem[new.fem$colgpa_imp==TRUE,]
#set new values to df
df.clean[fem.idx,]$colgpa <- new.fem[new.fem$colgpa_imp==TRUE,]$colgpa

##compute men new registers
new.mas <- kNN( df.clean[ df.clean$female==FALSE, quant.variables], variable="colgpa", k=11)
df.clean[mas.idx, quant.variables]
new.mas[new.mas$colgpa_imp==TRUE,]
#set new values to df
df.clean[mas.idx,]$colgpa <- new.mas[new.mas$colgpa_imp==TRUE,]$colgpa

#Check that there is no NA values
colSums(is.na(df.clean))


boxplot(df.clean$colgpa)

#******
#6 - Creació d'una nova variable
#******

library('mappings')
gpaletter_intervals <- c(0,1.50, 2.50, 3.50, 4.01)
m <- cut_mapping(gpaletter_intervals, right=FALSE, to=c("D", "C", "B", "A"))
print(m)
df.clean$gpaletter<- m(df.clean$colgpa)

#******
#7 - Estudi descriptiu
#******

## 7.1 Estudi descriptiu de les variables qualitatives

### athlete
library('ggplot2')
athlete_table <- table(df.clean$athlete)
rownames(athlete_table) = c("No athlete", "Athlete")
barplot(athlete_table, main="Athlete - No Athlete Distribution")


### athlete and female
files=dim(df.clean)[1]
ggplot(data=df.clean[1:files,],aes(x=female,fill=athlete))+geom_bar() 



library('plyr')
library('dplyr')
df.femaleAthlete <- df.clean %>% 
  group_by( female , athlete) %>% 
  dplyr::summarise(counts = n())

df.femaleAthlete

#Plot
ggplot(df.femaleAthlete, aes(y = female, x = counts)) +
  geom_bar(
    aes(fill = athlete),
    stat = "identity", position = position_stack()
  ) + geom_text(aes(label=counts))

##7.2 Estudi descriptiu de les variables quantitatives

#index quantitative var
idx.quant <- c("sat","tothrs","hsize","hsrank","hsperc","colgpa")
#measures
mean <- as.vector(sapply(df.clean[,idx.quant], mean, na.rm=TRUE))
sd <- as.vector(sapply(df.clean[,idx.quant], sd, na.rm=TRUE))
median <- as.vector(sapply(df.clean[,idx.quant], median, na.rm=TRUE))
IQR <- as.vector(sapply(df.clean[,idx.quant],IQR, na.rm=TRUE))

#table creation
table.quant <- kable(data.frame(Variables=names(df.clean[idx.quant]),
                      Mean = mean,
                      StandardDeviation = sd,
                      Median = median,
                      IQR = IQR
                      ),
                digits=2, caption="Table")
table.quant

#sat Box plot
boxplot(df.clean$sat, main="Sat Distribution")

#sat Box plot by gender 
boxplot(data = df.clean, sat~female, main="Sat Distribution by Gender", names=c("men","female"))



#******
#8 - Arxiu final
#******

write.csv(df,"gpa_clean.csv", row.names = TRUE)

#******
#9 - Informe executiu
#******
#idx var
idx.var <- c("sat","tothrs","colgpa","hsize","hsrank","hsperc", "athlete", "female", "white", "black", "gpaletter")
#modificacio per each var
mod.var <- c( "Validació dins del rang establert. Transformació a tipus 'numeric'.",
              "Variable inicialment de tipus 'character'. Extracció de les unitats, reemplaç de la coma pel punt decimal, transformació a tipus 'numeric' i arrodoniment de decimals.",
              "Variable inicialment amb valor perduts. Cerca a través de l'algorisme _'kNN'_ aplicant la distància de Gower.",
              "Variable inicialment de tipus 'character'. Reemplaç de la coma pel punt decimal, transformació a tipus 'numeric' i arrodoniment de decimals.",
              "-",
              "Arrodoniment de decimals",
              "Variable inicialment de tipus 'character'. Estandarització dels valors únics i transformació a tipus 'factor'.",
              "Variable inicialment de tipus 'logical'. Transformació a tipus 'factor'.",
              "Variable inicialment de tipus 'character'. Estandarització dels valors únics i transformació a tipus 'factor'.",
              "Variable inicialment de tipus 'character'. Estandarització dels valors únics i transformació a tipus 'factor'.",
              "Creació de la variable mitjançant la variable _'colgpa'_ i uns intervals de valoració."
              )


#table creation
table.preproc <- kable(data.frame(Variables=names(df.clean[idx.var]),
                                  Modificació = mod.var
),
digits=2, caption="Table")
table.preproc



sum(df.clean$female == TRUE)
sum(df.clean$black == TRUE)
ggplot(df.clean, aes(x = tothrs)) +
  geom_density(fill = "indianred3") + 
  labs(title = "tothrs Distribution")
ggplot(df.clean, aes(x = sat)) +
  geom_density(fill = "indianred3") + 
  labs(title = "sat Distribution")