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
df$naCount <- apply(df, 1, function(x){sum(is.na(x))})
library('dplyr')
df <- df %>% filter(naCount < 5)
df <- df[!names(df) %in% c("naCount")]

### https://cran.r-project.org/web/packages/mappings/mappings.pdf
if (!require('mappings'))  install.packages("remotes") require(remotes) remotes::install_github("benjaminrich/mappings") ;
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
a<-strsplit(duplicates$CS_ID, split="S")

ddd <- function(dfRow){
  temp <- strsplit(dfRow, split="S")
  paste(temp[1],"0",temp[2])
}

ddd(duplicates$CS_ID)




