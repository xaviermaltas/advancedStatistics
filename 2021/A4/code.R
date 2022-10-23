if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('logr')) install.packages('logr'); library('logr')

dat <- read.csv("gpa.csv", sep=",")

#1
##1.1

extractNum <- function(row){
  num <- gsub("[a-zA-Z ]", "", row)
  num <- as.numeric(gsub(",", ".",num))
}
sapply(dat,class)
dat$tothrs <- as.character( dat$tothrs )
dat$tothrs <- extractNum(dat$tothrs)
sapply(dat,class)


##1.2

colSums(is.na(dat)) ##41 na colgpa -> nota mitjana de l'estudiant al final del primer semestre ( 0 a 4 )

row.has.na <- apply(dat, 1, function(x){any(is.na(x))})
df.with.na <- dat[row.has.na,]
nrow.has.na <- nrow(df.with.na)

nrow <- nrow(dat)
na.percentage <- (nrow.has.na/nrow)*100
dat$naCount <- apply(dat, 1, function(x){sum(is.na(x))})

gpaclean <- dat %>% filter(naCount == 0)
gpaclean <- gpaclean[!names(gpaclean) %in% c("naCount")]

##1.3


if(!require('mappings'))
  install.packages('remotes')
library('remotes')
remotes::install_github("benjaminrich/mappings")
library(mappings)


colgpa_types <- c("D", "C", "B", "A")
colgpa_intervals <- c(0, 1.50, 2.50, 3.50, Inf)
m <- cut_mapping(colgpa_intervals, right=FALSE,
                 to=c("D", "C", "B", "A"))
print(m)

gpaclean$gpletter<- m(gpaclean$colgpa)
#colSums(is.na(gpaclean))
gpaclean[gpaclean$colgpa == '2.49',]

