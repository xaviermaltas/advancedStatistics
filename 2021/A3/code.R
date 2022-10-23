#IMPORTS

if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('logr')) install.packages('logr'); library('logr')

# Lectura del fitxer de dades
df.raw <- read.csv("dat_Air_Stations.csv", sep=",")
# head(df.raw)


# Regressió lineal
## Estudi comparatiu entre estacions
contaminants <- c("PM10", "O3", "NO2", "SO2")
ids <- c("1","2","3","10")

df.split<-split(df.raw, df.raw$Estacion) #split by 'Estacion'
df.split[5]

maxMeanContaminants <- function(largeList, names){
     
  newLargeList <- list()
       
  for( i in 1:length(largeList)){
  
    # mat = matrix(ncol=1, nrow=365)
    # tmp_list <- rep(list(mat),8)
    
    mat = matrix(1:2920,ncol = (length(names)*2), nrow = 365)
    print(mat)

    for(j in 1:length(names)){
      currentList<-largeList[i]
      currentName <- names[j]
  
      newMeanName <- paste(currentName,'.mean', sep="" )
      newMaxName <- paste(currentName,'.mean', sep="" )
  
      computedMeanList<- aggregate(currentList[[1]][currentName], list(currentList[[1]]$Fecha), FUN=mean, na.rm=TRUE )
      computedMaxList<- aggregate(currentList[[1]][currentName], list(currentList[[1]]$Fecha), FUN=max, na.rm=TRUE )
  
      meanList <- list(computedMeanList[2])
      maxList <- list(computedMaxList[2])
      
      # tmp_list[[(j*2)]] <- meanList
      # tmp_list[[((j*2)+1)]] <- maxList
      
      # mat[,(j)] <- meanList
      # mat[,((j)+1)] <- maxList
      
    }
    print(mat)
    # print(tmp_list)
    # newDF <- as.data.frame(tmp_list)
    # newLargeList[i] <- newDF
  }
  return(newLargeList)
}

maxMeanLargeList <- maxMeanContaminants(df.split, contaminants)
# 
# con <- file("test.log")
# sink(con, append=TRUE)
# sink(con, append=TRUE, type="message")
# 
# # This will echo all input and not truncate 150+ character lines...
# source("code.R", echo=TRUE, max.deparse.length=10000)
# 
# # Restore output to console
# sink() 
# sink(type="message")
# 
# # And look at the log...
# cat(readLines("test.log"), sep="\n")
# 
