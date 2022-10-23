orderByFechaPeriodo<-function(df){
  df<-df[
    with(df, order(Fecha, Periodo)),
  ]
  return(df)
}

df.1 <- orderByFechaPeriodo(df.raw[df.raw$Estacion == 1, ])
df.1.PM10.mean <- aggregate(df.1$PM10, list(df.1$Fecha), FUN=mean, na.rm=TRUE)  #https://www.statology.org/r-mean-by-group/
df.1.PM10.max <- aggregate(df.1$PM10, list(df.1$Fecha), FUN=max, na.rm=TRUE)
df.1.O3.mean <- aggregate(df.1$O3, list(df.1$Fecha), FUN=mean, na.rm=TRUE)
df.1.O3.max <- aggregate(df.1$O3, list(df.1$Fecha), FUN=max, na.rm=TRUE)
df.1.NO2.mean <- aggregate(df.1$NO2, list(df.1$Fecha), FUN=mean, na.rm=TRUE)
df.1.NO2.max <- aggregate(df.1$NO2, list(df.1$Fecha), FUN=max, na.rm=TRUE)
df.1.SO2.mean <- aggregate(df.1$SO2, list(df.1$Fecha), FUN=mean, na.rm=TRUE)
df.1.SO2.max <- aggregate(df.1$SO2, list(df.1$Fecha), FUN=max, na.rm=TRUE)


df.2 <- orderByFechaPeriodo(df.raw[df.raw$Estacion == 2, ])
df.2.PM10.mean <- aggregate(df.2$PM10, list(df.2$Fecha), FUN=mean, na.rm=TRUE)
df.2.PM10.max <- aggregate(df.2$PM10, list(df.2$Fecha), FUN=max, na.rm=TRUE)
df.2.O3.mean <- aggregate(df.2$O3, list(df.2$Fecha), FUN=mean, na.rm=TRUE)
df.2.O3.max <- aggregate(df.2$O3, list(df.2$Fecha), FUN=max, na.rm=TRUE)
df.2.NO2.mean <- aggregate(df.2$NO2, list(df.2$Fecha), FUN=mean, na.rm=TRUE)
df.2.NO2.max <- aggregate(df.2$NO2, list(df.2$Fecha), FUN=max, na.rm=TRUE)
df.2.SO2.mean <- aggregate(df.2$SO2, list(df.2$Fecha), FUN=mean, na.rm=TRUE)
df.2.SO2.max <- aggregate(df.2$SO2, list(df.2$Fecha), FUN=max, na.rm=TRUE)


df.3 <- orderByFechaPeriodo(df.raw[df.raw$Estacion == 3, ])
df.3.PM10.mean <- aggregate(df.3$PM10, list(df.3$Fecha), FUN=mean, na.rm=TRUE)
df.3.PM10.max <- aggregate(df.3$PM10, list(df.3$Fecha), FUN=max, na.rm=TRUE)
df.3.O3.mean <- aggregate(df.3$O3, list(df.3$Fecha), FUN=mean, na.rm=TRUE)
df.3.O3.max <- aggregate(df.3$O3, list(df.3$Fecha), FUN=max, na.rm=TRUE)
df.3.NO2.mean <- aggregate(df.3$NO2, list(df.3$Fecha), FUN=mean, na.rm=TRUE)
df.3.NO2.max <- aggregate(df.3$NO2, list(df.3$Fecha), FUN=max, na.rm=TRUE)
df.3.SO2.mean <- aggregate(df.3$SO2, list(df.3$Fecha), FUN=mean, na.rm=TRUE)
df.3.SO2.max <- aggregate(df.3$SO2, list(df.3$Fecha), FUN=max, na.rm=TRUE)


df.4 <- orderByFechaPeriodo(df.raw[df.raw$Estacion == 4, ])
df.4.PM10.mean <- aggregate(df.4$PM10, list(df.4$Fecha), FUN=mean, na.rm=TRUE)
df.4.PM10.max <- aggregate(df.4$PM10, list(df.4$Fecha), FUN=max, na.rm=TRUE)
df.4.O3.mean <- aggregate(df.4$O3, list(df.4$Fecha), FUN=mean, na.rm=TRUE)
df.4.O3.max <- aggregate(df.4$O3, list(df.4$Fecha), FUN=max, na.rm=TRUE)
df.4.NO2.mean <- aggregate(df.4$NO2, list(df.4$Fecha), FUN=mean, na.rm=TRUE)
df.4.NO2.max <- aggregate(df.4$NO2, list(df.4$Fecha), FUN=max, na.rm=TRUE)
df.4.SO2.mean <- aggregate(df.4$SO2, list(df.4$Fecha), FUN=mean, na.rm=TRUE)
df.4.SO2.max <- aggregate(df.4$SO2, list(df.4$Fecha), FUN=max, na.rm=TRUE)


df.10 <- orderByFechaPeriodo(df.raw[df.raw$Estacion == 10, ])
df.10.PM10.mean <- aggregate(df.10$PM10, list(df.10$Fecha), FUN=mean, na.rm=TRUE)
df.10.PM10.max <- aggregate(df.10$PM10, list(df.10$Fecha), FUN=max, na.rm=TRUE)
df.10.O3.mean <- aggregate(df.10$O3, list(df.10$Fecha), FUN=mean, na.rm=TRUE)
df.10.O3.max <- aggregate(df.10$O3, list(df.10$Fecha), FUN=max, na.rm=TRUE)
df.10.NO2.mean <- aggregate(df.10$NO2, list(df.10$Fecha), FUN=mean, na.rm=TRUE)
df.10.NO2.max <- aggregate(df.10$NO2, list(df.10$Fecha), FUN=max, na.rm=TRUE)
df.10.SO2.mean <- aggregate(df.10$SO2, list(df.10$Fecha), FUN=mean, na.rm=TRUE)
df.10.SO2.max <- aggregate(df.10$SO2, list(df.10$Fecha), FUN=max, na.rm=TRUE)
# 
# plotMax <- function(names,ids){
#   for(i in 1:length(names)){
#     concatNames <-rep(NA,length(names))
#     dfMean$i <- df
#     
#     AllDfMean <- rbind(paste0("df.1.",names[i],".mean", sep=""),paste0("df.2.", names[i],".mean", sep=""),paste0("df.3.",names[i],".mean", sep=""),paste0("df.4.",names[i],".mean", sep=""),paste0("df.10.",names[i],".mean", sep=""))
#     View(AllDfMean)
#     AllDfMax <- rbind(paste0("df.1.",i,".mean", sep=""),paste0("df.2.",i,".mean", sep=""),paste0("df.3.",i,".mean", sep=""),paste0("df.4.",i,".mean", sep=""),paste0("df.10.",i,".mean", sep=""))
#   }
# }
# plotMax(contaminants, ids)
# 
# paste0("age.",years[1])
# 
# tes <- function(ids){
#   for(i in 1:length(ids)){
#     df$ids[i] <- df.ids[i].PM10.mean$x
#   }
# }
# df.PM10.mean <- data.frame()
# df.PM10.mean['new'] <- df.1.PM10.mean$x
# # , df.2.PM10.mean$x, df.3.PM10.mean$x, df.4.PM10.mean$x, df.10.PM10.mean$x
# 
# ggplot(data=df.PM10.mean)
# 
# graph.PM10.mean <- lapply(df.PM10.mean,function(x)
#   p<-ggplot(x) +
#     geom_line() + 
#     facet_wrap(~col1)
# )
# print (graph.PM10.mean)
# 
# 
#  
# 
# ## Model de regressió lineal
# ## Model de regressió lineal múltiple
# ## Diagnosi del model
# ## Predicció del model
# 
# # Regressó logística
# ## Anàlisi cru de possibles factors de risc. Cáculo de OR
# ## Model de regressió logística
# ## Predicció
# ## Bondat de l'ajust
# ## Corba ROC
# 
# # Conclusió de l'anàlisi