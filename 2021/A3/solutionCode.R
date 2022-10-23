if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('logr')) install.packages('logr'); library('logr')

dat <- read.csv("dat_Air_Stations.csv", sep=",")
dat_max <- ddply(dat, c("Fecha", "Nombre"), summarise,N= length(O3),O3_max = max(O3),NO2_max = max(NO2), SO2_max=max(SO2),PM10_max = max(PM10))
table.max<-by (dat_max, dat_max$Nombre, summary )
table.max

dat_mean <- ddply(dat, c("Fecha", "Nombre"), summarise,N= length(O3),O3_m = mean(O3), NO2_m = mean(NO2), SO2_m=mean(SO2),PM10_m = mean(PM10))
table.mean<-by (dat_mean, dat_mean$Nombre, summary )
table.mean

#b) Representeu gràficament l'evolució
#Per PM10
g1<-ggplot(dat_max, aes(x = Fecha, y = PM10_max, group = Nombre, colour = Nombre)) + geom_line() +  facet_grid(.~Nombre)
g1

# Per SO2
g2<-ggplot(dat_max, aes(x = Fecha, y = SO2_max, group = Nombre, colour = Nombre)) + geom_line() + facet_grid(.~Nombre)
g2

# Per NO2
g3<-ggplot(dat_max, aes(x = Fecha, y = NO2_max, group = Nombre, colour = Nombre)) + geom_line() + facet_grid(.~Nombre)
g3

# Per O3
g4<-ggplot(dat_max, aes(x = Fecha, y = O3_max, group = Nombre, colour = Nombre)) + geom_line() + facet_grid(.~Nombre)
g4

#c) Estudi de correlació lineal
#Estació de Montevil
dat_MO<-dat[dat$Nombre=="Estacion de Montevil",]
dat_max_MO <- ddply(dat_MO, c("Fecha"), summarise,N= length(O3),O3_max = max(O3),
                    NO2_max = max(NO2), SO2_max=max(SO2),PM10_max = max(PM10),
                    TMP_max= max(TMP), vv_max= max(vv), LL_max= max(LL),
                    HR_max= max(HR), RS_max=max(RS), PRB_max = max(PRB))

var.cor_max<- select(dat_max_MO,O3_max,NO2_max,SO2_max,PM10_max, TMP_max, vv_max,
                     LL_max,HR_max,RS_max,PRB_max)
cor(var.cor_max, method = "pearson", use="pairwise.complete.obs")


# Estació de AC
dat_AC<-dat[dat$Nombre=="Estacion Avenida Constitucion",]
dat_max_AC <- ddply(dat_AC, c("Fecha"), summarise,N= length(O3),O3_max = max(O3),
                    NO2_max = max(NO2), SO2_max=max(SO2),PM10_max = max(PM10),
                    TMP_max= max(TMP), vv_max= max(vv), LL_max= max(LL),
                    HR_max= max(HR), RS_max=max(RS), PRB_max = max(PRB))
var.cor_max<- select(dat_max_AC,O3_max,NO2_max,SO2_max,PM10_max, TMP_max, vv_max,
                     LL_max,HR_max,RS_max,PRB_max)
cor(var.cor_max, method = "pearson", use="pairwise.complete.obs")
