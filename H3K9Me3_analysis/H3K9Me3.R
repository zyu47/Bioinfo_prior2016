setwd("/Users/Zhixian/Desktop/H3K9Me3_quant")

####################positive control (Etop&HU) calculation####################
PC1 <- read.csv("H3K9Me3_Etop_HU.csv", header = T)
#normalize intensity by being divided by group mean
for(i in 0:2)
{
  avg <- aggregate(PC1[,(1+3*i):(2+3*i)],list(PC1[,(1+3*i)]),mean)
  for(j in 1:length(na.omit(PC1[,1+3*i])))
  {
      PC1[j,3+3*i] = PC1[j,2+3*i]/avg[avg[,1]==PC1[j,1+3*i],3]
  }
}
nctl <- na.omit(PC1[,4:6])
HU <- na.omit(PC1[,1:3])
etop <-na.omit(PC1[,7:9])

####################positive control (H2O2) calculation####################
PC2 <- read.csv("H3K9Me3 H2O2.csv", header = T)
for(i in 0:1)
{
    avg <- aggregate(PC2[,(1+3*i):(2+3*i)],list(PC2[,(1+3*i)]),mean)
    for(j in 1:length(na.omit(PC2[,1+3*i])))
    {
        PC2[j,3+3*i] = PC2[j,2+3*i]/avg[avg[,1]==PC2[j,1+3*i],3]
    }
}
nctl2 <- na.omit(PC2[,1:3])
H2O2 <- na.omit(PC2[,4:6])



count_pixel_perc <- function(t){
    sum(t<(1 - 0.1*i) | t>(1 + 0.1*i))
}

#############test to choose the threshold#####################
##########from 0.9&1.1 to 0.0&2.0#########################
perc <- array(0, c(3,10))

#control percentage
for(i in 1:10)
{
    pixel_num_total <- aggregate(nctl[,1],list(nctl[,1]),length)
    pixel_num_group <- aggregate(nctl[,3],list(nctl[,1]),count_pixel_perc)
    perc[1,i] = mean(pixel_num_group[,2]/pixel_num_total[,2])
}

#HU percentage
for(i in 1:10)
{
    pixel_num_total <- aggregate(HU[,1],list(HU[,1]),length)
    pixel_num_group <- aggregate(HU[,3],list(HU[,1]),count_pixel_perc)
    perc[2,i] = mean(pixel_num_group[,2]/pixel_num_total[,2])
}

#etoposide percentage
for(i in 1:10)
{
    pixel_num_total <- aggregate(etop[,1],list(etop[,1]),length)
    pixel_num_group <- aggregate(etop[,3],list(etop[,1]),count_pixel_perc)
    perc[3,i] = mean(pixel_num_group[,2]/pixel_num_total[,2])
}

#Use 0.5 and 1.5 as cutoff, giving the largest difference
i = 5
{#HUVEC negative control
    pixel_num_total <- aggregate(nctl[,1],list(nctl[,1]),length)
    nctl_percent_within_0.5_and_1.5_avg <- aggregate(nctl[,3],list(nctl[,1]),count_pixel_perc)
    nctl_percent_within_0.5_and_1.5_avg[,2] <- nctl_percent_within_0.5_and_1.5_avg[,2]/pixel_num_total[,2]
}
{#Hydroxyurea treatment
    pixel_num_total <- aggregate(HU[,1],list(HU[,1]),length)
    HU_percent_within_0.5_and_1.5_avg <- aggregate(HU[,3],list(HU[,1]),count_pixel_perc)
    HU_percent_within_0.5_and_1.5_avg[,2] <- HU_percent_within_0.5_and_1.5_avg[,2]/pixel_num_total[,2]
}
{#etoposide treatment
    pixel_num_total <- aggregate(etop[,1],list(etop[,1]),length)
    etop_percent_within_0.5_and_1.5_avg <- aggregate(etop[,3],list(etop[,1]),count_pixel_perc)
    etop_percent_within_0.5_and_1.5_avg[,2] <- etop_percent_within_0.5_and_1.5_avg[,2]/pixel_num_total[,2]
}

library(rowr)
final_result <- rowr::cbind.fill(nctl_percent_within_0.5_and_1.5_avg,
                            HU_percent_within_0.5_and_1.5_avg,
                            etop_percent_within_0.5_and_1.5_avg,
                            fill=NA)
names(final_result) <- c("Control","%",
                         "HydroxyUrea","%",
                         "Etoposide","%")
write.csv(final_result,"H3K9Me3_Etop_HU_analyzed.csv")

######H2O2######
i = 5
{#HUVEC negative control
    pixel_num_total <- aggregate(nctl2[,1],list(nctl2[,1]),length)
    nctl2_percent_within_0.5_and_1.5_avg <- aggregate(nctl2[,3],list(nctl2[,1]),count_pixel_perc)
    nctl2_percent_within_0.5_and_1.5_avg[,2] <- nctl2_percent_within_0.5_and_1.5_avg[,2]/pixel_num_total[,2]
}
{#H2O2 treatment
    pixel_num_total <- aggregate(H2O2[,1],list(H2O2[,1]),length)
    H2O2_percent_within_0.5_and_1.5_avg <- aggregate(H2O2[,3],list(H2O2[,1]),count_pixel_perc)
    H2O2_percent_within_0.5_and_1.5_avg[,2] <- H2O2_percent_within_0.5_and_1.5_avg[,2]/pixel_num_total[,2]
}
library(rowr)
final_result2 <- rowr::cbind.fill(nctl2_percent_within_0.5_and_1.5_avg,
                                 H2O2_percent_within_0.5_and_1.5_avg,
                                 fill=NA)
names(final_result2) <- c("Control","%",
                         "H2O2","%")
write.csv(final_result2,"H3K9Me3_H2O2_analyzed.csv")


#########################experiment calculation##############
tetPlk4 <- read.csv("H3K9Me3_tetPlk4.csv", header = FALSE, 
                    stringsAsFactors=FALSE)
#normalize intensity by being divided by group mean
for(i in 0:9)
{
    avg <- aggregate(tetPlk4[,(2+6*i)],
                     list(tetPlk4[,(1+6*i)]),
                     mean)
    for(j in 1:length(na.omit(tetPlk4[,1+6*i])))
    {
        tetPlk4[j,3+6*i] = tetPlk4[j,2+6*i]/avg[avg[,1]==tetPlk4[j,1+6*i],2]
    }
}

count_pixel_perc_5 <- function(t){
    sum(t<0.5 | t>1.5)
}
perc_calc <- function(x){
    #x as input (3 columns)
    total_pixel_num <- aggregate(x[,1],list(x[,1]),length)
    y <- aggregate(as.numeric(x[,3]),list(x[,1]),count_pixel_perc_5)
    y[,2] <- y[,2]/total_pixel_num[,2]
    return(y)
}

tetPlk4_final_result <- 1
for(i in 0:9)
{
    tmp <- perc_calc(tetPlk4[,(1+6*i):(3+6*i)]) * 100
    tetPlk4_final_result <- rowr::cbind.fill(tetPlk4_final_result,
                                             tmp, tetPlk4[3:4,(4+6*i):(5+6*i)],
                                             fill=NA)
}

#hist(table[table[,1]==1,3])
write.csv(tetPlk4_final_result,"H3K9Me3_tetPlk4_analyzed.csv")


#########################12-13-15 experiment analysis##############
tetPlk4 <- read.csv("121315_tetPlk4.csv", header = FALSE, 
                    stringsAsFactors=FALSE)
#normalize intensity by being divided by group mean
for(i in 0:6)
{
    avg <- aggregate(tetPlk4[,(2+6*i)],
                     list(tetPlk4[,(1+6*i)]),
                     mean)
    for(j in 1:length(na.omit(tetPlk4[,1+6*i])))
    {
        tetPlk4[j,3+6*i] = tetPlk4[j,2+6*i]/avg[avg[,1]==tetPlk4[j,1+6*i],2]
    }
}

count_pixel_perc_5 <- function(t){
    sum(t<0.5 | t>1.5)
}
perc_calc <- function(x){
    #x as input (3 columns)
    total_pixel_num <- aggregate(x[,1],list(x[,1]),length)
    y <- aggregate(as.numeric(x[,3]),list(x[,1]),count_pixel_perc_5)
    y[,2] <- y[,2]/total_pixel_num[,2]
    return(y)
}

tetPlk4_final_result <- 1
for(i in 0:6)
{
    tmp <- perc_calc(tetPlk4[,(1+6*i):(3+6*i)]) * 100
    tetPlk4_final_result <- rowr::cbind.fill(tetPlk4_final_result,
                                             tmp, tetPlk4[3:4,(4+6*i):(5+6*i)],
                                             fill=NA)
}

#hist(table[table[,1]==1,3])
write.csv(tetPlk4_final_result,"H3K9Me3_121315_tetPlk4_analyzed.csv")


#########################01-20-16 experiment analysis##############
tetPlk4 <- read.csv("012016.csv", header = FALSE, 
                    stringsAsFactors=FALSE)
#normalize intensity by being divided by group mean
for(i in 0:13)
{
    avg <- aggregate(tetPlk4[,(2+6*i)],
                     list(tetPlk4[,(1+6*i)]),
                     mean)
    for(j in 1:length(na.omit(tetPlk4[,1+6*i])))
    {
        tetPlk4[j,3+6*i] = tetPlk4[j,2+6*i]/avg[avg[,1]==tetPlk4[j,1+6*i],2]
    }
}

count_pixel_perc_5 <- function(t){
    sum(t<0.5 | t>1.5)
}
perc_calc <- function(x){
    #x as input (3 columns)
    total_pixel_num <- aggregate(x[,1],list(x[,1]),length)
    y <- aggregate(as.numeric(x[,3]),list(x[,1]),count_pixel_perc_5)
    y[,2] <- y[,2]/total_pixel_num[,2]
    return(y)
}

tetPlk4_final_result <- 1
for(i in 0:13)
{
    tmp <- perc_calc(tetPlk4[,(1+6*i):(3+6*i)]) * 100
    tetPlk4_final_result <- rowr::cbind.fill(tetPlk4_final_result,
                                             tmp, tetPlk4[3:4,(4+6*i):(5+6*i)],
                                             fill=NA)
}

#hist(table[table[,1]==1,3])
write.csv(tetPlk4_final_result,"H3K9Me3_012016_tetPlk4_analyzed.csv")

#######################plot density map for OD and normal#########
#########using image3.oif from "DAPI_H3K9Me3_AddDOX_Day6_repr No1" as the representative#######
##########No1 as Normal and No2 as OD################
od_norm <-read.csv("Norm_OD_repr.csv", header = T, 
                    stringsAsFactors=FALSE)
#normalize intensity by being divided by group mean
avg <- aggregate(od_norm[,2],
                 list(od_norm[,1]),
                 mean)
for(j in 1:length(na.omit(od_norm[,1])))
{
    od_norm[j,3] = od_norm[j,2]/avg[avg[,1]==od_norm[j,1],2]
}
####seperate data######
normal <- na.omit(od_norm[od_norm[,1]==1,3])
od <- na.omit(od_norm[od_norm[,1]==2,3])
od.density <-density(od)
normal.density <- density(normal)

dev.new()
pdf("norm_vs_od_density.pdf")
plot(normal.density, col = "red", lwd = 3, ylim = c(0,1.2))
lines(od.density, col = "blue", lwd = 3)
abline(v = 0.5, lty = 3, col = "purple", lwd = 3)
abline(v = 1.5, lty= 3, col = "purple", lwd = 3)
legend("topright",lwd = 3,legend = c("N","OD"), col = c("red","blue"))
dev.off()

#######################plot density map for HUVEC control&treatment#########
pc_repr <-read.csv("pos_ctrl_repr.csv", header = T, 
                   stringsAsFactors=FALSE)
#normalize intensity by being divided by group mean
for(i in 0:4)
{
    avg <- aggregate(pc_repr[,(1+3*i):(2+3*i)],list(pc_repr[,(1+3*i)]),mean)
    for(j in 1:length(na.omit(pc_repr[,1+3*i])))
    {
        pc_repr[j,3+3*i] = pc_repr[j,2+3*i]/avg[avg[,1]==pc_repr[j,1+3*i],3]
    }
}
####seperate data######
ctrl1_repr <- na.omit(pc_repr[,3])
h2o2_repr <- na.omit(pc_repr[,6])
ctrl2_repr <- na.omit(pc_repr[,9])
hu_repr <- na.omit(pc_repr[,12])
etop_repr <- na.omit(pc_repr[,15])
ctrl1.density <-density(ctrl1_repr)
h2o2.density <- density(h2o2_repr)
ctrl2.density <-density(ctrl2_repr)
hu.density <- density(hu_repr)
etop.density <- density(etop_repr)


#dev.new()
pdf("ctrl_vs_h2o2.pdf")
plot(ctrl1.density, col = "red", lwd = 3, ylim = c(0,1.5))
lines(h2o2.density, col = "blue", lwd = 3)
abline(v = 0.5, lty = 3, col = "purple", lwd = 3)
abline(v = 1.5, lty= 3, col = "purple", lwd = 3)
legend("topright",lwd = 3,legend = c("control","H2O2"), col = c("red","blue"))
dev.off()

pdf("ctrl_vs_hu_etop.pdf")
plot(ctrl2.density, col = "red", lwd = 3, ylim = c(0,1.5))
lines(hu.density, col = "blue", lwd = 3)
lines(etop.density, col = "green", lwd = 3)
abline(v = 0.5, lty = 3, col = "purple", lwd = 3)
abline(v = 1.5, lty= 3, col = "purple", lwd = 3)
legend("topright",lwd = 3,legend = c("control","HU","Etop"), col = c("red","blue","green"))
dev.off()


#########################3D experiment analysis##############
tetPlk4 <- read.csv("H3K9Me3_3D.csv", header = T, 
                    stringsAsFactors=FALSE)
#normalize intensity by being divided by group mean
for(i in 0:1)
{
    avg <- aggregate(tetPlk4[,(2+3*i)],
                     list(tetPlk4[,(1+3*i)]),
                     mean)
    for(j in 1:length(na.omit(tetPlk4[,1+3*i])))
    {
        tetPlk4[j,3+3*i] = tetPlk4[j,2+3*i]/avg[avg[,1]==tetPlk4[j,1+3*i],2]
    }
}

count_pixel_perc_5 <- function(t){
    sum(t<0.5 | t>1.5)
}
perc_calc <- function(x){
    #x as input (3 columns)
    total_pixel_num <- aggregate(x[,1],list(x[,1]),length)
    y <- aggregate(as.numeric(x[,3]),list(x[,1]),count_pixel_perc_5)
    y[,2] <- y[,2]/total_pixel_num[,2]
    return(y)
}

tet3D_final_result <- 1
for(i in 0:1)
{
    tmp <- perc_calc(tetPlk4[,(1+3*i):(3+3*i)]) * 100
    tet3D_final_result <- rowr::cbind.fill(tet3D_final_result,
                                             tmp, fill=NA)
}

#hist(table[table[,1]==1,3])
write.csv(tet3D_final_result,"H3K9Me3_3D_analyzed.csv")
