

# set working directory and load modules and add data ----------------------------------

library(ggplot2)



setwd("D:/Backup/OneDrive - University of Exeter/year_4/Dissertation_data/ITS_LIVE_velocity/diffVfromLongTermaverage")




errorvalues <- c(17.69671233,12.33132947,16.6256045,14.73404304,12.47125506,13.2009014,12.42836414,15.33614732,14.64654767,17.26210522,
                 13.91467611,15.59964901,14.68576702,17.10407649,32.54791908,18.46412887,12.9413071,16.72542512,16.23509249,16.72586478,
                 17.36610512,22.65365213,21.21346303,19.25155072,28.32482441,18.77610537,18.75622354,12.42115573,6.566635852,3.849937597,
                 4.445399344,3.687521884,2.782522411,3.30507646)




vData <- read.csv("diffBaselineAverage1985to2018120mIncNEoutlet.csv", header=TRUE,sep=",")
vData2008to2016 <- read.csv("2008to2016.csv", header = TRUE, sep=",")
vData1985to2018 <- read.csv("1985to2018_1.csv", header = TRUE, sep=",")


vData1985to2018$error <- ifelse(vData1985to2018$year == 1985, errorvalues[1],
                                  ifelse(vData1985to2018$year == 1986, errorvalues[2],
                                         ifelse(vData1985to2018$year == 1987, errorvalues[3],
                                                ifelse(vData1985to2018$year == 1988, errorvalues[4],
                                                       ifelse(vData1985to2018$year == 1989, errorvalues[5],
                                                              ifelse(vData1985to2018$year == 1990, errorvalues[6],
                                                                     ifelse(vData1985to2018$year == 1991, errorvalues[7],
                                                                            ifelse(vData1985to2018$year == 1992, errorvalues[8],
                                                                                   ifelse(vData1985to2018$year == 1993, errorvalues[9],
                                                                                          ifelse(vData1985to2018$year == 1994, errorvalues[10],
                                                                                                 ifelse(vData1985to2018$year == 1995, errorvalues[11],
                                                                                                        ifelse(vData1985to2018$year == 1996, errorvalues[12],
                                                                                                               ifelse(vData1985to2018$year == 1997, errorvalues[13],
                                                                                                                      ifelse(vData1985to2018$year == 1998, errorvalues[14],
                                                                                                                             ifelse(vData1985to2018$year == 1999, errorvalues[15],
                                                                                                                                    ifelse(vData1985to2018$year == 2000, errorvalues[16],
                                                                                                                                           ifelse(vData1985to2018$year == 2001, errorvalues[17],
                                                                                                                                                  ifelse(vData1985to2018$year == 2002, errorvalues[18],
                                                                                                                                                         ifelse(vData1985to2018$year == 2003, errorvalues[19],
                                                                                                                                                                ifelse(vData1985to2018$year == 2004, errorvalues[20],
                                                                                                                                                                       ifelse(vData1985to2018$year == 2005, errorvalues[21],
                                                                                                                                                                              ifelse(vData1985to2018$year == 2006, errorvalues[22],
                                                                                                                                                                                     ifelse(vData1985to2018$year == 2007, errorvalues[23],
                                                                                                                                                                                            ifelse(vData1985to2018$year == 2008, errorvalues[24],
                                                                                                                                                                                                   ifelse(vData1985to2018$year == 2009, errorvalues[25],
                                                                                                                                                                                                          ifelse(vData1985to2018$year == 2010, errorvalues[26],
                                                                                                                                                                                                                 ifelse(vData1985to2018$year == 2011, errorvalues[27],
                                                                                                                                                                                                                        ifelse(vData1985to2018$year == 2012, errorvalues[28],
                                                                                                                                                                                                                               ifelse(vData1985to2018$year == 2013, errorvalues[29],
                                                                                                                                                                                                                                      ifelse(vData1985to2018$year == 2014, errorvalues[30],
                                                                                                                                                                                                                                             ifelse(vData1985to2018$year == 2015, errorvalues[31],
                                                                                                                                                                                                                                                    ifelse(vData1985to2018$year == 2016, errorvalues[32],
                                                                                                                                                                                                                                                           ifelse(vData1985to2018$year == 2017, errorvalues[33],
                                                                                                                                                                                                                                                                  ifelse(vData1985to2018$year == 2018, errorvalues[34],0))))))))))))))))))))))))))))))))))



perc_diff <- read.csv("perc_diff.csv", header = TRUE, sep = ",")
longterm_av <- read.csv("LongtermAverageV2.csv", header = TRUE, sep = ",")


CentralFlowline <- subset(vData, Flowline == 'Central')

tributaryRight <- subset(vData, Flowline == 'tributary right')

tributaryMiddleRight <- subset(vData, Flowline == 'tributary middle right')

secondaryOutletLeft <- subset(vData, Flowline == 'secondary outlet left')

northEastOutlet <- subset(vData, Flowline == "NorthEastOulet")

lower_glacier_1985to2018 <- subset(vData1985to2018, distance >= 30000)


perc_diff_central <- subset(perc_diff, Flowline == 'Central')


longterm_av_centralflowline <- subset(longterm_av, Flowline == 'Central')




# plot long term average --------------------------------------------------


plot(longterm_av_centralflowline$distance,longterm_av_centralflowline$vel_1)



# data Analysis working out percentage v diff -----------------------------------------------------------

perc_v_diff = data.frame(vData$id,
                         vData$Flowline,
                         vData$distance,
                         vData$angle,
                         (vData$vel_1/longterm_av$vel_1),
                         (vData$vel_2/longterm_av$vel_1),
                         (vData$vel_3/longterm_av$vel_1),
                         (vData$vel_4/longterm_av$vel_1),
                         (vData$vel_5/longterm_av$vel_1),
                         (vData$vel_6/longterm_av$vel_1),
                         (vData$vel_7/longterm_av$vel_1),
                         (vData$vel_8/longterm_av$vel_1),
                         (vData$vel_9/longterm_av$vel_1),
                         (vData$vel_10/longterm_av$vel_1),
                         (vData$vel_11/longterm_av$vel_1),
                         (vData$vel_12/longterm_av$vel_1),
                         (vData$vel_13/longterm_av$vel_1),
                         (vData$vel_14/longterm_av$vel_1),
                         (vData$vel_15/longterm_av$vel_1),
                         (vData$vel_16/longterm_av$vel_1),
                         (vData$vel_17/longterm_av$vel_1),
                         (vData$vel_18/longterm_av$vel_1),
                         (vData$vel_19/longterm_av$vel_1),
                         (vData$vel_20/longterm_av$vel_1),
                         (vData$vel_21/longterm_av$vel_1),
                         (vData$vel_22/longterm_av$vel_1),
                         (vData$vel_23/longterm_av$vel_1),
                         (vData$vel_24/longterm_av$vel_1),
                         (vData$vel_25/longterm_av$vel_1),
                         (vData$vel_26/longterm_av$vel_1),
                         (vData$vel_27/longterm_av$vel_1),
                         (vData$vel_28/longterm_av$vel_1),
                         (vData$vel_29/longterm_av$vel_1),
                         (vData$vel_30/longterm_av$vel_1),
                         (vData$vel_31/longterm_av$vel_1),
                         (vData$vel_32/longterm_av$vel_1),
                         (vData$vel_33/longterm_av$vel_1),
                         (vData$vel_34/longterm_av$vel_1))


#perc_v_diff <- CentralFlowline / longterm_av_centralflowline$vel_1


Average_lower_glacier <- aggregate(lower_glacier_1985to2018$V, 
                                   by=list(Category=lower_glacier_1985to2018$year), 
                                   FUN=mean)



#X <- merge(x=perc_v_diff,
#           y=vData1985to2018,
#           by.x = c(""), 
#           by.y = c(""))




# plot annual averages along lower glacier --------------------------------



#finds an average velocity for the lower half of the glacier, where the most
#activity has occurred, per year. so graph produced is annual average of 
#difference from long term average for lower portion of glacier.

#also happens to be when the glacier bed goes below sea level


Average_lower_glacier <- aggregate(lower_glacier_1985to2018$V, 
                                   by=list(Category=lower_glacier_1985to2018$year), 
                                   FUN=mean)
  

par(mfrow=c(1,1))
plot(Average_lower_glacier$Category, Average_lower_glacier$x)
abline(0,0)



# plot central flowline data ---------------------------------------------------------------

plot(CentralFlowline$distance, CentralFlowline$vel_25, ylim = c(-120,500), col='yellow')
par(new=TRUE)
plot(CentralFlowline$distance, CentralFlowline$vel_26, ylim = c(-120,500))
par(new=TRUE)
plot(CentralFlowline$distance, CentralFlowline$vel_27, ylim = c(-120,500))
par(new=TRUE)
plot(CentralFlowline$distance, CentralFlowline$vel_28, ylim = c(-120,500))
par(new=TRUE)
plot(CentralFlowline$distance, CentralFlowline$vel_29, ylim = c(-120,500))
par(new=TRUE)
plot(CentralFlowline$distance, CentralFlowline$vel_30, ylim = c(-120,500), col = 'red')
par(new=TRUE)
plot(CentralFlowline$distance, CentralFlowline$vel_31, ylim = c(-120,500))



# plot right tributary flowline data --------------------------------------

set.seed(0)
dev.off(dev.list()["RStudioGD"])

plot(tributaryRight$distance, tributaryRight$vel_25, ylim = c(-100,50), col='yellow')
par(new=TRUE)
plot(tributaryRight$distance, tributaryRight$vel_26, ylim = c(-100,50))
par(new=TRUE)
plot(tributaryRight$distance, tributaryRight$vel_27, ylim = c(-100,50))
par(new=TRUE)
plot(tributaryRight$distance, tributaryRight$vel_28, ylim = c(-100,50))
par(new=TRUE)
plot(tributaryRight$distance, tributaryRight$vel_29, ylim = c(-100,50))
par(new=TRUE)
plot(tributaryRight$distance, tributaryRight$vel_30, ylim = c(-100,50), col = 'red')
par(new=TRUE)
plot(tributaryRight$distance, tributaryRight$vel_31, ylim = c(-100,50))



# plot central right tributary flowline data test ------------------------------

dev.off(dev.list()["RStudioGD"])

for (i in tributaryMiddleRight)
  print(i)
  plot(i)
  par(new=TRUE)

  #plot(tributaryMiddleRight$distance,tributaryMiddleRight$i)
  #par(new=TRUE)


  

# percentage difference v to longterm average -----------------------------

  set.seed(0)
  dev.off(dev.list()["RStudioGD"])
  
  ylimits = c(-20,100)
  
  plot(perc_diff_central$distance, perc_diff_central$X2009, ylim = ylimits, col='yellow')
  par(new=TRUE)
  plot(perc_diff_central$distance, perc_diff_central$X2010, ylim = ylimits)
  par(new=TRUE)
  plot(perc_diff_central$distance, perc_diff_central$X2011, ylim = ylimits)
  par(new=TRUE)
  plot(perc_diff_central$distance, perc_diff_central$X2012, ylim = ylimits)
  par(new=TRUE)
  plot(perc_diff_central$distance, perc_diff_central$X2013, ylim = ylimits)
  par(new=TRUE)
  plot(perc_diff_central$distance, perc_diff_central$X2014, ylim = ylimits, col = 'red')
  par(new=TRUE)
  plot(perc_diff_central$distance, perc_diff_central$X2014, ylim = ylimits)
  abline(0,0)
  
  
  
    

# GGPLOT ------------------------------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  


cvandregression <- lm(V~distance, vData1985to2018)
summary(cvandregression)
  
  
textSize <- 20
pointsize <- 3


ggplot(vData1985to2018, 
       aes(x = distance,
           y = V,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha = 0.5) +
  theme_classic() +
  ylim(-500, 1000) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=V-error, ymax=V+error), width=.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                    colour = "gray",
                                    size = 0.5, linetype = "solid")) +
  geom_hline(yintercept=0)+
 # geom_smooth(method='lm',
 #             formula = y~x, 
 #             se=TRUE, na.rm = FALSE, 
 #             inherit.aes = TRUE) +
 # annotate(geom="text",
 #          x=5990,
 #          y=800,
 #          label="p value = n\nAdjusted R-Squared value = n",
 #          color=cbbPalette[1],
 #          size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the central flowline (m)",
       y = "Change in Velocity from long term average (1985-2018) (m/yr)")



# GGPLOT LINEAR REGRESSIONS -----------------------------------------------



cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



cvandregression <- lm(V~distance, vData1985to2018)
summary(cvandregression)


textSize <- 20
pointsize <- 3


ggplot(vData1985to2018, 
       aes(x = distance,
           y = V,
           color = factor(year),
           fill=factor(year))) +
 # geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-200, 500) +
  #scale_fill_viridis_c(option = "magma") +
  #scale_color_viridis_c(option = "magma") +
  #geom_errorbar(aes(ymin=V-25.8, ymax=V+25.8), width=.3,
  #              position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid")) +
  geom_hline(yintercept=0)+
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE) +
  annotate(geom="text",
           x=5990,
           y=300,
           label="p value = n\nAdjusted R-Squared value = n",
           color=cbbPalette[1],
           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the central flowline (m)",
       y = "Change in Velocity from long term average (1985-2018) (m/yr)")



# PLOTTING AVERAGE V ALONG GLACIER FOR EACH YEAR --------------------------


Diff_V_central_glacier <- aggregate(vData1985to2018$V, 
                                     by=list(Category=vData1985to2018$year), 
                                     FUN=mean)


par(mfrow=c(1,1))
plot(Diff_V_central_glacier$Category, Diff_V_central_glacier$x)
abline(0,0)




# GGPLOT PERC AV V --------------------------------------------------------

textSize <- 26
pointsize <- 3

ggplot(perc_v_diff) +
  geom_point(aes(x = vData.distance,
                 y = X.vData.vel_1.longterm_av.vel_1.,
                 color=vData.Flowline,
                 fill=vData.Flowline),
             size=pointsize) +
  geom_point(aes(x = vData.distance,
                 y = X.vData.vel_2.longterm_av.vel_1.,
                 color=vData.Flowline,
                 fill=vData.Flowline),
             size=pointsize)



# plot all years for appendix ----------------------------------------------

par(mfrow=c(4,9))
plot(CentralFlowline$distance, CentralFlowline$vel_1, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_2, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_3, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_4, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_5, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_6, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_7, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_8, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_9, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_10, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_11, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_12, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_13, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_14, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_15, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_16, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_17, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_18, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_19, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_20, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_21, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_22, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_23, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_24, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_25, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_26, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_27, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_28, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_29, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_30, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_31, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_32, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_33, ylim = c(-120,500), xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_34, ylim = c(-120,500), xlim=c(0,36000))




# plot all perc diff for appendix -----------------------------------------

ylimits = c(-20,100)
xlimits = c(0,36000)

par(mfrow=c(4,9))

plot(perc_diff_central$distance, perc_diff_central$X1985, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1986, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1987, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1988, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1989, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1990, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1991, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1992, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1993, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1994, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1995, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1996, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1997, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1998, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X1999, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2000, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2001, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2002, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2003, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2004, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2005, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2006, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2007, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2008, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2009, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2010, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2011, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2012, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2013, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2014, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2015, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2016, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2017, ylim= ylimits, xlim= xlimits)
abline(0,0)
plot(perc_diff_central$distance, perc_diff_central$X2018, ylim= ylimits, xlim= xlimits)
abline(0,0)







# plot all diff values for North East Outlet ------------------------------


ylimits = c(-50,50)
xlimits = c(0,15000)

par(mfrow=c(4,9))


plot(northEastOutlet$distance,northEastOutlet$vel_1, xlim=xlimits, ylim = ylimits)
abline(0,0)
plot(northEastOutlet$distance,northEastOutlet$vel_2, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_3, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_4, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_5, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_6, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_7, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_8, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_9, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_10, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_11, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_12, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_13, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_14, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_15, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_16, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_17, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_18, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_19, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_20, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_21, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_22, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_23, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_24, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_25, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_26, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_27, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_28, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_29, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_30, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_31, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_32, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_33, xlim=xlimits, ylim = ylimits)
plot(northEastOutlet$distance,northEastOutlet$vel_34, xlim=xlimits, ylim = ylimits)





# plot tributary middle right ---------------------------------------------


ylimits = c(-60,60)
xlimits = c(0,15000)

par(mfrow=c(4,9))

plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_1, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_2, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_3, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_4, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_5, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_6, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_7, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_8, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_9, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_10, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_11, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_12, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_13, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_14, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_15, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_16, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_17, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_18, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_19, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_20, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_21, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_22, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_23, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_24, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_25, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_26, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_27, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_28, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_29, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_30, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_31, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_32, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_33, xlim = xlimits, ylim = ylimits)
plot(tributaryMiddleRight$distance,tributaryMiddleRight$vel_34, xlim = xlimits, ylim = ylimits)
