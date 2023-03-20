
# set working directory and load modules and add data ----------------------------------

library(ggplot2)
library(tidyverse)
library(cowplot)
library(dplyr)

setwd("D:/Backup/OneDrive - University of Exeter/year_4/Dissertation_data/ITS_LIVE_velocity/Vplotted")


errorvalues <- c(17.69671233,12.33132947,16.6256045,14.73404304,12.47125506,13.2009014,12.42836414,15.33614732,14.64654767,17.26210522,
                 13.91467611,15.59964901,14.68576702,17.10407649,32.54791908,18.46412887,12.9413071,16.72542512,16.23509249,16.72586478,
                 17.36610512,22.65365213,21.21346303,19.25155072,28.32482441,18.77610537,18.75622354,12.42115573,6.566635852,3.849937597,
                 4.445399344,3.687521884,2.782522411,3.30507646)


vData2 <- read.csv("Velocityyears1985to2018alongflowlines.csv", header=TRUE,sep=",")

vData2reformatted <- read.csv("Velocityyearsreformatted2.csv", header=TRUE,sep=",")


vData2reformatted$error <- ifelse(vData2reformatted$year == 1985, errorvalues[1],
                                  ifelse(vData2reformatted$year == 1986, errorvalues[2],
                                         ifelse(vData2reformatted$year == 1987, errorvalues[3],
                                                ifelse(vData2reformatted$year == 1988, errorvalues[4],
                                                       ifelse(vData2reformatted$year == 1989, errorvalues[5],
                                                              ifelse(vData2reformatted$year == 1990, errorvalues[6],
                                                                     ifelse(vData2reformatted$year == 1991, errorvalues[7],
                                                                            ifelse(vData2reformatted$year == 1992, errorvalues[8],
                                                                                   ifelse(vData2reformatted$year == 1993, errorvalues[9],
                                                                                          ifelse(vData2reformatted$year == 1994, errorvalues[10],
                                                                                                 ifelse(vData2reformatted$year == 1995, errorvalues[11],
                                                                                                        ifelse(vData2reformatted$year == 1996, errorvalues[12],
                                                                                                               ifelse(vData2reformatted$year == 1997, errorvalues[13],
                                                                                                                      ifelse(vData2reformatted$year == 1998, errorvalues[14],
                                                                                                                             ifelse(vData2reformatted$year == 1999, errorvalues[15],
                                                                                                                                    ifelse(vData2reformatted$year == 2000, errorvalues[16],
                                                                                                                                           ifelse(vData2reformatted$year == 2001, errorvalues[17],
                                                                                                                                                  ifelse(vData2reformatted$year == 2002, errorvalues[18],
                                                                                                                                                         ifelse(vData2reformatted$year == 2003, errorvalues[19],
                                                                                                                                                                ifelse(vData2reformatted$year == 2004, errorvalues[20],
                                                                                                                                                                       ifelse(vData2reformatted$year == 2005, errorvalues[21],
                                                                                                                                                                              ifelse(vData2reformatted$year == 2006, errorvalues[22],
                                                                                                                                                                                     ifelse(vData2reformatted$year == 2007, errorvalues[23],
                                                                                                                                                                                            ifelse(vData2reformatted$year == 2008, errorvalues[24],
                                                                                                                                                                                                   ifelse(vData2reformatted$year == 2009, errorvalues[25],
                                                                                                                                                                                                          ifelse(vData2reformatted$year == 2010, errorvalues[26],
                                                                                                                                                                                                                 ifelse(vData2reformatted$year == 2011, errorvalues[27],
                                                                                                                                                                                                                        ifelse(vData2reformatted$year == 2012, errorvalues[28],
                                                                                                                                                                                                                               ifelse(vData2reformatted$year == 2013, errorvalues[29],
                                                                                                                                                                                                                                      ifelse(vData2reformatted$year == 2014, errorvalues[30],
                                                                                                                                                                                                                                             ifelse(vData2reformatted$year == 2015, errorvalues[31],
                                                                                                                                                                                                                                                    ifelse(vData2reformatted$year == 2016, errorvalues[32],
                                                                                                                                                                                                                                                           ifelse(vData2reformatted$year == 2017, errorvalues[33],
                                                                                                                                                                                                                                                                  ifelse(vData2reformatted$year == 2018, errorvalues[34],0))))))))))))))))))))))))))))))))))


CentralFlowline <- subset(vData2, Flowline == 'Central')

tributaryRight <- subset(vData2, Flowline == 'tributary right')

tributaryMiddleRight <- subset(vData2, Flowline == 'tributary middle right')

secondaryOutletLeft <- subset(vData2, Flowline == 'secondary outlet left')

CentralFlowlineReformatted <- subset(vData2reformatted, Flowline == 'Central')

righttribReformatted <- subset(vData2reformatted, Flowline == 'tributary right')

NEoutletReformatted <- subset(vData2reformatted, Flowline == 'NE ')

rightmiddletribReformatted <- subset(vData2reformatted, Flowline == 'tributary middle right')





Average_central_glacier <- aggregate(CentralFlowlineReformatted$v, 
                                   by=list(Category=CentralFlowlineReformatted$year), 
                                   FUN=mean)

Average_righttrib <- aggregate(righttribReformatted$v, 
                               by=list(Category=righttribReformatted$year), 
                               FUN=mean)

Average_middlerighttrib <- aggregate(rightmiddletribReformatted$v, 
                               by=list(Category=rightmiddletribReformatted$year), 
                               FUN=mean)



Average_central_glacier <- Average_central_glacier %>%
  add_column(errors=errorvalues)

Average_righttrib <- Average_righttrib %>%
  add_column(errors=errorvalues)

Average_middlerighttrib <- Average_middlerighttrib %>%
  add_column(errors=errorvalues)



par(mfrow=c(1,1))
plot(Average_central_glacier$Category, Average_central_glacier$x)
abline(0,0)




Metersperday <- Average_central_glacier$x/365

par(mfrow=c(1,1))
plot(Average_central_glacier$Category, Metersperday)
abline(0,0)



# Analysis: decadal averages ----------------------------------------------


Av1980 <- sum(subset(Average_central_glacier, Category < 1990)$x)/(1990-1985)
Av1990 <- sum(subset(Average_central_glacier, Category < 2000 & Category >= 1990)$x)/(10)
Av2000 <- sum(subset(Average_central_glacier, Category < 2010 & Category >= 2000)$x)/(10)
Av2010 <- sum(subset(Average_central_glacier, Category >= 2010)$x)/(8)

Av_decades = list(Av1980, Av1990, Av2000, Av2010)

Decades = list(1980,1990,2000,2010)

par(mfrow=c(1,1))

plot(Decades, Av_decades)


# GGPLOT Average velocity on central flowline 1985-2018 ------------------------------------------------------------------


vandtimeregression <- lm(x~Category, data=Average_central_glacier)
summary(vandtimeregression)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize = 18
pointsize = 5

ggplot(Average_central_glacier, 
       aes(x = Category,
           y = x)) +
  geom_point(size=pointsize, alpha=0.5) +
  theme_classic() +
  ylim(300, 600) +
  geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.6,
                position=position_dodge(.9), alpha=0.3)  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.8, linetype = "solid")) +
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE)+
  theme (legend.position = c(0.9, 0.6), text = element_text(size = textSize)) +
  labs(x = "Time (Years)",
       y = "Average velocity along central flowline (m/yr)") +
  annotate(geom="text",
           x=1990,
           y=570,
           label="p value = 0.009\nAdjusted R-Squared value = 0.167",
           color=cbbPalette[1],
           size=6)





# GGPLOT velocity down central flowline for each year ------------------------------------------------------------------

# central flowline



vanddregression <- lm(v~distance, CentralFlowlineReformatted)
summary(vanddregression)


#exp.model <-lm(v ~ exp(distance), CentralFlowlineReformatted)

#log.model.centrallFlowlineReformatted <- data.frame(x = CentralFlowlineReformatted$distance,
#                           y = exp(fitted(log.model)))



textSize <- 20
pointsize <- 2


ggplot(CentralFlowlineReformatted, 
       aes(x = distance,
           y = v,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha=0.5) +
  theme_classic() +
  ylim(200, 2100) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=v-error, ymax=v+error), width=.3,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"))+
#  geom_smooth(method='lm',
#              formula = y~x, 
#              se=TRUE, na.rm = FALSE, 
#              inherit.aes = TRUE) +
#  annotate(geom="text",
#           x=5990,
#           y=1500,
#           label="p value = 2.2*10^-16\nAdjusted R-Squared value = 0.288",
#           color=cbbPalette[1],
#           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the central flowline (m)",
       y = "Velocity (m/yr)")




# plotting all years for appendix -----------------------------------------



xlimit = c(0,36000)
ylimit = c(-100,2000)
par(mfrow=c(6,6))
plot(CentralFlowline$distance, CentralFlowline$vel_1, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_2, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_3, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_4, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_5, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_6, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_7, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_8, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_9, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_10, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_11, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_12, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_13, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_14, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_15, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_16, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_17, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_18, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_19, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_20, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_21, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_22, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_23, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_24, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_25, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_26, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_27, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_28, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_29, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_30, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_31, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_32, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_33, ylim = ylimit, xlim=c(0,36000))
plot(CentralFlowline$distance, CentralFlowline$vel_34, ylim = ylimit, xlim=c(0,36000))



# GGPLOT Velocity down right tributary 1985-2018 --------------------------


# central flowline

# add annually variable error values


vanddregression2 <- lm(v~distance, righttribReformatted)
summary(vanddregression2)


#exp.model <-lm(v ~ exp(distance), CentralFlowlineReformatted)

#log.model.centrallFlowlineReformatted <- data.frame(x = CentralFlowlineReformatted$distance,
#                           y = exp(fitted(log.model)))



textSize <- 20
pointsize <- 2


ggplot(righttribReformatted, 
       aes(x = distance,
           y = v,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha=0.5) +
  theme_classic() +
  ylim(-50, 300) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=v-28.86154799, ymax=v+28.86154799), width=.3,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"))+
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE) +
  annotate(geom="text",
           x=5990,
           y=1500,
           label="p value = 2.2*10^-16\nAdjusted R-Squared value = 0.288",
           color=cbbPalette[1],
           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the central flowline (m)",
       y = "Velocity (m/yr)")




# GGPLOT velocity down NE outlet ------------------------------------------

# central flowline

# add annually variable error values


vanddregression3 <- lm(v~distance, CentralFlowlineReformatted)
summary(vanddregression)


#exp.model <-lm(v ~ exp(distance), CentralFlowlineReformatted)

#log.model.centrallFlowlineReformatted <- data.frame(x = CentralFlowlineReformatted$distance,
#                           y = exp(fitted(log.model)))



textSize <- 20
pointsize <- 2


ggplot(CentralFlowlineReformatted, 
       aes(x = distance,
           y = v,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha=0.5) +
  theme_classic() +
  ylim(200, 2100) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=v-28.86154799, ymax=v+28.86154799), width=.3,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"))+
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE) +
  annotate(geom="text",
           x=5990,
           y=1500,
           label="p value = 2.2*10^-16\nAdjusted R-Squared value = 0.288",
           color=cbbPalette[1],
           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the central flowline (m)",
       y = "Velocity (m/yr)")



# GGPLOT PLOTTING TRIBUTARIES ---------------------------------------------


# plot diff along flowm lines and annual averages.

textSize <- 16
pointsize <- 2

# g1 <- ggplot(CentralFlowlineReformatted, 
#              aes(x = distance,
#                  y = v,
#                  color = year,
#                  fill=year)) +
#   geom_point(size=pointsize, alpha=0.5) +
#   theme_classic() +
#   ylim(200, 2100) +
#   scale_fill_viridis_c(option = "magma") +
#   scale_color_viridis_c(option = "magma") +
#   geom_errorbar(aes(ymin=v-28.86154799, ymax=v+28.86154799), width=.3,
#                 position=position_dodge(.9))  +
#   theme(panel.background = element_rect(fill = "gray",
#                                         colour = "gray",
#                                         size = 0.5, linetype = "solid"))+
#   geom_smooth(method='lm',
#               formula = y~x, 
#               se=TRUE, na.rm = FALSE, 
#               inherit.aes = TRUE) +
#   annotate(geom="text",
#            x=5990,
#            y=1500,
#            label="p value = 2.2*10^-16\nAdjusted R-Squared value = 0.288",
#            color=cbbPalette[1],
#            size=6)+
#   theme (text = element_text(size = textSize)) +
#   labs(x = "distance down the central flowline (m)",
#        y = "Velocity (m/yr)")

g2 <- ggplot(righttribReformatted, 
             aes(x = distance,
                 y = v,
                 color = year,
                 fill=year)) +
  geom_point(size=pointsize, alpha=0.5) +
  theme_classic() +
  ylim(-50, 300) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=v-error, ymax=v+error), width=.3,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank())+
#  geom_smooth(method='lm',
#              formula = y~x, 
#              se=TRUE, na.rm = FALSE, 
#              inherit.aes = TRUE) +
#  annotate(geom="text",
#           x=5990,
#           y=1500,
#           label="p value = 2.2*10^-16\nAdjusted R-Squared value = 0.288",
#           color=cbbPalette[1],
#           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the right tributary (m)",
       y = "Velocity (m/yr)") +
  geom_hline(yintercept = 0)

g3 <- ggplot(rightmiddletribReformatted, 
             aes(x = distance,
                 y = v,
                 color = year,
                 fill=year)) +
  geom_point(size=pointsize, alpha=0.5) +
  theme_classic() +
  ylim(-50, 200) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=v-error, ymax=v+error), width=.3,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank())+
#  geom_smooth(method='lm',
#              formula = y~x, 
#              se=TRUE, na.rm = FALSE, 
#              inherit.aes = TRUE) +
#  annotate(geom="text",
#           x=5990,
#           y=1500,
#           label="p value = 2.2*10^-16\nAdjusted R-Squared value = 0.288",
#           color=cbbPalette[1],
#           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the middle tributary (m)",
       y = "Velocity (m/yr)") +
  geom_hline(yintercept = 0)


g4 <- ggplot(Average_righttrib, 
             aes(x = Category,
                 y = x,
                 colour=Category,
                 fill=Category)) +
  geom_point(size=4, alpha=0.9) +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  theme_classic() +
  ylim(50, 220) +
  geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.6,
                position=position_dodge(.9), alpha=0.9)  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.8, linetype = "solid")) +
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE)+
  theme (text = element_text(size = textSize), legend.title = element_blank()) +
  labs(x = "Time (Years)",
       y = "Avg velocity along right tributary (m/yr)") +
  annotate(geom="text",
           x=2008,
           y=175,
           label="p value = 1.48*10^-6\nAdjusted R-Squared value = 0.51",
           color=cbbPalette[1],
           size=4)

 g5 <- ggplot(Average_middlerighttrib, 
              aes(x = Category,
                  y = x,
                  colour=Category,
                  fill = Category)) +
   geom_point(size=4, alpha=0.9) +
   scale_fill_viridis_c(option = "magma") +
   scale_color_viridis_c(option = "magma") +
   theme_classic() +
   ylim(-20, 100) +
   geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.6,
                 position=position_dodge(.9), alpha=0.9)  +
   theme(panel.background = element_rect(fill = "gray",
                                         colour = "gray",
                                         size = 0.8, linetype = "solid")) +
   geom_smooth(method='lm',
               formula = y~x, 
               se=TRUE, na.rm = FALSE, 
               inherit.aes = TRUE)+
   theme (text = element_text(size = textSize), legend.title = element_blank()) +
   labs(x = "Time (Years)",
        y = "Avg velocity along middle tributary (m/yr)") +
   annotate(geom="text",
            x=2008,
            y=70,
            label="p value = 1.8*10^-4\nAdjusted R-Squared value = 0.3382",
            color=cbbPalette[1],
            size=4) +
   geom_hline(yintercept = 0)
 


ggdraw() +
#  draw_plot(g1, x=0, y=.5, width=1, height=.5) +
  draw_plot(g2, x=0, y=.5, width=.5,height=.5) +
  draw_plot(g3, x=0.5, y=.5, width=.5, height=.5) +
  draw_plot(g4, x=0, y=0, width=.5, height=.5) +
  draw_plot(g5, x=0.5, y=0, width=.5, height=.5) +
  draw_plot_label(label = c("A", "B","C","D"), size = 13,
                  x = c(0, 0.5,0,0.5), y = c(1, 1,0.5,0.5))


rightribvreg <- lm(Average_righttrib$x~Average_righttrib$Category)
summary(rightribvreg)

middletribvreg <- lm(Average_middlerighttrib$x~Average_middlerighttrib$Category)
summary(middletribvreg)


