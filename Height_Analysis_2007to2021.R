# set up ------------------------------------------------------------------


# import required libraries
library(ggplot2)
library(tidyverse)
library(hdf5r)
library(cowplot)

# define annual errors calculated in Appendix.N
errors <- c(3.726944,
            9.980137,
            1.666382,
            1.501514,
            0.81486,
            3.079602,
            3.237897,
            1.165585,
            1.676387
)

# set working directory
setwd("D:/Backup/OneDrive - University of Exeter/year_4/Dissertation_data/DEM/Height_R_Analysis")

# import csv datasets for annual height values, bedrock and a reformatted version of heights and bedrock
height =read.csv("height_along_flowlines.csv", header=TRUE, sep=",")

bedrock =read.csv("bedtopographyAlongCentralFlowline.csv", header=TRUE, sep=",")

height2 = read.csv("height_along_flowlines_reformatted.csv", header=TRUE, sep=",")

#add a column for error values where the error is selected based on the associated
height2$error <- ifelse(height2$year == 2012, errors[1],
                                  ifelse(height2$year == 2013, errors[2],
                                         ifelse(height2$year == 2014, errors[3],
                                                ifelse(height2$year == 2015, errors[4],
                                                       ifelse(height2$year == 2016, errors[5],
                                                                     ifelse(height2$year == 2018, errors[6],
                                                                            ifelse(height2$year == 2019, errors[7],
                                                                                   ifelse(height2$year == 2020, errors[8],
                                                                                          ifelse(height2$year == 2021, errors[9],9)))))))))
                                                                                                                                                                                                                      
# subset the dataset for the main flowline and tributaries
centralFlowlineHeight <- subset(height, Flowline == "Central")
righttrib <- subset(height, Flowline == "tributary right")
middletrib <- subset(height, Flowline == "tributary middle right")

centralFlowlineHeight2 <- subset(height2, Flowline == "Central")
centralFlowlineHeight2$h <- ifelse(centralFlowlineHeight2$h ==0,10000, centralFlowlineHeight2$h)

rightTributaryHeight2 <- subset(height2, Flowline == "tributary right")
rightTributaryHeight2$h <- ifelse(rightTributaryHeight2$h ==0,10000, rightTributaryHeight2$h)

middleTributaryHeight2 <-subset(height2, Flowline =="tributary middle right")
middleTributaryHeight2$h <- ifelse(middleTributaryHeight2$h ==0,10000, middleTributaryHeight2$h)


# create list of height differences for each ArcticDEM from the 2007 GIMPDEM
Difffrom2007 <- c((centralFlowlineHeight$X2007 - centralFlowlineHeight$X2012), (centralFlowlineHeight$X2007 - centralFlowlineHeight$X2013),
              (centralFlowlineHeight$X2007 - centralFlowlineHeight$X2014),(centralFlowlineHeight$X2007 - centralFlowlineHeight$X2015),
              (centralFlowlineHeight$X2007 - centralFlowlineHeight$X2016),(centralFlowlineHeight$X2007 - centralFlowlineHeight$X2018),
              (centralFlowlineHeight$X2007 - centralFlowlineHeight$X2019),(centralFlowlineHeight$X2007 - centralFlowlineHeight$X2020),
              (centralFlowlineHeight$X2007 - centralFlowlineHeight$X2021))

# turn above list into data frame and add distance, year and error values excluding 2007, 
# the year the data was referenced against, and 2017 a year with no data
Difffrom2007data <- data_frame(Difffrom2007,
                               subset(centralFlowlineHeight2,
                                      year != 2007 & year != 2017)$distance,
                               subset(centralFlowlineHeight2,
                                      year != 2007 & year != 2017)$year,
                               subset(centralFlowlineHeight2,
                                      year != 2007 & year != 2017)$error)

# same code as above but for rightr tributary
rightTribDifffrom2007 <- c((righttrib$X2007 - righttrib$X2012), (righttrib$X2007 - righttrib$X2013),
                  (righttrib$X2007 - righttrib$X2014),(righttrib$X2007 - righttrib$X2015),
                  (righttrib$X2007 - righttrib$X2016),(righttrib$X2007 - righttrib$X2018),
                  (righttrib$X2007 - righttrib$X2019),(righttrib$X2007 - righttrib$X2020),
                  (righttrib$X2007 - righttrib$X2021))

rightTribDifffrom2007data <- data_frame(rightTribDifffrom2007,
                               subset(rightTributaryHeight2,
                                      year != 2007 & year != 2017)$distance,
                               subset(rightTributaryHeight2,
                                      year != 2007 & year != 2017)$year,
                               subset(rightTributaryHeight2,
                                      year != 2007 & year != 2017)$error)

# same code as above but for middle tributary
middleTribDifffrom2007 <- c((middletrib$X2007 - middletrib$X2012), (middletrib$X2007 - middletrib$X2013),
                           (middletrib$X2007 - middletrib$X2014),(middletrib$X2007 - middletrib$X2015),
                           (middletrib$X2007 - middletrib$X2016),(middletrib$X2007 - middletrib$X2018),
                           (middletrib$X2007 - middletrib$X2019),(middletrib$X2007 - middletrib$X2020),
                           (middletrib$X2007 - middletrib$X2021))

middleTribDifffrom2007data <- data_frame(middleTribDifffrom2007,
                                        subset(middleTributaryHeight2,
                                               year != 2007 & year != 2017)$distance,
                                        subset(middleTributaryHeight2,
                                               year != 2007 & year != 2017)$year,
                                        subset(middleTributaryHeight2,
                                               year != 2007 & year != 2017)$error)

# create annual average difference from 2007 for each ArcticDEM, where height is above 200, removing no data values and error values.
Avg_Annual_height_change <- aggregate(-(subset(Difffrom2007data, Difffrom2007 < 200)$Difffrom2007), 
                                   by=list(Category=subset(Difffrom2007data, Difffrom2007 < 200)$`...$year`), 
                                   FUN=mean)

rightTribAvg_Annual_height_change <- aggregate(-(subset(rightTribDifffrom2007data, rightTribDifffrom2007 < 200)$rightTribDifffrom2007), 
                                               by=list(Category=subset(rightTribDifffrom2007data, rightTribDifffrom2007 < 200)$`...$year`), 
                                               FUN=mean)

middleTribAvg_Annual_height_change <- aggregate(-(subset(middleTribDifffrom2007data, middleTribDifffrom2007 < 200)$middleTribDifffrom2007), 
                                               by=list(Category=subset(middleTribDifffrom2007data, middleTribDifffrom2007 < 200)$`...$year`), 
                                               FUN=mean)

# define nominal date values calculated from appendix.n
nominaldate <- c(2012.391521,
                 2013.295694,
                 2014.35319,
                 2015.421638,
                 2016.202605,
                 2018.626981,
                 2019.711856,
                 2020.572223,
                 2021.284742)

# define date range values calculated from appendix.n
daterange <-c(22,
               3.5,
               13.5,
               0.5,
               30.5,
               53.5,
               17.5,
               13.5,
               11.5)



# add nominal date, errors and date range to Annual average data frame
Avg_Annual_height_change <- Avg_Annual_height_change %>%
  add_column(nominaldate, daterange, season = (nominaldate - as.integer(nominaldate)), errors)

rightTribAvg_Annual_height_change <- rightTribAvg_Annual_height_change %>%
  add_column(nominaldate, daterange, season = (nominaldate - as.integer(nominaldate)), errors)

middleTribAvg_Annual_height_change <- middleTribAvg_Annual_height_change %>%
  add_column(nominaldate, daterange, season = (nominaldate - as.integer(nominaldate)), errors)

#  3 dataframes created of annual height changes from 2007 for 2012-2021, with error values, nominal dates and date ranges attached

# end of script and analysis

par(mfrow=c(1,1))
plot(Avg_Annual_height_change$nominaldate, Avg_Annual_height_change$x)
abline(0,0)
abline(lm(Avg_Annual_height_change$x~Avg_Annual_height_change$nominaldate))


heightitmereg <- lm(Avg_Annual_height_change$x~Avg_Annual_height_change$Category)
summary(heightitmereg)





# GGPLOT AVERAGE ANNUAL CHANGE --------------------------------------------

# add when the DEMs used were created in text above error bars, also add horizontal
# errorbars for range of time when the changes were observed


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize = 20
pointsize = 8

ggplot(Avg_Annual_height_change, 
       aes(x = nominaldate,
           y = x
           )) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-40,0) +
  xlim(2012,2023) +
  geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.3,
                position=position_dodge(.9), alpha=0.9)  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
#  geom_smooth(method='lm',
#              formula = y~x, 
#              se=TRUE, na.rm = FALSE, 
#              inherit.aes = TRUE,
#              fullrange=TRUE,
#              aes(color = ifelse(Category < 2012, "red","black")))+
  theme (text = element_text(size = textSize)) +
  labs(x = "Time (Years)",
       y = "Average Annual height change from 2007 along central flowline (m)") +
  annotate(geom="text",
           x=2018,
           y=-5,
           label="p value = 1.329*10^-5\nAdjusted R-Squared value = 0.9347",
           color=cbbPalette[1],
           size=6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2007) +
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE)+
  geom_linerange(aes(xmin = nominaldate - (daterange/365.2425), xmax = nominaldate + (daterange/365.2425))) +
  geom_vline(xintercept = 2012, colour="gray") +
  geom_vline(xintercept = 2013, colour="gray") +
  geom_vline(xintercept = 2014, colour="gray") +
  geom_vline(xintercept = 2015, colour="gray") +
  geom_vline(xintercept = 2016, colour="gray") +
  geom_vline(xintercept = 2017, colour="gray") +
  geom_vline(xintercept = 2018, colour="gray") +
  geom_vline(xintercept = 2019, colour="gray") +
  geom_vline(xintercept = 2020, colour="gray") +
  geom_vline(xintercept = 2021, colour="gray") +
  geom_vline(xintercept = 2022, colour="gray") +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma")


heighttimereg <- lm(Avg_Annual_height_change$x~Avg_Annual_height_change$Category)
summary(heighttimereg)

# GGPLOT2 central flowline height -----------------------------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 20
pointsize <- 2

ggplot(centralFlowlineHeight2, 
       aes(x = distance,
           y = h-59.23,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-160, 1000) +
  xlim(0,37000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-error, ymax=(h-59.23)+error), width=.3,
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
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b)) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                    position=position_dodge(.9))
 # geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])


# chnage in height along flow line from 2007 ------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 20
pointsize <- 5

ggplot(Difffrom2007data, 
       aes(x = `...$distance`,
           y = -Difffrom2007,
           color = `...$year`,
           fill=`...$year`)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-80,20) +
  xlim(0,37000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(-Difffrom2007)-`...$error`, ymax=(-Difffrom2007)+`...$error`), width=1, alpha=0.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank()) +
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
       y = "height a.s.l (m)")
#  geom_point(aes(x=distance, y = b)) #+
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])




# GGPLOT combine height profile and height chnage from 2007 ---------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 20
pointsize <- 4


g1 <-ggplot(centralFlowlineHeight2, 
            aes(x = distance,
                y = h-59.23,
                color = year,
                fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-160, 1000) +
  xlim(0,37000)+
  scale_fill_viridis_c(option = "magma", limits=c(2007,2021)) +
  scale_color_viridis_c(option = "magma", limits=c(2007,2021)) +
  geom_errorbar(aes(ymin=(h-59.23)-error, ymax=(h-59.23)+error), width=.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank()) +
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
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b)) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                position=position_dodge(.9))
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])

  
g2 <-ggplot(Difffrom2007data, 
            aes(x = `...$distance`,
                y = -Difffrom2007,
                color = `...$year`,
                fill=`...$year`)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-80,20) +
  xlim(0,37000)+
  scale_fill_viridis_c(option = "magma", limits=c(2007,2021)) +
  scale_color_viridis_c(option = "magma", limits=c(2007,2021)) +
  geom_errorbar(aes(ymin=(-Difffrom2007)-`...$error`, ymax=(-Difffrom2007)+`...$error`), width=.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank()) +
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
       y = "chnage in height from 2007 (m)")
#  geom_point(aes(x=distance, y = b)) #+
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])


  
  
  
ggdraw() +
#  draw_plot(g1, x=0, y=.5, width=1, height=.5) +
draw_plot(g1, x=0, y=.5, width=1,height=.5) +
draw_plot(g2, x=0, y=0, width=1, height=.5) +
draw_plot_label(label = c("A", "B"), size = 13,
                x = c(0, 0), y = c(1, 0.5))


# GGPLOT zoomed in central flowline height --------------------------------------------------------

ggplot(centralFlowlineHeight3, 
       aes(x = distance,
           y = h-59.23,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-150, 300) +
  xlim(25000,36000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-1, ymax=(h-59.23)+1), width=.3,
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
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b))



# combining velocity and height along central flowline --------------------


# set same scales for colour !!!!

  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



cvandregression <- lm(V~distance, vData1985to2018)
summary(cvandregression)


textSize <- 20
pointsize <- 3


g1 <- ggplot(subset(vData1985to2018, year > 2011), 
             aes(x = distance,
                 y = V,
                 color = year,
                 fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-200, 500) +
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

g2 <- ggplot(Difffrom2007data, 
             aes(x = `...$distance`,
                 y = -Difffrom2007,
                 color = `...$year`,
                 fill=`...$year`)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-80,20) +
  xlim(0,37000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(-Difffrom2007)-`...$error`, ymax=(-Difffrom2007)+`...$error`), width=.3,
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
       y = "height a.s.l (m)")
#  geom_point(aes(x=distance, y = b)) #+
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])



ggdraw() +
  draw_plot(g1, x=0, y=.5, width=1, height=.5) +
  draw_plot(g2, x=0, y=0, width=1,height=.5) +
  #  draw_plot(g3, x=0, y=0, width=1, height=.5) +
  draw_plot_label(label = c("A", "B"), size = 13,
                  x = c(0, 0), y = c(1, 0.5))




# GGPLOT right tributary --------------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 20
pointsize <- 2

ggplot(rightTributaryHeight2, 
       aes(x = distance,
           y = h-59.23,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-490, 1000) +
  xlim(0,33500)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-1, ymax=(h-59.23)+1), width=.3,
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
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b)) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                position=position_dodge(.9))
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])


# GGPLOT middle right tributary  ------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 20
pointsize <- 2

ggplot(middleTributaryHeight2, 
       aes(x = distance,
           y = h-59.23,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(250, 800) +
  xlim(0,15000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-1, ymax=(h-59.23)+1), width=.3,
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
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b)) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                position=position_dodge(.9))
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])





# GGPLOT NE outlet --------------------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 20
pointsize <- 2

ggplot(NEoutletheight2, 
       aes(x = distance,
           y = h-59.23,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(0, 600) +
  xlim(0,33500)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-1, ymax=(h-59.23)+1), width=.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid")) +
  geom_hline(yintercept=0)+ 
  scale_x_reverse() +
  # geom_smooth(method='lm',
  #             formula = y~x, 
  #             se=TRUE, na.rm = FALSE, 
  #             inherit.aes = TRUE) +
  # annotate(geom="text",
  #          x=5990,\
  #          y=800,
  #          label="p value = n\nAdjusted R-Squared value = n",
  #          color=cbbPalette[1],
  #          size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "distance down the central flowline (m)",
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b)) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                position=position_dodge(.9))
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])

# GGPLOT SEASONAL VARIATIONS ----------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize = 14
pointsize = 8

ggplot(Avg_Annual_height_change, 
       aes(x = season*365.2425,
           y = x,
           colour=factor(Category),
           fill=factor(Category))) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-40,0) +
  xlim(0,365) +
  geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.01,
                position=position_dodge(.9), alpha=0.9)  +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank()) +
    geom_smooth(method='lm',
                formula = y~x, 
                se=TRUE, na.rm = FALSE, 
                inherit.aes = TRUE,
                fullrange=TRUE,
                aes(color = ifelse(Category < 2012, "red","black")))+
  theme (text = element_text(size = textSize)) +
  labs(x = "days of the year (days)",
       y = "Average difference from 2007 (m)") +
  annotate(geom="text",
           x=250,
           y=-5,
           label="p value = 0.1106\nAdjusted R-Squared value = 0.2259",
           color=cbbPalette[1],
           size=6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2007) +
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE)+
  geom_linerange(aes(xmin = season*365.2425 - (daterange), xmax = season*365.2425 + (daterange)))
#  geom_curve(aes(y=sin(x),
#                 x=season,
#                 xend=1,
#                 yend=-20))

heighttimereg <- lm(Avg_Annual_height_change$x~Avg_Annual_height_change$season)
summary(heighttimereg)


# GGPLOT tributaries analysis ---------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

textSize <- 12
pointsize <- 3


g1 <- ggplot(rightTributaryHeight2, 
             aes(x = distance,
                 y = h-59.23,
                 color = year,
                 fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-490, 1000) +
  xlim(0,33500)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-1, ymax=(h-59.23)+1), width=.3,
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
  labs(x = "distance down the right tributary (m)",
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b)) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                position=position_dodge(.9))
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])

  
g2 <-ggplot(middleTributaryHeight2, 
            aes(x = distance,
                y = h-59.23,
                color = year,
                fill=year)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(250, 800) +
  xlim(0,15000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(h-59.23)-1, ymax=(h-59.23)+1), width=.3,
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
  labs(x = "distance down the middle tributary (m)",
       y = "height a.s.l (m)") +
  geom_point(aes(x=distance, y = b), alpha=0.5) +
  geom_errorbar(aes(ymin=(b-b_err), ymax=(b+b_err)), width =.3,
                position=position_dodge(.9))
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])

g3 <-ggplot(rightTribDifffrom2007data, 
            aes(x = `...$distance`,
                y = -rightTribDifffrom2007,
                color = `...$year`,
                fill=`...$year`)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-50,20) +
  xlim(0,33500)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(-rightTribDifffrom2007)-1, ymax=(-rightTribDifffrom2007)+1), width=.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank()) +
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
  labs(x = "distance down the right Tributary (m)",
       y = "height change from 2007 (m)")
#  geom_point(aes(x=distance, y = b)) #+
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])


g4 <-ggplot(middleTribDifffrom2007data, 
            aes(x = `...$distance`,
                y = -middleTribDifffrom2007,
                color = `...$year`,
                fill=`...$year`)) +
  geom_point(size=pointsize, alpha = 0.3) +
  theme_classic() +
  ylim(-30,20) +
  xlim(0,15000)+
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma") +
  geom_errorbar(aes(ymin=(-middleTribDifffrom2007)-1, ymax=(-middleTribDifffrom2007)+1), width=.3,
                position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid"), legend.title = element_blank()) +
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
  labs(x = "distance down the middle Tributary (m)",
       y = "height chnage from 2007 (m)")
#  geom_point(aes(x=distance, y = b)) #+
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])


g5 <-ggplot(rightTribAvg_Annual_height_change, 
            aes(x = nominaldate,
                y = x
            )) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-20,0) +
  xlim(2012,2023) +
  geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.3,
                position=position_dodge(.9), alpha=0.9)  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  #  geom_smooth(method='lm',
  #              formula = y~x, 
  #              se=TRUE, na.rm = FALSE, 
  #              inherit.aes = TRUE,
  #              fullrange=TRUE,
  #              aes(color = ifelse(Category < 2012, "red","black")))+
  theme (text = element_text(size = textSize)) +
  labs(x = "Time (Years)",
       y = "Avg height change from 2007, right Trib (m)") +
  annotate(geom="text",
           x=2018,
           y=-5,
           label="p value = 0.0087\nAdjusted R-Squared value = 0.6",
           color=cbbPalette[1],
           size=4) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2007) +
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE)+
  geom_linerange(aes(xmin = nominaldate - (daterange/365.2425), xmax = nominaldate + (daterange/365.2425))) +
  geom_vline(xintercept = 2012, colour="gray") +
  geom_vline(xintercept = 2013, colour="gray") +
  geom_vline(xintercept = 2014, colour="gray") +
  geom_vline(xintercept = 2015, colour="gray") +
  geom_vline(xintercept = 2016, colour="gray") +
  geom_vline(xintercept = 2017, colour="gray") +
  geom_vline(xintercept = 2018, colour="gray") +
  geom_vline(xintercept = 2019, colour="gray") +
  geom_vline(xintercept = 2020, colour="gray") +
  geom_vline(xintercept = 2021, colour="gray") +
  geom_vline(xintercept = 2022, colour="gray") +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma")


g6 <- ggplot(middleTribAvg_Annual_height_change, 
             aes(x = nominaldate,
                 y = x
             )) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-20,15) +
  xlim(2012,2023) +
  geom_errorbar(aes(ymin=x-errors, ymax=x+errors), width=.3,
                position=position_dodge(.9), alpha=0.9)  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  #  geom_smooth(method='lm',
  #              formula = y~x, 
  #              se=TRUE, na.rm = FALSE, 
  #              inherit.aes = TRUE,
  #              fullrange=TRUE,
  #              aes(color = ifelse(Category < 2012, "red","black")))+
  theme (text = element_text(size = textSize)) +
  labs(x = "Time (Years)",
       y = "Avg height change from 2007, along middle trib (m)") +
  annotate(geom="text",
           x=2018,
           y=5,
           label="p value = 0.00027\nAdjusted R-Squared value = 0.85",
           color=cbbPalette[1],
           size=4) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2007) +
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE)+
  geom_linerange(aes(xmin = nominaldate - (daterange/365.2425), xmax = nominaldate + (daterange/365.2425))) +
  geom_vline(xintercept = 2012, colour="gray") +
  geom_vline(xintercept = 2013, colour="gray") +
  geom_vline(xintercept = 2014, colour="gray") +
  geom_vline(xintercept = 2015, colour="gray") +
  geom_vline(xintercept = 2016, colour="gray") +
  geom_vline(xintercept = 2017, colour="gray") +
  geom_vline(xintercept = 2018, colour="gray") +
  geom_vline(xintercept = 2019, colour="gray") +
  geom_vline(xintercept = 2020, colour="gray") +
  geom_vline(xintercept = 2021, colour="gray") +
  geom_vline(xintercept = 2022, colour="gray") +
  scale_fill_viridis_c(option = "magma") +
  scale_color_viridis_c(option = "magma")

  
  
ggdraw() +
#  draw_plot(g1, x=0, y=.5, width=1, height=.5) +
draw_plot(g3, x=0, y=1/3, width=.5,height=1/3) +
draw_plot(g4, x=0.5, y=1/3, width=.5, height=1/3) +
draw_plot(g1, x=0, y=2/3, width=.5, height=1/3) +
draw_plot(g2, x=0.5, y=2/3, width=.5, height=1/3) +
draw_plot(g5, x=0, y=0, width=.5, height=1/3) +
draw_plot(g6, x=0.5, y=0, width=.5, height=1/3) +
draw_plot_label(label = c("A", "B","E","F","C","D"), size = 13,
                x = c(0, 0.5,0,0.5,0,0.5), y = c(1, 1,1/3,1/3,2/3,2/3))

righttribreg <- lm(rightTribAvg_Annual_height_change$x~rightTribAvg_Annual_height_change$nominaldate)
summary(righttribreg)

middletribreg <- lm(middleTribAvg_Annual_height_change$x~middleTribAvg_Annual_height_change$nominaldate)
summary(middletribreg)
