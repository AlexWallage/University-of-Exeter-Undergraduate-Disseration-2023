

# set up ------------------------------------------------------------------


setwd("D:/Backup/OneDrive - University of Exeter/year_4/Dissertation_data/Kingetal2020/doi_10.5061_dryad.qrfj6q5cb__v2/individual_glaciers")

library(ggplot2)
library(tidyverse)
library(hdf5r)
library(cowplot)

hdffile <- h5file('032_dendrit.h5', 'r')

# front position
frontpos <- hdffile[['/front_position/frontposition']]
frontpostime <- hdffile[['/front_position/time']]
frontposvals <- readDataSet(frontpos)
frontpostimevals <- readDataSet(frontpostime)
frontposition <- data_frame(frontposvals, frontpostimevals)

# ice thickness

icethick <- hdffile[['ice_thickness/thickness']]
icethicktime <- hdffile[['ice_thickness/time']]
icethickval <- readDataSet(icethick)
icethocktimeval <- readDataSet(icethicktime)

plot(frontposition$frontpostimevals/365.2425, frontposition$frontposvals)
#plot(icethickval, icethocktimeval)
abline(0,0)

# Average annual cahneg analysis for comparison with other data.


frontposition$frontpostimevals <- frontposition$frontpostimevals/365.2425
frontposition <- frontposition %>%
  add_column(year = NA)
frontposition$year <- floor(frontposition$frontpostimevals)


Average_front_pos <- aggregate(frontposition$frontposvals, 
                               by=list(Category=frontposition$year), 
                               FUN=mean)

plot(Average_front_pos$Category, Average_front_pos$x)



setwd("D:/Backup/OneDrive - University of Exeter/year_4/Dissertation_data/Mass_height_velocity_stats")

Annual_Av <- read.csv("Dendrit_annual_averages.csv", header=TRUE,sep=",")


complete_data <- subset(Annual_Av, year >= 1985)

complete_data <- complete_data %>%
  add_column(frontpos = subset(Average_front_pos, Category > 1984 & Category < 2019)$x)


#testing relationship between velocity and mass balance
#does a change in velocity correspond with a chnage in mass balance


mbandvreg <- lm(mass_bal~Av_V, data=complete_data)
summary(mbandvreg)

mbandyrreg <- lm(mass_bal~year, data=complete_data)
summary(mbandyrreg)

vandyrreg <- lm(Av_V~year, data=complete_data)
summary(vandyrreg)

par(mfrow=c(2,2))
plot(complete_data$year, complete_data$mass_bal)
abline(lm(complete_data$mass_bal~complete_data$year))
plot(complete_data$year, complete_data$Av_V)
abline(lm(complete_data$Av_V~complete_data$year))
plot(complete_data$Av_V, complete_data$mass_bal)
abline(lm(complete_data$mass_bal~complete_data$Av_V))



# GGPLOT MASS BALANCE ANALYSIS------------------------------------------------------------------



textSize <- 12
pointsize <- 6


ggplot(Annual_Av, 
       aes(x = year,
           y = mass_bal,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-2,0) +
  xlim(1970,2020) +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  geom_errorbar(aes(ymin=mass_bal-mass_bal_err, ymax=mass_bal+mass_bal_err), width=1,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))+
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE) +
  annotate(geom="text",
           x=500,
           y=-0.3,
           label="p value = 0.001034\nAdjusted R-Squared value = 0.2672",
           color="black",
           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "Average annual velocity along central flowline (m/yr)",
       y = "Annual Mass balance (Gt/yr)") +
  geom_linerange(aes(xmin = Av_V - Av_V_err, xmax = Av_V + Av_V_err)) +
geom_hline(yintercept = 0)



# GGPLOT MASS CHANGE  -----------------------------------------------------


textSize <- 12
pointsize <- 6

ggplot(Annual_Av, 
       aes(x = year,
           y = mass_ch,
           color = year,
           fill=year)) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-20,0) +
  xlim(1970,2020) +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  geom_errorbar(aes(ymin=mass_ch-mass_ch_err, ymax=mass_ch+mass_ch_err), width=1,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))+
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE) +
  annotate(geom="text",
           x=500,
           y=-0.3,
           label="p value = 0.001034\nAdjusted R-Squared value = 0.2672",
           color="black",
           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "time (years)",
       y = "cumulative mass change from 1972 baseline (Gt)") +
  geom_linerange(aes(xmin = Av_V - Av_V_err, xmax = Av_V + Av_V_err))+
  geom_hline(yintercept = 0)



# GGPLOT MASS BALANCE AND MASS CHANGE TOGETHER ----------------------------



textSize <- 18
pointsize <- 6

g1 <- ggplot(Annual_Av, 
             aes(x = year,
                 y = mass_bal,
                 color = year,
                 fill=year)) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-2,.3) +
  xlim(1970,2020) +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  geom_errorbar(aes(ymin=mass_bal-mass_bal_err, ymax=mass_bal+mass_bal_err), width=1,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))+
  geom_smooth(method='lm',
              formula = y~x, 
              se=TRUE, na.rm = FALSE, 
              inherit.aes = TRUE) +
  annotate(geom="text",
           x=2015,
           y=-0.3,
           label="p value = 5.2*10^-9\nAdjusted R-Squared value = 0.5248",
           color="black",
           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "time (years)",
       y = "Annual mass balance\n(Gtyr^-1)") +
  geom_linerange(aes(xmin = Av_V - Av_V_err, xmax = Av_V + Av_V_err)) +
  geom_hline(yintercept = 0)


g2 <- ggplot(Annual_Av, 
             aes(x = year,
                 y = mass_ch,
                 color = year,
                 fill=year)) +
  geom_point(size=pointsize, alpha=0.9) +
  theme_classic() +
  ylim(-15,0) +
  xlim(1970,2020) +
  scale_fill_viridis_c(option = "D") +
  scale_color_viridis_c(option = "D") +
  geom_errorbar(aes(ymin=mass_ch-mass_ch_err, ymax=mass_ch+mass_ch_err), width=1,
                position=position_dodge(.9))  +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))+
#  geom_smooth(method='lm',
#              formula = y~x, 
#              se=TRUE, na.rm = FALSE, 
#              inherit.aes = TRUE) +
#  annotate(geom="text",
#           x=500,
#           y=-0.3,
#           label="p value = 0.001034\nAdjusted R-Squared value = 0.2672",
#           color="black",
#           size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "time (years)",
       y = "cumulative mass change from 1972\nbaseline (Gt)") +
  geom_linerange(aes(xmin = Av_V - Av_V_err, xmax = Av_V + Av_V_err))+
  geom_hline(yintercept = 0)


ggdraw() +
  draw_plot(g1, x=0, y=.5, width=1, height=.5) +
  draw_plot(g2, x=0, y=0, width=1,height=.5) +
 # draw_plot(g3, x=0, y=0, width=1, height=.5) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0), y = c(1, 0.5))

mbtimereg <- lm(Annual_Av$mass_bal~Annual_Av$year)
summary(mbtimereg)


# GGPLOT ALL 3 ------------------------------------------------------------

# plot v aginst yr mb against yr and v against mb

textSize <- 12
pointsize <- 4

g1 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = year,
                            y = mass_bal,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.5) +
                   theme_classic() +
                   ylim(-2,0) +
                   xlim(1985,2018) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin=mass_bal-mass_bal_err, ymax=mass_bal+mass_bal_err), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
                   geom_smooth(method='lm',
                               formula = y~x, 
                               se=TRUE, na.rm = FALSE, 
                               inherit.aes = TRUE) +
                   annotate(geom="text",
                            x=500,
                            y=-0.3,
                            label="p value = n\nAdjusted R-Squared value = n",
                            color="black",
                            size=2)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "time (years)",
                        y = "Annual Mass balance (Gt/yr)") 
)


g2 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = year,
                            y = Av_V,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.5) +
                   theme_classic() +
                   ylim(350,580) +
                   xlim(1985,2018) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin = Av_V - Av_V_err, ymax = Av_V + Av_V_err), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
                   geom_smooth(method='lm',
                               formula = y~x, 
                               se=TRUE, na.rm = FALSE, 
                               inherit.aes = TRUE) +
                   annotate(geom="text",
                            x=500,
                            y=-0.3,
                            label="p value = n\nAdjusted R-Squared value = n",
                            color="black",
                            size=6)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "time(years)",
                        y = "Average annual velocity along central flowline (m/yr)") 
)



g3 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = Av_V,
                            y = mass_bal,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.5) +
                   theme_classic() +
                   ylim(-2,0) +
                   xlim(350,580) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin=mass_bal-mass_bal_err, ymax=mass_bal+mass_bal_err), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
                   geom_smooth(method='lm',
                               formula = y~x, 
                               se=TRUE, na.rm = FALSE, 
                               inherit.aes = TRUE) +
                   annotate(geom="text",
                            x=500,
                            y=-0.3,
                            label="p value = 0.001034\nAdjusted R-Squared value = 0.2672",
                            color="black",
                            size=4)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "Average annual velocity along central flowline (m/yr)",
                        y = "Annual Mass balance (Gt/yr)") +
                   geom_linerange(aes(xmin = Av_V - Av_V_err, xmax = Av_V + Av_V_err))
)



# g <- rbind(g1, g2, g3, size = "first")
# 
# 
# 
# g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
# #g$heights <- unit.pmax(g1$heights, g2$heights, g3$heights)
# 
# grid.newpage()
# grid.draw(g)

ggdraw() +
  draw_plot(g1, x=0, y=.5, width=.5, height=.5) +
  draw_plot(g2, x=.5, y=.5, width=.5,height=.5) +
  draw_plot(g3, x=0, y=0, width=1, height=.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))


# GGPLOT VELOCITY MASS BALANCE FRONTAL POSITION ---------------------------


# need to run extract hdf5 file script first

textSize <- 18
pointsize <- 4

g1 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = year,
                            y = mass_bal,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.9) +
                   theme_classic() +
                   ylim(-2,0) +
                   xlim(1985,2020) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin=mass_bal-mass_bal_err, ymax=mass_bal+mass_bal_err), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
                   geom_smooth(method='lm',
                               formula = y~x, 
                               se=TRUE, na.rm = FALSE, 
                               inherit.aes = TRUE) +
                   annotate(geom="text",
                            x=500,
                            y=-0.3,
                            label="p value = n\nAdjusted R-Squared value = n",
                            color="black",
                            size=2)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "time (years)",
                        y = "Annual Mass balance\n(Gt/yr)") +
                   geom_vline( xintercept = 2011, alpha = 0.5)
)


g2 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = year,
                            y = Av_V,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.9) +
                   theme_classic() +
                   ylim(300,580) +
                   xlim(1985,2020) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin = Av_V - Av_V_err, ymax = Av_V + Av_V_err), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
                   geom_smooth(method='lm',
                               formula = y~x, 
                               se=TRUE, na.rm = FALSE, 
                               inherit.aes = TRUE) +
                   annotate(geom="text",
                            x=500,
                            y=-0.3,
                            label="p value = n\nAdjusted R-Squared value = n",
                            color="black",
                            size=6)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "time(years)",
                        y = "Avg annual velocity\n(m/yr)") +
                   geom_vline( xintercept = 2011, alpha = 0.5)
)

g3 <- ggplot(complete_data, 
             aes(x = year,
                 y = frontpos,
                 color = year,
                 fill = year)) +
  geom_point(size=pointsize, alpha = 0.9) +
  theme_classic() +
  ylim(-1500,500) +
  xlim(1985,2020)+
  scale_fill_viridis_c(option = "magma", name = "year") +
  scale_color_viridis_c(option = "magma", name = "year") +
  geom_errorbar(aes(ymin=frontpos-100, ymax=frontpos+100), width=.3,
                  position=position_dodge(.9)) +
  theme(panel.background = element_rect(fill = "gray",
                                        colour = "gray",
                                        size = 0.5, linetype = "solid")) +
  geom_hline(yintercept=0)+
   geom_smooth(method='lm',
               formula = y~x, 
               se=TRUE, na.rm = FALSE, 
               inherit.aes = TRUE) +
  # annotate(geom="text",
  #          x=5990,
  #          y=800,
  #          label="p value = n\nAdjusted R-Squared value = n",
  #          color=cbbPalette[1],
  #          size=6)+
  theme (text = element_text(size = textSize)) +
  labs(x = "time (years)",
       y = "Retreat from 1985\n(m)") +
  geom_vline( xintercept = 2011, alpha = 0.5)
#  geom_point(aes(x=distance, y = b)) #+
# geom_hline(aes(x=distance, y=b), distance[which(distance ==min(b))])


  



ggdraw() +
  draw_plot(g1, x=0, y=0, width=1, height=1/3) +
  draw_plot(g2, x=0, y=1/3, width=1,height=1/3) +
  draw_plot(g3, x=0, y=2/3, width=1, height=1/3) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0, 0), y = c(1, 2/3,1/3 ))



# GGPLOT VELOCITY AND FRONTAL POSITION COMPARISON -------------------------


textSize <- 14
pointsize <- 6

g1 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = year,
                            y = Av_V,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.9) +
                   theme_classic() +
                   ylim(300,600) +
                   xlim(1985,2020) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin=Av_V-Av_V_err, ymax=Av_V+Av_V_err), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
#                   geom_smooth(method='lm',
#                               formula = y~x, 
#                               se=TRUE, na.rm = FALSE, 
#                               inherit.aes = TRUE) +
#                   annotate(geom="text",
#                            x=500,
#                            y=-0.3,
#                            label="p value = n\nAdjusted R-Squared value = n",
#                            color="black",
#                            size=2)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "time (years)",
                        y = "Avg Annual Velocity (myr^-1)") 
)


g2 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = year,
                            y = frontpos,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.9) +
                   theme_classic() +
                   ylim(-1300,200) +
                   xlim(1985,2018) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin = frontpos - 100, ymax = frontpos + 100), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
#                   geom_smooth(method='lm',
#                               formula = y~x, 
#                               se=TRUE, na.rm = FALSE, 
#                               inherit.aes = TRUE) +
#                   annotate(geom="text",
#                            x=500,
#                            y=-0.3,
#                            label="p value = n\nAdjusted R-Squared value = n",
#                            color="black",
#                            size=6)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "time(years)",
                        y = "Annual frontal psoiton change from 1985 (m)") 
)



g3 <- ggplotGrob(ggplot(complete_data, 
                        aes(x = Av_V,
                            y = frontpos,
                            color = year,
                            fill=year)) +
                   geom_point(size=pointsize, alpha=0.9) +
                   theme_classic() +
                   ylim(-1300,200) +
                   xlim(350,580) +
                   scale_fill_viridis_c(option = "magma") +
                   scale_color_viridis_c(option = "magma") +
                   geom_errorbar(aes(ymin = frontpos - 100, ymax = frontpos + 100), width=1,
                                 position=position_dodge(.9))  +
                   theme(panel.background = element_rect(fill = "lightgray",
                                                         colour = "lightgray",
                                                         size = 0.5, linetype = "solid"))+
                   geom_smooth(method='lm',
                               formula = y~x, 
                               se=TRUE, na.rm = FALSE, 
                               inherit.aes = TRUE) +
                   annotate(geom="text",
                            x=525,
                            y=-0.3,
                            label="p value = 4*10^-9\nAdjusted R-Squared value = 0.66",
                            color="black",
                            size=5)+
                   theme (text = element_text(size = textSize)) +
                   labs(x = "Average annual velocity along central flowline (m/yr)",
                        y = "Annual frontal position from 1985") +
                   geom_linerange(aes(xmin = Av_V - Av_V_err, xmax = Av_V + Av_V_err))
)




ggdraw() +
  draw_plot(g1, x=0, y=.5, width=.5, height=.5) +
  draw_plot(g2, x=.5, y=.5, width=.5,height=.5) +
  draw_plot(g3, x=0, y=0, width=1, height=.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))



frontposvelreg <- lm(complete_data$frontpos~complete_data$Av_V)
summary(frontposvelreg)



