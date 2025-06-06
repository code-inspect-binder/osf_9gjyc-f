library(tidyverse)
library(brms)
library(ggdistribute)
library(bayesplot)
library(tidybayes)

          ## PILOT EXPERIMENT ## 

DF0 <- read.csv("Data.Exp0.csv", header = T)
summary(DF0)
DF0$c_trial = DF0$trial - 1  

# brms
M0  <- brm(sel.adv ~ 0 + c_trial + (1|id),
           data = DF0, 
           family = "bernoulli",
           iter = 6000,
           chains = 3, 
           cores = 3,
           save_all_pars = TRUE,
           file = "M0")

summary(M0)
plot(M0, ask = FALSE)
pp_check(M0, check = "distributions")
pp_check(M0, check = "residuals")
pp_check(M0, "error_scatter_avg")
pp_check(M0, check = "scatter")
plot(conditional_effects(M0), points = TRUE)


          ## EXPERIMENT 1 ## 

DF1 <- read.csv("Data.Exp1.csv", header = T)
summary(DF1)
DF1$treatment <- as.factor(DF1$treatment)
DF1$c_trial = DF1$trial - 1  

# brms
M1 <- brm(sel.adv ~ 0 + c_trial:treatment + (1|id),
                data = DF1, 
                family = "bernoulli",
                iter = 6000,
                chains = 3,
                cores = 3,
                save_all_pars = TRUE,
                file = "M1")

summary(M1)
plot(M1, ask = FALSE)
pp_check(M1, check = "distributions")
pp_check(M1, check = "residuals")
pp_check(M1, "error_scatter_avg")
pp_check(M1, check = "scatter")
plot(conditional_effects(M1), points = TRUE)



          ## EXPERIMENT 2 ## 

DF2 <- read.csv("Data.Exp2.csv", header = T)
summary(DF2)
DF2$treatment <- as.factor(DF2$treatment)
DF2$c_trial = DF2$trial - 1  

# brms
M2 <- brm(sel.adv ~ 0 + c_trial:treatment + (1|id),
          data = DF2, 
          family = "bernoulli",
          iter = 6000,
          chains = 3,
          cores = 3,
          save_all_pars = TRUE,
          file = "M2")

summary(M2)
plot(M2, ask = FALSE)
pp_check(M2, check = "distributions")
pp_check(M2, check = "residuals")
pp_check(M2, "error_scatter_avg")
pp_check(M2, check = "scatter")
plot(conditional_effects(M2), points = TRUE)



          ## EXPERIMENT 3 ## 

DF3 <- read.csv("Data.Exp3.csv", header = T)
summary(DF3)
DF3$c_trial = DF3$trial - 1  

# brms
M3 <- brm(sel.adv ~ 0 + c_trial + (1|id),
          data = DF3, 
          family = "bernoulli",
          iter = 6000,
          chains = 3,
          cores = 3,
          save_all_pars = TRUE,
          file = "M3")

summary(M3)
plot(M3, ask = FALSE)
pp_check(M3, check = "distributions")
pp_check(M3, check = "residuals")
pp_check(M3, "error_scatter_avg")
pp_check(M3, check = "scatter")
plot(conditional_effects(M3), points = TRUE)



          ## EXPERIMENT 4 ##

DF4 <- read.csv("Data.Exp4.csv", header = T)
summary(DF4)
DF4$treatment <- as.factor(DF4$treatment)
DF4$c_trial = DF4$trial - 1  

# brms
M4 <- brm(sel.adv ~ 0 + c_trial:treatment + (1|id),
          data = DF4, 
          family = "bernoulli",
          iter = 6000,
          chains = 3,
          cores = 3,
          save_all_pars = TRUE,
          file = "M4")

summary(M4)
plot(M4, ask = FALSE)
pp_check(M4, check = "distributions")
pp_check(M4, check = "residuals")
pp_check(M4, "error_scatter_avg")
pp_check(M4, check = "scatter")
plot(conditional_effects(M4), points = TRUE)



          ## EXPERIMENT 5 ## 

DF5 <- read.csv("Data.Exp5.csv", header = T)
summary(DF5)
DF5$treatment <- as.factor(DF5$treatment)
DF5$c_trial = DF5$trial - 1  

# brms
M5 <- brm(sel.adv ~ 0 + c_trial:treatment + (1|id),
          data = DF5, 
          family = "bernoulli",
          iter = 6000,
          chains = 3,
          cores = 3,
          save_all_pars = TRUE,
          file = "M5")

summary(M5)
plot(M5, ask = FALSE)
pp_check(M5, check = "distributions")
pp_check(M5, check = "residuals")
pp_check(M5, "error_scatter_avg")
pp_check(M5, check = "scatter")
plot(conditional_effects(M5), points = TRUE)



          ## EXPERIMENT 6 ## 

DF6 <- read.csv("Data.Exp6.csv", header = T)
summary(DF6)
DF6$treatment <- as.factor(DF6$treatment)
DF6$c_trial = DF6$trial - 1  

# brms
M6 <- brm(sel.adv ~ 0 + c_trial:treatment + (1|id),
          data = DF6, 
          family = "bernoulli",
          iter = 6000,
          chains = 3,
          cores = 3,
          save_all_pars = TRUE,
          file = "M6")

summary(M6)
plot(M6, ask = FALSE)
pp_check(M6, check = "distributions")
pp_check(M6, check = "residuals")
pp_check(M6, "error_scatter_avg")
pp_check(M6, check = "scatter")
plot(conditional_effects(M6), points = TRUE)






          ## FIGURES SHOWING POSTERIOR PREDICTIONS ACROSS ROUNDS PER EXPERIMENT ##

library(ggeffects)
library(ggplot2)
library(cowplot)

# Function for showing axis ticks
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

library(stringr)
library(pkgbuild)
library(devtools)
library(RColorBrewer)
library(wesanderson)
library(ggthemes) 
library(ggpubr)


          ## PILOT EXPERIMENT ## 

P0 <- ggpredict(M0, terms = c("c_trial [all]"))
P0$x <- P0$x + 1
P0$Evidence <- P0$group

Fig0 <- P0 %>% ggplot(aes(x = x, y = predicted, colour = Evidence)) +  geom_line(size=1.4) +
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Evidence, colour = Evidence), alpha = 0.05, size = 0.001) +
                scale_color_manual(values=c("#D53E4F")) +
                scale_fill_manual(values=c("#D53E4F")) +
                ggtitle("Pilot \n") +
                theme(plot.title = element_text(hjust = 0.5)) +
                scale_y_continuous(name = "Probability selecting SA", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
                scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                geom_hline(yintercept=0.5, linetype="dashed") +
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                theme(legend.position = "none" )
Fig0


          ## EXPERIMENT 1 ## 

P1 <- ggpredict(M1, terms = c("c_trial [all]","treatment"))
P1$x <- P1$x + 1
P1$Evidence <- P1$group

Fig1 <- P1 %>% ggplot(aes(x = x, y = predicted, colour = Evidence)) +  geom_line(size=1.4) +
              geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Evidence, colour = Evidence), alpha = 0.05, size = 0.001) +
              scale_color_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD")) +
              scale_fill_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD")) +
              ggtitle("Exp 1 \n With incentives") +
              theme(plot.title = element_text(hjust = 0.5)) +
              scale_y_continuous(name = "", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
              scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
              geom_hline(yintercept=0.5, linetype="dashed") +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
              theme(legend.position = "none" )
Fig1


          ## EXPERIMENT 2 ## 

P2 <- ggpredict(M2, terms = c("c_trial [all]","treatment"))
P2$x <- P2$x + 1
P2$Evidence <- P2$group

Fig2 <- P2 %>% ggplot(aes(x = x, y = predicted, colour = Evidence)) +  geom_line(size=1.4) +
              geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Evidence, colour = Evidence), alpha = 0.05, size = 0.001) +
              scale_color_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD")) +
              scale_fill_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD")) +
              ggtitle("Exp 2 \n No incentives") +
              theme(plot.title = element_text(hjust = 0.5)) +
              scale_y_continuous(name = "", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
              scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
              geom_hline(yintercept=0.5, linetype="dashed") +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.position = "none" )
Fig2


          ## EXPERIMENT 3 ##

P3 <- ggpredict(M3, terms = c("c_trial [all]"))
P3$x <- P3$x + 1
P3$Evidence <- P3$group

Fig3 <- P3 %>% ggplot(aes(x = x, y = predicted, colour = Evidence)) +  geom_line(size=1.4) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Evidence, colour = Evidence), alpha = 0.05, size = 0.001) +
                  scale_color_manual(values=c("#D53E4F")) +
                  scale_fill_manual(values=c("#D53E4F")) +
                  ggtitle("Exp 3 \n Maximum uncertainty") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(name = "", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
                  scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                  geom_hline(yintercept=0.5, linetype="dashed") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                  theme(legend.position = "none" )
Fig3


          ## EXPERIMENT 4 ##

P4 <- ggpredict(M4, terms = c("c_trial [all]","treatment"))
P4$x <- P4$x + 1
P4$Treatment <- P4$group

Fig4 <- P4 %>% ggplot(aes(x = x, y = predicted, colour = Treatment)) +  geom_line(size=1.4) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Treatment, colour = Treatment), alpha = 0.05, size = 0.001) +
                  scale_color_manual(values=c("#D53E4F", "#008080", "#0f2e91")) +
                  scale_fill_manual(values=c("#D53E4F", "#008080", "#0f2e91")) +
                  ggtitle("Exp 4 \n Majority vote - Lab") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(name = "Probability selecting SA", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
                  scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                  geom_hline(yintercept=0.5, linetype="dashed") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                  theme(legend.position = "none" )
Fig4


          ## EXPERIMENT 5 ##

P5 <- ggpredict(M5, terms = c("c_trial [all]","treatment"))
P5$x <- P5$x + 1
P5$Treatment <- P5$group

Fig5 <- P5 %>% ggplot(aes(x = x, y = predicted, colour = Treatment)) +  geom_line(size=1.4) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Treatment, colour = Treatment), alpha = 0.05, size = 0.001) +
                  scale_color_manual(values=c("#D53E4F", "#008080", "#0f2e91")) +
                  scale_fill_manual(values=c("#D53E4F", "#008080", "#0f2e91")) +
                  ggtitle("Exp 5 \n Majority vote - Online") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(name = "", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
                  scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                  geom_hline(yintercept=0.5, linetype="dashed") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                  theme(legend.position = "none" )
Fig5


        ## EXPERIMENT 6 ##

P6 <- ggpredict(M6, terms = c("c_trial [all]", "treatment"))
P6$x <- P6$x + 1
P6$Treatment <- P6$group

Fig6 <- P6 %>% ggplot(aes(x = x, y = predicted, colour = Treatment)) +  geom_line(size=1.4) +
                  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  Treatment, colour = Treatment), alpha = 0.05, size = 0.001) +
                  scale_color_manual(values=c("#CF11D2","#D53E4F")) +
                  scale_fill_manual(values=c("#CF11D2","#D53E4F")) +
                  ggtitle("Exp 6 \n Communicating dyads") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_y_continuous(name = "", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
                  scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                  geom_hline(yintercept=0.5, linetype="dashed") +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                  theme(legend.position = "none" )

Fig6


          ## LEGEND ##

DF.leg <- read.csv("ForLegend.csv", header = T)
DF.leg$Treatment <- factor(DF.leg$Treatment, levels = c("Individuals, evidence 1", "Individuals, evidence 2", "Individuals, evidence 3", 
                                                        "Individuals, evidence 4", "Individuals in voting groups", "Majority vote", "Dyads"))

Fig.legend <- DF.leg %>% ggplot(aes(x = x, y = y, colour = Treatment)) +  geom_line(size=0.001) +
                            scale_color_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD", "#008080", "#0f2e91", "#CF11D2")) +
                            theme_void() +
                            theme(legend.text = element_text(size = 10), 
                                  legend.direction = "vertical",
                                  legend.position = c(0.55, 0.7)) +
                            guides(colour = guide_legend(override.aes = list(size=1)))

Fig.legend


          ## COMBINE FIGURES ##

FigA <- plot_grid(Fig0, Fig1, Fig2, Fig3, labels = c("a", "b", "c", "d"), nrow = 1)
FigB <- plot_grid(Fig4, Fig5, Fig6, Fig.legend, labels = c("e", "f", "g",""), nrow = 1)
FigC <- plot_grid(FigA, NULL, FigB, rel_heights = c(1, 0.05, 1), ncol = 1)
save_plot("Figure 3.png", FigC, base_height = 8, base_width = 10.5)
