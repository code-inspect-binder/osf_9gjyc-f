library(tidyverse)
library(ggplot2)
library(cowplot)

## FIGURE OBSERVED CHANGING PROBABILITIES GROUP SIZE 1 ##
pchange <- read.csv("p_switch_ind_maj.csv", header = TRUE)
pchange
to_string <- as_labeller(c(`0` = "Lost", `1` = "Won"))
pchange$Oppose <- ifelse(pchange$Conf.unsel.adv > 0.5, "Confirm" , "Oppose")
pchange_gs1 <- subset(pchange, Group.size == 1) 

Fig1 <- pchange_gs1 %>% ggplot(aes(x = Conf.unsel.adv, y = Changed, label = Freq, fill = Oppose)) + 
  facet_grid(~ Reward, labeller = to_string) +
  geom_bar(stat="identity") + 
  scale_y_continuous(name = "Probability changing advisor", limits=c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_discrete(name = "Confidence igored advisor", limits = c(-4, -3, -2, 1, 2, 3, 4, 5)) +
  geom_vline(xintercept = 0, linetype="dashed", size = 0.2) +
  geom_text(size = 2.3, position = position_stack(vjust = 1.0)) +
  theme(legend.position = c(0.85, 0.7),
        legend.title = element_blank()) +
  scale_fill_manual("legend", values = c("Confirm" = "deepskyblue", "Oppose" = "brown1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

Fig1 <- ggdraw(Fig1) + draw_label("Observed probability of changing advisor \n \n             Individuals", x = 0.37, y = 0.75, vjust = 0, hjust = 0.25, fontface = "bold", size = 10)
Fig1

## FIGURE EXPECTED CHANGING PROBABILITIES GROUP SIZE 5 ##
pchange_gs5 <- subset(pchange, Group.size == 5) 

Fig2 <- pchange_gs5 %>% ggplot(aes(x = Conf.unsel.adv, y = Changed, fill = Oppose)) + 
  facet_grid(~ Reward, labeller = to_string) +
  geom_bar(stat="identity") + 
  scale_y_continuous(name = "Probability changing advisor", limits=c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_discrete(name = "Confidence igored advisor", limits = c(-4, -3, -2, 1, 2, 3, 4, 5)) +
  geom_vline(xintercept = 0, linetype="dashed", size =0.2)+
  theme(legend.position = "none") +
  scale_fill_manual("legend", values = c("Confirm" = "deepskyblue", "Oppose" = "brown1")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

Fig2 <- ggdraw(Fig2) + draw_label("Expected probability of changing advisor \n \n             Group size 5", x = 0.37, y = 0.75, vjust = 0, hjust = 0.25, fontface = "bold", size = 10)
Fig2


### FIGURE SIMULATIONS GROUP SIZE 1 ###
DF <- read.table("RunS majority vote.txt",header=F)
colnames(DF) <- c("Dummy", "Reward", "SelAdvisor", "Round", "GroupSize")
summary(DF)

DF1 <- DF %>%
  group_by(Round, GroupSize) %>%
  count(SelAdvisor)

DF2 <- spread(DF1, SelAdvisor, n)
DF2

DF2$P_HA <- DF2$SA / (DF2$SA + DF2$HA)

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

DF2_gs1 <- subset(DF2, GroupSize == 1) 

Fig3 <- DF2_gs1 %>% ggplot(aes(x = Round, y = P_HA)) + geom_point(color='black') + geom_line(color='black') +
                       scale_y_continuous(name = "Probability selecting SA", limits=c(0, 1), breaks = seq(0, 1, 0.2)) +
                       scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                       geom_hline(yintercept=0.5, linetype = "dashed") +
                       theme(legend.position=c(0.8,0.85))  +
                       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                             panel.background = element_blank(), axis.line = element_line(colour = "black"))

Fig3 <- ggdraw(Fig3) + draw_label("Simulated probability of selecting SA \n \n             Individuals", x = 0.37, y = 0.8, vjust = 0, hjust = 0.25, fontface = "bold", size = 10)
Fig3


### FIGURE SIMULATIONS GROUP SIZE 5 ###
DF2_gs5 <- subset(DF2, GroupSize == 5) 

Fig4 <- DF2_gs5 %>% ggplot(aes(x = Round, y = P_HA)) + geom_point(color='black') + geom_line(color='black') +
                  scale_y_continuous(name = "Probability selecting SA", limits=c(0, 1), breaks = seq(0, 1, 0.2)) +
                  scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
                  geom_hline(yintercept=0.5, linetype = "dashed") +
                  theme(legend.position=c(0.8,0.85))  +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))

Fig4 <- ggdraw(Fig4) + draw_label("Simulated probability of selecting SA \n \n               Group size 5", x = 0.37, y = 0.8, vjust = 0, hjust = 0.25, fontface = "bold", size = 10)
Fig4

# COMPLETE FIGURE
Fig <- plot_grid(Fig1, Fig2, Fig3, Fig4, labels = c("a", "b", "c","d"), nrow = 2, ncol = 2, align = "hv")
Fig

# save plot
save_plot("Fig Predictions Majority vote.png", Fig, base_height = 7, base_width = 7)