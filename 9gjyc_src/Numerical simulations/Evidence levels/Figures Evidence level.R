library(tidyverse)
library(ggplot2)
library(cowplot)

### FIGURE SWITCHING PROBABILITIES PILOT ###

# Read csv file
DF <- read.csv(file = "p_switch_ind.csv", header = TRUE)
summary(DF)

to_string <- as_labeller(c(`0` = "Lost", `1` = "Won"))
DF$Oppose <- ifelse(DF$Conf.unsel.adv > 0.5, "Confirm" , "Oppose")

Fig_Pilot <- DF %>% ggplot(aes(x = Conf.unsel.adv, y = Changed, label = Freq, fill = Oppose)) + 
              facet_grid(~ Reward, labeller = to_string) +
              geom_bar(stat="identity") + 
              scale_y_continuous(name = "Probability changing advisor", limits=c(0, 1), breaks = seq(0, 1, 0.2)) +
              scale_x_discrete(name = "Confidence ignored advisor", limits = c(-4, -3, -2, 1, 2, 3, 4, 5)) +
              geom_vline(xintercept = 0, linetype="dashed") +
              geom_text(size = 2.5, position = position_stack(vjust = 1.0))  +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              scale_fill_manual("legend", values = c("Confirm" = "deepskyblue", "Oppose" = "brown1")) +
              theme(legend.position = c(0.475, 0.85),
                    legend.title = element_blank(),
                    #legend.text = element_text(color = "red", size = 1),
                    legend.direction = "vertical")
Fig_Pilot


### FIGURE PREDICTIONS SIMULATIONS ###

DF1 <-read.table("Runs evidence level.txt",header=F)
colnames(DF1) <- c("Dummy", "Reward", "Selected_Advisor", "Difficulty","Round")
summary(DF1)

DF1 <- DF1 %>%
  mutate(Difficulty = replace(Difficulty, Difficulty == 50, 1)) %>%
  mutate(Difficulty = replace(Difficulty, Difficulty == 60, 2)) %>%
  mutate(Difficulty = replace(Difficulty, Difficulty == 70, 3)) %>%
  mutate(Difficulty = replace(Difficulty, Difficulty == 80, 4))

# Determine how often both advisors were chosen per round
DF2 <- DF1 %>%
          group_by(Difficulty, Round) %>%
          count(Selected_Advisor)

DF2 <- spread(DF2, Selected_Advisor, n)

DF2$P_HA <- DF2$SA / (DF2$SA + DF2$HA)

DF2$Evidence <- as.factor(DF2$Difficulty)

# Function for axis ticks
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

Fig_Runs <- DF2 %>% ggplot(aes(x = Round, y = P_HA, fill = Evidence, colour = Evidence)) + 
  geom_point(aes(color = Evidence)) +
  geom_line(aes(color = Evidence)) +
  scale_color_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD")) +
  scale_fill_manual(values=c("#D53E4F","#FDAE61", "#66C2A5", "#3288BD")) +
  scale_y_continuous(name = "Predicted probability selecting SA", limits=c(0.2, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(name = "Round", breaks = seq(1,20,1), labels = every_nth(seq(1,20,1), 2, inverse = TRUE)) +
  geom_hline(yintercept=0.5, linetype = "dashed") +
  theme(legend.position=c(0.5,0.85), legend.direction = "horizontal") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

Fig_Runs

# Combine figs
Fig <- plot_grid(Fig_Pilot, Fig_Runs, labels = c("a", "b"), nrow = 1, ncol = 2)
Fig

# SAVE PLOT
save_plot("Fig Predictions Evidence level.png", Fig, base_height = 4, base_width = 8)
