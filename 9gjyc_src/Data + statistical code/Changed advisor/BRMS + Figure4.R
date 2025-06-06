library(tidyverse)
library(brms)
library(tidybayes)

## COMBINE ALL DATA IN ONE DATA FRAME ##
## BECAUSE THESE ANALYSES ARE DONE AT THE LEVEL OF TREATMENT, AND NOT EXPERIMENT ##

          ## PILOT ##

DF0 <- read.csv("Data.Exp0.csv",header=T)
DF0

# rename and select relevant columns
DF0 <- DF0 %>%
        rename (evidence = treatment) %>%
        rename (confirm = confirm.oppose) %>%
        mutate(group.id = id) %>%
        mutate(treatment = "Individuals") %>%
        mutate(experiment = "0") %>%
        mutate(majmin = "NA") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF0

          ## EXPERIMENT 1 ##

DF1 <- read.csv("Data.Exp1.csv",header=T)
DF1

# rename and select relevant columns
DF1 <- DF1 %>%
        rename (evidence = treatment) %>%
        rename (confirm = confirm.oppose) %>%
        mutate(group.id = id) %>%
        mutate(treatment = "Individuals") %>%
        mutate(experiment = "1") %>%
        mutate(majmin = "NA") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF1

          ## EXPERIMENT 2 ##

DF2 <- read.csv("Data.Exp2.csv",header=T)
DF2

# rename and select relevant columns
DF2 <- DF2 %>%
        rename (evidence = treatment) %>%
        rename (confirm = confirm.oppose) %>%
        mutate(group.id = id) %>%
        mutate(treatment = "Individuals") %>%
        mutate(experiment = "2") %>%
        mutate(majmin = "NA") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF2

          ## EXPERIMENT 3 ##

DF3 <- read.csv("Data.Exp3.csv",header=T)
DF3

# rename and select relevant columns
DF3 <- DF3 %>%
        rename (evidence = treatment) %>%
        rename (confirm = confirm.oppose) %>%
        mutate(group.id = id) %>%
        mutate(treatment = "Individuals") %>%
        mutate(experiment = "3") %>%
        mutate(majmin = "NA") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF3

          ## EXPERIMENT 4 ##

DF4 <- read.csv("Data.Exp4.csv",header=T)
DF4

# rename and select relevant columns
DF4 <- DF4 %>%
        rename (confirm = confirm.oppose) %>%
        mutate(experiment = "4") %>%
        mutate(evidence = "1") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF4

          ## EXPERIMENT 5 ##

DF5 <- read.csv("Data.Exp5.csv",header=T)
summary(DF5)

# rename and select relevant columns
DF5 <- DF5 %>%
        rename (confirm = confirm.oppose) %>%
        mutate(experiment = "5") %>%
        mutate(evidence = "1") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF5

          ## EXPERIMENT 6 ##

DF6 <- read.csv("Data.Exp6.csv",header=T)
summary(DF6)

# rename and select relevant columns
DF6 <- DF6 %>%
        rename (confirm = confirm.oppose) %>%
        mutate(group.id = id) %>%
        mutate(evidence = "1") %>%
        mutate(experiment = "6") %>%
        mutate(majmin = "NA") %>%
        select(id, group.id, experiment, treatment, evidence, trial, confirm, won, changed, majmin) 
DF6

# Combine DATAFRAMES
DF <- rbind(DF0, DF1, DF2, DF3, DF4, DF5, DF6)
summary(DF)

# Recode confirm(oppose/confirm) and won(win/lost) to factors
DF$confirm <- recode(DF$confirm, `0` = "Confirmed", `1` = "Opposed")
DF$won <- recode(DF$won, `0` = "Lost", `1` = "Won")

DF <- DF %>%
        mutate(confirm = as_factor(confirm)) %>%
        mutate(treatment = as_factor(treatment)) %>%
        mutate(won = as_factor(won)) %>%
        mutate(majmin = as_factor(majmin)) %>%
        mutate(experiment = as_factor(experiment)) %>%
        mutate(evidence = as_factor(evidence))

summary(DF)



          ## BRMS ##

          ## INDIVIDUALS ## EVIDENCE 1 ##

Ind_E1 <- DF %>%
          filter(treatment == "Individuals") %>%
          filter(evidence == "1") %>%
          # remove experiment 6 as these are the same single individuals as experiment 4
          filter(!experiment == "6") %>% 
          # remove trial 20. In this last round individuals could not change anymore
          filter(!trial == 20) %>% 
          # create unique id number for each unique individual
          mutate(uniq.id = group_indices(., id, experiment)) %>% 
          mutate(uniq.id = as_factor(uniq.id))

summary(Ind_E1)

M_Ind_E1 <- brm(changed ~ won*confirm + trial + (1|uniq.id),
                data = Ind_E1, 
                family = "bernoulli",
                iter = 6000,
                chains = 3,
                cores = 3,
                save_all_pars = TRUE,
                file = "Ind_E1")

summary(M_Ind_E1)
plot(M_Ind_E1, ask = FALSE)
pp_check(M_Ind_E1, check = "distributions")
pp_check(M_Ind_E1, check = "residuals")
pp_check(M_Ind_E1, "error_scatter_avg")
pp_check(M_Ind_E1, check = "scatter")



          ## INDIVIDUALS ## EVIDENCE 2 ##

Ind_E2 <- DF %>%
            filter(treatment == "Individuals") %>%
            filter(evidence == "2") %>% 
            # remove trial 20. In this last round individuals could not change anymore
            filter(!trial == 20) %>% 
            # create unique number for each unique individual
            mutate(uniq.id = group_indices(., id, experiment)) %>% 
            mutate(uniq.id = as_factor(uniq.id))

summary(Ind_E2)

M_Ind_E2 <- brm(changed ~ won*confirm + trial + (1|uniq.id),
                  data = Ind_E2, 
                  family = "bernoulli",
                  iter = 6000,
                  chains = 3,
                  cores = 3,
                  save_all_pars = TRUE,
                  file = "Ind_E2")

summary(M_Ind_E2)
plot(M_Ind_E2, ask = FALSE)
pp_check(M_Ind_E2, check = "distributions")
pp_check(M_Ind_E2, check = "residuals")
pp_check(M_Ind_E2, "error_scatter_avg")
pp_check(M_Ind_E2, check = "scatter")


          ## INDIVIDUALS ## EVIDENCE 3 ##

Ind_E3 <- DF %>% 
            filter(treatment == "Individuals") %>%
            filter(evidence == "3") %>% 
            # remove trial 20. In this last round individuals could not change anymore
            filter(!trial == 20) %>% 
            # create unique number for each unique individual
            mutate(uniq.id = group_indices(., id, experiment)) %>% 
            mutate(uniq.id = as_factor(uniq.id))

summary(Ind_E3)

M_Ind_E3 <- brm(changed ~ won*confirm + trial + (1|uniq.id),
                  data = Ind_E3, 
                  family = "bernoulli",
                  iter = 6000,
                  chains = 3,
                  cores = 3,
                  save_all_pars = TRUE,
                  file = "Ind_E3")

summary(M_Ind_E3)
plot(M_Ind_E3, ask = FALSE)
pp_check(M_Ind_E3, check = "distributions")
pp_check(M_Ind_E3, check = "residuals")
pp_check(M_Ind_E3, "error_scatter_avg")
pp_check(M_Ind_E3, check = "scatter")


          ## INDIVIDUALS ## EVIDENCE 4 ##

Ind_E4 <- DF %>%
          filter(treatment == "Individuals") %>%
          filter(evidence == "4") %>% 
          # remove trial 20. In this last round individuals could not change anymore
          filter(!trial == 20) %>% 
          # create unique number for each unique individual
          mutate(uniq.id = group_indices(., id, experiment)) %>% 
          mutate(uniq.id = as_factor(uniq.id))

summary(Ind_E4)

# This model only contains 'won', as this treatment only had 7 instances of opposing advice (i.e. too few to reliably estimate in a model)
M_Ind_E4 <- brm(changed ~ won + trial + (1|uniq.id),
                          data = Ind_E4, 
                          family = "bernoulli",
                          iter = 6000,
                          chains = 3,
                          cores = 3,
                          save_all_pars = TRUE,
                          file = "Ind_E4")

summary(M_Ind_E4)
plot(M_Ind_E4, ask = FALSE)
pp_check(M_Ind_E4, check = "distributions")
pp_check(M_Ind_E4, check = "residuals")
pp_check(M_Ind_E4, "error_scatter_avg")
pp_check(M_Ind_E4, check = "scatter")


          ## INDIVIDUALS IN VOTING GROUPS ## IN MAJORITY ##

Grp_Maj <- DF %>%
              # remove trial 20. In this last round individuals could not change anymore            
              filter(!trial == 20) %>% 
              filter(treatment == "Individuals in voting groups") %>%
              filter(majmin == "majority") %>% 
              # create unique number for each unique individual
              mutate(uniq.id = group_indices(., id, experiment)) %>% 
              mutate(uniq.id = as_factor(uniq.id)) %>% 
              mutate(group.id = as_factor(group.id))

# Remove 14 cases with missing values (individuals in the groups that dropped out next round, hence missing value for changed)
Grp_Maj <- Grp_Maj[complete.cases(Grp_Maj), ]
summary(Grp_Maj)

M_Grp_Maj <- brm(changed ~ won*confirm + trial + (1|group.id/uniq.id),
                  data = Grp_Maj, 
                  family = "bernoulli",
                  iter = 6000,
                  chains = 3,
                  cores = 3,
                  save_all_pars = TRUE,
                  # Increase adapt_delta to avoid divergent transitions after warmup.
                  control = list(adapt_delta = 0.9),
                  file = "Grp_Maj")

summary(M_Grp_Maj)
plot(M_Grp_Maj, ask = FALSE)
pp_check(M_Grp_Maj, check = "distributions")
pp_check(M_Grp_Maj, check = "residuals")
pp_check(M_Grp_Maj, "error_scatter_avg")
pp_check(M_Grp_Maj, check = "scatter")


          ## INDIVIDUALS IN VOTING GROUPS ## IN MINORITY ##

Grp_Min <- DF %>%
            # remove trial 20. In this last round individuals could not change anymore            
            filter(!trial == 20) %>% 
            filter(treatment == "Individuals in voting groups") %>%
            filter(majmin == "minority") %>% 
            # create unique number for each unique individual
            mutate(uniq.id = group_indices(., id, experiment)) %>% 
            mutate(uniq.id = as_factor(uniq.id)) %>% 
            mutate(group.id = as_factor(group.id))

# Remove 12 cases with missing values (individuals in the groups that dropped out next round, hence missing value for changed)
Grp_Min <- Grp_Min[complete.cases(Grp_Min), ]
summary(Grp_Min)

M_Grp_Min <- brm(changed ~ won*confirm + trial + (1|group.id/uniq.id),
                 data = Grp_Min, 
                 family = "bernoulli",
                 iter = 6000,
                 chains = 3,
                 cores = 3,
                 save_all_pars = TRUE,
                 # Increase adapt_delta to avoid divergent transitions after warmup.
                 control = list(adapt_delta = 0.9),
                 file = "Grp_Min")

summary(M_Grp_Min)
plot(M_Grp_Min, ask = FALSE)
pp_check(M_Grp_Min, check = "distributions")
pp_check(M_Grp_Min, check = "residuals")
pp_check(M_Grp_Min, "error_scatter_avg")
pp_check(M_Grp_Min, check = "scatter")


          ## DYADS ##

Dyads <- DF %>%
              # remove trial 20. In this last round individuals could not change anymore            
              filter(!trial == 20) %>% 
              filter(treatment == "Dyads") %>% 
              mutate(id = as_factor(id))

summary(Dyads)

M_Dyads <- brm(changed ~ won + confirm + trial + (1|id),
                          data = Dyads, 
                          family = "bernoulli",
                          iter = 6000,
                          chains = 3,
                          cores = 3,
                          save_all_pars = TRUE,
                          file = "Dyads")

summary(M_Dyads)
plot(M_Dyads, ask = FALSE)
pp_check(M_Dyads, check = "distributions")
pp_check(M_Dyads, check = "residuals")
pp_check(M_Dyads, "error_scatter_avg")
pp_check(M_Dyads, check = "scatter")




          ## FIGURES SHOWING POSTERIOR PREDICTIONS OF CHANGING LIKELIHOOD PER TREATMENT (MAIN FIGURE 4) ##

library(ggeffects)
library(ggplot2)
library(cowplot)

          ## INDIVIDUALS, EVIDENCE 1 ## 

P_Ind_E1 <- ggpredict(M_Ind_E1, terms = c("won", "confirm"))
P_Ind_E1

Fig1 <- P_Ind_E1 %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                      scale_color_manual(values=c("blue", "red")) +
                      scale_fill_manual(values=c("blue", "red")) +
                      ggtitle("Individuals\nEvidence 1") +
                      theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                      scale_y_continuous(name = "Probability changing advisor", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                      scale_x_discrete(name = "") +
                      theme(axis.text.x = element_text(size = 11)) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
                      theme(legend.title=element_blank(),
                            legend.text = element_text(size = 10), 
                            legend.key.size = unit(0.3, "cm"),
                            legend.key.width = unit(0.3,"cm"), 
                            legend.position = c(0.3, 0.9)) 
Fig1

          ## INDIVIDUALS, EVIDENCE 2 ## 
          
P_Ind_E2 <- ggpredict(M_Ind_E2, terms = c("won", "confirm"))
P_Ind_E2

Fig2 <- P_Ind_E2 %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                      scale_color_manual(values=c("blue", "red")) +
                      ggtitle("Individuals\nEvidence 2") +
                      theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                      scale_y_continuous(name = "", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                      scale_x_discrete(name = "") +
                      theme(axis.text.x = element_text(size = 11)) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                      theme(legend.position = "none" )
Fig2

          ## INDIVIDUALS, EVIDENCE 3 ## 

P_Ind_E3 <- ggpredict(M_Ind_E3, terms = c("won", "confirm"))
P_Ind_E3

Fig3 <- P_Ind_E3 %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                      scale_color_manual(values=c("blue", "red")) +
                      ggtitle("Individuals\nEvidence 3") +
                      theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                      scale_y_continuous(name = "", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                      scale_x_discrete(name = "") +
                      theme(axis.text.x = element_text(size = 11)) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                      theme(legend.position = "none" )
Fig3

          ## INDIVIDUALS, EVIDENCE 4 ## 

P_Ind_E4 <- ggpredict(M_Ind_E4, terms = c("won"))
P_Ind_E4

Fig4 <- P_Ind_E4 %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                      scale_color_manual(values=c("blue")) +
                      ggtitle("Individuals\nEvidence 4") +
                      theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                      scale_y_continuous(name = "", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                      scale_x_discrete(name = "") +
                      theme(axis.text.x = element_text(size = 11)) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                      theme(legend.position = "none" )
Fig4

          ## VOTING GROUPS, IN MAJORITY ## 

P_Grp_Maj <- ggpredict(M_Grp_Maj, terms = c("won", "confirm"))
P_Grp_Maj

Fig5 <- P_Grp_Maj %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                      scale_color_manual(values=c("blue", "red")) +
                      ggtitle("Individuals in the majority\nof the voting groups") +
                      theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                      scale_y_continuous(name = "Probability changing advisor", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                      scale_x_discrete(name = "") +
                      theme(axis.text.x = element_text(size = 11)) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                      theme(legend.position = "none" )
Fig5

          ## VOTING GROUPS, IN MINORITY ## 

P_Grp_Min <- ggpredict(M_Grp_Min, terms = c("won", "confirm"))
P_Grp_Min

Fig6 <- P_Grp_Min %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                      scale_color_manual(values=c("blue", "red")) +
                      ggtitle("Individuals in the minority\nof the voting groups") +
                      theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                      scale_y_continuous(name = "", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                      scale_x_discrete(name = "") +
                      theme(axis.text.x = element_text(size = 11)) +
                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                      theme(legend.position = "none" )
Fig6

          ## DYADS ## 

P_Dyads <- ggpredict(M_Dyads, terms = c("won", "confirm"))
P_Dyads

Fig7 <- P_Dyads %>% ggplot(aes(x = x, y = predicted, colour = group)) +  geom_point(size=1.8, position = position_dodge(width = 0.5)) +
                    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,  fill =  group, colour = group), position = position_dodge(width = 0.5)) +
                    scale_color_manual(values=c("blue", "red")) +
                    ggtitle("Communicating dyads\n") +
                    theme(plot.title = element_text(hjust = 0.5, size = 11.5)) +
                    scale_y_continuous(name = "", limits=c(0, 0.8), breaks = seq(0, 1, 0.2)) +
                    scale_x_discrete(name = "") +
                    theme(axis.text.x = element_text(size = 11)) +
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                    theme(legend.position = "none" )

Fig7

          ## COMBINE FIGURES ##

Fig_Comp <- plot_grid(Fig1, Fig2, Fig3, Fig4, Fig5, Fig6, Fig7, NULL, labels = c("a", "b", "c", "d", "e", "f", "g",""), nrow = 2)
save_plot("Figure Changing advisor.png", Fig_Comp, base_height = 7, base_width = 9.5)
