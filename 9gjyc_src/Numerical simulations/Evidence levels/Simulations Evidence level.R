library(tidyverse)

# Number of simulations
total <- 1000

# 20 trials
# Always 5 easy trails
easy.trials <- c(90,90,90,90,90)

# Four different evidence levels for hard trials, either 50, 60, 70 or 80 balls of 1 color (1-4).
hard.trials <- c(50,60,70,80)

# switching matrix (based on pilot experiment)
p_switch <- read.csv("P_switch_ind.csv", header = TRUE)
p_switch

#hard = 50

for (runs in 1:total)
    {
      for (hard in hard.trials)
      {
      # 15 hard trials
      hard.tr <- rep(hard,15)
      all.tr <- c(easy.trials, hard.tr)
      
      # shuffle order of trials  
      all.tr <- sample(all.tr)
      
      # loop through all trials
      TRIAL.NR <- 1
      
      results <- data.frame(NULL)
      
      for (TRIAL in all.tr)
          { 
          # Determine which advisor is being selected by client
          # At first trial, randomly select honest advisor (0) or strategic advisor (1)
          if(TRIAL.NR == 1){sel.adv <- sample(c("HA","SA"), 1)}
          
          # If not first trial, then base it on p_switch matrix
          if(TRIAL.NR > 1)
            {
            # look up pchange in table based on reward (yes/no) and confidence unselected advisor
            sel.row <- subset(p_switch, Reward == reward &  Conf.unsel.adv == unselect.adv.conf)
            sel.row
            pswitch <- sel.row$Changed
            pswitch
            
            # determine if switch happens yes/no
            switched <- rbinom(n = 1, size = 1, prob = pswitch) 
            switched
            
            if(TRIAL.NR > 1){sel.adv <- ifelse(switched == 0, sel.adv, unselect.adv)}  
            sel.adv
            }
            sel.adv
            
              # Play out trial
              nr.black <- TRIAL
              nr.white <- 100 - nr.black
              balls.bl <- rep(1,nr.black)
              balls.wh <- rep(0, nr.white)
              balls <- c(balls.bl, balls.wh)
              balls
              
              # sample 75 balls as evidence
              evidence <- sample(balls, 75)
              evidence
              
              # HA Evidence
              HA_evidence <- mean(evidence)
              
              # HA Decision
              HA_decision <- if(HA_evidence < 0.5) {0} else {1}
              
              # HA Confidence
              # Transform advice to concrete confidence scales (1-5)
              HA_confidence    <- ifelse(HA_evidence < 0.10, 5, 
                                   ifelse(HA_evidence < 0.20, 4,
                                     ifelse(HA_evidence < 0.30, 3,
                                       ifelse(HA_evidence < 0.40, 2, 
                                         ifelse(HA_evidence < 0.60, 1, 
                                           ifelse(HA_evidence < 0.70, 2,
                                              ifelse(HA_evidence < 0.80, 3, 
                                                ifelse(HA_evidence < 0.90, 4, 5))))))))
              
                  # SA Evidence
                  SA_evidence <- mean(evidence)
                  
                  # SA Decision. If selected, then decision equal to HA,   
                  # If not selected then contradict HA for weak evidence (CONF 1+2), but confirm HA for good evidence (CONF 3+4+5)
                  SA_decision    <- ifelse(sel.adv == "SA", HA_decision, 
                                      ifelse(HA_confidence < 2.5, 1 - HA_decision, HA_decision))
                                                
                  SA_decision
                  
                  # SA Confidence
                  SA_confidence <- ifelse(sel.adv == "SA", HA_confidence, 
                                     ifelse(HA_confidence < 2.5, sample(c(-2,-3,-4), 1), HA_confidence))
                  SA_confidence
                  
                  # Confidence unselected advisor (necessary for calculating switching likelihood)
                  unselect.adv <- ifelse(sel.adv == "HA", "SA", "HA")
                  unselect.adv.conf <- ifelse(sel.adv == "HA", SA_confidence, HA_confidence)
                  
                      # Client's decision. Takes over decision of selected advisor
                      Client_decision <- ifelse(sel.adv == "HA", HA_decision, SA_decision)
                      Client_decision
                      
                          # Determine outcome of trial, draw 1 ball
                          outcome <- sample(balls, 1)
                          
                          # Did client win?
                          reward <- ifelse(Client_decision == outcome, 1, 0)
                          
                              # store results
                              outcomes <- data.frame(reward, sel.adv, hard, TRIAL.NR)
                              results <- rbind(results, outcomes)
                              
                              # Update trial number
                              TRIAL.NR <- TRIAL.NR + 1
        }
                              
        # save results
        write.table(results, "Runs evidence level.txt", col.names= FALSE, append = TRUE)
        }
}