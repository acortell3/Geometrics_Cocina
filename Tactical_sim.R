
#### Code for the article:
#### Cortell-Nicolau, A., García-Puchol, O., Juan-Cabanilles, J. (2023). "The geometric microliths of Cueva de la Cocina
#### and their significance in the Mesolithic of Eastern Iberia: A morphometric study", in O. García-Puchol, S. McClure, 
#### J. Juan-Cabanilles (eds.) The last hunter-gatherers on the Iberian Peninsula: an integrative evolutionary and 
#### multiscalar approach from Cueva de la Cocina (Western Mediterranean). Quaternary International Special Issue.

#### Code author: A. Cortell-Nicolau

#### Cambridge, 2023

#### License: Permission is granted to use, adapt and distribute this code. Please, cite as appropriate

#### Warranty: No warranty is expressed or implied. Use at your own risk

#### About: 
####        This code simulates the process of cultural aggregation from groups coinciding repeatedly in the same site
####        There are four groups, each with one predominant type. The predominancy of the type is considered with the following probabilistic levels
####        40%, 55%, 70% and 85% probability for the predominant type, the rest of the types being distributed equally within the probability
####        space left.

####        Because the objective is to emulate a GM approach, each type is considered only as xy coordinates (simulating PC1 and PC2) with different
####        means but same standard deviation for each type


#### 1. CREATE THE TYPES 

## Define types in xy space
Type1 <- data.frame("X" = rnorm(1000), "Y" = rnorm(1000))
Type2 <- data.frame("X" = rnorm(1000), "Y" = rnorm(1000,3))
Type3 <- data.frame("X" = rnorm(1000,3), "Y" = rnorm(1000))
Type4 <- data.frame("X" = rnorm(1000,3), "Y" = rnorm(1000,3))

Types <- data.frame("X" = c(Type1$X,Type2$X,Type3$X,Type4$X),
                    "Y" = c(Type1$Y,Type2$Y,Type3$Y,Type4$Y),
                    "Type" = as.factor(c(rep("Type1",nrow(Type1)),
                                         rep("Type2",nrow(Type2)),
                                         rep("Type3",nrow(Type3)),
                                         rep("Type4",nrow(Type4)))))

#### 2. SET PROBABILITIES FOR SIMULATIONS
prob1 <- list("G1" = c(rep(.0004,nrow(Type1)),
                       rep(.0002,nrow(Type2)),
                       rep(.0002,nrow(Type3)),
                       rep(.0002,nrow(Type4))),
              
              "G2" = c(rep(.0002,nrow(Type1)),
                       rep(.0004,nrow(Type2)),
                       rep(.0002,nrow(Type3)),
                       rep(.0002,nrow(Type4))),
              
              "G3" = c(rep(.0002,nrow(Type1)),
                       rep(.0002,nrow(Type2)),
                       rep(.0004,nrow(Type3)),
                       rep(.0002,nrow(Type4))),
              
              "G4" = c(rep(.0002,nrow(Type1)),
                       rep(.0002,nrow(Type2)),
                       rep(.0002,nrow(Type3)),
                       rep(.0004,nrow(Type4))))

prob2 <- list("G1" = c(rep(.00055,nrow(Type1)),
                       rep(.00015,nrow(Type2)),
                       rep(.00015,nrow(Type3)),
                       rep(.00015,nrow(Type4))),
              
              "G2" = c(rep(.00015,nrow(Type1)),
                       rep(.00055,nrow(Type2)),
                       rep(.00015,nrow(Type3)),
                       rep(.00015,nrow(Type4))),
              
              "G3" = c(rep(.00015,nrow(Type1)),
                       rep(.00015,nrow(Type2)),
                       rep(.00055,nrow(Type3)),
                       rep(.00015,nrow(Type4))),
              
              "G4" = c(rep(.00015,nrow(Type1)),
                       rep(.00015,nrow(Type2)),
                       rep(.00015,nrow(Type3)),
                       rep(.00055,nrow(Type4))))

prob3 <- list("G1" = c(rep(.0007,nrow(Type1)),
                       rep(.0001,nrow(Type2)),
                       rep(.0001,nrow(Type3)),
                       rep(.0001,nrow(Type4))),
              
              "G2" = c(rep(.0001,nrow(Type1)),
                       rep(.0007,nrow(Type2)),
                       rep(.0001,nrow(Type3)),
                       rep(.0001,nrow(Type4))),
              
              "G3" = c(rep(.0001,nrow(Type1)),
                       rep(.0001,nrow(Type2)),
                       rep(.0007,nrow(Type3)),
                       rep(.0001,nrow(Type4))),
              
              "G4" = c(rep(.0001,nrow(Type1)),
                       rep(.0001,nrow(Type2)),
                       rep(.0001,nrow(Type3)),
                       rep(.0007,nrow(Type4))))

prob4 <- list("G1" = c(rep(.00085,nrow(Type1)),
                       rep(.00005,nrow(Type2)),
                       rep(.00005,nrow(Type3)),
                       rep(.00005,nrow(Type4))),
              
              "G2" = c(rep(.00005,nrow(Type1)),
                       rep(.00085,nrow(Type2)),
                       rep(.00005,nrow(Type3)),
                       rep(.00005,nrow(Type4))),
              
              "G3" = c(rep(.00005,nrow(Type1)),
                       rep(.00005,nrow(Type2)),
                       rep(.00085,nrow(Type3)),
                       rep(.00005,nrow(Type4))),
              
              "G4" = c(rep(.00005,nrow(Type1)),
                       rep(.00005,nrow(Type2)),
                       rep(.00005,nrow(Type3)),
                       rep(.00085,nrow(Type4))))

probabilities <- list(prob1,prob2,prob3,prob4)

#### 3. START SIMULATION

results <- list()

for (i in 1:length(probabilities)){
  Group1 <- sample(c(rownames(Types)),1000,
                   prob = probabilities[[i]]$G1,
                   replace = TRUE)
  Group1 <- Types[Group1,]
  Group1$Group <- rep("Group1",nrow(Group1))

  Group2 <- sample(c(rownames(Types)),1000,
                   prob = probabilities[[i]]$G2,
                   replace = TRUE)

  Group2 <- Types[Group2,]
  Group2$Group <- rep("Group2",nrow(Group2))

  Group3 <- sample(c(rownames(Types)),1000,
                   prob = probabilities[[i]]$G3,
                   replace = TRUE)

  Group3 <- Types[Group3,]
  Group3$Group <- rep("Group3",nrow(Group3))

  Group4 <- sample(c(rownames(Types)),1000,
                   prob = probabilities[[i]]$G4,
                   replace = TRUE)

  Group4 <- Types[Group4,]
  Group4$Group <- rep("Group4",nrow(Group4))

  Groups <- rbind(Group1,Group2,Group3,Group4)

  Groups$Group <- as.factor(Groups$Group)

  ## Create site and process to deposit

  ## Set time
  t <- 1000

  ## One site per group and one site with 4 slots (one for each group)
 
  Site1 <- data.frame("X" = rep(NA,t),
                      "Y" = rep(NA,t),
                      "Type" = rep(NA,t),
                      "Group" = rep(NA,t))
  Site2 <- data.frame("X" = rep(NA,t),
                      "Y" = rep(NA,t),
                      "Type" = rep(NA,t),
                      "Group" = rep(NA,t))
  Site3 <- data.frame("X" = rep(NA,t),
                      "Y" = rep(NA,t),
                      "Type" = rep(NA,t),
                      "Group" = rep(NA,t))
  Site4 <- data.frame("X" = rep(NA,t),
                      "Y" = rep(NA,t),
                      "Type" = rep(NA,t),
                      "Group" = rep(NA,t))
  Site_co_1 <- data.frame("X" = rep(NA,t),
                          "Y" = rep(NA,t),
                          "Type" = rep(NA,t),
                          "Group" = rep(NA,t))
  Site_co_2 <- data.frame("X" = rep(NA,t),
                          "Y" = rep(NA,t),
                          "Type" = rep(NA,t),
                          "Group" = rep(NA,t))
  Site_co_3 <- data.frame("X" = rep(NA,t),
                          "Y" = rep(NA,t),
                          "Type" = rep(NA,t),
                          "Group" = rep(NA,t))
  Site_co_4 <- data.frame("X" = rep(NA,t),
                           "Y" = rep(NA,t),
                          "Type" = rep(NA,t),
                          "Group" = rep(NA,t))

  for (j in 1:t){
    Site1[j,] <- Group1[sample(nrow(Group1),1),]
    Site2[j,] <- Group2[sample(nrow(Group2),1),]
    Site3[j,] <- Group3[sample(nrow(Group3),1),]
    Site4[j,] <- Group4[sample(nrow(Group4),1),]
  
    Site_co_1[j,] <- Group1[sample(nrow(Group1),1),]
    Site_co_2[j,] <- Group2[sample(nrow(Group2),1),]
    Site_co_3[j,] <- Group3[sample(nrow(Group3),1),]
    Site_co_4[j,] <- Group4[sample(nrow(Group4),1),]
  }

  Site_co_1$Group <- rep("Group5",nrow(Site_co_1))
  Site_co_2$Group <- rep("Group5",nrow(Site_co_1))
  Site_co_3$Group <- rep("Group5",nrow(Site_co_1))
  Site_co_4$Group <- rep("Group5",nrow(Site_co_1))

  All_sites <- rbind(Site1,Site2,Site3,Site4,Site_co_1,Site_co_2,Site_co_3,Site_co_4)
  
  current <- list(Types,Groups,All_sites)
  results[[i]] <- current
  
}

labels_tact <- c(rep("G1",1000),rep("G2",1000),rep("G3",1000),rep("G4",1000),rep("G5",4000))

label_imbalance(as.factor(labels_tact))


#### 4. COMPUTE PAIRWISE MANOVA FOR ALL SIMULATIONS

library(car)
tact_pills <- data.frame("G15" = rep(NA,length(probabilities)), "G14" = rep(NA,length(probabilities)),
                         "G13" = rep(NA,length(probabilities)), "G12" = rep(NA,length(probabilities)),
                         "G25" = rep(NA,length(probabilities)), "G24" = rep(NA,length(probabilities)),
                         "G23" = rep(NA,length(probabilities)), "G35" = rep(NA,length(probabilities)),
                         "G34" = rep(NA,length(probabilities)), "G45" = rep(NA,length(probabilities)))

for (i in 1:length(probabilities)){
  
  for_man <- results[[i]][[3]]
  
  pw1 <- for_man[for_man$Group == "Group1",]
  pw2 <- for_man[for_man$Group == "Group2",]
  pw3 <- for_man[for_man$Group == "Group3",]
  pw4 <- for_man[for_man$Group == "Group4",]
  pw5 <- for_man[for_man$Group == "Group5",]
  
  pv15 <- rbind(pw1,pw5)
  pv14 <- rbind(pw1,pw4)
  pv13 <- rbind(pw1,pw3)
  pv12 <- rbind(pw1,pw2)
  pv25 <- rbind(pw2,pw5)
  pv24 <- rbind(pw2,pw4)
  pv23 <- rbind(pw2,pw3)
  pv35 <- rbind(pw3,pw5)
  pv34 <- rbind(pw3,pw4)
  pv45 <- rbind(pw4,pw5)
  
  m15 <- manova(lm(cbind(X,Y) ~ Group, pv15))
  m14 <- manova(lm(cbind(X,Y) ~ Group, pv14))
  m13 <- manova(lm(cbind(X,Y) ~ Group, pv13))
  m12 <- manova(lm(cbind(X,Y) ~ Group, pv12))
  m25 <- manova(lm(cbind(X,Y) ~ Group, pv25))
  m24 <- manova(lm(cbind(X,Y) ~ Group, pv24))
  m23 <- manova(lm(cbind(X,Y) ~ Group, pv23))
  m35 <- manova(lm(cbind(X,Y) ~ Group, pv35))
  m34 <- manova(lm(cbind(X,Y) ~ Group, pv34))
  m45 <- manova(lm(cbind(X,Y) ~ Group, pv45))
  
  pils <- c(summary(m15)$stats[1,2], summary(m14)$stats[1,2], summary(m13)$stats[1,2], summary(m12)$stats[1,2], summary(m25)$stats[1,2],
            summary(m24)$stats[1,2], summary(m23)$stats[1,2], summary(m35)$stats[1,2], summary(m34)$stats[1,2], summary(m45)$stats[1,2])
  
  tact_pills[i,] <- pils
  
}



