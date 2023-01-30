

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
####        The code has the following two parts:
####        1. Geometric morphometric analysis
####        2. t-SNE for imbalanced data

#### The analyses are carried on independently in the sites belonging to the Mesolithic phase A (Abric de la Falguera,
#### Benàmer, Cueva de la Cocina, Baños de Ariño, Botiqueria dels Moros, Pontet) and phase B (Cingle del Mas Nou, Cocina,
#### Botiqueria dels Moros, Pontet). Therefore, and for clarity, two identical scripts are provided, one for each phase

#### THIS IS SCRIPT IS FOR PHASE A

## Load data and arrange data
geo_dfA <- read.csv('Data/Phase_A.csv', header = FALSE) ## Your route
colnames(geo_dfA) <- c("ID","Site","Level","Campaign","Layer","Sector","Shape","Microburin","Width","Area","Inc_dist","Inc_prox","Dir_dist","Dir_prox")
geo_dfA$Phase <- rep("A",nrow(geo_dfA))
colnames(geo_dfA)
geo_dfA$Site<-as.factor(geo_dfA$Site) ## Sites as factors
geo_dfA <- geo_dfA[-c(which(geo_dfA$ID == 3210)),] ## Remove Falguera's outlier
geo_dfA <- geo_dfA[order(geo_dfA$ID),]

################################################################################
################################################################################
####              PART 1. GEOMETRIC MORPHOMETRICS ANALYSIS                  ####
################################################################################
################################################################################

library(Momocs)
set.seed(12345)
## Extract outlines
dir <- getwd()
setwd("./Outlines/Phase_A")
geo_list <- geo_dfA$ID
geo_list <- sort(geo_list)
geo_list <- paste0(geo_list,".jpg")
geo_out_A <- Out(import_jpg(geo_list, auto.notcentered = T), fac=geo_dfA)
setwd(dir)
## length(geo_out_A) We have 508 geometric microliths

## Elliptical fourier analysis at n=20 harmonics
geo_out_AF <- efourier(coo_center(coo_scale(geo_out_A)), nb.h = 7, norm=F, start=T)

## PCA
geo_out_A.pca <- PCA(geo_out_AF)
#plot(geo_out_A.pca, 1, pos.shp="circle",stars=TRUE, palette=col_qual)
#scree_plot(geo_out_A.pca) PC1 and PC2 justify an 85% of the variance

## LDA
geo_out_A.lda<-LDA(geo_out_AF, fac=geo_dfA$Site)

## For plotting without Cocina
geo_dfA_NoCoc <- geo_dfA[geo_dfA$Site != "Cueva de la Cocina",]
setwd("./Outlines/Phase_A")
geo_list <- geo_dfA_NoCoc$ID
geo_list <- sort(geo_list)
geo_list <- paste0(geo_list,".jpg")
geo_out_A_NoCoc <- Out(import_jpg(geo_list, auto.notcentered = T), fac=geo_dfA_NoCoc)
setwd(dir)
geo_out_A_NoCocF <- efourier(coo_center(coo_scale(geo_out_A_NoCoc)), nb.h = 7, norm=F, start=T)
geo_out_A_NoCoc.lda<-LDA(geo_out_A_NoCocF, fac=geo_dfA_NoCoc$Site)

## Iterations (n=25) for cross validation table
cvn40A<-LDA(geo_out_AF, fac=geo_dfA$Site)
cvn40A<-cvn40A$CV.tab

for (i in 1:25){
  provcv40A<-LDA(geo_out_AF, fac=geo_dfA$Site)
  provcv40A<-provcv40A$CV.tab
  cvn40A<-cvn40A+provcv40A
}

## Without Cocina
cvn40A_NoCoc<-LDA(geo_out_A_NoCocF, fac=geo_dfA_NoCoc$Site)
cvn40A_NoCoc<-cvn40A_NoCoc$CV.tab

for (i in 1:25){
  provcv40A_NoCoc<-LDA(geo_out_A_NoCocF, fac=geo_dfA_NoCoc$Site)
  provcv40A_NoCoc<-provcv40A_NoCoc$CV.tab
  cvn40A_NoCoc<-cvn40A_NoCoc+provcv40A_NoCoc
}

## Pariwise MANOVA
geo_out_A.man<-MANOVA_PW(geo_out_A.pca, fac=geo_dfA$Site)
geo_out_Atab<-geo_out_A.man$summary
rownames(geo_out_Atab) <- c("F-BA", "F-BE", "F-BO", "F-CO",
                            "F-PO", "BA-BE","BA-BO", "BA-CO",
                            "BA-PO", "BE-BO", "BE-CO", "BE-PO",
                            "BO-CO", "BO-PO", "CO-PO")
#geo_out_A.man
## K-Means
PCA1 <- geo_out_A.pca$x[,1]
PCA2 <- geo_out_A.pca$x[,2]
res.kmeansA1<-list()
res.kmeansA2<-list()
prA1<-c()
prA2<-c()
for (k in 2:10){ ## 10 is the max number of centers considered
  res.kmeansA1[[k]]<-kmeans(PCA1, centers = k, nstart = 50)
  prA1[k]<-res.kmeansA1[[k]]$tot.withinss
}
for (k in 2:10){ ## 10 is the max number of centers considered
  res.kmeansA2[[k]]<-kmeans(PCA2, centers = k, nstart = 50)
  prA2[k]<-res.kmeansA2[[k]]$tot.withinss
}


################################################################################
################################################################################
####                            PART 2. t-SNE                               ####
################################################################################
################################################################################

### Compute data imbalance
label_imbalance <- function(labels) { ## Function by Courtenay 2022
  
  n <- length(labels)
  g <- length(levels(labels))
  c <- c()
  entropy_index <- c()
  for (ci in 1:g) {
    c <- c(
      c, table(labels)[ci][[1]]
    )
    entropy_index <- c(
      entropy_index, (c[ci] / n) * log(c[ci] / n)
    )
  }
  H <- -sum(entropy_index)
  balance_index <- H / log(g)
  return(balance_index)
  
}

labelsA <- c(geo_out_AF$fac$Site[geo_out_AF$fac$Site=="Abric de la Falguera"],
             geo_out_AF$fac$Site[geo_out_AF$fac$Site=="Baños de Ariño"],
             geo_out_AF$fac$Site[geo_out_AF$fac$Site=="Benàmer"],
             geo_out_AF$fac$Site[geo_out_AF$fac$Site=="Botiqueria dels Moros"],
             geo_out_AF$fac$Site[geo_out_AF$fac$Site=="Cueva de la Cocina"],
             geo_out_AF$fac$Site[geo_out_AF$fac$Site=="Pontet"])

label_imbalance(labelsA) ## It is 0.55, quite imbalanced

## Perform Rtsne
library(Rtsne)

## Perform Rtsne. Perplexity is set to 20 and max_iter to 10000, as it seems to be the best compromise between speed and accuracy
tsne_eval_A <- list()
iters <- 100

for (i in 1:iters){
  tsne_A <- Rtsne(geo_out_AF$coe, dims = 2, perplexity=20, verbose=TRUE, max_iter = 10000, theta = 0)
  tsne_eval_A[[i]] <- tsne_A
}

for (i in 1:iters){
  pillsA <- data.frame("X" = tsne_eval_A[[i]]$Y[,1], "Y" = tsne_eval_A[[i]]$Y[,2], "Site" = geo_out_AF$fac$Site)
  
  dfFA <- pillsA[pillsA$Site == "Abric de la Falguera",]
  dfBA <- pillsA[pillsA$Site == "Baños de Ariño",]
  dfBE <- pillsA[pillsA$Site == "Benàmer",]
  dfBO <- pillsA[pillsA$Site == "Botiqueria dels Moros",]
  dfCO <- pillsA[pillsA$Site == "Cueva de la Cocina",]
  dfPO <- pillsA[pillsA$Site == "Pontet",]
  
  ## Group for pairwise
  dfFABA <- rbind(dfFA,dfBA)
  dfFABE <- rbind(dfFA,dfBE)
  dfFABO <- rbind(dfFA,dfBO)
  dfFACO <- rbind(dfFA,dfCO)
  dfFAPO <- rbind(dfFA,dfPO)
  dfBABE <- rbind(dfBA,dfBE)
  dfBABO <- rbind(dfBA,dfBO)
  dfBACO <- rbind(dfBA,dfCO)
  dfBAPO <- rbind(dfBA,dfPO)
  dfBEBO <- rbind(dfBE,dfBO)
  dfBECO <- rbind(dfBE,dfCO)
  dfBEPO <- rbind(dfBE,dfPO)
  dfBOCO <- rbind(dfBO,dfCO)
  dfBOPO <- rbind(dfBO,dfPO)
  dfCOPO <- rbind(dfCO,dfPO)
  
  ## Do pairwise
  mFABA <- manova(lm(cbind(X,Y) ~ Site, dfFABA))
  mFABE <- manova(lm(cbind(X,Y) ~ Site, dfFABE))
  mFABO <- manova(lm(cbind(X,Y) ~ Site, dfFABO))
  mFACO <- manova(lm(cbind(X,Y) ~ Site, dfFACO))
  mFAPO <- manova(lm(cbind(X,Y) ~ Site, dfFAPO))
  mBABE <- manova(lm(cbind(X,Y) ~ Site, dfBABE))
  mBABO <- manova(lm(cbind(X,Y) ~ Site, dfBABO))
  mBACO <- manova(lm(cbind(X,Y) ~ Site, dfBACO))
  mBAPO <- manova(lm(cbind(X,Y) ~ Site, dfBAPO))
  mBEBO <- manova(lm(cbind(X,Y) ~ Site, dfBEBO))
  mBECO <- manova(lm(cbind(X,Y) ~ Site, dfBECO))
  mBEPO <- manova(lm(cbind(X,Y) ~ Site, dfBEPO))
  mBOCO <- manova(lm(cbind(X,Y) ~ Site, dfBOCO))
  mBOPO <- manova(lm(cbind(X,Y) ~ Site, dfBOPO))
  mCOPO <- manova(lm(cbind(X,Y) ~ Site, dfCOPO))
  
  ## Results
  pillsA <- c(summary(mFABA)$stats[1,2],
             summary(mFABE)$stats[1,2],
             summary(mFABO)$stats[1,2],
             summary(mFACO)$stats[1,2],
             summary(mFAPO)$stats[1,2],
             summary(mBABE)$stats[1,2],
             summary(mBABO)$stats[1,2],
             summary(mBACO)$stats[1,2],
             summary(mBAPO)$stats[1,2],
             summary(mBEBO)$stats[1,2],
             summary(mBECO)$stats[1,2],
             summary(mBEPO)$stats[1,2],
             summary(mBOCO)$stats[1,2],
             summary(mBOPO)$stats[1,2],
             summary(mCOPO)$stats[1,2])
  names(pillsA) <- c("mFABA","mFABE","mFABO","mFACO","mFAPO","mBABE","mBABO","mBAPO","mBACO","mBEBO","mBECO","mBEPO","mBOCO","mBOPO","mCOPO")
  
  tsne_eval_A[[i]]$pillai <- pillsA
}

