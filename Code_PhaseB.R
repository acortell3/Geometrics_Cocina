

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

#### THIS IS SCRIPT IS FOR PHASE B

## Load data and arrange data
geo_dfB <- read.csv('Data/Phase_B.csv', header = FALSE) ## Your route
colnames(geo_dfB) <- c("ID","Site","Level","Campaign","Layer","Sector","Shape","Microburin","Width","Area","Inc_dist","Inc_prox","Dir_dist","Dir_prox")
geo_dfB$Phase <- rep("B",nrow(geo_dfB))

geo_dfB$Site <- as.factor(geo_dfB$Site) ## Sites as factors
geo_dfB <- geo_dfB[order(geo_dfB$ID),]

################################################################################
################################################################################
####              PART 1. GEOMETRIC MORPHOMETRICS ANALYSIS                  ####
################################################################################
################################################################################

library(Momocs)
set.seed(12345)
## Extract outlines
dir <- getwd()
setwd("./Outlines/Phase_B")
geo_list <- geo_dfB$ID
geo_list <- sort(geo_list)
geo_list <- paste0(geo_list,".jpg")
geo_out_B <- Out(import_jpg(geo_list, auto.notcentered = T), fac=geo_dfB)
setwd(dir)
## length(geo_out_B) We have 508 geometric microliths

## Elliptical fourier analysis at n=20 harmonics
geo_out_BF <- efourier(coo_center(coo_scale(geo_out_B)), nb.h = 7, norm=F, start=T)
geo_out_BF$coe
## PCA
geo_out_B.pca <- PCA(geo_out_BF)
# plot(geo_out_B.pca, 1, pos.shp="circle",stars=TRUE, palette=col_qual)
# scree_plot(geo_out_B.pca) PC1 and PC2 justify an 85% of the variance

## LDA
geo_out_B.lda<-LDA(geo_out_BF, fac=geo_dfB$Site)

## For plotting without Cocina
geo_dfB_NoCoc <- geo_dfB[geo_dfB$Site != "Cueva de la Cocina",]
setwd("./Outlines/Phase_B")
geo_list <- geo_dfB_NoCoc$ID
geo_list <- sort(geo_list)
geo_list <- paste0(geo_list,".jpg")
geo_out_B_NoCoc <- Out(import_jpg(geo_list, auto.notcentered = T), fac=geo_dfB_NoCoc)
setwd(dir)
geo_out_B_NoCocF <- efourier(coo_center(coo_scale(geo_out_B_NoCoc)), nb.h = 7, norm=F, start=T)
geo_out_B_NoCoc.lda<-LDA(geo_out_B_NoCocF, fac=geo_dfB_NoCoc$Site)

## Check ratios between Mas Nou/Cocina and Botiqueria/Mas Nou for sample size
#lco <- nrow(geo_dfB[geo_dfB$Site == "Cueva de la Cocina",])
#msn <- nrow(geo_dfB[geo_dfB$Site == "Cingle del Mas Nou",])
#btq <- nrow(geo_dfB[geo_dfB$Site == "Botiqueria dels Moros",])
#msn/lco
#btq/msn

## Iterations (n=25) for cross validation table
cvn40B<-LDA(geo_out_BF, fac=geo_dfB$Site)
cvn40B<-cvn40B$CV.tab

for (i in 1:25){
  provcv40B<-LDA(geo_out_BF, fac=geo_dfB$Site)
  provcv40B<-provcv40B$CV.tab
  cvn40B<-cvn40B+provcv40B
}

## Without Cocina
cvn40B_NoCoc<-LDA(geo_out_B_NoCocF, fac=geo_dfB_NoCoc$Site)
cvn40B_NoCoc<-cvn40B_NoCoc$CV.tab

for (i in 1:25){
  provcv40B_NoCoc<-LDA(geo_out_B_NoCocF, fac=geo_dfB_NoCoc$Site)
  provcv40B_NoCoc<-provcv40B_NoCoc$CV.tab
  cvn40B_NoCoc<-cvn40B_NoCoc+provcv40B_NoCoc
}


## Pariwise MANOVA
geo_out_B.man<-MANOVA_PW(geo_out_B.pca, fac=geo_dfB$Site)
geo_out_Btab<-geo_out_B.man$summary
rownames(geo_out_Btab) <- c("BO-MA", "BO-CO", "BO-PO", "MA-CO", "MA-PO", "CO-PO")

## K-Means
PCB1 <- geo_out_B.pca$x[,1]
PCB2 <- geo_out_B.pca$x[,2]
res.kmeansB1<-list()
res.kmeansB2<-list()
prB1<-c()
prB2<-c()
for (k in 2:10){ ## 10 is the max number of centers considered
  res.kmeansB1[[k]]<-kmeans(PCB1, centers = k, nstart = 50)
  prB1[k]<-res.kmeansB1[[k]]$tot.withinss
}
for (k in 2:10){ ## 10 is the max number of centers considered
  res.kmeansB2[[k]]<-kmeans(PCB2, centers = k, nstart = 50)
  prB2[k]<-res.kmeansB2[[k]]$tot.withinss
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

labelsB <- c(geo_out_BF$fac$Site[geo_out_BF$fac$Site=="Botiqueria dels Moros"],
             geo_out_BF$fac$Site[geo_out_BF$fac$Site=="Cingle del Mas Nou"],
             geo_out_BF$fac$Site[geo_out_BF$fac$Site=="Cueva de la Cocina"],
             geo_out_BF$fac$Site[geo_out_BF$fac$Site=="Pontet"])

label_imbalance(labelsB) ## It is 0.38, very imbalanced

## Perform Rtsne

library(Rtsne)

## Perform Rtsne. Perplexity is set to 10 and max_iter to 10000, as it seems to be the best compromise between speed and accuracy
tsne_eval_B <- list()
iters <- 100

for (i in 1:iters){
  tsne_B <- Rtsne(geo_out_BF$coe, dims = 2, perplexity=10, verbose=TRUE, max_iter = 10000, theta = 0)
  tsne_eval_B[[i]] <- tsne_B
}

for (i in 1:iters){
  pillsB <- data.frame("X" = tsne_eval_B[[i]]$Y[,1], "Y" = tsne_eval_B[[i]]$Y[,2], "Site" = geo_out_BF$fac$Site)

  dfBO <- pillsB[pillsB$Site == "Botiqueria dels Moros",]
  dfMA <- pillsB[pillsB$Site == "Cingle del Mas Nou",]
  dfCO <- pillsB[pillsB$Site == "Cueva de la Cocina",]
  dfPO <- pillsB[pillsB$Site == "Pontet",]
  
  ## Group for pairwise
  dfBOMA <- rbind(dfBO,dfMA)
  dfBOCO <- rbind(dfBO,dfCO)
  dfBOPO <- rbind(dfBO,dfPO)
  dfMACO <- rbind(dfMA,dfCO)
  dfMAPO <- rbind(dfMA,dfPO)
  dfCOPO <- rbind(dfCO,dfPO)

  ## Do pairwise
  mBOMA <- manova(lm(cbind(X,Y) ~ Site, dfBOMA))
  mBOCO <- manova(lm(cbind(X,Y) ~ Site, dfBOCO))
  mBOPO <- manova(lm(cbind(X,Y) ~ Site, dfBOPO))
  mMACO <- manova(lm(cbind(X,Y) ~ Site, dfMACO))
  mMAPO <- manova(lm(cbind(X,Y) ~ Site, dfMAPO))
  mCOPO <- manova(lm(cbind(X,Y) ~ Site, dfCOPO))

  ## Results
  pillsB <- c(summary(mBOMA)$stats[1,2],
              summary(mBOCO)$stats[1,2],
              summary(mBOPO)$stats[1,2],
              summary(mMACO)$stats[1,2],
              summary(mMAPO)$stats[1,2],
              summary(mCOPO)$stats[1,2])
  
  names(pillsB) <- c("mBOMA","mBOCO","mBOPO","mMACO","mMAPO","mCOPO")
  
  tsne_eval_B[[i]]$pillai <- pillsB
}
