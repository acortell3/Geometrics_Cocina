

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
#### This script reproduces the plots of the article. The objects must be retrieved from the scripts
#### Code_PhaseA.R, Code_PhaseB.R and Tactical_sim.R

library(colorBlindness)

#### Figure 3 (S1)
tiff(file="./Figures/Fig.3.1_SM.tiff", width = 15, height = 12, units = "in", res = 100)
par(mfrow=c(2,2))

plot_object1 <- results[[1]][[1]]
plot_object2 <- results[[1]][[2]]
plot_object3 <- results[[1]][[3]]

## Assign colorblind palettes
plot_object1$bg <- as.character(plot_object1$Type)
plot_object1$bg[plot_object1$bg == "Type1"] <- SteppedSequential5Steps[1]
plot_object1$bg[plot_object1$bg == "Type2"] <- SteppedSequential5Steps[6]
plot_object1$bg[plot_object1$bg == "Type3"] <- SteppedSequential5Steps[11]
plot_object1$bg[plot_object1$bg == "Type4"] <- SteppedSequential5Steps[16]

plot_object2$bg <- as.character(plot_object2$Group)
plot_object2$bg[plot_object2$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object2$bg[plot_object2$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object2$bg[plot_object2$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object2$bg[plot_object2$bg == "Group4"] <- SteppedSequential5Steps[16]

plot_object3$bg <- as.character(plot_object3$Group)
plot_object3$bg[plot_object3$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object3$bg[plot_object3$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object3$bg[plot_object3$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object3$bg[plot_object3$bg == "Group4"] <- SteppedSequential5Steps[16]
plot_object3$bg[plot_object3$bg == "Group5"] <- SteppedSequential5Steps[23]

## Plot 1
plot(x=plot_object1$X, y = plot_object1$Y, pch = "", main = "Original Types", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object1$Y)+max(plot_object1$Y))/2, col = "black")
abline(v = (min(plot_object1$X)+max(plot_object1$X))/2, col = "black")
points(x=plot_object1$X, y = plot_object1$Y, col = plot_object1$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 2
plot(x=plot_object2$X, y = plot_object2$Y, pch = "", main = "Groups previous to interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object2$Y)+max(plot_object2$Y))/2, col = "black")
abline(v = (min(plot_object2$X)+max(plot_object2$X))/2, col = "black")
points(x=plot_object2$X, y = plot_object2$Y, col = plot_object2$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 3
plot(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], pch = "", main = "Groups after interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], col = plot_object3$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 4
plot(x=plot_object3$X, y = plot_object3$Y, pch = "", main = "Aggregation", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X, y = plot_object3$Y, col = plot_object3$bg, bg = rgb(0,0,0,0.1), pch = 21, cex = 0.8)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("G1","G2","G3","G4","G5"), 
       col = c(SteppedSequential5Steps[1],SteppedSequential5Steps[6],SteppedSequential5Steps[11],SteppedSequential5Steps[16],SteppedSequential5Steps[23]),
       pch = c(16,16,16,16,16), xpd = TRUE, horiz = TRUE, cex = 0.8, seg.len=1, bty = 'n')

dev.off()

#### Figure 3 (S2)
tiff(file="./Figures/Fig.3.2_SM.tiff", width = 15, height = 12, units = "in", res = 100)
par(mfrow=c(2,2))

plot_object1 <- results[[2]][[1]]
plot_object2 <- results[[2]][[2]]
plot_object3 <- results[[2]][[3]]

## Assign colorblind palettes
plot_object1$bg <- as.character(plot_object1$Type)
plot_object1$bg[plot_object1$bg == "Type1"] <- SteppedSequential5Steps[1]
plot_object1$bg[plot_object1$bg == "Type2"] <- SteppedSequential5Steps[6]
plot_object1$bg[plot_object1$bg == "Type3"] <- SteppedSequential5Steps[11]
plot_object1$bg[plot_object1$bg == "Type4"] <- SteppedSequential5Steps[16]

plot_object2$bg <- as.character(plot_object2$Group)
plot_object2$bg[plot_object2$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object2$bg[plot_object2$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object2$bg[plot_object2$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object2$bg[plot_object2$bg == "Group4"] <- SteppedSequential5Steps[16]

plot_object3$bg <- as.character(plot_object3$Group)
plot_object3$bg[plot_object3$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object3$bg[plot_object3$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object3$bg[plot_object3$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object3$bg[plot_object3$bg == "Group4"] <- SteppedSequential5Steps[16]
plot_object3$bg[plot_object3$bg == "Group5"] <- SteppedSequential5Steps[23]

## Plot 1
plot(x=plot_object1$X, y = plot_object1$Y, pch = "", main = "Original Types", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object1$Y)+max(plot_object1$Y))/2, col = "black")
abline(v = (min(plot_object1$X)+max(plot_object1$X))/2, col = "black")
points(x=plot_object1$X, y = plot_object1$Y, col = plot_object1$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 2
plot(x=plot_object2$X, y = plot_object2$Y, pch = "", main = "Groups previous to interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object2$Y)+max(plot_object2$Y))/2, col = "black")
abline(v = (min(plot_object2$X)+max(plot_object2$X))/2, col = "black")
points(x=plot_object2$X, y = plot_object2$Y, col = plot_object2$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 3
plot(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], pch = "", main = "Groups after interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], col = plot_object3$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 4
plot(x=plot_object3$X, y = plot_object3$Y, pch = "", main = "Aggregation", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X, y = plot_object3$Y, col = plot_object3$bg, bg = rgb(0,0,0,0.1), pch = 21, cex = 0.8)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("G1","G2","G3","G4","G5"), 
       col = c(SteppedSequential5Steps[1],SteppedSequential5Steps[6],SteppedSequential5Steps[11],SteppedSequential5Steps[16],SteppedSequential5Steps[23]),
       pch = c(16,16,16,16,16), xpd = TRUE, horiz = TRUE, cex = 0.8, seg.len=1, bty = 'n')

dev.off()

#### Figure 3 (S3)
tiff(file="./Figures/Fig.3.3_SM.tiff", width = 15, height = 12, units = "in", res = 100)
par(mfrow=c(2,2))

plot_object1 <- results[[3]][[1]]
plot_object2 <- results[[3]][[2]]
plot_object3 <- results[[3]][[3]]

## Assign colorblind palettes
plot_object1$bg <- as.character(plot_object1$Type)
plot_object1$bg[plot_object1$bg == "Type1"] <- SteppedSequential5Steps[1]
plot_object1$bg[plot_object1$bg == "Type2"] <- SteppedSequential5Steps[6]
plot_object1$bg[plot_object1$bg == "Type3"] <- SteppedSequential5Steps[11]
plot_object1$bg[plot_object1$bg == "Type4"] <- SteppedSequential5Steps[16]

plot_object2$bg <- as.character(plot_object2$Group)
plot_object2$bg[plot_object2$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object2$bg[plot_object2$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object2$bg[plot_object2$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object2$bg[plot_object2$bg == "Group4"] <- SteppedSequential5Steps[16]

plot_object3$bg <- as.character(plot_object3$Group)
plot_object3$bg[plot_object3$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object3$bg[plot_object3$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object3$bg[plot_object3$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object3$bg[plot_object3$bg == "Group4"] <- SteppedSequential5Steps[16]
plot_object3$bg[plot_object3$bg == "Group5"] <- SteppedSequential5Steps[23]

## Plot 1
plot(x=plot_object1$X, y = plot_object1$Y, pch = "", main = "Original Types", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object1$Y)+max(plot_object1$Y))/2, col = "black")
abline(v = (min(plot_object1$X)+max(plot_object1$X))/2, col = "black")
points(x=plot_object1$X, y = plot_object1$Y, col = plot_object1$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 2
plot(x=plot_object2$X, y = plot_object2$Y, pch = "", main = "Groups previous to interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object2$Y)+max(plot_object2$Y))/2, col = "black")
abline(v = (min(plot_object2$X)+max(plot_object2$X))/2, col = "black")
points(x=plot_object2$X, y = plot_object2$Y, col = plot_object2$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 3
plot(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], pch = "", main = "Groups after interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], col = plot_object3$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 4
plot(x=plot_object3$X, y = plot_object3$Y, pch = "", main = "Aggregation", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X, y = plot_object3$Y, col = plot_object3$bg, bg = rgb(0,0,0,0.1), pch = 21, cex = 0.8)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("G1","G2","G3","G4","G5"), 
       col = c(SteppedSequential5Steps[1],SteppedSequential5Steps[6],SteppedSequential5Steps[11],SteppedSequential5Steps[16],SteppedSequential5Steps[23]),
       pch = c(16,16,16,16,16), xpd = TRUE, horiz = TRUE, cex = 0.8, seg.len=1, bty = 'n')

dev.off()

#### Figure 3 Main
tiff(file="./Figures/Fig.3.tiff", width = 15, height = 12, units = "in", res = 100)
par(mfrow=c(2,2))

plot_object1 <- results[[4]][[1]]
plot_object2 <- results[[4]][[2]]
plot_object3 <- results[[4]][[3]]

## Assign colorblind palettes
plot_object1$bg <- as.character(plot_object1$Type)
plot_object1$bg[plot_object1$bg == "Type1"] <- SteppedSequential5Steps[1]
plot_object1$bg[plot_object1$bg == "Type2"] <- SteppedSequential5Steps[6]
plot_object1$bg[plot_object1$bg == "Type3"] <- SteppedSequential5Steps[11]
plot_object1$bg[plot_object1$bg == "Type4"] <- SteppedSequential5Steps[16]

plot_object2$bg <- as.character(plot_object2$Group)
plot_object2$bg[plot_object2$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object2$bg[plot_object2$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object2$bg[plot_object2$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object2$bg[plot_object2$bg == "Group4"] <- SteppedSequential5Steps[16]

plot_object3$bg <- as.character(plot_object3$Group)
plot_object3$bg[plot_object3$bg == "Group1"] <- SteppedSequential5Steps[1]
plot_object3$bg[plot_object3$bg == "Group2"] <- SteppedSequential5Steps[6]
plot_object3$bg[plot_object3$bg == "Group3"] <- SteppedSequential5Steps[11]
plot_object3$bg[plot_object3$bg == "Group4"] <- SteppedSequential5Steps[16]
plot_object3$bg[plot_object3$bg == "Group5"] <- SteppedSequential5Steps[23]

## Plot 1
plot(x=plot_object1$X, y = plot_object1$Y, pch = "", main = "Original Types", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object1$Y)+max(plot_object1$Y))/2, col = "black")
abline(v = (min(plot_object1$X)+max(plot_object1$X))/2, col = "black")
points(x=plot_object1$X, y = plot_object1$Y, col = plot_object1$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 2
plot(x=plot_object2$X, y = plot_object2$Y, pch = "", main = "Groups previous to interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object2$Y)+max(plot_object2$Y))/2, col = "black")
abline(v = (min(plot_object2$X)+max(plot_object2$X))/2, col = "black")
points(x=plot_object2$X, y = plot_object2$Y, col = plot_object2$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 3
plot(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], pch = "", main = "Groups after interaction", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X[1:4000], y = plot_object3$Y[1:4000], col = plot_object3$bg, bg = rgb(0,0,0,0.3), pch = 21, cex = 0.8)

## Plot 4
plot(x=plot_object3$X, y = plot_object3$Y, pch = "", main = "Aggregation", xlab = "X", ylab = "Y")
grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
abline(h = (min(plot_object3$Y)+max(plot_object3$Y))/2, col = "black")
abline(v = (min(plot_object3$X)+max(plot_object3$X))/2, col = "black")
points(x=plot_object3$X, y = plot_object3$Y, col = plot_object3$bg, bg = rgb(0,0,0,0.1), pch = 21, cex = 0.8)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("G1","G2","G3","G4","G5"), 
       col = c(SteppedSequential5Steps[1],SteppedSequential5Steps[6],SteppedSequential5Steps[11],SteppedSequential5Steps[16],SteppedSequential5Steps[23]),
       pch = c(16,16,16,16,16), xpd = TRUE, horiz = TRUE, cex = 0.8, seg.len=1, bty = 'n')

dev.off()

#### Figure 4
tiff(file="./Figures/Fig.4.tiff", width = 15, height = 8, units = "in", res = 100)

plot(as.numeric(tact_pills[1,]), xaxt = "n", xlab = "Pairwise sites", ylab = "Pillai", pch = "", main = "Simulated Pillai", ylim = c(-0.01,0.7))
for (i in 1:ncol(tact_pills)){
  abline(v = i, col = "whitesmoke")
}

pal_col <- c(SteppedSequential5Steps[1],SteppedSequential5Steps[6],SteppedSequential5Steps[11],SteppedSequential5Steps[16])
col_lin <- c("pink", "lightgoldenrod", "mediumaquamarine", "lightblue")

for (i in 1:ncol(tact_pills)){
  lines(as.numeric(tact_pills[i,]), col = col_lin[i])
}

for (i in 1:ncol(tact_pills)){
  points(as.numeric(tact_pills[i,]), col = "black", bg = pal_col[i], pch = 21, lwd = 1)
}

axis(1,at=1:ncol(tact_pills), labels = colnames(tact_pills))
legend("topright", legend = c("P(t) = 0.4", "P(t) = 0.55", "P(t) = 0.7", "P(t) = 0.85"),
       col = pal_col, pch = 16)

dev.off()

#### Figure 5
tiff(file="./Figures/Fig.5.tiff", width = 15, height = 10, units = "in", res = 100)

par(mfrow = c(1,2))
hcontrib(geo_out_AF, col = "seagreen", harm.r = c(1:7))
text(x = 3, y = -0.5, "Phase A")
hcontrib(geo_out_BF, col = "seagreen", harm.r = c(1:7))
text(x = 3, y = -0.5, "Phase B")
dev.off()

#### Figure 6
tiff(file="./Figures/Fig.6.tiff", width = 15, height = 10, units = "in", res = 100)

par(mfrow = c(2,2))
plot_LDA(geo_out_A.lda, points=TRUE, chull = TRUE, zoom = 1, labelgroups = TRUE, morphospace = TRUE, palette = col_solarized,
        center_origin = TRUE, title = "Phase A", legend = FALSE)
plot_LDA(geo_out_B.lda, points=TRUE, chull = TRUE, zoom = 1.3, labelgroups = TRUE, morphospace = TRUE, palette = col_solarized,
         center_origin = TRUE, title = "Phase B", legend = FALSE)
plot_LDA(geo_out_A_NoCoc.lda, points=TRUE, chull = TRUE, zoom = 1, labelgroups = TRUE, morphospace = TRUE, palette = col_solarized,
         center_origin = TRUE, title = "Phase A", legend = FALSE)
plot_LDA(geo_out_B_NoCoc.lda, points=TRUE, chull = TRUE, zoom = 1.3, labelgroups = TRUE, morphospace = TRUE, palette = col_solarized,
         center_origin = TRUE, title = "Phase B", legend = FALSE)
dev.off()

#### Figure 7
library(cowplot)
tiff(file="./Figures/Fig.7.tiff", width = 12, height = 8, units = "in", res = 100)
ggdraw()+
  draw_plot(plot_CV(cvn40A, freq = TRUE, rm0 = TRUE, axis.size = 5), x = 0, y = 0.5, width = 0.5, height = 0.5)+
  draw_plot(plot_CV(cvn40B, freq = TRUE, rm0 = TRUE, axis.size = 5), x = 0.5, y = 0.5, width = 0.5, height = 0.5)+
  draw_plot(plot_CV(cvn40A_NoCoc, freq = TRUE, rm0 = TRUE, axis.size = 5), x = 0, y = 0, width = 0.5, height = 0.5)+
  draw_plot(plot_CV(cvn40B_NoCoc, freq = TRUE, rm0 = TRUE, axis.size = 5), x = 0.5, y = 0, width = 0.5, height = 0.5)+
  draw_plot_label(label = c("Phase A"), size = 10, x = 0, y = 0.95)+
  draw_plot_label(label = c("Phase B"), size = 10, x = 0.5, y = 0.95)
dev.off()

#### Figure 8
tiff(file="./Figures/Fig.8.tiff", width = 8, height = 8, units = "in", res = 100)

## Prepare data

pal_col <- c(SteppedSequential5Steps[1],SteppedSequential5Steps[6],SteppedSequential5Steps[11],
             SteppedSequential5Steps[16],SteppedSequential5Steps[21],"black")

A_Coc <- data.frame("X" = tsne_eval_A[[1]]$Y[,1], 
                    "Y" = tsne_eval_A[[1]]$Y[,2], 
                    "Site" = geo_out_AF$fac$Site)

A_Coc$bg <- as.character(A_Coc$Site)
A_Coc$bg[A_Coc$bg == "Abric de la Falguera"] <- pal_col[1]
A_Coc$bg[A_Coc$bg == "Baños de Ariño"] <- pal_col[6]
A_Coc$bg[A_Coc$bg == "Benàmer"] <- pal_col[2]
A_Coc$bg[A_Coc$bg == "Botiqueria dels Moros"] <- pal_col[5]
A_Coc$bg[A_Coc$bg == "Cueva de la Cocina"] <- pal_col[3]
A_Coc$bg[A_Coc$bg == "Pontet"] <- pal_col[4]

A_NoCoc <- A_Coc[A_Coc$Site != "Cueva de la Cocina",]

B_Coc <- data.frame("X" = tsne_eval_B[[1]]$Y[,1], 
                    "Y" = tsne_eval_B[[1]]$Y[,2], 
                    "Site" = geo_out_BF$fac$Site)

B_Coc$bg <- as.character(B_Coc$Site)
B_Coc$bg[B_Coc$bg == "Cingle del Mas Nou"] <- pal_col[1]
B_Coc$bg[B_Coc$bg == "Botiqueria dels Moros"] <- pal_col[5]
B_Coc$bg[B_Coc$bg == "Cueva de la Cocina"] <- pal_col[3]
B_Coc$bg[B_Coc$bg == "Pontet"] <- pal_col[4]

B_NoCoc <- B_Coc[B_Coc$Site != "Cueva de la Cocina",]

tsne_plots <- list(A_Coc, A_NoCoc, B_Coc, B_NoCoc)
mains <- c("Phase A", "Phase A without Cocina", "Phase B", "Phase B without Cocina")

## Do plots
par(mfrow = c(2,2))

for (i in 1:4){
  plobj <- tsne_plots[[i]]
  
  plot(x=plobj$X, y = plobj$Y, pch = "", main = mains[i], xlab = "X", ylab = "Y")
  grid(nx = NULL, ny = NULL, lty = 1, col = "whitesmoke", lwd = 1)
  abline(h = (min(plobj$Y)+max(plobj$Y))/2, col = "black")
  abline(v = (min(plobj$X)+max(plobj$X))/2, col = "black")
  points(x=plobj$X, y = plobj$Y, col = "white", bg = plobj$bg, pch = 21, cex = 1)
}

dev.off()

#### Figure 9
tiff(file="./Figures/Fig.9.tiff", width = 12, height = 4, units = "in", res = 100)
par(mfrow = c(1,2))
op <- par(cex = 0.5)

## Single Pillai

## a (Phase A)
plot(geo_out_Atab[,2], xaxt = "n", xlab = "Pairwise sites", ylab = "Pillai", pch = "", main = "Phase A", ylim = c(-0.01,0.4))
legend("topright", legend = c("LDA","tSNE"), col = c("aquamarine3","black"), pch = c(16,16))
for (i in 1:nrow(geo_out_Atab)){
  segments(i,geo_out_Atab[i,2],i,0, col = "aquamarine3")
}
points(geo_out_Atab[,2], col = "darkblue", bg = "aquamarine3", pch = 21, lwd = 1)

pil_A <- as.data.frame(matrix(nrow = iters, ncol = length(names(pillsA))))
colnames(pil_A) <- names(pillsA)
for (i in 1:iters){
  pil_A[i,] <- tsne_eval_A[[i]]$pillai
  points(as.numeric(pil_A[i,]), col = "red", pch = 21, cex = 0.7)
}

pil_Am <- apply(pil_A,2,mean)
points(pil_Am, col = "darkred", bg = "black", pch = 21, lwd = 1)

axis(1,at=1:nrow(geo_out_Atab), labels = rownames(geo_out_Atab))

# b (Phase B)

plot(geo_out_Btab[,2], xaxt = "n", xlab = "Pairwise sites", ylab = "Pillai", pch = "", main = "Phase B", ylim = c(-0.01,0.45))
legend("topright", legend = c("LDA","tSNE"), col = c("aquamarine3","black"), pch = c(16,16))
for (i in 1:nrow(geo_out_Btab)){
  segments(i,geo_out_Btab[i,2],i,0, col = "aquamarine3")
}
points(geo_out_Btab[,2], col = "darkblue", bg = "aquamarine3", pch = 21, lwd = 1)

pil_B <- as.data.frame(matrix(nrow = iters, ncol = length(names(pillsB))))
colnames(pil_B) <- names(pillsB)
for (i in 1:iters){
  pil_B[i,] <- tsne_eval_B[[i]]$pillai
  points(as.numeric(pil_B[i,]), col = "red", pch = 21, cex = 0.7)
}

pil_Bm <- apply(pil_B,2,mean)
points(pil_Bm, col = "darkred", bg = "black", pch = 21, lwd = 1)
axis(1,at=1:nrow(geo_out_Btab), labels = rownames(geo_out_Btab))

dev.off()
