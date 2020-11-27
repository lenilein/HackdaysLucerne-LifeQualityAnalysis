library("stats")
library("dplyr")
library("ggplot2")
library("ggfortify")

df <- read_csv2("cleaned_data.csv")

#----------------------------------------------------
#preprocessing to this specific model
#----------------------------------------------------

colnames(df)

#drop
drop2 <- c("lfdn", "v_238", "v_211", "quality", "v_360", "v_205" , "v_290", "v_88", "v_363")
df2 <- dplyr::select(df, -drop2)

#remove rows where survey was not completes
df2 <- subset(df2, df2$v_321 != -77)

#substitute value
df2$v_83[df2$v_83 == -77] <- 0
df2$v_84[df2$v_84 == -77] <- 0
df2$v_85[df2$v_85 == -77] <- 0
df2$v_86[df2$v_86 == -77] <- 0
df2$v_87[df2$v_87 == -77] <- 0
df2$v_104[df2$v_104 == -77] <- 0
df2$v_105[df2$v_105 == -77] <- 0

#drop columns where not enough answers provided
drop3 <- c("v_73", "v_74", "v_75", "v_152", "v_153", "v_154", "v_269", "v_274", "v_275", "ArbeitsplatzLuzern", "v_89", "v_90")
df2 <- dplyr::select(df2, -drop3)

#drop columns because doesnt work in PCA
drop4 <- c("lastpage", "duration", "X1")
df2 <- dplyr::select(df2, -drop4)

#----------------------------------------------------
#PCA
#----------------------------------------------------

library("MASS")
pca <- princomp(df2, scores=TRUE)
plot(pca,main="scree plot")
summary(pca)


#----------------------------------------------------
#clustering with k-means
#----------------------------------------------------

## Scale data
df2_scaled <- scale(df2, center = FALSE, scale = TRUE)
boxplot(df2_scaled)

## Distance matrix (used below)
dp <- dist(df2_scaled)

## Choose number of clusters k with scree plot
wss <- rep(0, 20)
for (i in 1:20) wss[i] <- sum(kmeans(df2, centers = i, nstart = 20)$withinss)
par(mfrow = c(1,1))
plot(1:20, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares") ## 3 centers was a good choice
## Note: If nstart=1 is used, the results can vary because of random starting configurations in kmeans


## k-means 
ckm <- kmeans(df2, centers = 5, nstart = 10)
grpsKM <- ckm$cluster


## Silhouette Plot -> not working
library(cluster)
plot(silhouette(grpsKM, dp))

##vizualize in Principal components

pc <- princomp(df2)
summary(pc)
par(mfrow = c(1,3))

# visualize in PC 1 & 2
pr <- pc$scores[,1:2]
plot(pr, pch = grpsKM, col=grpsKM, lwd=2)
legend("bottomright", legend = 1:5, pch = 1:5, col=1:5, bty="n")
title(main = "PC1 vs PC2")

# visualize in PC  2 & 3
pr <- pc$scores[,2:3]
plot(pr, pch = grpsKM, col=grpsKM, lwd=2)
legend("bottomright", legend = 1:5, pch = 1:5, col=1:5, bty="n")
title(main = "PC2 vs PC3")

# visualize in PC  1 & 3
pr <- cbind("Comp.1" = pc$scores[,1], "Comp.3" = pc$scores[,3])
plot(pr, pch = grpsKM, col=grpsKM, lwd=2)
legend("bottomright", legend = 1:5, pch = 1:5, col=1:5, bty="n")
title(main = "PC1 vs PC3")

##interpret results

df.means <- aggregate(df2, by=list(cluster=grpsKM), mean)
df.means
plot(df.means)

df.var <- data.frame(sapply(df.means, var))
df.var <- df.var[order(-df.var$sapply.df.means..var.),,drop=FALSE]

print(df.var, row.names=TRUE)


df.means.labeled <- df.means %>% rename(Anbindung_Velo_ÖV = v_323,  
                        Anbindung_Velo_Bus = v_325,
                        Anbindung_Velo_Bahn = v_324,
                        Möglichkeit_Einbringen_städt_Projekte = v_366,
                        Zuzug = Zuzug,
                        Möglichkeit_Einbringen_städt_Ebene = v_106,
                        Möglichkeit_Vorschlagen_Projekte = v_367,
                        Medizinische_Betreuung_Zuhause = v_294,
                        Möglichkeiten_bekannt_städt_Projekte = v_212,
                        Möglichkeit_bekannt__städt_Ebene =v_316,
                        Möglichkeit_Vorschlagen_städt_Projekte = v_365,
                        Kommunikationskanal_Tagesmedien = v_245,
                        Optimierung_Digitale_Abwicklung = v_361,
                        Altergruppe = Altergruppe,
                        Veloparkplatz_zu_weng = v_27,
                        Optimierung_Verbindung_Arbeit_Schule = v_53,
                        Kulturangebote_Vereinsleben = v_305,
                        Kulturangebote_Kinder_Jugendliche = v_307,
                        Engagement_Gestaltung_städt_Leben = v_311,
                        Kommunikationskanal_Soziale_Meden = v_246)


selection = c("Zuzug",
              "Altergruppe",
              "Lebensqualallg",
              "Bildung",
              "Kinder_Schule",
              "Beziehungstatus",
              "HH",
              "Erwerb",
              "Anbindung_Velo_ÖV",  
              "Anbindung_Velo_Bus",
              "Anbindung_Velo_Bahn",
              "Veloparkplatz_zu_weng",
              "Möglichkeit_Einbringen_städt_Projekte",
              "Möglichkeit_Einbringen_städt_Ebene",
              "Möglichkeit_Vorschlagen_Projekte",
              "Möglichkeiten_bekannt_städt_Projekte",
              "Möglichkeit_bekannt__städt_Ebene",
              "Möglichkeit_Vorschlagen_städt_Projekte",
              "Engagement_Gestaltung_städt_Leben",
              "Kulturangebote_Vereinsleben",
              "Kulturangebote_Kinder_Jugendliche",
              "Kommunikationskanal_Tagesmedien",
              "Kommunikationskanal_Soziale_Meden",
              "Optimierung_Verbindung_Arbeit_Schule",
              "Optimierung_Digitale_Abwicklung",
              "Medizinische_Betreuung_Zuhause")

df.selected <- dplyr::select(df.means.labeled, selection)

#myplot <- ggplot(data=df.selected, 
#       mapping = aes(x= col.names(df.selected)),
#                     group = as.numeric(row.names(df.selected)))

#myplot


