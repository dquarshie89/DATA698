library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(readr)
library(datasets)
library(corrplot)
library(stats)
library(ggrepel)
library(textir) 
library(BBmisc)
library(rstudioapi)
library(caret)


#Load Data------
traditional_url <- 'https://raw.githubusercontent.com/dquarshie89/DATA698/master/2018%20nba%20stats.csv'
traditionalStats <- read.csv(traditional_url, header=TRUE)

advanced_url <- 'https://raw.githubusercontent.com/dquarshie89/DATA698/master/2018%20nba%20advanced.csv'
advancedStats <- read.csv(advanced_url, header=TRUE)

mergedData<-merge(traditionalStats,advancedStats)

misc_url <- 'https://raw.githubusercontent.com/dquarshie89/DATA698/master/2019%20nba%20misc.csv'
miscStats<- read.csv(misc_url, header=TRUE)

#Clean Data-----
mergedData$Player <- gsub(".", "", mergedData$Player, fixed = TRUE)
miscStats$Player <- gsub(".", "", miscStats$Player, fixed = TRUE)

mergedData$Player[!(mergedData$Player %in% miscStats$Player)]
miscStats$Player[!(miscStats$Player %in% mergedData$Player)]

#Merge Data-------
mergedData<-merge(mergedData,miscStats)

write.csv(mergedData, 'Final Merged.csv', row.names = FALSE)

final_url <- 'https://raw.githubusercontent.com/dquarshie89/DATA698/master/Final%20Merged.csv'
final <- read.csv(final_url, header=TRUE)

# MPG and Ast:TO------
final$MPG<-final$MP/final$G
final$A2TO<-final$AST/final$TOV

# Remove nulls
final[is.na(final)] <- 0 

# Filter to 30 Minutes per game and over 10 games played------
final <- subset(final, MPG >=30 )
final <- subset(final, G >=10 )


# Get only needed fields
mainData<- final[c(9:55)] #relevant overall features

# Normalize data
mainData <- normalize(mainData, method= "standardize") 

# Feature Correlation plot
corrplot(cor(mainData),method='color',tl.cex=.5)  

# PCA ----

res.cov <- cov(mainData)
round(res.cov,2)

#Eigenvectors
eig<-eigen(res.cov)$vectors
eigenv2<-eig[,(1:2)]

# PCA matrix
statsMatrix<-data.matrix(mainData, rownames.force = NA)
PrincipalComponents2<-statsMatrix%*%eigenv2

# Make datafrome
statsDF<-data.frame(PrincipalComponents2)

# PCA Plot
ggplot(statsDF,aes(statsDF$X1, statsDF$X2)) +
  labs(x="PC1",y="PC2")+
  geom_point(data=final,aes(col =Pos, size= VORP))+
  geom_text_repel(data=final, aes(label=Player), size=2+final$VORP/max(final$VORP))

#KMeans ----

#Add player name
row.names(mainData) <- make.names(final$Player, unique=TRUE)

#Euclidean Dist
res.dist <- get_dist(mainData, stand = TRUE, method = "euclidean")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Gap Stat
fviz_nbclust(mainData, kmeans, method = "gap_stat")

# Kmeans based on best cluster-number
km.res <- kmeans(mainData,6, nstart = 25)

# Visualize Kmeans clusters
fviz_cluster(km.res, mainData, ellipse = TRUE, ellipse.alpha= 0.05,
             palette = "jco",repel = TRUE, ggtheme = theme_minimal(), 
             main= FALSE, xlab= FALSE, ylab = FALSE) 

# Further cluster analysis
Clusters=data.frame(sort(km.res$cluster))
km.res


write.csv(Clusters, file="kmeans.csv")

#HCA-----

res.hc <- hclust(res.dist, method = "ward.D2" )

#Viz
fviz_dend(res.hc, k = 6,
          cex = 0.5,
          horiz= TRUE, rect = TRUE 
)

#NBA and NCAA-------------

#top 20 NBA player by VORP-------
final_pts <- head(arrange(final,desc(VORP)), n = 20)

final_nba <- final_pts  %>% 
  dplyr::select(Player, FG,	FGA,	FG.,	X3P,	X3PA,	X3P.,	FT,	FTA,	FT.,	ORB,	DRB,
         TRB,	AST,	STL,	BLK,	TOV,	PF,	PTS)

#Top 20 NCAA players by PPG
ncaa_url <- 'https://raw.githubusercontent.com/dquarshie89/DATA698/master/2018%20ncaa%20stats.csv'
ncaa <- read.csv(ncaa_url, header=TRUE)  

#Merged
both_leagues <- rbind(final_nba, ncaa)

#Get needed fields
both_main <- both_leagues[c(2:14)]

#Normalize
both_main<- normalize(both_main, method= "standardize") 

#Add names
row.names(both_main) <- make.names(both_leagues$Player, unique=TRUE)

#HCA NCAA-----
both.dist <- get_dist(both_main, stand = TRUE, method = "euclidean")

both.hc <- hclust(both.dist, method = "ward.D2" )

#Viz
fviz_dend(both.hc, k = 6, 
          cex = 0.5, 
          horiz= TRUE, rect = TRUE 
)

write.csv(both_leagues, 'nba_ncaa.csv', row.names = FALSE)
