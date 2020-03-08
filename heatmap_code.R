library("gplots")
library("RColorBrewer")

setwd("/Users/oscar/Downloads/Lemmon-etal_ReinforcementPaper")

data <- read.table("HetProfile_0.5_16_Filtered2_pHet_NaN.txt")

dataRowMeans <- cbind(data, rowMeans(data, na.rm=T))
colnames(dataRowMeans)[ncol(dataRowMeans)] <- "RMean"
dataRowMeans <- dataRowMeans[order(dataRowMeans$RMean), ]
dataRowMeans <- dataRowMeans[ , 1:ncol(data)]

dataColMeans <- rbind(data, colMeans(data, na.rm=T))
rownames(dataColMeans)[nrow(dataColMeans)] <- "CMean"
dataColMeans <- dataColMeans[ , order(dataColMeans[nrow(dataColMeans),])]
dataColMeans <- dataColMeans[1:nrow(data), ]

dataBothMeans <- dataColMeans
dataBothMeans <- cbind(dataBothMeans, rowMeans(dataBothMeans , na.rm=T))
colnames(dataBothMeans)[ncol(dataBothMeans)] <- "RMean"
dataBothMeans <- dataBothMeans[order(dataBothMeans$RMean), ]
dataBothMeans <- dataBothMeans[ , 1:ncol(data)]

data <- as.matrix(data)
dataRowMeans <- as.matrix(dataRowMeans)
dataColMeans <- as.matrix(dataColMeans)
dataBothMeans <- as.matrix(dataBothMeans)

lmat <- rbind(c(0,0),c(0,1))
lwid <- c(0.1,10)
lhei <- c(0.1,10)

my_palette <- colorRampPalette(c("green", "black"))(n = 59)

#my_palette <- colorRampPalette(c("white", "green", "black"))(n = 59)

#col_breaks = c(seq(-1,-0.1,length=20), # for white (NAs)
#			seq(0,0.5,length=20),  # for green
#			seq(0.51,1,length=20)) # for black

pdf(file="HetProfile_0.5_16_Filtered_pHet.pdf", width = 400, height = 400, pointsize = 100)

#heatmap.2(data, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, breaks=col_breaks)

heatmap.2(data, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")

graphics.off()

pdf(file="HetProfile_0.5_16_Filtered_pHet_ROW.pdf", width = 400, height = 400, pointsize = 100)

#heatmap.2(dataRowMeans, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, breaks=col_breaks)

heatmap.2(dataRowMeans, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")

graphics.off()

pdf(file="HetProfile_0.5_16_Filtered_pHet_COL.pdf", width = 400, height = 400, pointsize = 100)

#heatmap.2(dataColMeans, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, breaks=col_breaks)

heatmap.2(dataColMeans, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")

graphics.off()

pdf(file="HetProfile_0.5_16_Filtered_pHet_BOTH.pdf", width = 400, height = 400, pointsize = 100)

#heatmap.2(dataBothMeans, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, breaks=col_breaks)

heatmap.2(dataBothMeans, Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")

graphics.off()
