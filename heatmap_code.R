library("tidyverse")
library("gplots")
library("RColorBrewer")

# Change the filepath of the tab-separated data
df_fpath <- "./HetProfile_0.5_16_Filtered_pHet.txt"

# Read the file containing the data. Convert first column to row names.
df <- read_delim(df_fpath, delim="\t")
df <- column_to_rownames(df, var = "X1")

# Convert -1.0 values to NAs.
df[df == -1] <- NA

# Calculate row means and attach to data frame to use for sorting.
df_SortRows <- df
df_SortRows$row_means <- rowMeans(df_SortRows, na.rm=T)
# write.table(df_SortRows, "/Users/OSCAREOSPINATOBON/Desktop/tableRows.csv", sep=',', quote=F)
df_SortRows <- df_SortRows[order(df_SortRows$row_means), ]
# write.table(df_SortRows, "/Users/OSCAREOSPINATOBON/Desktop/tableRows2.csv", sep=',', quote=F)

# Calculate column means and attach to data frame to use for sorting.
df_SortCols <- df
df_SortCols <- rbind(df_SortCols, colMeans(df_SortCols, na.rm=T))
rownames(df_SortCols)[length(rownames(df_SortCols))] <- "col_means"
# write.table(df_SortCols, "/Users/OSCAREOSPINATOBON/Desktop/tableCols.csv", sep=',', quote=F)
df_SortCols <- df_SortCols[, order(df_SortCols[nrow(df_SortCols), ])]
# write.table(df_SortCols, "/Users/OSCAREOSPINATOBON/Desktop/tableCols2.csv", sep=',', quote=F)

# Calculate column and row means and attach to data frame to use for sorting.
df_SortTwoWay <- df
df_SortTwoWay$row_means <- rowMeans(df_SortTwoWay, na.rm=T)
df_SortTwoWay <- rbind(df_SortTwoWay, colMeans(df_SortTwoWay, na.rm=T))
rownames(df_SortTwoWay)[length(rownames(df_SortTwoWay))] <- "col_means"
# write.table(df_SortTwoWay, "/Users/OSCAREOSPINATOBON/Desktop/tableBoth.csv", sep=',', quote=F)
df_SortTwoWay <- df_SortTwoWay[, order(df_SortTwoWay[nrow(df_SortTwoWay), ])]
df_SortTwoWay <- df_SortTwoWay[order(df_SortTwoWay[, ncol(df_SortTwoWay)]), ]
# write.table(df_SortTwoWay, "/Users/OSCAREOSPINATOBON/Desktop/tableBoth2.csv", sep=',', quote=F)

# Set graphical parameters for heatmaps.
lmat <- rbind(c(0,0),c(0,1))
lwid <- c(0.1,10)
lhei <- c(0.1,10)
my_palette <- colorRampPalette(c("green", "black"))(n = 59)

pdf(file="./HetProfile_0.5_16_Filtered_pHet_SortRows.pdf", width = 400, height = 400, pointsize = 100)
heatmap.2(data.matrix(df_SortRows), Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")
graphics.off()

pdf(file="HetProfile_0.5_16_Filtered_pHet_SortCols.pdf", width = 400, height = 400, pointsize = 100)
heatmap.2(data.matrix(df_SortCols), Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")
graphics.off()

pdf(file="HetProfile_0.5_16_Filtered_pHet_SortBoth.pdf", width = 400, height = 400, pointsize = 100)
heatmap.2(data.matrix(df_SortTwoWay), Colv=F, Rowv=F, key=F, dendrogram="none", col=my_palette, lmat=lmat, lwid=lwid, lhei=lhei, mar=c(3, 3), trace="none", offsetRow=0.1, offsetCol=0.1, na.color="white")
graphics.off()
