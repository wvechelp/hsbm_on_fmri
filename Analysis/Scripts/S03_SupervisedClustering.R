##############Part 0: General information######################################
# S03_SupervisedClustering.R - This scripts builds further on the data that is
# summarised in the Python notebook (S02_DataSelection.ipynb). The goal is to 
# perform conventional clustering/analysis on the complete data to get an 
# overview of the potential clustering (i.e. comparison purposes).
# 
# Written by Wout Van Echelpoel - Last update: May 2023
###############################################################################

##############Part 1: Set environment##########################################
###############Section 1: Define working folders###############################
s.input <- './Data/02 Cleaned data/'
s.output <- './Data/03 Analysed data/'
###############Section 2: Install required packages############################
# install.packages('ggplot2') # To create plots
# install.packages('ggpubr') # To arrange graphs together
# install.packages('patchwork') # To arrange different graphs together
# install.packages('dendextend') # To create hierarchies
# install.packages('aricode') # To determine NMI score
###############Section 3: Load required packages###############################
library(ggplot2)
library(ggpubr)
library(patchwork)
library(dendextend)
library(aricode)
###############Section 4: Define global variables##############################
v.colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7") # Category colors
b.save <- T

##############Part 2: Description missing data#################################
###############Section 1: Read data############################################
df.dat <- read.table(paste0(s.input, 'D_PearsonCoefficient.txt'), header = T,
                     row.names = 1, sep = ';', stringsAsFactors = F)

###############Section 2: Rearrange data#######################################
# Convert to binary data frame (1 if missing) and 1D-sums
df.na <- as.data.frame(ifelse(is.na(df.dat), 1, NA))
v.part <- rowSums(df.na, na.rm = T)
v.pair <- colSums(df.na, na.rm = T)

# Extract those rows and columns with missing data (-> reduced data)
df.naR <- df.na[v.part > 0, v.pair > 0]
v.partR <- v.part[v.part > 0]
v.pairR <- v.pair[v.pair > 0]

# Stack dataframe for scatterplot purposes and add labels
df.naL <- stack(df.naR)
df.naL$patient <- row.names(df.naR)
df.naL$ind <- as.numeric(gsub('Pair', '', df.naL$ind))
df.naL <- df.naL[!is.na(df.naL$values), ]

# Make dataframes for 1D histograms
df.partR <- data.frame(v.partR); names(df.partR) <- 'participant'
df.pairR <- data.frame(v.pairR); names(df.pairR) <- 'ROI-pair'
df.pairR$id <- as.numeric(gsub('Pair', '', row.names(df.pairR)))

###############Section 3: Graphical display####################################
# Scatterplot
p.partPair <- ggplot(df.naL, aes(x = patient, y = ind)) + 
  geom_point(size = 0.1, colour = '#2D8CA8') + 
  scale_x_discrete('Participant ID') + 
  scale_y_continuous('ROI-pair ID [-]') + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p.partPair

# Histogram participants
p.part <- ggplot(df.partR, aes(x = row.names(df.partR), y = participant)) + 
  geom_col(width = 0.5, fill = '#2D8CA8') + 
  scale_y_continuous('Amount [-]', breaks = c(0, 2000, 4000)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
p.part

# Histogram ROI-pairs
p.pair <- ggplot(df.pairR, aes(x = id, y = `ROI-pair`)) + 
  geom_col(fill = '#2D8CA8') + 
  coord_flip() + 
  scale_y_continuous('Amount [-]') + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())
p.pair

# Combined display (spacer for white space top-right)
p.miss <- p.part + plot_spacer() + p.partPair + p.pair + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4) ) 
p.miss

###############Section 4: Save data and graphs#################################
if(b.save){
  # Overview missing data
  tiff(paste0(s.output, 'F_MissingData.tiff'), units = 'mm', width = 160, 
       height = 150, res = 300, pointsize = 7)
  plot(p.miss)
  dev.off()
}

###############Section 5: Remove obsolete variables############################
rm(list = setdiff(ls(), c('b.save', 's.input', 's.output', 'v.colors')))

##############Part 3: Group-comparison#########################################
###############Section 1: Read data############################################
df.mns <- t(read.table(paste0(s.input, 'D_MeanPerCategory.txt'), header = T, 
                       row.names = 1, sep = ';', stringsAsFactors = F))

# Add names and define labels
df.mns <- as.data.frame(df.mns)
names(df.mns) <- c('HC', 'SCH', 'BD', 'ADHD')
v.lbl <- c('Healthy control (HC)', 'Schizophrenia (SCH)',
           'Bipolar disorder (BD)', 'Attention-deficit/Hyperactivity disorder (ADHD)')

###############Section 2: Rearrange data#######################################
# Generate contrast-dataframes per group
l.mns <- list()
n.cnt <- 1
for (i in c(1:3)){
  for (j in c((i+1):4)){
    df.tmp <- df.mns[, c(i, j)]
    df.tmp$contrast <- paste0(names(df.tmp)[1], ' vs. ', names(df.tmp)[2])
    names(df.tmp)[1:2] <- c('x', 'y')
    l.mns[[n.cnt]] <- df.tmp
    n.cnt <- n.cnt + 1
  }
}

# Bring data together and check contrasts
df.mnsL <- do.call('rbind', l.mns)
unique(df.mnsL$contrast)

###############Section 3: Visualise data#######################################
# Turn contrasts into factors (visualisation purposes)
df.mnsL$contrast <- factor(df.mnsL$contrast, levels = unique(df.mnsL$contrast))

# Create scatterplot with additional reference lines
p.mns <- ggplot(df.mnsL, aes(x = x, y = y)) + 
  geom_abline(slope = 1, intercept = 0, colour = 'grey60') +
  geom_abline(slope = 1, intercept = c(-0.10, 0.10), colour = 'grey60', linetype = 'dashed') +
  geom_abline(slope = 1, intercept = c(-0.25, 0.25), colour = 'grey60', linetype = 'dotted') +
  geom_point() + 
  facet_wrap(~contrast, ncol = 3) + 
  scale_x_continuous(limits = c(-0.5, 1)) + 
  scale_y_continuous(limits = c(-0.5, 1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'))
p.mns

# Create violin plots, with boxplots and jittered scatter
# Data is subsampled randomly to avoid overloading the graph
set.seed(621)
p.box <- ggplot(stack(df.mns), aes(x = ind, y = values)) + 
  geom_jitter(data = stack(df.mns[runif(1000, min = 0, max = nrow(df.mns)), ]), 
              width = 0.05, size = 1) + 
  geom_violin(aes(fill = ind), trim = F, alpha = 0.5) + 
  geom_boxplot(alpha = 0, width = 0.4) + 
  scale_y_continuous('Mean Pearson coefficient [-]', limits = c(-0.5, 1)) + 
  scale_fill_manual(values = v.colors) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        axis.title.x = element_blank(),
        legend.position = 'none')
p.box

###############Section 4: Save data and graphs#################################
if(b.save){
  # Scatterplot per contrast
  tiff(paste0(s.output, 'F_GroupContrastsScatter.tiff'), 
       units = 'mm', width = 160, height = 120, res = 300, pointsize = 7)
  plot(p.mns)
  dev.off()
  # Violin plots
  tiff(paste0(s.output, 'F_GroupContrastsViolin.tiff'), 
       units = 'mm', width = 160, height = 60, res = 300, pointsize = 7)
  plot(p.box)
  dev.off()
}
###############Section 5: Remove obsolete variables############################
rm(list = setdiff(ls(), c('b.save', 's.input', 's.output', 'v.colors')))

##############Part 4: Principal component (PCA)################################
###############Section 1: Read data############################################
df.dat <- read.table(paste0(s.input, 'D_PearsonCoefficient_NoNa.txt'), 
                     header = T, row.names = 1, sep = ';', stringsAsFactors = F)

###############Section 2: Perform PCA##########################################
# Perform PCA with centering and scaling
pca.dat <- prcomp(df.dat, center = T, scale. = T, rank. = 2)

# Check explained variance
(pca.dat$sdev^2 / sum(pca.dat$sdev^2))[1:3]

# Extract new scores and add factorised class label
df.pca <- as.data.frame(pca.dat$x)
df.pca$class <- c(rep('HC', 120), rep('SCH', 50), rep('BD', 49), rep('ADHD', 40))

###############Section 3: Visualise data#######################################
# Turn class into factors (visualisation purposes)
df.pca$class <- factor(df.pca$class, c('HC', 'SCH', 'BD', 'ADHD'))

# Create scatterplot with additional reference lines
p.pca <- ggplot(df.pca, aes(x = df.pca[, 1], y = df.pca[, 2])) + 
  geom_vline(xintercept = 0, linetype = 'dotted', colour = 'grey80') + 
  geom_hline(yintercept = 0, linetype = 'dotted', colour = 'grey80') + 
  geom_point(aes(colour = class)) + 
  scale_x_continuous('PC1 (21.0%)', limits = c(-260, 260)) + 
  scale_y_continuous('PC2 (4.1%)', limits = c(-120, 120)) + 
  scale_colour_manual(values = v.colors) + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black'),
        legend.title = element_blank(),
        legend.position = c(0.875, 0.90),
        legend.background = element_blank())
p.pca

###############Section 4: Save data and graphs#################################
if(b.save){
  # PCA-biplot
  tiff(paste0(s.output, 'F_PCACompletePearson.tiff'), 
       units = 'mm', width = 160, height = 105, res = 300, pointsize = 7)
  plot(p.pca)
  dev.off()
}
###############Section 5: Remove obsolete variables############################
rm(list = setdiff(ls(), c('b.save', 's.input', 's.output', 'v.colors')))

##############Part 5: Hierarchical clustering##################################
###############Section 1: Read data############################################
df.dat <- read.table(paste0(s.input, 'D_PearsonCoefficient_NoNa.txt'), 
                     header = T, row.names = 1, sep = ';', stringsAsFactors = F)

###############Section 2: Perform Clustering###################################
# Calculate distance matrix (option Euclidean)
m.dist <- dist(df.dat, method = 'euclidean')

# Perform clustering (Ward.D2) and extract cluster membership for 4 categories
o.clust <- hclust(m.dist, method = 'ward.D2')
v.clust <- cutree(o.clust, k = 4)

# Contrast with original categories
df.chck <- data.frame(class = c(rep('HC', 120), rep('SCH', 50), rep('BD', 49), 
                                rep('ADHD', 40)), cluster = v.clust)

# Generate quick overview: NMI score and distribution
n.nmi <- NMI(df.chck$class, df.chck$cluster, variant = 'sqrt')
table(df.chck)

# Comparison NMI with null model (shuffling obtained clusters)
v.null <- c()
for (i in c(1:10)){
  set.seed(i)
  v.null[i] <- NMI(sample(df.chck$cluster, replace = F), df.chck$class, variant = 'sqrt')
}
print(paste0('NMI: ', n.nmi)); print(paste0('NMIr: ', n.nmi / mean(v.null)))

###############Section 3: Visualise data#######################################
# Turn clustering into dendrogram
o.dend <- as.dendrogram(o.clust)

# Define vector with colors
v.grp <- c(rep(v.colors[1], 120), rep(v.colors[2], 50), 
           rep(v.colors[3], 49), rep(v.colors[4], 40))

# Create tree with colored differences
o.dendCol <- color_branches(o.dend, value = c('green', 'pink', 'brown', 'grey'), k = 4)
o.dend %>% 
  set("labels", "") %>% 
  set("branches_k_color", value = c('#702E52', '#FEBE28', '#B5C266', '#F2503B'), k = 4) %>% 
  plot

# Add bar with original categories
colored_bars(colors = data.frame(Group = v.grp), dend = o.dendCol, y_shift = -5)

# Create barplot to visualise distribution
df.chck$class <- factor(df.chck$class, c('HC', 'SCH', 'BD', 'ADHD'))
p.ClustBar <- ggplot(as.data.frame(table(df.chck)), aes(x = cluster, y = Freq)) + 
  geom_col(aes(colour = class, fill = class), width = 0.8) + 
  scale_x_discrete('Cluster') + 
  scale_y_continuous('Frequency [-]', limits = c(0, 90)) + 
  scale_colour_manual(values = v.colors) +
  scale_fill_manual(values = v.colors) + 
  coord_flip() + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black', size = 7),
        axis.title = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        legend.position = c(0.9, 0.8))
p.ClustBar

###############Section 4: Save data and graphs#################################
if(b.save){
  # Dendrogram
  tiff(paste0(s.output, 'F_HierarchicalCompletePearson.tiff'), 
       units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
  o.dend %>% 
    set("labels", "") %>% 
    set("branches_k_color", value = c('#702E52', '#FEBE28', '#B5C266', '#F2503B'), k = 4) %>% 
    plot
  colored_bars(colors = data.frame(Group = v.grp), dend = o.dendCol, y_shift = -10,
               y_scale = 20)
  dev.off()
  # Barplot
  tiff(paste0(s.output, 'F_ClustersBarplot.tiff'), 
       units = 'mm', width = 75, height = 45, res = 300, pointsize = 7)
  plot(p.ClustBar)
  dev.off()
}
###############Section 5: Remove obsolete variables############################
rm(list = ls())
