##############Part 0: General information######################################
# S05_GraphsMembership.R - This scripts builds further on the results that have
# been obtained through the Python notebook used for the hSBM inference
# (S04_HSBMInference.ipynb). The goal is to display the obtained membership
# distributions with respect to their original categories.
# 
# Written by Wout Van Echelpoel - Last update: May 2023
###############################################################################

##############Part 1: Set environment##########################################
###############Section 1: Define working folders###############################
s.input <- './Data/03 Analysed data/MembershipData/'
s.output <- './Data/03 Analysed data/MembershipGraphs/'
###############Section 2: Install required packages############################
# install.packages('ggplot2') # To create plots
###############Section 3: Load required packages###############################
library(ggplot2)
###############Section 4: Define global variables##############################
v.colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7") # Category colors
b.save <- T

##############Part 2: Visualise memberships####################################
###############Section 1: Read data############################################
# List all files in folder
v.files <- list.files(s.input)

# Read data into list
lst.dat <- list()
for (i in c(1:length(v.files))){
  lst.dat[[i]] <- read.table(paste0(s.input, v.files[i]), header = F,
                             sep = ' ', stringsAsFactors = F)
  names(lst.dat[[i]]) <- c(1:ncol(lst.dat[[i]]))
}

###############Section 2: Rearrange data#######################################
# Perform for every individual data set
lst.datL <- list()
for (i in c(1:length(lst.dat))){
  # Assign to data frame
  df.dat <- lst.dat[[i]]
  
  # Derive number of clusters (rows = cluster; cols = participants)
  n.cl <- nrow(df.dat)
  
  # Stack data to 1D dataframe and add cluster information
  df.datL <- stack(df.dat)
  df.datL$cluster <- rep(c(1:nrow(df.dat)), ncol(df.dat))
  
  # Add original category as contrast
  if (grepl('HC', v.files[i])){
    df.datL$class <- factor(df.datL$values * c(rep(1, 120 * n.cl)))
  } else if (grepl('SCH', v.files[i])){
    df.datL$class <- factor(df.datL$values * c(rep(2, 50 * n.cl)))
  } else if (grepl('BD', v.files[i])){
    df.datL$class <- factor(df.datL$values * c(rep(3, 49 * n.cl)))
  } else if (grepl('ADHD', v.files[i])){
    df.datL$class <- factor(df.datL$values * c(rep(4, 40 * n.cl)))
  } else {
    df.datL$class <- factor(df.datL$values * c(rep(1, 120 * n.cl), 
                                               rep(2, 50  *n.cl), 
                                               rep(3, 49 * n.cl), 
                                               rep(4, 40 * n.cl)))
  }
  
  # Store new data back in (different) list
  lst.datL[[i]] <- df.datL
}

###############Section 3: Graphical display####################################
# Perform for every individual data set
lst.plt <- list()
for (i in c(1:length(lst.datL))){
  # Assign to data frame
  df.datL <- lst.datL[[i]]
  
  # Define the colors to be used
  v.indxCol <- as.numeric(levels(df.datL$class)) + 1
  
  # Define the breaks to be used
  if (grepl('HC', v.files[i])){
    v.breaks <- seq(0, 150, 50)
  } else if (grepl('SCH', v.files[i]) | grepl('BD', v.files[i]) | grepl('ADHD', v.files[i])){
    v.breaks <- seq(0, 50, 10)
  } else {
    v.breaks <- seq(0, 260, 50)
  }
  
  # Scatterplot
  p.clust <- ggplot(df.datL, aes(x = ind, y = cluster)) + 
    geom_tile(aes(fill = class)) + 
    scale_fill_manual(values = c('white', v.colors), 
                      breaks = c('0', '1', '2', '3', '4')) + 
    scale_x_discrete('Participant p (index)', breaks = v.breaks) + 
    scale_y_continuous('Participant cluster', 
                       breaks = seq(1, max(df.datL$cluster), by = 1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 8),
          legend.position = 'none')
  plot(p.clust)
  
  # Store in list
  lst.plt[[i]] <- p.clust
}

###############Section 4: Save data and graphs#################################
if(b.save){
  # Perform for every graph
  for (i in c(1:length(lst.plt))){
    # Membership distribution
    tiff(paste0(s.output, gsub('D_M', 'F_M', gsub('.txt', '.tiff', v.files[i]))), 
         units = 'mm', width = 75, height = 60, res = 300, pointsize = 7)
    plot(lst.plt[[i]])
    dev.off()
  }
}

###############Section 5: Remove obsolete variables############################
rm(list = setdiff(ls(), c('b.save', 'v.colors')))