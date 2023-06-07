##############Part 0: General information######################################
# S01_DataDemographics.R - This is a stand-alone script that starts from the 
# info file that was included in the original data. The goal is to get an 
# overview of the participant characteristics.
# 
# Written by Wout Van Echelpoel - Last update: May 2023
###############################################################################

##############Part 1: Set environment##########################################
###############Section 1: Define working folders###############################
s.input <- './Data/01 Raw data/'
s.output <- './Data/03 Analysed data/'
###############Section 2: Install required packages############################
# install.packages('ggplot2') # To create plots
# install.packages('ggpubr') # To arrange graphs together
###############Section 3: Load required packages###############################
library(ggplot2)
library(ggpubr)
###############Section 4: Define global variables##############################
b.save <- T

##############Part 2: Summary analysis#########################################
###############Section 1: Read data############################################
df.dat <- read.csv(file = paste0(s.input, 'D_Demographics.csv'))

# Data not available for sub-10524
df.dat <- df.dat[df.dat$sub_id != 'sub-10524', ]

###############Section 2: Numerical description################################
# Sex-ratio
100 * sum(df.dat$sex == 'Male') / nrow(df.dat)
100 * sum(df.dat$sex == 'Female') / nrow(df.dat)

# Group-specific Sex distribution
table(df.dat[, c('sex', 'group')])

# Overall Age
mean(df.dat$age); sd(df.dat$age)

# Group-specific Age description
aggregate(df.dat$age, list(df.dat$group), FUN = mean) 
aggregate(df.dat$age, list(df.dat$group), FUN = sd)

###############Section 3: Graphical display Age (and Sex)######################
# Create overview table
df.ageSex <- as.data.frame(table(df.dat[, c('age', 'sex')]))

# Create barplot of Age distribution
p.age <- ggplot(df.ageSex, aes(x = age, y = Freq)) + 
  geom_bar(stat = 'identity', fill = '#A6A6A6') + 
  coord_flip() + 
  scale_x_discrete('Age [-]') + 
  scale_y_continuous('No. of participants [-]') + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = 'black'))
p.age

# Create barplot of Age distribution per Sex category
p.ageSex <- ggplot(df.ageSex, aes(x = age, y = ifelse(test = sex == "Male",
                                                      yes = -Freq, no = Freq))) + 
  geom_bar(stat = "identity", aes(fill = sex)) +
  coord_flip() + 
  scale_x_discrete('Age [-]') + 
  scale_y_continuous('No. of participants [-]', labels = abs, 
                     limits = max(df.ageSex$Freq) * c(-1.25, 1.25)) +
  scale_fill_manual(values = c('#ED7D31', '#2D8CA8')) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = 'black'),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.85, 0.95))
p.ageSex

###############Section 4: Graphical display Group-specific#####################
# Add group label to data and factorise
df.dat$grplbl <- factor(c('HC', 'SCH', 'BD', 'ADHD')[df.dat$group],
                        levels = c('HC', 'SCH', 'BD', 'ADHD'))

# Create barplot with overview
p.grpAgeSex <- ggplot(as.data.frame(table(df.dat[, c('age', 'sex', 'grplbl')])), 
                     aes(x = age, y = Freq)) + 
  geom_bar(stat = 'identity', aes(fill = sex), position = 'dodge') + 
  geom_vline(xintercept = c(5.5, 10.5, 15.5, 20.5, 25.5), 
             linetype = 'dashed', colour = 'grey60') + 
  facet_wrap(~grplbl, nrow = 1) + 
  coord_flip() + 
  scale_x_discrete('Age [-]') + 
  scale_y_continuous('No. of participants [-]') + 
  scale_fill_manual(values = c('#ED7D31', '#2D8CA8')) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = 'black'),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.94, 0.97))
p.grpAgeSex

###############Section 5: Save data and graphs#################################
if(b.save){
  # Barplots Age distributions
  tiff(paste0(s.output, 'F_DemographicAgeSex.tiff'), 
       units = 'mm', width = 160, height = 120, res = 300, pointsize = 7)
  plot(ggarrange(p.age, p.ageSex, nrow = 1, align = 'h'))
  dev.off()
  
  # Barplots group-specific overview
  tiff(paste0(s.output, 'F_DemographicGroupAgeSex.tiff'), units = 'mm', width = 160, 
       height = 120, res = 300, pointsize = 7)
  plot(p.grpAgeSex)
  dev.off()
}

###############Section 6: Remove obsolete variables############################
rm(list = ls()) # Remove variables that are no longer needed


