# We'll start by loading the packages we need.

library(tidyverse) #load the tidyverse packages
library(psych) #load the psych packages for generating descriptives
library(yarrr) #load yarr for pirate plots
library(afex) #load afex for running factorial ANOVA
library(DescTools) #load DescTools for calculating effect sizes
library(emmeans) #load emmeans for running pairwise comparisons

#ANOVA - one factor, three levels between participants design
#the following code creates our data
set.seed(1234)
x1 <- rnorm(n = 15, mean = 5.3, sd = .4)
x2 <- rnorm(n = 15, mean = 7.1, sd = .45)
x3 <- rnorm(n = 15, mean = 9.2, sd = .5)
x4 <- (1:45)
x5 <- c (x1, x2, x3)
x6 <- c(rep("Water", 15), rep("Single Espresson", 15), rep("Double Espresso", 15))

colnames (condc) <- c("Participant", "Condition", "Ability")

cond <- as.data.frame(cond)
cond$Condition <- as.factor(cond$Condition)

cond$Condition <- relevel(cond$Condition, ref=3)

str(cond)

head(cond)

pirateplot (formula = Ability ~ Condition, data=cond , theme = 0, # Start from scratch
            inf.method = "sd", # Use standard deviations to define band limits
            inf.f.o = .7, # Band opacity
            inf.f.col = piratepal("basel"), # Add color to bands
            point.o = 1, # Point opacity
            avg.line.o = .8, # Average line opacity
            gl.col = gray(.6), # Gridline specifications
            gl.lty = 1, # Gridline specifications
            gl.lwd = c(.5, 0), # Gridline specifications
            ylim=c(0,10), # Y-axis limits
            cex.names=.85, # Axis labels scale (1=full scale)
            main="Data of Ability by Condition") # Graph title

#generate some descriptives using describeBy
describeBy(cond$Ability, group = cond$Condition)

#generate some descriptives using dplyr
cond %>% group_by(Condition) %>% summarise(mean = mean(Ability), sd = sd(Ability), count = n())

#run the ANOVA
model <- aov(Ability ~ Condition, data = cond)
anova(model)

#running pairwise comparisons with corrections - also get CIs
emmeans(model, pairwise ~ Condition, adjust = "Bonferroni")
emmeans(model, pairwise ~ Condition, adjust = "Tukey")

#get effect size of Condition
EtaSq(model, type = 3, anova = TRUE)



#Repeated Measures ANOVA - one factor
#generate data - repeated measures, one factor with 4 levels 
set.seed(1234)
x1 <- rnorm(n = 32, mean = 85, sd = 4)
x2 <- rnorm(n = 32, mean = 84, sd = 4)
x3 <- rnorm(n = 32, mean = 72, sd = 7)
x4 <- rnorm(n = 32, mean = 54, sd = 6)
x5 <- c((1:32), (1:32), (1:32), (1:32))
x6 <- c(rep("Very Easy", 32), rep("Easy", 32), rep("Hard", 32), rep("Very Hard", 32))

data <- c(x1, x2, x3, x4)
data <- cbind (x5, x6, data)
data <- as.tibble(data)

colnames (data) <- c("Participant", "Condition", "Score")

data$Condition <- factor(data$Condition, levels = c("Very Easy", "Easy", "Hard", "Very Hard"))

data$Score <- as.integer(data$Score)

#Pirateplot
pirateplot (formula = Score ~ Condition, data=data , theme = 0, # Start from scratch
            inf.f.o = .7, # Band opacity
            inf.f.col = piratepal("basel"), # Add color to bands
            point.o = 1, # Point opacity
            avg.line.o = .8, # Average line opacity
            gl.col = gray(.6), # Gridline specifications
            gl.lty = 1,
            inf.method = 'sd',
            gl.lwd = c(.5, 0), ylim=c(0,100))

#plot for each participant
ggplot(data, aes (Condition, Score, colour = Condition)) + 
  ylim(0,100) + geom_point() + facet_wrap(~ data$Participant)

#generate some descriptives
describeBy(data$Score, group = data$Condition)

#build our model and get the output
model <- aov_4(Score ~ Condition + (1 + Condition | Participant), data = data)
summary(model)
anova(model)

#running pairwise comparisons with corrections - also get CIs
emmeans (model, pairwise ~ Condition, adjust = "Bonferroni")


#2 x 2 repeated design long data format
DV <- read_csv("DV.csv", col_types = cols(Context = col_factor(levels = c("Positive", 
                                                                          "Negative")), 
                                          Sentence = col_factor(levels = c("Positive", "Negative"))))

#Generate descriptives
describeBy(DV$RT, group = list(DV$Sentence, DV$Context))

#Plotting raw data in pirateplot
pirateplot (formula = RT ~ Sentence*Context, data=DV , theme = 0, # Start from scratch
            inf.f.o = .7, # Band opacity
            inf.f.col = piratepal("basel"), # Add color to bands
            point.o = 1, # Point opacity
            avg.line.o = .8, # Average line opacity
            gl.col = gray(.6), # Gridline specifications
            gl.lty = 1,
            inf.method = 'sd',
            gl.lwd = c(.5, 0))

#Plotting data aggregated
data_agg <- DV %>% group_by(Sentence, Context) %>% summarise_at("RT", c(Mean, sd), na.rm = T)
colnames(data_agg) <- c("Sentence", "Context", "RT", "SD")
ggplot(data_agg, aes(x = Sentence, y = RT, group = Context, colour = Context)) + geom_point() + geom_line()

#Visualising as Raincloud Plots
library(RColorBrewer)
library(plyr) #note, need to detach this after this plot as clashes with aspects of dplyr
source("https://gist.githubusercontent.com/ajstewartlang/6c4cd8ab9e0c27747424acdfb3b4cff6/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

raincloud_theme = theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- ddply(dataRT, ~Condition, summarise, mean = mean(RT), median = median(RT), 
               lower = lb(RT), upper = ub(RT))

ggplot(data = DV, aes(y = RT, x = Sentence:Context, fill = Sentence:Context)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = RT, color = Sentence:Context), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs(x=NULL)  +
  scale_y_continuous(breaks = seq(0,9000,by = 2000))

detach("package:plyr", unload=TRUE) #need to detach as some clashes with dplyr

#By Subjects
model <- aov_4(RT ~ Sentence * Context + (1+Sentence * Context | Subject), data = DV, na.rm = TRUE)
anova(model)

#By Items
model1 <- aov_4(RT ~ Sentence * Context + (1 + Sentence * Context | Item), data = DV, na.rm = TRUE)
anova(model1)

emmeans(model, pairwise ~ Sentence * Context, adjust = "none")
