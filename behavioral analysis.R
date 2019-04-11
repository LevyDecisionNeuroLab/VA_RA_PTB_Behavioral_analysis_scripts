#### Load packages ####
library(psych)

# MVN package for multivariate normality test
# install.packages("MVN")
library(MVN)

library(ggplot2)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

library(ez)

# install.packages("corrplot")
library(corrplot)

library(emmeans)

# install.packages("multcomp")
library(multcomp)

# install.packages('nlme')
library(nlme)

# install.packages('lme4')
library(lme4)

library(cocor)

##### Local functions #####

data_summary <- function(data, varname, groupnames){
  # Function to calculate the mean and the standard error
  # for each group
  #+++++++++++++++++++++++++
  # data : a data frame
  # varname : the name of a column containing the variable
  #to be summariezed
  # groupnames : vector of column names to be used as
  # grouping variables
  
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



data_meanstd <- function(x) {
  # Function to produce summary statistics (mean and +/- sd)
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

data_meanse <- function(x) {
  # Function to produce summary statistics (mean and +/- sd)
  m <- mean(x)
  ymin <- m-sd(x)/sqrt(length(x))
  ymax <- m+sd(x)/sqrt(length(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}

##### Load data and select subjects ####

path <- "D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral"
setwd(path)
load('data_all_noPCA_day1day2_04092019.rda')
load('data_all_noPCA_04092019.rda')
load('data_all_04082019.rda')
load('data_all_day1day2_04082019.rda')

tb = tball[tball$isExcluded_behavior==0,]
tb = tball[tball$isExcluded_behavior==0 & !tball$group=='R',]

names(tb)

# load data (78 subjects)
# tball = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/question scores EFA_09152018.csv", header = TRUE)
#tball = tball[tball$isExcluded_behavior==0,]
# tb = tball[tball$isExcluded_behavior==0 & tball$isMale==1,]

# exclude remitted (58 subjects)
# tb = tball[tball$isExcluded_behavior==0 & tball$isMale==1 & !tball$group=='R',]


# tb$isGain <- as.factor(tb$isGain)
# tb$group <- as.factor(tb$group)
describeBy(tball[tball$isGain == 1,], group = tball[tball$isGain == 1,]$group)

# load data already EFAed (69 subjects), oblimin rotation
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score gain_09152018.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score loss_09152018.csv", header = TRUE)
names(tb2faGain)
names(tb2faLoss)

# load data already PCAed (69 subjects)
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score gain_allSubj_10102018.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score loss_allSubj_10102018.csv", header = TRUE)

# load data already EFAed, control and PTSD only (55 subjects)
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score gain_noRemitted_10082018.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score loss_noRemitted_10082018.csv", header = TRUE)
names(tb2faGain)
names(tb2faLoss)

# load data already PCAed, control and PTSD only (55 subjects)
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score gain_noRemitted_10102018.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score loss_noRemitted_10102018.csv", header = TRUE)

# load data already PCAed, control and PTSD only including female (59 subjects)
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score gain_femaleIn_noRemitted_01172019.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score loss_femaleIn_noRemitted_01172019.csv", header = TRUE)

# load data already PCAed, remitted only (14 subjects)
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score gain_Remitted_10112018.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/pca score loss_Remitted_10112018.csv", header = TRUE)

# load data already EFAed, all subjects (69 subjects), orthogonal rotation
tb2faGain = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score gain_allSubj_orthogonal_10102018.csv", header = TRUE)
tb2faLoss = read.csv("D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral/factor score loss_allSubj_orthogonal_10102018.csv", header = TRUE)

names(tb2faGain)
names(tb2faLoss)


##### select day ####
# day1
tb <- tball[tball$isExcluded_behavior==0 & tball$isDay1 == 1,]
# day2
tb <- tball[tball$isExcluded_behavior==0 & tball$isDay1 == 0,]

##### Compare between day1 day2 #####
  ##### reorganize data ####
tb_day1 <- tb[tb$isDay1==1, ]
tb_day2 <- tb[tb$isDay1==0, ]
names(tb_day1)
names(tb_day2)

colnames(tb_day1)[c(4:16,25:30)]<-c('alpha_day1', 'beta_day1',  'gamma_day1',
                                   'r2_day1', 'LL_day1', 'AIC_day1', 'BIC_day1',
                                   'r25_day1', 'r50_day1', 'r75_day1', 'a24_day1',  
                                   'a50_day1', 'a74_day1', 'error_day1',
                                   'r_day1', 'a_day1', 'a_r50_day1', 
                                   'alpha_t_day1', 'beta_t_day1')

colnames(tb_day2)[c(4:16,25:30)]<-c('alpha_day2', 'beta_day2',  'gamma_day2',
                                    'r2_day2', 'LL_day2', 'AIC_day2', 'BIC_day2',
                                    'r25_day2', 'r50_day2', 'r75_day2', 'a24_day2',  
                                    'a50_day2', 'a74_day2', 'error_day2',
                                    'r_day2', 'a_day2', 'a_r50_day2', 
                                    'alpha_t_day2', 'beta_t_day2')
tb_day1$isDay1 <- NULL
tb_day2$isDay1 <- NULL

tb_day12 <- merge(tb_day1, tb_day2, by = intersect(names(tb_day1), names(tb_day2)))

# sort
tb_day12 <- tb_day12[with(tb_day12, order(isGain, id)),]

# calculate day difference
tb_day12$alpha_t_d <- tb_day12$alpha_t_day2 - tb_day12$alpha_t_day1
tb_day12$beta_t_d <- tb_day12$beta_t_day2 - tb_day12$beta_t_day1
tb_day12$r_d <- tb_day12$r_day2 - tb_day12$r_day1
tb_day12$a_r50_d <- tb_day12$a_r50_day2 - tb_day12$a_r50_day1
tb_day12$error_d <- tb_day12$error_day2 - tb_day12$error_day1
tb_day12$r2_d <- tb_day12$r2_day2 - tb_day12$r2_day1

  ##### correlation #####
group2plot = 'P'
tb_day12_plot = tb_day12[tb_day12$group == group2plot,]

# gain
ggplot(tb_day12_plot[tb_day12_plot$isGain==1, ], aes(x=alpha_t_day1, y=alpha_t_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-based Risk Attitude, gain"))

ggplot(tb_day12_plot[tb_day12_plot$isGain==1, ], aes(x=beta_t_day1, y=beta_t_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-based Ambig Attitude, gain"))

ggplot(tb_day12_plot[tb_day12_plot$isGain==1, ], aes(x=r_day1, y=r_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-free risk Attitude, gain"))

ggplot(tb_day12_plot[tb_day12_plot$isGain==1, ], aes(x=a_r50_day1, y=a_r50_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-free ambig Attitude, gain"))

# loss
ggplot(tb_day12_plot[tb_day12_plot$isGain==0, ], aes(x=alpha_t_day1, y=alpha_t_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-based risk Attitude, loss"))

ggplot(tb_day12_plot[tb_day12_plot$isGain==0, ], aes(x=beta_t_day1, y=beta_t_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-based ambig Attitude, loss"))

ggplot(tb_day12_plot[tb_day12_plot$isGain==0, ], aes(x=r_day1, y=r_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-free risk Attitude, loss"))

ggplot(tb_day12_plot[tb_day12_plot$isGain==0, ], aes(x=a_r50_day1, y=a_r50_day2)) + 
  geom_point(size=2) +
  theme_classic() +
  labs(title=paste(group2plot, ", Model-free ambig Attitude, loss"))


  ##### difference between day1 day2 ####

  ##### compare days means  #####
# bar plot, model based risk
group2plot = 'R'
tb_group <- tb[tb$group == group2plot,]
tb_group <- tb

tbplot <- data_summary(tb_group,varname="alpha_t",groupnames=c("isGain","isDay1"))
ggplot(data=tbplot,aes(x=isGain, y=alpha_t, fill=isDay1)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=alpha_t-sd, ymax=alpha_t+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-based Risk Attitude"), y = "Transformed Attitude")

# violin plot
ggplot(tb_group, aes(isGain, y=alpha_t, fill=isDay1)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-based Risk Attitude"), y = "Transformed Attitude")

# bar plot, model based ambiguity
group2plot = 'R'
tb_group <- tb[tb$group == group2plot,]
tb_group <- tb

tbplot <- data_summary(tb_group,varname="beta_t",groupnames=c("isGain","isDay1"))
ggplot(data=tbplot,aes(x=isGain, y=beta_t, fill=isDay1)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=beta_t-sd, ymax=beta_t+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-based Amb Attitude"), y = "Transformed Attitude")

# violin plot
ggplot(tb_group, aes(isGain, y=beta_t, fill=isDay1)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-based Amb Attitude"), y = "Transformed Attitude")

# barplot model free risk
group2plot = 'R'
tb_group <- tb[tb$group == group2plot,]
tb_group <- tb

tbplot <- data_summary(tb_group,varname="r",groupnames=c("isGain","isDay1"))
ggplot(data=tbplot,aes(x=isGain, y=r, fill=isDay1)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=r-sd, ymax=r+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-free Risk Attitude"), y = "Choice proportion")

# violin plot
ggplot(tb_group, aes(isGain, y=r, fill=isDay1)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-free Risk Attitude"), y = "Choice proportion")

# bar plot, model free ambiguity
group2plot = 'R'
tb_group <- tb[tb$group == group2plot,]
tb_group <- tb

tbplot <- data_summary(tb_group,varname="a_r50",groupnames=c("isGain","isDay1"))
ggplot(data=tbplot,aes(x=isGain, y=a_r50, fill=isDay1)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=a_r50-sd, ymax=a_r50+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-free Amb Attitude"), y = "Choice proportion")

# violin plot
ggplot(tb_group, aes(isGain, y=a_r50, fill=isDay1)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Day2","Day1")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title=paste(group2plot, "Model-free Amb Attitude"), y = "Choice proportion")


  ##### histrogram of change in variables #####
group2plot = c('P', 'C')
domain2plot = 0 # 1-gain, 0-loss
tb_day12_plot = tb_day12[tb_day12$isGain == domain2plot & is.element(tb_day12$group, group2plot), ]

# model based ambig
mu <- ddply(tb_day12_plot, "group", summarise, grp.mean=mean(beta_t_d))
ggplot(tb_day12_plot, aes(x = beta_t_d, color = group, fill = group)) +
  geom_histogram(alpha = 0.4, position ='identity') +
  geom_vline(aes(xintercept=0),linetype="dashed")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),linetype="dashed") +
  theme_classic() +
  labs(title = paste('Domain isGain:', domain2plot, 'Model-based ambig att change')) +
  theme(title = element_text(size = 10))

# model free ambig
mu <- ddply(tb_day12_plot, "group", summarise, grp.mean=mean(a_r50_d))
ggplot(tb_day12_plot, aes(x = a_r50_d, color = group, fill = group)) +
  geom_histogram(alpha = 0.4, position ='identity') +
  geom_vline(aes(xintercept=0),linetype="dashed")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),linetype="dashed") +
  theme_classic() +
  labs(title = paste('Domain isGain:', domain2plot, 'Model-free ambig att change')) +
  theme(title = element_text(size = 10))

# model based risk
mu <- ddply(tb_day12_plot, "group", summarise, grp.mean=mean(alpha_t_d))
ggplot(tb_day12_plot, aes(x = alpha_t_d, color = group, fill = group)) +
  geom_histogram(alpha = 0.4, position ='identity') +
  geom_vline(aes(xintercept=0),linetype="dashed")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),linetype="dashed") +
  theme_classic() +
  labs(title = paste('Domain isGain:', domain2plot, 'Model-based risk att change')) +
  theme(title = element_text(size = 10))

# model free risk
mu <- ddply(tb_day12_plot, "group", summarise, grp.mean=mean(r_d))
ggplot(tb_day12_plot, aes(x = r_d, color = group, fill = group)) +
  geom_histogram(alpha = 0.4, position ='identity') +
  geom_vline(aes(xintercept=0),linetype="dashed")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),linetype="dashed") +
  theme_classic() +
  labs(title = paste('Domain isGain:', domain2plot, 'Model-free risk att change')) +
  theme(title = element_text(size = 10))

  ##### violin plot of change in variables #####
group2plot = c('P', 'C')
domain2plot = 0 # 1-gain, 0-loss
tb_day12_plot = tb_day12[tb_day12$isGain == domain2plot & is.element(tb_day12$group, group2plot), ]



##### Correlation analysis ##### 
##### correlation between attitudes #####
chart.Correlation(tb[tb$isGain==1,c(16:17, 21:22)], 
                  histogram=TRUE, 
                  method = c("pearson"))

test1 <- corr.test(x=tb[tb$isGain==1,c(16:17, 21:22)],
                   y=tb[tb$isGain==1,c(16:17, 21:22)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

chart.Correlation(tb[tb$isGain==0,c(16:17, 21:22)], 
                  histogram=TRUE, 
                  method = c("pearson"))

test1 <- corr.test(x=tb[tb$isGain==0,c(16:17, 21:22)],
                   y=tb[tb$isGain==0,c(16:17, 21:22)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)


##### correlation between domain #####
tbDomain=data.frame(id_gain = tb$id[tb$isGain==1],
                    id_loss = tb$id[tb$isGain==0],
                    alpha_t_gain = tb$alpha_t[tb$isGain==1],
                    beta_t_gain = tb$beta_t[tb$isGain==1],
                    r_gain = tb$r[tb$isGain==1],
                    a_r50_gain = tb$a_r50[tb$isGain==1],
                    alpha_t_loss = tb$alpha_t[tb$isGain==0],
                    beta_t_loss = tb$beta_t[tb$isGain==0],
                    r_loss = tb$r[tb$isGain==0],
                    a_r50_loss = tb$a_r50[tb$isGain==0])

chart.Correlation(tbDomain[,c(3:4, 7:8)], 
                  histogram=TRUE, 
                  method = c("pearson"))

test1 <- corr.test(x=tbDomain[,c(3:4, 7:8)],
                   y=tbDomain[,c(3:4, 7:8)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

chart.Correlation(tbDomain[,c(5:6, 9:10)], 
                  histogram=TRUE, 
                  method = c("pearson"))

test1 <- corr.test(x=tbDomain[,c(5:6, 9:10)],
                   y=tbDomain[,c(5:6, 9:10)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

##### model-based and model-free correlation with CAPS #####
names(tb)
colnames(tb)[35]<-'Reexp'
colnames(tb)[36]<-'Avoid'
colnames(tb)[37]<-'Numb'
colnames(tb)[38]<-'Dysphoric'
colnames(tb)[39]<-'Anxious'
colnames(tb)[40]<-'Total'

# model free gains
chart.Correlation(tb[tb$isGain==1,c(21:22, 35:40)],
                  histogram = TRUE,
                  method = "pearson")

# test for correlation
test1 <- corr.test(x=tb[tb$isGain==1,c(21:22)],
          y=tb[tb$isGain==1,c(35:40)],
          use = "pairwise",
          method = "spearman",
          adjust = "none",
          alpha=.05)
print(test1, digits = 4)


# model based gains
chart.Correlation(tb[tb$isGain==1,c(16:17, 35:40)],
                  histogram = TRUE,
                  method = 'pearson')
# test for correlation
test1 <- corr.test(x=tb[tb$isGain==1,c(16:17)],
                   y=tb[tb$isGain==1,c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

test1 <- corr.test(x=tb[tb$isGain==1 & !tb$id == 3, c(16:17)],
                   y=tb[tb$isGain==1 & !tb$id == 3, c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

# model free loss
chart.Correlation(tb[tb$isGain==0,c(21:22, 35:40)],
                  histogram = TRUE,
                  method = 'pearson')
# test for correlation
test1 <- corr.test(x=tb[tb$isGain==0,c(21:22)],
                   y=tb[tb$isGain==0,c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

# model based loss
chart.Correlation(tb[tb$isGain==0,c(16:17, 35:40)],
                  histogram = TRUE,
                  method = 'pearson')
# test for correlation
test1 <- corr.test(x=tb[tb$isGain==0,c(16:17)],
                   y=tb[tb$isGain==0,c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)


# exclude the one with extreme ambig att in gain
exclude1 = tb$id[tb$isGain==1 & tb$beta_t < -3]
exclude1 # 3

# exclude the one with extreme risk att in loss
exclude1 = tb$id[tb$isGain==0 & tb$alpha_t < -2.5]
exclude1 # 1069
test1 <- corr.test(x=tb[tb$isGain==0 & !tb$id == 1069,c(16:17)],
                   y=tb[tb$isGain==0 & !tb$id == 1069,c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)


exclude1 = tb$id[tb$isGain==0 & tb$r > 0.7]
exclude1 # 38, 96

# exclude the one with extreme ambig att in loss
exclude2 = tb$id[tb$isGain==0 & tb$beta_t < -2]
exclude2 # 1074
exclude2 = tb$id[tb$isGain==0 & tb$beta_t > 2]
exclude2 # 1354

test1 <- corr.test(x=tb[tb$isGain==0 & !tb$id == 1074,c(16:17)],
                   y=tb[tb$isGain==0 & !tb$id == 1074,c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

test1 <- corr.test(x=tb[tb$isGain==0 & !tb$id == 1074 & !tb$id == 1354,c(16:17)],
                   y=tb[tb$isGain==0 & !tb$id == 1074 & !tb$id == 1354,c(35:40)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)


exclude2 = tb$id[tb$isGain==0 & tb$a_r50 > 0.4]
exclude2 # 95
exclude2 = tb$id[tb$isGain==0 & tb$a_r50 < -0.3]
exclude2 # 1234

# change names for graphs
colnames(tb2faGain)[16] <- 'Risk'
colnames(tb2faGain)[17] <- 'Ambiguity'

colnames(tb2faLoss)[16] <- 'Risk'
colnames(tb2faLoss)[17] <- 'Ambiguity'

##### model-based and model-free correlation with CAPS, single scatter plot ####

names(tb)
colnames(tb)[35]<-'Reexp'
colnames(tb)[36]<-'Avoid'
colnames(tb)[37]<-'Numb'
colnames(tb)[38]<-'Dysphoric'
colnames(tb)[39]<-'Anxious'
colnames(tb)[40]<-'Total'

ggplot(tb[tb$isGain==1,], aes(x=Total, y=alpha_t)) +
  geom_point(size = 5) + 
  geom_smooth(method=lm, color = "black")+
  theme_classic() +
  theme(text = element_text(size=26),
        axis.line = element_line(size = 1.5)) +
  labs(x="CAPS total", y="Risk Attitude in Gain")

ggplot(tb[tb$isGain==0,], aes(x=Total, y=beta_t)) +
  geom_point(size = 5) + 
  geom_smooth(method=lm, color = "black")+
  theme_classic() +
  theme(text = element_text(size=26),
        axis.line = element_line(size = 1.5)) +
  labs(x="CAPS total", y="Ambiguity Attitude in Loss")

ggplot(tb,aes(x=Total, color = group, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth=5, size =1.25, closed = "right") +
  scale_color_manual(values = c("olivedrab","darkorange3","grey50")) +
  scale_fill_manual(values = c("olivedrab","darkorange3","grey50")) +
  theme_classic() +
  theme(text = element_text(size=24), axis.line = element_line(size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        axis.text = element_text(colour = "black", size = 20)) +
  labs(x="CAPS total", y="Count of participant")

  
ggplot(tbnoRemit,aes(x=Total, color = group, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth=5, size =0, closed = "right") +
  scale_color_manual(values = c("olivedrab","darkorange3")) +
  scale_fill_manual(values = c("olivedrab","darkorange3")) +
  theme_classic() +
  theme(text = element_text(size=24), axis.line = element_line(size = 1.25),
        axis.ticks = element_line(colour = "black", size = 1.25),
        axis.text = element_text(colour = "black", size = 20)) +
  labs(x="CAPS total", y="Count of participant")

##### model-based and model-free correlation with factors #####
cor.plot(tb2faGain[,c(55:57, 16:17, 21:22)],
         main = 'Gains',
         numbers = TRUE,xlas=2)

pairs.panels(tb2faGain[,c(55:57, 16:17, 21:22)],main = 'Gains', pch='.')

chart.Correlation(tb2faGain[,c(55:57, 16:17, 21:22)], 
                  histogram=TRUE, 
                  method = c("pearson")
)

test1 <- corr.test(x=tb2faGain[,c(55:57)],
                   y=tb2faGain[,c(16:17, 21:22)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

# only with model-based attitudes
chart.Correlation(tb2faGain[,c(55:57, 16:17)], 
                  histogram=TRUE, 
                  method = c("pearson"))


chart.Correlation(tb[,c(16:17, 21:22)], 
                  histogram=TRUE, 
                  method = c("pearson"))

cor.plot(tb2faLoss[,c(55:57, 16:17, 21:22)],
         main='Loss',
         numbers = TRUE,xlas=2)

pairs.panels(tb2faLoss[,c(55:57, 16:17, 21:22)],main='Loss',pch='.')

chart.Correlation(tb2faLoss[,c(55:57, 16:17, 21:22)], 
                  histogram=TRUE, 
                  method = c("pearson"))

test1 <- corr.test(x=tb2faLoss[,c(55:57)],
                   y=tb2faLoss[,c(16:17, 21:22)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)




chart.Correlation(tb2faLoss[,c(55:57, 16:17)], 
                  histogram=TRUE, 
                  method = c("pearson"))

chart.Correlation(tb2faLoss[,c(16:17, 21:22)], 
                  histogram=TRUE, 
                  method = c("pearson"))

# test for correlation model based
corr.test(x=tb2faGain[,c(55:57)],
          y=tb2faGain[,c(16:17)],
          use = "pairwise",
          method = "pearson",
          adjust = "none",
          alpha=.05
)

corr.test(x=tb2faLoss[,c(55:57)],
          y=tb2faLoss[,c(16:17)],
          use = "pairwise",
          method = "pearson",
          adjust = "none",
          alpha=.05
)

# test for correlation model free
corr.test(x=tb2faGain[,c(55:57)],
          y=tb2faGain[,c(21:22)],
          use = "pairwise",
          method = "pearson",
          adjust = "none",
          alpha=.05
)

corr.test(x=tb2faLoss[,c(55:57)],
          y=tb2faLoss[,c(21:22)],
          use = "pairwise",
          method = "pearson",
          adjust = "none",
          alpha=.05
)



##### Group analysis #####
# tb$isGain <- as.factor(tb$isGain)
# tb$group <- as.factor(tb$group)
# tb$id <- as.factor(tb$id)
# count subject
sum(tb$group == 'C')/2
sum(tb$group == 'P')/2
sum(tb$group == 'R')/2

  ##### model based #####
# risk attitudes, gain-loss, 2groups-controls and PTSD only
tbnoRemit <- tb[tb$group == 'C'| tb$group == 'P',]
# bar plot
tbplot <- data_summary(tbnoRemit,varname="alpha_t",groupnames=c("isGain","group"))
ggplot(data=tbplot,aes(x=isGain, y=alpha_t, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=alpha_t-sd, ymax=alpha_t+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-based Risk Attitude", y = "Transformed Attitude")

# violin plot
ggplot(tbnoRemit, aes(isGain, y=alpha_t, fill=group)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-based Risk Attitude", y = "Transformed Attitude")

  
# plot ambig attitudes, gain-loss, 2groups-control and PTSD only
tbnoRemit <- tb[tb$group == 'C'| tb$group == 'P',]

# bar plot
tbplot <- data_summary(tbnoRemit,varname="beta_t",groupnames=c("isGain","group"))
ggplot(data=tbplot,aes(x=isGain, y=beta_t, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=beta_t-sd, ymax=beta_t+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-based Ambiguity Attitude", y = "Transformed Attitude")

# violin plot
ggplot(tbnoRemit, aes(x=isGain, y=beta_t, fill=group)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-based Ambiguity Attitude", y = "Transformed Attitude")

risk_anova = ezANOVA(data=tbnoRemit, 
                   dv = alpha_t,
                   wid = .(id),
                   within = .(isGain),
                   between = .(group),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE
)
risk_anova
TukeyHSD(risk_anova$aov)

rt = ezBoot(
  data=tbnoRemit,
  dv = 'alpha_t',
  wid = 'id',
  within = 'isGain',
  between = 'group'
)

t.test(tbnoRemit[tbnoRemit$group=='C' & tbnoRemit$isGain==1,]$alpha_t,
       mu = 0)
t.test(tbnoRemit[tbnoRemit$group=='P' & tbnoRemit$isGain==1,]$alpha_t,
       mu = 0)

t.test(tbnoRemit[tbnoRemit$group=='C' & tbnoRemit$isGain==0,]$alpha_t,
       mu = 0)
t.test(tbnoRemit[tbnoRemit$group=='P' & tbnoRemit$isGain==0,]$alpha_t,
       mu = 0)

t.test(tbnoRemit[tbnoRemit$group=='C' & tbnoRemit$isGain==1,]$beta_t,
       mu = 0)
t.test(tbnoRemit[tbnoRemit$group=='P' & tbnoRemit$isGain==1,]$beta_t,
       mu = 0)

t.test(tbnoRemit[tbnoRemit$group=='C' & tbnoRemit$isGain==0,]$beta_t,
       mu = 0)
t.test(tbnoRemit[tbnoRemit$group=='P' & tbnoRemit$isGain==0,]$beta_t,
       mu = 0)

pairwise.t.test(tbnoRemit[,], g, p.adjust.method = p.adjust.methods,
                pool.sd = !paired, paired = FALSE,
                alternative = c("two.sided"),
                ...)

ambig_anova = ezANOVA(data=tbnoRemit[tbnoRemit$id != 120,], 
                     dv = beta_t,
                     wid = .(id),
                     within = .(isGain),
                     between = .(group),
                     type = 3,
                     detailed = TRUE,
                     return_aov = TRUE
)
ambig_anova


# plot risk attitudes, gain-loss, 3groups
tbplot <- data_summary(tb,varname="alpha_t",groupnames=c("isGain","group"))

ggplot(data=tbplot,aes(x=isGain, y=alpha_t, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=alpha_t-sd, ymax=alpha_t+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Control","PTSD","Remitted")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-based Risk Attitude", y = "Transformed Attitude")

# plot ambig attitudes, gain-loss, 3groups
tbplot <- data_summary(tb,varname="beta_t",groupnames=c("isGain","group"))

ggplot(data=tbplot,aes(x=isGain, y=beta_t, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=beta_t-sd, ymax=beta_t+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Control","PTSD","Remitted")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-based Ambiguity Attitude", y = "Transformed Attitude")

  ##### model free #####
# risk attitudes, gain-loss, 2groups-controls and PTSD only
tbnoRemit <- tb[tb$group == 'C'| tb$group == 'P',]
# bar plot
tbplot <- data_summary(tbnoRemit,varname="r",groupnames=c("isGain","group"))
ggplot(data=tbplot,aes(x=isGain, y=r, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=r-sd, ymax=r+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-free Risk Attitude", y = "Choice Proportion")

# violin plot
ggplot(tbnoRemit, aes(isGain, y=r, fill=group)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-free Risk Attitude", y = "Choice proportion")


# plot ambig attitudes, gain-loss, 2groups-control and PTSD only
tbnoRemit <- tb[tb$group == 'C'| tb$group == 'P',]

# bar plot
tbplot <- data_summary(tbnoRemit,varname="a_r50",groupnames=c("isGain","group"))
ggplot(data=tbplot,aes(x=isGain, y=a_r50, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=a_r50-sd, ymax=a_r50+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-free Ambiguity Attitude", y = "Choice Proportion")

# violin plot
ggplot(tbnoRemit, aes(x=isGain, y=a_r50, fill=group)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.data=data_meanstd, geom="pointrange", color = "red",
               position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Control","PTSD")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-free Ambiguity Attitude", y = "Choice Proportion")

# Anova
risk_anova = ezANOVA(data=tbnoRemit, 
                     dv = r,
                     wid = .(id),
                     within = .(isGain),
                     between = .(group),
                     type = 3,
                     detailed = TRUE,
                     return_aov = TRUE
)
risk_anova

ambig_anova = ezANOVA(data=tbnoRemit, 
                     dv = a_r50,
                     wid = .(id),
                     within = .(isGain),
                     between = .(group),
                     type = 3,
                     detailed = TRUE,
                     return_aov = TRUE
)
ambig_anova
# plot risk attitudes, gain-loss, 3groups
tbplot <- data_summary(tb,varname="r",groupnames=c("isGain","group"))

ggplot(data=tbplot,aes(x=isGain, y=r, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=r-sd, ymax=r+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Blues", labels = c("Control","PTSD","Remitted")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-free Risk Attitude", y = "Choice Proportion")

# plot ambig attitudes, gain-loss, 3groups
tbplot <- data_summary(tb,varname="a_r50",groupnames=c("isGain","group"))

ggplot(data=tbplot,aes(x=isGain, y=a_r50, fill=group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=a_r50-sd, ymax=a_r50+sd), width=0.2, position=position_dodge(0.9)) +
  scale_fill_brewer(palette="Reds", labels = c("Control","PTSD","Remitted")) +
  theme_classic() +
  scale_x_discrete(name ="Domain", limits=c("1", "0"), labels=c("Gain","Loss")) +
  labs(title="Model-free Ambiguity Attitude", y = "Corrected Choice Proportion")


##### Demographic
age_anova <- ezANOVA(data=tb, 
                        dv = age,
                        wid = .(id),
                        between = .(group),
                        type = 3,
                        detailed = TRUE,
                        return_aov = TRUE
  )
age_anova

kbit_anova <- ezANOVA(data=tb, 
                     dv = kbit,
                     wid = .(id),
                     between = .(group),
                     type = 3,
                     detailed = TRUE,
                     return_aov = TRUE
)
kbit_anova

ces_anova <- ezANOVA(data=tb[!is.nan(tb$ces_total),], 
                      dv = ces_total,
                      wid = .(id),
                      between = .(group),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE
)
ces_anova

##### Behavioral, cognitive, and symptom #####




# correlation between symptom and KBIT
names(tb)
# all
chart.Correlation(tb[tb$isGain==1,c(31,34,40,46,47:48,49,54)],
                  histogram = TRUE)

# by group
chart.Correlation(tb[tb$isGain==1 & tb$group == 'C',c(31,34,40,46,47:48,49,54)],
                  histogram = TRUE)





# correlation between kbit and behavior
chart.Correlation(tb[tb$isGain==1,c(31,16:17,21:22)],
                  histogram = TRUE)
# test for correlation
test1 <- corr.test(x=tb[tb$isGain==1,c(31)],
                   y=tb[tb$isGain==1,c(16:17,21:22)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

# per group
chart.Correlation(tb[tb$isGain==1 & tb$group == 'P',c(31,16:17,21:22)],
                  histogram = TRUE)

# loss
chart.Correlation(tb[tb$isGain==0,c(31,16:17,21:22)],
                  histogram = TRUE)

# test for correlation
test1 <- corr.test(x=tb[tb$isGain==0,c(31)],
                   y=tb[tb$isGain==0,c(16:17,21:22)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha=.05)
print(test1, digits = 4)

# per group
chart.Correlation(tb[tb$isGain==0 & tb$group == 'P',c(31,16:17,21:22)],
                  histogram = TRUE)


# correlation between factor and kbit
names(tb2faGain)
chart.Correlation(tb2faGain[,c(31,55:57)], histogram = TRUE)

chart.Correlation(tb2faGain[tb2faGain$group == 'R',c(31,55:57)], histogram = TRUE)
test1 <- corr.test(x = tb2faGain[tb2faGain$group == 'P',c(31)],
                   y = tb2faGain[tb2faGain$group == 'P',c(55:57)],
                   use = "pairwise",
                   method = "spearman",
                   adjust = "none",
                   alpha = 0.05)
print(test1, digits = 4)

##### Reference #####
# plot correlation matrix
# https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
