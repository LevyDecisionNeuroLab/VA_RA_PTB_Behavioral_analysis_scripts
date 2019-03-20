path <- "D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral"
setwd(path)

# load data
par_file <- file.path(path, "par nonpar att_allSubj_03202019.csv")
log_file <- file.path(path, "log_allSubj.csv")
pca_noRemit_gain_file <- file.path(path, "pca score gain_femaleIn_noRemitted_03202019.csv")
pca_noRemit_loss_file <- file.path(path, "pca score loss_femaleIn_noRemitted_03202019.csv")
pca_Remit_gain_file <- file.path(path, "pca score gain_femaleIn_Remitted_03202019.csv")
pca_Remit_loss_file <- file.path(path, "pca score loss_femaleIn_Remitted_03202019.csv")

par <- read.csv(par_file, header=TRUE)
log <- read.csv(log_file, header=TRUE)
pca_noRemit_gain <- read.csv(pca_noRemit_gain_file, header=TRUE)
pca_noRemit_loss <- read.csv(pca_noRemit_loss_file, header=TRUE)
pca_Remit_gain <- read.csv(pca_Remit_gain_file, header=TRUE)
pca_Remit_loss <- read.csv(pca_Remit_loss_file, header=TRUE)

# bind pca tables
pca_gain <- rbind(pca_noRemit_gain, pca_Remit_gain)
pca_loss <- rbind(pca_noRemit_loss, pca_Remit_loss)
pca <- rbind(pca_gain, pca_loss)

# combine par and log tables
par_log <- merge(par, log, by = intersect(names(par), names(log)))

# combine data and pca tables
tb <- merge(par_log, pca, by=intersect(names(par_log), names(pca)))

# calculate model-free, and transformed score
tb$r <- rowMeans(cbind(tb$r25, tb$r50, tb$r75))
tb$a <- rowMeans(cbind(tb$a24, tb$a50, tb$a74))
tb$a_r50 <- tb$a - tb$r50

tb$alpha_t = tb$alpha - 1
tb$alpha_t[tb$isGain==0] = -tb$alpha_t[tb$isGain==0]
tb$beta_t= -tb$beta
tb$beta_t[tb$isGain==0] = -tb$beta_t[tb$isGain==0]

tb$isGain <- as.factor(tb$isGain)
tb$group <- as.factor(tb$group)
tb$id <- as.factor(tb$id)

tball <- tb
# save data frame
save(tball, file = "data_all_03202019.rda")
