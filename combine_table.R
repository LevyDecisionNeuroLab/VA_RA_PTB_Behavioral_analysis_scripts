path <- "D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral"
setwd(path)

# load data
par_file <- file.path(path, "par nonpar att_allSubj_03202019.csv")
par_day12_file <- file.path(path, "par nonpar att_allSubj_day1day2_04082019.csv")
log_file <- file.path(path, "log_allSubj.csv")
pca_noRemit_gain_file <- file.path(path, "pca score gain_femaleIn_noRemitted_03202019.csv")
pca_noRemit_loss_file <- file.path(path, "pca score loss_femaleIn_noRemitted_03202019.csv")
pca_Remit_gain_file <- file.path(path, "pca score gain_femaleIn_Remitted_03202019.csv")
pca_Remit_loss_file <- file.path(path, "pca score loss_femaleIn_Remitted_03202019.csv")
error_file <- file.path(path, "error miss_allSubj.csv")
error_day12_file <- file.path(path, "error_day1day2_04082019.csv")

par <- read.csv(par_file, header=TRUE)
par_day12 <- read.csv(par_day12_file, header = TRUE)
log <- read.csv(log_file, header=TRUE)
pca_noRemit_gain <- read.csv(pca_noRemit_gain_file, header=TRUE)
pca_noRemit_loss <- read.csv(pca_noRemit_loss_file, header=TRUE)
pca_Remit_gain <- read.csv(pca_Remit_gain_file, header=TRUE)
pca_Remit_loss <- read.csv(pca_Remit_loss_file, header=TRUE)
error <- read.csv(error_file, header = TRUE)
error_day12 <- read.csv(error_day12_file, header = TRUE)

# bind pca tables
pca_gain <- rbind(pca_noRemit_gain, pca_Remit_gain)
pca_loss <- rbind(pca_noRemit_loss, pca_Remit_loss)
pca <- rbind(pca_gain, pca_loss)



# if for undivided
# combine par and log tables
par_log <- merge(par, log, by = intersect(names(par), names(log)))

# combine error
par_log_error <- merge(par_log, error, by = intersect(names(par_log), names(error)))

# if do not include clinical scores
tb <- par_log_error

# combine data and pca tables
tb <- merge(par_log_error, pca, by=intersect(names(par_log_error), names(pca)))




# if for divided 
# combine par and log tables
par_log <- merge(par_day12, log, by = intersect(names(par_day12), names(log)))

# combine error
par_log_error <- merge(par_log, error_day12, by = intersect(names(par_log), names(error_day12)))

# combine data and pca tables
tb <- merge(par_log_error, pca, by=intersect(names(par_log_error), names(pca)))





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
tb$isDay1 <- as.factor(tb$isDay1)

# sort 
tb_sorted<- tb[
  with(tb, order(isGain, id)),
  ]

tb_sorted<- tb[
  with(tb, order(isDay1, isGain, id)),
  ]

tball <- tb_sorted
# save data frame
save(tball, file = "data_all_noPCA_day1day2_04092019.rda")
