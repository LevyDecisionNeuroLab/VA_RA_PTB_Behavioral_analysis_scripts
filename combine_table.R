path <- "D:/Ruonan/Projects in the lab/VA_RA_PTB/Clinical and behavioral"
setwd(path)

# load data
par_file <- file.path(path, "par nonpar att_allSubj_03202019.csv")
par_day12_file <- file.path(path, "par nonpar att_allSubj_day1day2_08262019.csv")
log_file <- file.path(path, "log_allSubj.csv")

#pca_noRemit_gain_file <- file.path(path, "pca score gain_femaleIn_noRemitted_03202019.csv")
#pca_noRemit_loss_file <- file.path(path, "pca score loss_femaleIn_noRemitted_03202019.csv")
#pca_Remit_gain_file <- file.path(path, "pca score gain_femaleIn_Remitted_03202019.csv")
#pca_Remit_loss_file <- file.path(path, "pca score loss_femaleIn_Remitted_03202019.csv")

pca_noRemit_gain_file <- file.path(path, "pca score gain_noRemitted_10102018.csv")
pca_noRemit_loss_file <- file.path(path, "pca score loss_noRemitted_10102018.csv")
pca_Remit_gain_file <- file.path(path, "pca score gain_Remitted_10112018.csv")
pca_Remit_loss_file <- file.path(path, "pca score loss_Remitted_10112018.csv")

clinical_file <- file.path(path, "question scores EFA_09152018.csv")
error_file <- file.path(path, "error miss_allSubj.csv")
error_day12_file <- file.path(path, "error_day1day2_04082019.csv")

par <- read.csv(par_file, header=TRUE)
par_day12 <- read.csv(par_day12_file, header = TRUE)
log <- read.csv(log_file, header=TRUE)
pca_noRemit_gain <- read.csv(pca_noRemit_gain_file, header=TRUE)
pca_noRemit_loss <- read.csv(pca_noRemit_loss_file, header=TRUE)
pca_Remit_gain <- read.csv(pca_Remit_gain_file, header=TRUE)
pca_Remit_loss <- read.csv(pca_Remit_loss_file, header=TRUE)
clini <- read.csv(clinical_file, header = TRUE)
error <- read.csv(error_file, header = TRUE)
error_day12 <- read.csv(error_day12_file, header = TRUE)

# bind pca tables
pca_gain <- rbind(pca_noRemit_gain, pca_Remit_gain)
pca_loss <- rbind(pca_noRemit_loss, pca_Remit_loss)
pca <- rbind(pca_gain, pca_loss)
colnames(pca)
pca_clean <- pca[,c(1,2,55:57)]

# clean clinical
colnames(clini)
clini_clean <- clini[,c(1:2, 34:54)]

# if for undivided
# combine par and log tables
par_log <- merge(par, log, by = intersect(names(par), names(log)))

# combine error
par_log_error <- merge(par_log, error, by = intersect(names(par_log), names(error)))

# look at Subject IDs before merge table
pca_id <- unique(pca$id)
clini_id <- unique(clini$id)
behav_id <- unique(par_log_error[par_log_error$isExcluded_behavior == 0,]$id)
image_id <- unique(par_log_error[par_log_error$isExcluded_imaging == 0,]$id)
female_id <- unique(par_log_error[par_log_error$isMale == 0,]$id)

is.element(pca_id, image_id)
is.element(pca_id, behav_id)

behav_id[!is.element(behav_id, pca_id)]
image_id[!is.element(image_id, pca_id)]

# subject 1285,117,102, 3 included but no PCA, need to create two rows in the PCA table
pca_clean[139, 1] <- 1285
pca_clean[140, 1] <- 1285

pca_clean[139, 2] <- 1
pca_clean[140, 2] <- 0

pca_clean[141, 1] <- 117
pca_clean[142, 1] <- 117

pca_clean[141, 2] <- 1
pca_clean[142, 2] <- 0

pca_clean[143, 1] <- 102
pca_clean[144, 1] <- 102

pca_clean[143, 2] <- 1
pca_clean[144, 2] <- 0

pca_clean[145, 1] <- 3
pca_clean[146, 1] <- 3

pca_clean[145, 2] <- 1
pca_clean[146, 2] <- 0

# combine PCA with clinical
pca_clini <- merge(pca_clean, clini_clean, by=intersect(names(pca_clean), names(clini_clean)))


# if do not include clinical scores
tb <- par_log_error

# combine data and pca_clini tables
tb <- merge(par_log_error, pca_clini, by=intersect(names(par_log_error), names(pca_clini)))


# if for divided 
# combine par and log tables
par_log <- merge(par_day12, log, by = intersect(names(par_day12), names(log)))

# combine error
par_log_error <- merge(par_log, error_day12, by = intersect(names(par_log), names(error_day12)))

# combine data and pca tables
tb <- merge(par_log_error, pca_clini, by=intersect(names(par_log_error), names(pca_clini)))


# sort 
tb_sorted<- tb[
  with(tb, order(isGain, id)),
  ]

tb_sorted<- tb[
  with(tb, order(isDay1, isGain, id)),
  ]


# calculate model-free, and transformed score
tb_sorted$r <- rowMeans(cbind(tb_sorted$r25, tb_sorted$r50, tb_sorted$r75))
tb_sorted$a <- rowMeans(cbind(tb_sorted$a24, tb_sorted$a50, tb_sorted$a74))
tb_sorted$a_r50 <- tb_sorted$a - tb_sorted$r50

tb_sorted$alpha_t = tb_sorted$alpha - 1
tb_sorted$alpha_t[tb_sorted$isGain==0] = -tb_sorted$alpha_t[tb_sorted$isGain==0]
tb_sorted$beta_t= -tb_sorted$beta
tb_sorted$beta_t[tb_sorted$isGain==0] = -tb_sorted$beta_t[tb_sorted$isGain==0]

tb_sorted$isGain <- as.factor(tb_sorted$isGain)
tb_sorted$group <- as.factor(tb_sorted$group)
tb_sorted$id <- as.factor(tb_sorted$id)
tb_sorted$isDay1 <- as.factor(tb_sorted$isDay1)


tball <- tb_sorted

# save data frame
# save(tball, file = "data_all_noFemale_noPCA_day1day2_08272019.rda")
save(tball, file = "data_all_noFemale_day1day2_08272019.rda")
save(tball, file = "data_all_noFemale_08272019.rda")
