#INSTRUCTION: RUN MAIN.R FIRST
theme_set(theme_bw() +
            theme_light(base_size = 15) +
            theme(aspect.ratio = 1, legend.position="bottom")
)
#=================================
# machine learning
#=================================

# NON TL DAVIDSON TRAINED COMPARISON ===========================================
# theme
# prepare data

add_f1_calculation <- function(calc_table, order_type){
  if (order_type == 1){
    calc_table["0.f1"] <- 2 * (calc_table$`0.rec` * calc_table$`0.pre`) / (calc_table$`0.rec` + calc_table$`0.pre`)
    calc_table["1.f1"] <- 2 * (calc_table$`1.rec` * calc_table$`1.pre`) / (calc_table$`1.rec` + calc_table$`1.pre`)
    calc_table["2.f1"] <- 2 * (calc_table$`2.rec` * calc_table$`2.pre`) / (calc_table$`2.rec` + calc_table$`2.pre`)
    col_order <- c("X1", "acc", "0.rec", "0.pre", "0.f1", "1.rec", "1.pre", "1.f1", "2.rec", "2.pre", "2.f1")
  } else  if (order_type == 2){
    calc_table["0.f1"] <- format(round(2 * (as.numeric(calc_table$`0.rec`) * as.numeric(calc_table$`0.pre`)) / (as.numeric(calc_table$`0.rec`) + as.numeric(calc_table$`0.pre`)),2), nsmall = 2)
    calc_table["1.f1"] <- format(round(2 * (as.numeric(calc_table$`1.rec`) * as.numeric(calc_table$`1.pre`)) / (as.numeric(calc_table$`1.rec`) + as.numeric(calc_table$`1.pre`)),2), nsmall = 2)
    calc_table["2.f1"] <- format(round(2 * (as.numeric(calc_table$`2.rec`) * as.numeric(calc_table$`2.pre`)) / (as.numeric(calc_table$`2.rec`) + as.numeric(calc_table$`2.pre`)),2), nsmall = 2)
    col_order <- c("data", "X1", "acc mean", "acc", "0.rec", "0.pre", "0.f1", "1.rec", "1.pre", "1.f1", "2.rec", "2.pre", "2.f1") 
  }
  calc_table <- calc_table[, col_order]
  return(calc_table)
}

final57 <- readr::read_csv("experiment/final57/final57@summary.csv", stringsAsFactors = FALSE)
final57 <- add_f1_calculation(final57, 1)
acc <- as.numeric(unlist(final57[,"acc"]))
acc <- as.numeric(format(round(acc,2), nsmall = 2))
dataset <- c('davidson', 'davidson', 'davidson', 'founta', 'founta', 'founta', 'wulczyn', 'wulczyn', 'wulczyn')
alg <- c('lr', 'dt', 'svm', 'lr', 'dt', 'svm', 'lr', 'dt', 'svm')
all <- data.frame(dataset, alg, acc)

# davidson best results
davidson.best <- final57[c(1,2,3),]
davidson.best[c(1,2,3),] <- format(round(as.numeric(unlist(davidson.best[c(1,2,3),])),2), nsmall = 2)
davidson.best[,"X1"] <- final57[c(1,2,3),"X1"]
names(davidson.best)[1] <- "alg"
write.csv(davidson.best, "data_plot/nontl_davidson_trained_comparison_only_davidson.csv", row.names=FALSE)

# founta and wulczyn best results
other.best <- final57[c(4,5,6,7,8,9),]
other.best[c(1,2,3,4,5,6),] <- format(round(as.numeric(unlist(other.best[c(1,2,3,4,5,6),])),2), nsmall = 2)
other.best[,"X1"] <- final57[c(4,5,6,7,8,9),"X1"]
names(other.best)[1] <- "alg"
write.csv(other.best, "data_plot/nontl_davidson_trained_comparison_founta_wulczyn.csv", row.names=FALSE)

# save prepared data
write.csv(all, "data_plot/nontl_davidson_trained_comparison.csv", row.names=FALSE)

# plot
jpeg("data_plot/nontl_davidson_trained_comparison.jpg")
ggplot(all, aes(alg, acc, fill = dataset)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x = "Algorithm", y="Accuracy", fill="Dataset") +
  scale_y_continuous(limits=c(0, 1)) +
  theme(text = element_text(size=23))+
  scale_fill_manual(values = c("davidson" = "#0095B6", "founta" = "#E32636", "wulczyn" = "#E4D00A"))
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))


# davidson parameters analytic =======================================================

#prepare dataset
lr <- readr::read_csv("experiment_results/summary_davidson_best_parameters_compare_dt.csv")
dt <- readr::read_csv("experiment_results/summary_davidson_best_parameters_compare_lr.csv")
svm <- readr::read_csv("experiment_results/summary_davidson_best_parameters_compare_svm.csv")
lr <- lr[c('alg','gram','weight','minfreq','TotalMean')]
dt <- dt[c('alg','gram','weight','minfreq','TotalMean')]
svm <- svm[c('alg','gram','weight','minfreq','TotalMean')]
# combine
davidson <- rbind(lr,dt,svm)
# rename column
names(davidson)[4] <- "min"
names(davidson)[5] <- "t.mean"

# weighting
davidson.weighting <- davidson %>%
  dplyr::select(weight,t.mean) %>% 
  dplyr::group_by(weight)%>% 
  dplyr::summarise(
    total.mean = format(round(mean(t.mean),4), nsmall = 4)
  )

write.csv(davidson.weighting, "data_plot/davidson_weighting.csv", row.names=FALSE)
jpeg("data_plot/davidson_weighting.jpg")
ggplot(davidson.weighting, aes(x=weight, y=total.mean, fill=total.mean)) +
  geom_bar(stat="identity") + theme(legend.position = "none") +
  xlab("Weighting") +
  ylab("Accuracy")
dev.off()

# gram
davidson.gram <- davidson %>%
  dplyr::select(gram,t.mean) %>% 
  dplyr::group_by(gram)%>% 
  dplyr::summarise(
    total.mean = format(round(mean(t.mean),4), nsmall = 4)
  )
write.csv(davidson.gram, "data_plot/davidson_gram.csv", row.names=FALSE)
jpeg("data_plot/davidson_gram.jpg")
ggplot(davidson.gram, aes(x=gram, y=total.mean, fill=total.mean)) +
  geom_bar(stat="identity") + theme(legend.position = "none") +
  xlab("N-Gram") +
  ylab("Accuracy")
dev.off()

# min
davidson.min <- davidson %>%
  dplyr::select(min,t.mean) %>% 
  dplyr::group_by(min)%>% 
  dplyr::summarise(
    total.mean = format(round(mean(t.mean),4), nsmall = 4)
  )
write.csv(davidson.min, "data_plot/davidson.min.csv", row.names=FALSE)
jpeg("data_plot/davidson_min.jpg")
ggplot(davidson.min, aes(x=min, y=total.mean, fill=total.mean, label=total.mean)) +
  geom_bar(stat="identity") + theme(legend.position = "none")+
  xlab("Minimum Number of Words")+
  ylab("Accuracy")
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))




# functions ==========
# functions to prepare data
tl_combine_per_experiment_number_for_list <- function(dataset.name, experiment.number, phase){
  if(phase == "selftrained"){
    tl_exp <- readr::read_csv(paste0("experiment/final57",dataset.name,experiment.number,"/final57",dataset.name,experiment.number,"@summary.csv"))
    if(dataset.name == 'founta'){
      tl_exp <- tl_exp[c(4,5,6),"acc"]
    } else if(dataset.name == 'wulczyn'){
      tl_exp <- tl_exp[c(7,8,9),"acc"]
    }
  } else if (phase == "selftrainedcustom") {
    tl_exp <- readr::read_csv(paste0("experiment/final57",dataset.name,"preprocesscustom",experiment.number,"/final57",dataset.name,"preprocesscustom",experiment.number,"@summary.csv"))
    if(dataset.name == 'founta'){
      tl_exp <- tl_exp[c(4,5,6),"acc"]
    } else if(dataset.name == 'wulczyn'){
      tl_exp <- tl_exp[c(7,8,9),"acc"]
    }
  } else if (phase == "transferlearning") {
    tl_exp <- readr::read_csv(paste0("experiment/tl57daume",dataset.name,experiment.number,"/tl57daume",dataset.name,experiment.number,"@summary.csv"))
    tl_exp <- tl_exp[c(1,2,3),"acc"]
  }
  tl_exp[4,] <- mean(unlist(tl_exp))
  return(tl_exp)
}
tl_combine_per_experiment_number_for_table <- function(dataset.name, experiment.number, phase){
  if(phase == "selftrained"){
    tl_exp <- readr::read_csv(paste0("experiment/final57",dataset.name,experiment.number,"/final57",dataset.name,experiment.number,"@summary.csv"))
    if(dataset.name == 'founta'){
      tl_exp <- tl_exp[c(4,5,6),]
    } else if(dataset.name == 'wulczyn'){
      tl_exp <- tl_exp[c(7,8,9),]
    }
  } else if (phase == "selftrainedcustom") {
    tl_exp <- readr::read_csv(paste0("experiment/final57",dataset.name,"preprocesscustom",experiment.number,"/final57",dataset.name,"preprocesscustom",experiment.number,"@summary.csv"))
    if(dataset.name == 'founta'){
      tl_exp <- tl_exp[c(4,5,6),]
    } else if(dataset.name == 'wulczyn'){
      tl_exp <- tl_exp[c(7,8,9),]
    }
  } else if (phase == "transferlearning") {
    tl_exp <- readr::read_csv(paste0("experiment/tl57daume",dataset.name,experiment.number,"/tl57daume",dataset.name,experiment.number,"@summary.csv"))
    tl_exp <- tl_exp[c(1,2,3),]
  }
  tl_exp[,"acc mean"] <- " "
  tl_exp[2,"acc mean"] <- format(round(mean(unlist(tl_exp$acc)),2), nsmall = 2)
  return(tl_exp)
}
tl_combine_per_experiment_for_list <- function(dataset.name, phase){
  exp_001 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.01", phase)
  exp_002 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.02", phase)
  exp_004 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.04", phase)
  exp_006 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.06", phase)
  exp_008 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.08", phase)
  exp_01 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.1", phase)
  exp_02 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.2", phase)
  exp_04 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.4", phase)
  exp_06 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.6", phase)
  exp_08 <- tl_combine_per_experiment_number_for_list(dataset.name,"0.8", phase)
  exp_1 <- tl_combine_per_experiment_number_for_list(dataset.name,"1", phase)
  return(rbind(exp_001,exp_002,exp_004,exp_006, exp_008, exp_01, exp_02, exp_04, exp_06, exp_08, exp_1))
}
tl_combine_per_experiment_for_table <- function(dataset.name, phase){
  exp_001 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.01", phase)
  exp_002 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.02", phase)
  exp_004 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.04", phase)
  exp_006 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.06", phase)
  exp_008 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.08", phase)
  exp_01 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.1", phase)
  exp_02 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.2", phase)
  exp_04 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.4", phase)
  exp_06 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.6", phase)
  exp_08 <- tl_combine_per_experiment_number_for_table(dataset.name,"0.8", phase)
  exp_1 <- tl_combine_per_experiment_number_for_table(dataset.name,"1", phase)
  return(rbind(exp_001,exp_002,exp_004,exp_006, exp_008, exp_01, exp_02, exp_04, exp_06, exp_08, exp_1))
}

# NONTL SELF TRAINED COMPARISON 100% data =================================================
# theme

dav.trained.davidson <- readr::read_csv("experiment/e57/e57@summary.csv")
dav.trained.founta <- readr::read_csv("experiment/final57founta1/final57founta1@summary.csv")
dav.trained.wulczyn <- readr::read_csv("experiment/final57wulczyn1/final57wulczyn1@summary.csv")

dav.trained.davidson <- add_f1_calculation(dav.trained.davidson, 1)
dav.trained.founta <- add_f1_calculation(dav.trained.founta,1)
dav.trained.wulczyn <- add_f1_calculation(dav.trained.wulczyn,1)

dataset <- c('davidson', 'davidson', 'davidson', 'founta', 'founta', 'founta', 'wulczyn', 'wulczyn', 'wulczyn')
alg <- c('lr', 'dt', 'svm', 'lr', 'dt', 'svm', 'lr', 'dt', 'svm')
acc <- c(dav.trained.davidson[1,]$acc,
             dav.trained.davidson[2,]$acc,
             dav.trained.davidson[3,]$acc,
             dav.trained.founta[4,]$acc,
             dav.trained.founta[5,]$acc,
             dav.trained.founta[6,]$acc,
             dav.trained.wulczyn[7,]$acc,
             dav.trained.wulczyn[8,]$acc,
             dav.trained.wulczyn[9,]$acc
)
acc <- as.numeric(unlist(acc))
acc <- format(round(acc,2), nsmall = 2)

all <- data.frame(dataset, alg, acc)

# save results
best.self.trained <- rbind(dav.trained.founta[c(4,5,6),], dav.trained.wulczyn[c(7,8,9),])
best.self.trained[c(1,2,3,4,5,6),] <- format(round(as.numeric(unlist(best.self.trained[c(1,2,3,4,5,6),])),2), nsmall = 2)
best.self.trained[,"X1"] <- rbind(dav.trained.founta[c(4,5,6),"X1"],dav.trained.founta[c(7,8,9),"X1"])
names(best.self.trained)[1] <- "alg"

# save prepared data
write.csv(best.self.trained, "data_plot/nontl_self_trained_comparison_wulczyn_founta.csv", row.names=FALSE)

# save prepared data
write.csv(all, "data_plot/nontl_self_trained_comparison.csv", row.names=FALSE)

# clear environtment
rm(list = setdiff(ls(), lsf.str()))

# NONTL SELF TRAINED COMPARISON all data for founta ==================================
# functions to prepare data
# for transfer learning
# prepare data
data <- c(rep("001", 4),rep("002", 4),rep("004", 4), rep("006", 4), rep("008", 4), rep("010", 4), rep("020", 4), rep("040", 4), rep("060", 4), rep("080", 4), rep("100", 4))
Algorithm <- rep(c('lr', 'dt', 'svm', 'mean'), 11)
acc <- tl_combine_per_experiment_for_list("founta", "selftrained")$acc
phase <- rep('b', 44)
founta <- data.frame(data, phase, Algorithm, acc)
acc.csv <- format(round(acc,2), nsmall = 2)
founta.csv <- data.frame(data, Algorithm, acc.csv)

# save prepared data
write.csv(founta.csv, "data_plot/nontl_selftrained_data_amount_compared_founta.csv", row.names=FALSE)

# prepare data
founta.with.mean <- tl_combine_per_experiment_for_table("founta", "selftrained")
founta.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')] <- format(round(as.numeric(unlist(founta.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')])),2), nsmall = 2)
founta.with.mean$X1 <- rep(c('LR', 'DT', 'SVM'), 11)
founta.with.mean$data <- c(" ", "1%", " "," ", "2%", " "," ", "4%", " "," ", "6%", " "," ", "8%", " "," ", "10%", " "," ", "20%", " "," ", "40%", " "," ", "60%", " "," ", "80%", " "," ", "100%", " ")
founta.with.mean <- founta.with.mean[,c('data','X1','acc mean','acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')]
founta.with.mean <- add_f1_calculation(founta.with.mean,2)
founta.with.mean$`acc mean`[founta.with.mean$`acc mean`=='  NA'] <- ''
write.csv(founta.with.mean, "data_plot/nontl_selftrained_data_amount_compared_founta_mean.csv", row.names=FALSE)

# plot
jpeg("data_plot/nontl_selftrained_data_amount_compared_founta.jpg")
ggplot(founta, aes(x=data, y=acc, group=Algorithm)) +
  geom_line(aes(linetype=Algorithm, color=Algorithm, size=Algorithm))+
  geom_point(aes(color=Algorithm))+
  scale_y_continuous(limits=c(0.5, 0.9)) +
  scale_linetype_manual(values = c(dt="dotted", lr="dotted", svm="dotted", mean="solid"))+
  scale_color_manual(values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black")) +
  scale_size_manual("type", values = c(dt=1, lr=1, svm=1, mean=1), guide = "none")+
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Dataset used (%)")+
  ylab("Frequency")
dev.off()

founta.phaseb <- founta

# NONTL SELF TRAINED COMPARISON all data for wulczyn ==========================
# prepare data
data <- c(rep("001", 4),rep("002", 4),rep("004", 4), rep("006", 4), rep("008", 4), rep("010", 4), rep("020", 4), rep("040", 4), rep("060", 4), rep("080", 4), rep("100", 4))
Algorithm <- rep(c('lr', 'dt', 'svm', 'mean'), 11)
acc <- tl_combine_per_experiment_for_list("wulczyn", "selftrained")$acc
phase <- rep('b', 44)
wulzyn <- data.frame(data, phase, Algorithm, acc)
acc.csv <- format(round(acc,2), nsmall = 2)
wulzyn.csv <- data.frame(data, Algorithm, acc.csv)

# save prepared data
write.csv(wulzyn.csv, "data_plot/nontl_selftrained_data_amount_compared_wulzyn.csv", row.names=FALSE)

# prepare data
wulczyn.with.mean <- tl_combine_per_experiment_for_table("wulczyn", "selftrained")
wulczyn.with.mean$`0.rec` <- 'NA'
wulczyn.with.mean$`0.pre`[wulczyn.with.mean$`0.pre`!='  NA'] <- 0.00
wulczyn.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')] <- format(round(as.numeric(unlist(wulczyn.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')])),2), nsmall = 2)
wulczyn.with.mean$X1 <- rep(c('LR', 'DT', 'SVM'), 11)
wulczyn.with.mean$data <- c(" ", "1%", " "," ", "2%", " "," ", "4%", " "," ", "6%", " "," ", "8%", " "," ", "10%", " "," ", "20%", " "," ", "40%", " "," ", "60%", " "," ", "80%", " "," ", "100%", " ")
wulczyn.with.mean <- wulczyn.with.mean[,c('data','X1','acc mean','acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')]
wulczyn.with.mean <- add_f1_calculation(wulczyn.with.mean,2)
wulczyn.with.mean$`acc mean`[wulczyn.with.mean$`acc mean`=='  NA'] <- ''
write.csv(wulczyn.with.mean, "data_plot/nontl_selftrained_data_amount_compared_wulczyn_mean.csv", row.names=FALSE)

jpeg("data_plot/nontl_selftrained_data_amount_compared_wulzyn.jpg")
ggplot(wulzyn, aes(x=data, y=acc, group=Algorithm)) +
  geom_line(aes(linetype=Algorithm, color=Algorithm, size=Algorithm))+
  geom_point(aes(color=Algorithm))+
  scale_y_continuous(limits=c(0.5, 0.9)) +
  scale_linetype_manual(values = c(dt="dotted", lr="dotted", svm="dotted", mean="solid"))+
  scale_color_manual(values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black")) +
  scale_size_manual("type", values = c(dt=1, lr=1, svm=1, mean=1), guide = "none") +
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Dataset used (%)") +
  ylab("Frequency")
dev.off()

wulczyn.phaseb <- wulzyn

# NONTL SELF TRAINED CUSTOM PRE PROCESSING COMPARISON all data for wulczyn ==========================
# prepare data
data <- c(rep("001", 4),rep("002", 4),rep("004", 4), rep("006", 4), rep("008", 4), rep("010", 4), rep("020", 4), rep("040", 4), rep("060", 4), rep("080", 4), rep("100", 4))
Algorithm <- rep(c('lr', 'dt', 'svm', 'mean'), 11)
acc <- tl_combine_per_experiment_for_list("wulczyn", "selftrainedcustom")$acc
phase <- rep('b-custom', 44)
wulzyn <- data.frame(data, phase, Algorithm, acc)
acc.csv <- format(round(acc,2), nsmall = 2)
wulzyn.csv <- data.frame(data, Algorithm, acc.csv)

# save prepared data
write.csv(wulzyn.csv, "data_plot/nontl_selftrainedcustom_data_amount_compared_wulzyn.csv", row.names=FALSE)

# prepare data
wulczyn.with.mean <- tl_combine_per_experiment_for_table("wulczyn", "selftrainedcustom")
wulczyn.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')] <- format(round(as.numeric(unlist(wulczyn.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')])),2), nsmall = 2)
wulczyn.with.mean$X1 <- rep(c('LR', 'DT', 'SVM'), 11)
wulczyn.with.mean$data <- c(" ", "1%", " "," ", "2%", " "," ", "4%", " "," ", "6%", " "," ", "8%", " "," ", "10%", " "," ", "20%", " "," ", "40%", " "," ", "60%", " "," ", "80%", " "," ", "100%", " ")
wulczyn.with.mean <- wulczyn.with.mean[,c('data','X1','acc mean','acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')]
wulczyn.with.mean$`acc mean`[wulczyn.with.mean$`acc mean`=='  NA'] <- ''
write.csv(wulczyn.with.mean, "data_plot/nontl_selftrainedcustom_data_amount_compared_wulczyn_mean.csv", row.names=FALSE)

jpeg("data_plot/nontl_selftrainedcustom_data_amount_compared_wulzyn.jpg")
ggplot(wulzyn, aes(x=data, y=acc, group=Algorithm)) +
  geom_line(aes(linetype=Algorithm, color=Algorithm, size=Algorithm))+
  geom_point(aes(color=Algorithm))+
  scale_linetype_manual(values = c(dt="dotted", lr="dotted", svm="dotted", mean="solid"))+
  scale_color_manual(values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black")) +
  scale_size_manual("type", values = c(dt=1, lr=1, svm=1, mean=1), guide = "none") +
  xlab("Dataset used (%)") +
  ylab("Frequency")
dev.off()

wulczyn.phasebcustom <- wulzyn

# TL comparison per % of data used for founta =================================

# prepare data
data <- c(rep("001", 4),rep("002", 4),rep("004", 4), rep("006", 4), rep("008", 4), rep("010", 4), rep("020", 4), rep("040", 4), rep("060", 4), rep("080", 4), rep("100", 4))
Algorithm <- rep(c('lr', 'dt', 'svm', 'mean'), 11)
phase <- rep('c', 44)
acc <- tl_combine_per_experiment_for_list("founta", "transferlearning")$acc
founta <- data.frame(data, phase, Algorithm, acc)
acc.csv <- format(round(acc,2), nsmall = 2)
founta.csv <- data.frame(data, Algorithm, acc.csv)

# save prepared data
write.csv(founta.csv, "data_plot/tl_data_amount_compared_founta.csv", row.names=FALSE)

# prepare data
founta.with.mean <- tl_combine_per_experiment_for_table("founta", "transferlearning")
founta.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')] <- format(round(as.numeric(unlist(founta.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')])),2), nsmall = 2)
founta.with.mean$X1 <- rep(c('LR', 'DT', 'SVM'), 11)
founta.with.mean$data <- c(" ", "1%", " "," ", "2%", " "," ", "4%", " "," ", "6%", " "," ", "8%", " "," ", "10%", " "," ", "20%", " "," ", "40%", " "," ", "60%", " "," ", "80%", " "," ", "100%", " ")
founta.with.mean <- founta.with.mean[,c('data','X1','acc mean','acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')]
founta.with.mean <- add_f1_calculation(founta.with.mean, 2)
founta.with.mean$`acc mean`[founta.with.mean$`acc mean`=='  NA'] <- ''
write.csv(founta.with.mean, "data_plot/tl_data_amount_compared_founta_mean.csv", row.names=FALSE)

# plot
jpeg("data_plot/tl_data_amount_compared_founta.jpg")
ggplot(founta, aes(x=data, y=acc, group=Algorithm)) +
  geom_line(aes(linetype=Algorithm, color=Algorithm, size=Algorithm))+
  geom_point(aes(color=Algorithm))+
  scale_y_continuous(limits=c(0.5, 0.9)) +
  scale_linetype_manual(values = c(dt="dotted", lr="dotted", svm="dotted", mean="solid"))+
  scale_color_manual(values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black")) +
  scale_size_manual("type", values = c(dt=1, lr=1, svm=1, mean=1), guide = "none")+
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Dataset used (%)")+
  ylab("Frequency")
dev.off()

founta.phasec <- founta

# TL comparison per % of data used for wulzyn ==========================================================
# prepare data
data <- c(rep("001", 4),rep("002", 4),rep("004", 4), rep("006", 4), rep("008", 4), rep("010", 4), rep("020", 4), rep("040", 4), rep("060", 4), rep("080", 4), rep("100", 4))
Algorithm <- rep(c('lr', 'dt', 'svm', 'mean'), 11)
acc <- tl_combine_per_experiment_for_list("wulczyn", "transferlearning")$acc
phase <- rep('c', 44)
wulzyn <- data.frame(data, phase, Algorithm, acc)
acc.csv <- format(round(acc,2), nsmall = 2)
wulzyn.csv <- data.frame(data, Algorithm, acc.csv)

# save prepared data
write.csv(wulzyn.csv, "data_plot/tl_data_amount_compared_wulzyn.csv", row.names=FALSE)

# prepare data
wulczyn.with.mean <- tl_combine_per_experiment_for_table("wulczyn", "transferlearning")
wulczyn.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')] <- format(round(as.numeric(unlist(wulczyn.with.mean[,c('acc','acc mean','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')])),2), nsmall = 2)
wulczyn.with.mean$X1 <- rep(c('LR', 'DT', 'SVM'), 11)
wulczyn.with.mean$data <- c(" ", "1%", " "," ", "2%", " "," ", "4%", " "," ", "6%", " "," ", "8%", " "," ", "10%", " "," ", "20%", " "," ", "40%", " "," ", "60%", " "," ", "80%", " "," ", "100%", " ")
wulczyn.with.mean <- wulczyn.with.mean[,c('data','X1','acc mean','acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')]
wulczyn.with.mean <- add_f1_calculation(wulczyn.with.mean, 2)
wulczyn.with.mean$`acc mean`[wulczyn.with.mean$`acc mean`=='  NA'] <- ''
write.csv(wulczyn.with.mean, "data_plot/tl_data_amount_compared_wulczyn_mean.csv", row.names=FALSE)

jpeg("data_plot/tl_data_amount_compared_wulzyn.jpg")
ggplot(wulzyn, aes(x=data, y=acc, group=Algorithm)) +
  geom_line(aes(linetype=Algorithm, color=Algorithm, size=Algorithm))+
  geom_point(aes(color=Algorithm))+
  scale_y_continuous(limits=c(0.5, 0.9)) +
  scale_linetype_manual(values = c(dt="dotted", lr="dotted", svm="dotted", mean="solid"))+
  scale_color_manual(values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black")) +
  scale_size_manual("type", values = c(dt=1, lr=1, svm=1, mean=1), guide = "none") +
  theme(text = element_text(size=25),
        axis.text.x = element_text(angle=90, hjust=1))+
  xlab("Dataset used (%)") +
  ylab("Frequency")
dev.off()

wulczyn.phasec <- wulzyn

# TL vs NONTL founta ===========

final57 <- readr::read_csv("data_plot/nontl_davidson_trained_comparison.csv")
final57 <- unname(unlist(final57[c(4,5,6),"acc"]))
final57.mean <- mean(final57)

founta.combined <- rbind(founta.phaseb, founta.phasec)

jpeg("data_plot/final_comparison_founta.jpg")
ggplot(founta.combined, aes(x=data, y=acc, colour=Algorithm, linetype = phase,
                        group=interaction(Algorithm, phase))) +
scale_colour_manual(name="Algorithm", labels = c("Decision Tree", "Logistic Regression", "Mean", "SVM"), values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black"))+
scale_linetype_manual(name="Experiment", labels = c("Training on Target Only", "Domain Adaptation"), values=c(a = "dotted", b = "dashed", c="solid"))+
scale_y_continuous(limits=c(0.5, 0.9)) +
geom_hline(aes(yintercept=final57[1]), linetype="twodash", color = "#E32636")+ 
geom_hline(aes(yintercept=final57[2]), linetype="twodash", color = "#0095B6")+
geom_hline(aes(yintercept=final57[3]), linetype="twodash", color = "#E4D00A")+
geom_hline(aes(yintercept=final57.mean), linetype="twodash", color = "black")+  
geom_point() + geom_line() +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position="right") +
xlab("Target Domain Dataset Used For Training (%)")+
ylab("Accuracy")
dev.off()

# NONTL custom vs NONTL wulczyn ===========
jpeg("data_plot/final_comparison_wulczyn_custom.jpg")
wulczyn.combined <- rbind(wulczyn.phaseb, wulczyn.phasebcustom)
ggplot(wulczyn.combined, aes(x=data, y=acc, colour=Algorithm, linetype = phase,
                            group=interaction(Algorithm, phase))) + 
  geom_point() + geom_line()
# TL vs NONTL wulczyn ===========
final57 <- readr::read_csv("data_plot/nontl_davidson_trained_comparison.csv")
final57 <- unname(unlist(final57[c(7,8,9),"acc"]))
final57.mean <- mean(final57)


jpeg("data_plot/final_comparison_wulczyn.jpg")
wulczyn.combined <- rbind(wulczyn.phaseb, wulczyn.phasec)
ggplot(wulczyn.combined, aes(x=data, y=acc, colour=Algorithm, linetype = phase, 
                             group=interaction(Algorithm, phase))) + 
scale_colour_manual(name="Algorithm", labels = c("Decision Tree", "Logistic Regression", "Mean", "SVM"), values = c(dt="#0095B6", lr="#E32636", svm="#E4D00A", mean="black"))+
scale_linetype_manual(name="Experiment", labels = c("Training on Target Only", "Domain Adaptation"), values=c(a = "dotted", b = "dashed", c="solid"))+
scale_y_continuous(limits=c(0.5, 0.9)) +
geom_hline(yintercept=final57[1], linetype="dotted", color = "#0095B6")+
geom_hline(yintercept=final57[2], linetype="dotted", color = "#E32636")+
geom_hline(yintercept=final57[3], linetype="dotted", color = "#E4D00A")+
geom_hline(yintercept=final57.mean, linetype="dotted", color = "black")+  
  geom_point() + geom_line() +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position="right") +
xlab("Target Domain Dataset Used For Training (%)")+
ylab("Accuracy")
dev.off()

# Final summary===========================================================
# prepare data
final57 <- readr::read_csv("experiment/final57/final57@summary.csv")
final57founta <- readr::read_csv("experiment/final57founta1/final57founta1@summary.csv")
final57wulczyn <- readr::read_csv("experiment/final57wulczyn1/final57wulczyn1@summary.csv")
tl57daumefounta1 <- readr::read_csv("experiment/tl57daumefounta1/tl57daumefounta1@summary.csv")
tl57daumewulczyn1 <- readr::read_csv("experiment/tl57daumewulczyn1/tl57daumewulczyn1@summary.csv")
dataset <- c(rep("founta", 3), rep("wulczyn", 3))
phase <- rep(c('a', 'b', 'c'), 2)
acc <- c(
  mean(unlist(final57[c(4,5,6),'acc'])),
  mean(unlist(final57founta[c(4,5,6),'acc'])),
  mean(unlist(tl57daumefounta1[c(1,2,3),'acc'])),
  mean(unlist(final57[c(7,8,9),'acc'])),
  mean(unlist(final57wulczyn[c(7,8,9),'acc'])),
  mean(unlist(tl57daumewulczyn1[c(1,2,3),'acc']))
)
acc <- format(round(acc,2), nsmall = 2)
summary <- data.frame(dataset, phase, acc)
# save prepared data
write.csv(summary, "data_plot/final_summary.csv", row.names=FALSE)

no <- c(1:18)
target <- rep(" ", 18)
target[1] <- "Founta"
target[10] <- "Wulczyn"
phase <- rep(" ", 18)
phase[1] <- "A"
phase[4] <- "B"
phase[7] <- "C"
phase[10] <- "A"
phase[13] <- "B"
phase[16] <- "C"
alg <- rep(c("lr","dt","svm"), 6)
acc.detail <- rbind(
  final57[c(4,5,6),c('acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')],
  final57founta[c(4,5,6),c('acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')],
  tl57daumefounta1[c(1,2,3),c('acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')],
  final57[c(7,8,9),c('acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')],
  final57wulczyn[c(7,8,9),c('acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')],
  tl57daumewulczyn1[c(1,2,3),c('acc','0.rec','0.pre','1.rec','1.pre','2.rec','2.pre')]
)
acc.detail[] <- lapply(acc.detail, function(x) if(is.numeric(x)) format(round(x,2), nsmall = 2) else x)
acc.mean <- rep(" ", 18)
acc.mean[1] <- acc[1]
acc.mean[4] <- acc[2]
acc.mean[7] <- acc[3]
acc.mean[10] <- acc[4]
acc.mean[13] <- acc[5]
acc.mean[16] <- acc[6]
summary.details <- data.frame(no,target,phase,acc.mean,alg,acc.detail)
summary.details <- data.frame(no,target,phase,acc.mean,alg,acc.detail)
write.csv(summary.details, "data_plot/final_summary_details.csv", row.names=FALSE)

jpeg("data_plot/final_summary.jpg")
ggplot(summary, aes(dataset, acc, fill = phase)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x = "Dataset", y="Accuracy", fill="Phase") +
  scale_fill_manual(values = c("a" = "#0095B6", "b" = "#E32636", "c" = "#E4D00A"))
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))


#=================================
#=================================

# original dataset analysis =====================================================================================
# theme
# PART 1 
# twitter davidson load data 

# load data
davidson <- readr::read_csv("data/twitter.dataset.davidson/labeled_data.csv")

# select class & tweet and rename
davidson <- davidson %>%
  dplyr::select(tweet,class) %>%
  dplyr::rename(class_label = class,text = tweet)

# twitter founta load data 

# load data
founta <- readr::read_csv2("data/twitter.dataset.founta/hatespeech_text_label_vote_updated.csv")

# select class & tweet and rename
founta <- founta %>%
  dplyr::mutate(class_label = recode(class, "hateful"=0, "spam"=1, "abusive"=2,"normal"=3))%>% 
  dplyr::select(tweet,class_label) %>%
  dplyr::rename(text = tweet)

# wiki wulczyn load data 

# load data
wiki.label <- as.data.frame(fread("data/wikipedia.dataset.wulczyn/attack_annotated_comments.tsv"))
wiki.id <- as.data.frame(fread("data/wikipedia.dataset.wulczyn/attack_annotations.tsv"))

# decide for each record, which class wins the vote
wiki.id.class <- wiki.id %>% 
  dplyr::select(-worker_id) %>% 
  dplyr::group_by(rev_id) %>% 
  dplyr::summarise(
    quoting_attack = sum(quoting_attack),
    recipient_attack = sum(recipient_attack),
    third_party_attack = sum(third_party_attack),
    other_attack = sum(other_attack),
    attack = sum(attack)
  ) %>%
  mutate(combined_class = mapply(wiki_is_combined_class,quoting_attack,recipient_attack,third_party_attack,other_attack)) %>%
  mutate(class_label = mapply(wiki_classify_class,combined_class,quoting_attack,recipient_attack,third_party_attack,other_attack))

# join the comment and its label
wulczyn <- left_join(wiki.id.class,wiki.label,by="rev_id") %>% 
  dplyr::select(comment,class_label) %>%
  dplyr::rename(text = comment)


# PART 2 ANALYSIS
# twitter davidson DA 

# rename class column
davidson.analysis <- davidson %>% 
  dplyr::mutate(class_label = recode(class_label,"0" = "hate_speech","1" = "offensive_language","2" ="neither"))

#barplot
jpeg("data_plot/twitter_davidson_distribution.png")
davison.plot <- data.frame(Dataset=davidson.analysis$class_label)
ggplot(davison.plot, aes(x=Dataset, fill = Dataset))+
  stat_count()+
  theme(axis.text.x=element_blank())+
  ylab("Frequency")
dev.off()

# twitter founta DA 

# rename class column
founta.analysis <- founta %>% 
  dplyr::mutate(class_label = recode(class_label, "0" = "hateful","1" = "spam","2" ="abusive","3"="normal"))

#barplot class
jpeg("data_plot/twitter_founta_distribution.png")
founta.plot <- data.frame(Dataset=founta.analysis$class_label)
ggplot(founta.plot, aes(x=Dataset, fill = Dataset))+
  stat_count()+
  theme(axis.text.x=element_blank())+
  ylab("Frequency")
dev.off()

# wiki wulczyn DA 

wulczyn.analysis <- wulczyn %>% 
  dplyr::mutate(class_label = recode(class_label, "0" = "normal", "1" = "quoting_attack", "2" ="recipient_attack", "3"="third_party_attack", "4"="other_attack", "5"="multiple"))

#barplot class
jpeg("data_plot/wiki_qulczyn_distribution.png")
wulczyn.plot <- data.frame(Dataset=wulczyn.analysis$class_label)
ggplot(wulczyn.plot, aes(x=Dataset, fill = Dataset))+
  stat_count()+
  theme(axis.text.x=element_blank())+
  ylab("Frequency")
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))

# before preprocess ===============================================================
davidson.dataset.complete <- readRDS(file = "experiment/final57/final57_davidson_rds_1_load.rds")
founta.dataset.complete <- readRDS(file = "experiment/final57/final57_founta_rds_1_load.rds")
wulczyn.dataset.complete <- readRDS(file = "experiment/final57/final57_wulczyn_rds_1_load.rds")

# count
count(davidson.dataset.complete)
count(founta.dataset.complete)
count(wulczyn.dataset.complete)

# text examples
set.seed(1234)
example.davidson <- davidson.dataset.complete %>% dplyr::group_by(class_label) %>% sample_n(size = 5)
example.founta <- founta.dataset.complete %>% dplyr::group_by(class_label) %>% sample_n(size = 5)
example.wulczyn <- wulczyn.dataset.complete %>% dplyr::group_by(class_label) %>% sample_n(size = 5)
write.csv(example.davidson, "data_plot/example_davidson.csv", row.names=FALSE)
write.csv(example.founta, "data_plot/example_founta.csv", row.names=FALSE)
write.csv(example.wulczyn, "data_plot/example_wulczyn.csv", row.names=FALSE)


# clear environtment
rm(list = setdiff(ls(), lsf.str()))

# after preprocess # words length distribution==============================================
davidson.dataset.preprocess <- readRDS(file = "experiment/final57/final57_davidson_rds_3b_preprocess.rds")
founta.dataset.preprocess <- readRDS(file = "experiment/final57founta1/final57founta1_founta_rds_3b_preprocess.rds")
wulczyn.dataset.preprocess <- readRDS(file = "experiment/final57wulczyn1/final57wulczyn1_wulczyn_rds_3b_preprocess.rds")
davidson.dataset.preprocess$train$log$n99afterpreprocessing
founta.dataset.preprocess$train$log$n99afterpreprocessing
wulczyn.dataset.preprocess$train$log$n99afterpreprocessing
x <- davidson.dataset.preprocess$train$dataset
y <- founta.dataset.preprocess$train$dataset
z <- wulczyn.dataset.preprocess$train$dataset
x <- rowSums(x[sapply(x, is.numeric)], na.rm = TRUE)
y <- rowSums(y[sapply(y, is.numeric)], na.rm = TRUE)
z <- rowSums(z[sapply(z, is.numeric)], na.rm = TRUE)

dataset <- c('Davidson', 'Founta', 'Wulczyn')

# rata2 length
text.length.mean <- c(
  mean(x),
  mean(y),
  mean(z)
)

text.length.mean <- format(round(text.length.mean,2), nsmall = 2)

text.length.mean <- data.frame(dataset, text.length.mean)
write.csv(text.length.mean, 'data_plot/text_length_mean.csv', row.names=FALSE)

# max length
text.length.max <- c(
  max(x),
  max(y),
  max(z)
)

text.length.max <- data.frame(dataset, text.length.max)
write.csv(text.length.max, 'data_plot/text_length_max.csv', row.names=FALSE)

# hist
jpeg("data_plot/hist_text_length_davidson.jpg")
words <- data.frame(x,rep(0, length(x)))
ggplot(words, aes(x=x))+
  geom_histogram(bins=20) +
  theme(text = element_text(size=25))+
  xlab("Text Length")+
  ylab("Frequency")
dev.off()
jpeg("data_plot/hist_text_length_founta.jpg")
words <- data.frame(y,rep(0, length(y)))
ggplot(words, aes(x=y))+
  geom_histogram(bins=20) +
  theme(text = element_text(size=25))+
  xlab("Text Length")+
  ylab("Frequency")
dev.off()
jpeg("data_plot/hist_text_length_wulczyn.jpg")
words <- data.frame(z,rep(0, length(z)))
ggplot(words, aes(x=z))+
  geom_histogram(bins=20) +
  theme(text = element_text(size=25))+
  xlab("Text Length")+
  ylab("Frequency")
dev.off()

# for density filtering
x <- x[x < 75]
y <- y[y < 75]
z <- z[z < 75]

dataset <- c(rep('Davidson', length(x)), rep('Founta', length(y)), rep('Wulczyn', length(z)))
words.length <- c(x,y,z)
words <- data.frame(dataset, words.length)

# hist
jpeg("data_plot/density_text_length.jpg")
ggplot(words, aes(x=words.length, fill=dataset))+
  geom_density(alpha=.5)+ 
  theme(text = element_text(size=23))+
  xlab("Text Length")+
  ylab("Density")
dev.off()


# clear environtment
rm(list = setdiff(ls(), lsf.str()))

# after preprocess # words uniques distribution ================================
davidson.dataset.preprocess <- readRDS(file = "experiment/final57/final57_davidson_rds_3b_preprocess.rds")
founta.dataset.preprocess <- readRDS(file = "experiment/final57founta1/final57founta1_founta_rds_3b_preprocess.rds")
wulczyn.dataset.preprocess <- readRDS(file = "experiment/final57wulczyn1/final57wulczyn1_wulczyn_rds_3b_preprocess.rds")
x <- davidson.dataset.preprocess$train$dataset
y <- founta.dataset.preprocess$train$dataset
z <- wulczyn.dataset.preprocess$train$dataset
x <- rowSums(x != 0, na.rm = TRUE)
y <- rowSums(y != 0, na.rm = TRUE)
z <- rowSums(z != 0, na.rm = TRUE)

dataset <- c('Davidson', 'Founta', 'Wulczyn')

# rata2 length
count.words.mean <- c(
  mean(x),
  mean(y),
  mean(z)
)

count.words.mean <- format(round(count.words.mean,2), nsmall = 2)

count.words.mean <- data.frame(dataset, count.words.mean)
write.csv(count.words.mean, 'data_plot/count_words_mean.csv', row.names=FALSE)

# max length
count.words.max <- c(
  max(x),
  max(y),
  max(z)
)

count.words.max <- data.frame(dataset, count.words.max)
write.csv(count.words.max, 'data_plot/count_words_max.csv', row.names=FALSE)

jpeg("data_plot/hist_words_count_davidson.jpg")
words <- data.frame(x,rep(0, length(x)))
ggplot(words, aes(x=x))+
  geom_histogram(bins=20) +
  theme(text = element_text(size=25))+
  xlab("Unique Words")+
  ylab("Frequency")
dev.off()
jpeg("data_plot/hist_words_count_founta.jpg")
words <- data.frame(y,rep(0, length(y)))
ggplot(words, aes(x=y))+
  geom_histogram(bins=20) +
  theme(text = element_text(size=25))+
  xlab("Unique Words")+
  ylab("Frequency")
dev.off()
jpeg("data_plot/hist_words_count_wulczyn.jpg")
words <- data.frame(z,rep(0, length(z)))
ggplot(words, aes(x=z))+
  geom_histogram(bins=20) +
  theme(text = element_text(size=25))+
  xlab("Unique Words")+
  ylab("Frequency")
dev.off()

# for histogram filtering
x <- x[x < 75]
y <- y[y < 75]
z <- z[z < 75]

dataset <- c(rep('Davidson', length(x)), rep('Founta', length(y)), rep('Wulczyn', length(z)))
unique.words <- c(x,y,z)
words <- data.frame(dataset, unique.words)

# density
jpeg("data_plot/density_words_count.jpg")
ggplot(words, aes(x=unique.words, fill=dataset))+
  geom_density(alpha=.5)+
  theme(text = element_text(size=23))+
  xlab("Unique Words")+
  ylab("Density")
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))

# top words=====================================================================

stopwords <- c(stopwords("english"), "tmention", "thashtags", "tretweet", "turl")
stopwords <- data.frame(word = stopwords)

x <- readRDS(file = "experiment/final57/final57_davidson_rds_3b_preprocess.rds")
davidson.dataset.topwords <- analyse_words_freq(x$train$dataset)
davidson.dataset.topwords <- filter(davidson.dataset.topwords, word != 'class_label')
davidson.dataset.topwords <- anti_join(davidson.dataset.topwords, stopwords, by='word')
jpeg("data_plot/word_cloud_davidson.jpg")
analyse_words_cloud(davidson.dataset.topwords, 300)
dev.off()

y <- readRDS(file = "experiment/final57founta1/final57founta1_founta_rds_3b_preprocess.rds")
founta.dataset.topwords <- analyse_words_freq(y$train$dataset)
founta.dataset.topwords <- filter(founta.dataset.topwords, word != 'class_label')
founta.dataset.topwords <- anti_join(founta.dataset.topwords, stopwords, by='word')
jpeg("data_plot/word_cloud_founta.jpg")
analyse_words_cloud(founta.dataset.topwords, 300)
dev.off()

z <- readRDS(file = "experiment/final57wulczyn1/final57wulczyn1_wulczyn_rds_3b_preprocess.rds")
wulczyn.dataset.topwords <- analyse_words_freq(z$train$dataset)
wulczyn.dataset.topwords <- filter(wulczyn.dataset.topwords, word != 'class_label')
wulczyn.dataset.topwords <- anti_join(wulczyn.dataset.topwords, stopwords, by='word')
jpeg("data_plot/word_cloud_wulczyn.jpg")
analyse_words_cloud(founta.dataset.topwords, 300)
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))


# transfer learning founta top determiners ======================================================================
founta.dataset.tl <- readRDS(file = "experiment/tl57daumefounta1/tl57daumefounta1_davidson_rds_3b_preprocess.rds")
founta.dataset.tl.all <- founta.dataset.tl$train$dataset

founta.dataset.tl.0 <- founta.dataset.tl.all %>%
  filter(class_label == 0)
founta.dataset.tl.0 <- analyse_words_freq(founta.dataset.tl.0)
founta.dataset.tl.0 <- founta.dataset.tl.0 %>%
  filter(str_detect(word, '_g$'))
jpeg("data_plot/word_cloud_founta_determiner_0.jpg")
  analyse_words_cloud(founta.dataset.tl.0, 100)
dev.off()

founta.dataset.tl.1 <- founta.dataset.tl.all %>%
  filter(class_label == 1)
founta.dataset.tl.1 <- analyse_words_freq(founta.dataset.tl.1)
founta.dataset.tl.1 <- founta.dataset.tl.1 %>%
  filter(str_detect(word, '_g$'))
jpeg("data_plot/word_cloud_founta_determiner_1.jpg")
analyse_words_cloud(founta.dataset.tl.1, 100)
dev.off()

founta.dataset.tl.2 <- founta.dataset.tl.all %>%
  filter(class_label == 2)
founta.dataset.tl.2 <- analyse_words_freq(founta.dataset.tl.2)
founta.dataset.tl.2 <- founta.dataset.tl.2 %>%
  filter(str_detect(word, '_g$'))
jpeg("data_plot/word_cloud_founta_determiner_2.jpg")
analyse_words_cloud(founta.dataset.tl.2, 100)
dev.off()

# after stop words removed
stopwords <- c(paste0(stopwords("english"),"_g"), "tmention_g", "thashtags_g", "tretweet_g", "turl_g")
stopwords <- data.frame(word = stopwords)

founta.dataset.tl.0 <- anti_join(founta.dataset.tl.0, stopwords, by='word')
founta.dataset.tl.1 <- anti_join(founta.dataset.tl.1, stopwords, by='word')
founta.dataset.tl.2 <- anti_join(founta.dataset.tl.2, stopwords, by='word')

jpeg("data_plot/word_cloud_founta_determiner_without_stopwords0.jpg")
analyse_words_cloud(founta.dataset.tl.0, 100)
dev.off()

jpeg("data_plot/word_cloud_founta_determiner_without_stopwords1.jpg")
analyse_words_cloud(founta.dataset.tl.1, 100)
dev.off()

jpeg("data_plot/word_cloud_founta_determiner_without_stopwords2.jpg")
  analyse_words_cloud(founta.dataset.tl.2, 100)
dev.off()

# transfer learning wulczyn top determiners ======================================================================
wulczyn.dataset.tl <- readRDS(file = "experiment/tl57daumewulczyn1/tl57daumewulczyn1_davidson_rds_3b_preprocess.rds")
wulczyn.dataset.tl.all <- wulczyn.dataset.tl$train$dataset

wulczyn.dataset.tl.0 <- wulczyn.dataset.tl.all %>%
  filter(class_label == 0)
wulczyn.dataset.tl.0 <- analyse_words_freq(wulczyn.dataset.tl.0)
wulczyn.dataset.tl.0 <- wulczyn.dataset.tl.0 %>%
  filter(str_detect(word, '_g$'))
jpeg("data_plot/word_cloud_wulczyn_determiner_0.jpg")
analyse_words_cloud(wulczyn.dataset.tl.0, 100)
dev.off()

wulczyn.dataset.tl.1 <- wulczyn.dataset.tl.all %>%
  filter(class_label == 1)
wulczyn.dataset.tl.1 <- analyse_words_freq(wulczyn.dataset.tl.1)
wulczyn.dataset.tl.1 <- wulczyn.dataset.tl.1 %>%
  filter(str_detect(word, '_g$'))
jpeg("data_plot/word_cloud_wulczyn_determiner_1.jpg")
analyse_words_cloud(wulczyn.dataset.tl.1, 100)
dev.off()

wulczyn.dataset.tl.2 <- wulczyn.dataset.tl.all %>%
  filter(class_label == 2)
wulczyn.dataset.tl.2 <- analyse_words_freq(wulczyn.dataset.tl.2)
wulczyn.dataset.tl.2 <- wulczyn.dataset.tl.2 %>%
  filter(str_detect(word, '_g$'))
jpeg("data_plot/word_cloud_wulczyn_determiner_2.jpg")
analyse_words_cloud(wulczyn.dataset.tl.2, 100)
dev.off()

# after stop words removed

stopwords <- c(paste0(stopwords("english"),"_g"), "tmention_g", "thashtags_g", "tretweet_g", "turl_g")
stopwords <- data.frame(word = stopwords)

wulczyn.dataset.tl.0 <- anti_join(wulczyn.dataset.tl.0, stopwords, by='word')
wulczyn.dataset.tl.1 <- anti_join(wulczyn.dataset.tl.1, stopwords, by='word')
wulczyn.dataset.tl.2 <- anti_join(wulczyn.dataset.tl.2, stopwords, by='word')

jpeg("data_plot/word_cloud_wulczyn_determiner_without_stopwords0.jpg")
analyse_words_cloud(wulczyn.dataset.tl.0, 100)
dev.off()

jpeg("data_plot/word_cloud_wulczyn_determiner_without_stopwords1.jpg")
analyse_words_cloud(wulczyn.dataset.tl.1, 100)
dev.off()

jpeg("data_plot/word_cloud_wulczyn_determiner_without_stopwords2.jpg")
analyse_words_cloud(wulczyn.dataset.tl.2, 100)
dev.off()

# clear environtment
rm(list = setdiff(ls(), lsf.str()))


# transfer learning founta top target words ======================================================================
founta.dataset.tl <- readRDS(file = "experiment/tl57daumefounta1/tl57daumefounta1_davidson_rds_3b_preprocess.rds")
founta.dataset.tl.all <- founta.dataset.tl$train$dataset

founta.dataset.tl.0 <- founta.dataset.tl.all %>%
  filter(class_label == 0)
founta.dataset.tl.0 <- analyse_words_freq(founta.dataset.tl.0)
founta.dataset.tl.0 <- founta.dataset.tl.0 %>%
  filter(str_detect(word, '_t$'))
jpeg("data_plot/word_cloud_founta_source_0.jpg")
analyse_words_cloud(founta.dataset.tl.0, 100)
dev.off()

founta.dataset.tl.1 <- founta.dataset.tl.all %>%
  filter(class_label == 1)
founta.dataset.tl.1 <- analyse_words_freq(founta.dataset.tl.1)
founta.dataset.tl.1 <- founta.dataset.tl.1 %>%
  filter(str_detect(word, '_t$'))
jpeg("data_plot/word_cloud_founta_source_1.jpg")
analyse_words_cloud(founta.dataset.tl.1, 100)
dev.off()

founta.dataset.tl.2 <- founta.dataset.tl.all %>%
  filter(class_label == 2)
founta.dataset.tl.2 <- analyse_words_freq(founta.dataset.tl.2)
founta.dataset.tl.2 <- founta.dataset.tl.2 %>%
  filter(str_detect(word, '_t$'))
jpeg("data_plot/word_cloud_founta_source_2.jpg")
analyse_words_cloud(founta.dataset.tl.2, 100)
dev.off()

# after stop words removed
stopwords <- c(paste0(stopwords("english"),"_t"), "tmention_t", "thashtags_t", "tretweet_t", "turl_t")
stopwords <- data.frame(word = stopwords)

founta.dataset.tl.0 <- anti_join(founta.dataset.tl.0, stopwords, by='word')
founta.dataset.tl.1 <- anti_join(founta.dataset.tl.1, stopwords, by='word')
founta.dataset.tl.2 <- anti_join(founta.dataset.tl.2, stopwords, by='word')

jpeg("data_plot/word_cloud_founta_source_without_stopwords0.jpg")
analyse_words_cloud(founta.dataset.tl.0, 100)
dev.off()

jpeg("data_plot/word_cloud_founta_source_without_stopwords1.jpg")
analyse_words_cloud(founta.dataset.tl.1, 100)
dev.off()

jpeg("data_plot/word_cloud_founta_source_without_stopwords2.jpg")
analyse_words_cloud(founta.dataset.tl.2, 100)
dev.off()

# transfer learning wulczyn top target words ======================================================================
wulczyn.dataset.tl <- readRDS(file = "experiment/tl57daumewulczyn1/tl57daumewulczyn1_davidson_rds_3b_preprocess.rds")
wulczyn.dataset.tl.all <- wulczyn.dataset.tl$train$dataset

wulczyn.dataset.tl.0 <- wulczyn.dataset.tl.all %>%
  filter(class_label == 0)
wulczyn.dataset.tl.0 <- analyse_words_freq(wulczyn.dataset.tl.0)
wulczyn.dataset.tl.0 <- wulczyn.dataset.tl.0 %>%
  filter(str_detect(word, '_t$'))
jpeg("data_plot/word_cloud_wulczyn_source_0.jpg")
analyse_words_cloud(wulczyn.dataset.tl.0, 100)
dev.off()

wulczyn.dataset.tl.1 <- wulczyn.dataset.tl.all %>%
  filter(class_label == 1)
wulczyn.dataset.tl.1 <- analyse_words_freq(wulczyn.dataset.tl.1)
wulczyn.dataset.tl.1 <- wulczyn.dataset.tl.1 %>%
  filter(str_detect(word, '_t$'))
jpeg("data_plot/word_cloud_wulczyn_source_1.jpg")
analyse_words_cloud(wulczyn.dataset.tl.1, 100)
dev.off()

wulczyn.dataset.tl.2 <- wulczyn.dataset.tl.all %>%
  filter(class_label == 2)
wulczyn.dataset.tl.2 <- analyse_words_freq(wulczyn.dataset.tl.2)
wulczyn.dataset.tl.2 <- wulczyn.dataset.tl.2 %>%
  filter(str_detect(word, '_t$'))
jpeg("data_plot/word_cloud_wulczyn_source_2.jpg")
analyse_words_cloud(wulczyn.dataset.tl.2, 100)
dev.off()

# after stop words removed

stopwords <- c(paste0(stopwords("english"),"_t"), "tmention_t", "thashtags_t", "tretweet_t", "turl_t")
stopwords <- data.frame(word = stopwords)

wulczyn.dataset.tl.0 <- anti_join(wulczyn.dataset.tl.0, stopwords, by='word')
wulczyn.dataset.tl.1 <- anti_join(wulczyn.dataset.tl.1, stopwords, by='word')
wulczyn.dataset.tl.2 <- anti_join(wulczyn.dataset.tl.2, stopwords, by='word')

jpeg("data_plot/word_cloud_wulczyn_source_without_stopwords0.jpg")
analyse_words_cloud(wulczyn.dataset.tl.0, 100)
dev.off()

jpeg("data_plot/word_cloud_wulczyn_source_without_stopwords1.jpg")
analyse_words_cloud(wulczyn.dataset.tl.1, 100)
dev.off()

jpeg("data_plot/word_cloud_wulczyn_source_without_stopwords2.jpg")
analyse_words_cloud(wulczyn.dataset.tl.2, 100)
dev.off()





#=================================
#=================================

# confusion matrix =====================================

get_table_conf_matrix <- function(predict.result, dataset.predict, predict.set.type, file_name){
  x <- dataset.predict[[predict.set.type]]
  class.label <- x$dataset$class_label
  
  print_highlight('length')
  print(length(class.label))
  print(length(predict.result))
  
  confusionmatrix <- caret::confusionMatrix(predict.result, as.factor(class.label), dnn = c("Prediction", "Reference"))
  
  plt <- as.data.frame(confusionmatrix$table)
  plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
  print(plt$Prediction)
  
  jpeg(paste0("data_plot/",file_name,".jpg"))
  print({
    ggplot(plt, aes(Reference, Prediction, fill= Freq)) +
      geom_tile() + geom_text(aes(label=Freq), size = 8) +
      scale_fill_gradient(low="white", high="#E4D00A") +
      labs(x = "Reference",y = "Prediction") +
      scale_x_discrete(labels=c("0","1","2")) +
      scale_y_discrete(labels=c("2","1","0")) +
      theme(legend.position="none") +
      theme(text = element_text(size=40), axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  dev.off()
  
  print_highlight('label distribution')
  print(count(as.factor(class.label)))
  
  print_highlight('table')
  print(confusionmatrix$table)
  
  # show accuracy
  print_highlight('accuracy')
  print(confusionmatrix$overall['Accuracy'])
  
  # show recall
  print_highlight('recall')
  print(confusionmatrix$byClass[, 'Recall'])
  
  # show precision
  print_highlight('precision')
  print(confusionmatrix$byClass[, 'Precision']) 
  
  return(confusionmatrix$table)
}
get_table_conf_matrix_pre <- function(name, algorithm, predict.set.type, file_name){
  if (algorithm == 'lr'){
    letter <- 'a'
  } else if (algorithm == 'dt'){
    letter <- 'b'
  } else if (algorithm == 'svm'){
    letter <- 'c'
  }
  
  predict.result.dir <- paste0("experiment/",name,"/",name,"_",const.source.domain,"_rds_5",letter,"_predict_",algorithm,"_from_",const.source.domain,".rds")
  predict.result <- readRDS(file = predict.result.dir)
  print(predict.result.dir)
  
  dataset.predict.dir <- paste0("experiment/",name,"/",name,"_",const.source.domain,"_rds_3b_preprocess.rds")
  dataset.predict <- readRDS(file = dataset.predict.dir)
  print(dataset.predict.dir)
  
  return(get_table_conf_matrix(predict.result,dataset.predict,predict.set.type, file_name ))
  
  # clear environtment
  rm(list = setdiff(ls(), lsf.str()))
}

# target only founta
# with good results
const.source.domain <- 'founta'
name <- 'final57founta1'
algorithm <- 'dt'
predict.set.type <- 'test'
file_name <- "conf_matrix_1"
table <- get_table_conf_matrix_pre(name, algorithm, predict.set.type, file_name)
write.csv(table, "data_plot/cof_matrix_1.csv")

# target only founta
# the one all 0
const.source.domain <- 'founta'
name <- 'final57founta0.01'
algorithm <- 'lr'
predict.set.type <- 'test'
file_name <- "conf_matrix_2"
table <- get_table_conf_matrix_pre(name, algorithm, predict.set.type, file_name)
write.csv(table, "data_plot/cof_matrix_2.csv")

# target only founta
# the one with 1
const.source.domain <- 'founta'
name <- 'final57founta0.04'
algorithm <- 'svm'
predict.set.type <- 'test'
file_name <- "conf_matrix_3"
table <- get_table_conf_matrix_pre(name, algorithm, predict.set.type, file_name)
write.csv(table, "data_plot/cof_matrix_3.csv")


# target only wulczyn
const.source.domain <- 'wulczyn'
name <- 'final57wulczyn1'
algorithm <- 'svm'
predict.set.type <- 'test'
file_name <- "conf_matrix_4"
table <- get_table_conf_matrix_pre(name, algorithm, predict.set.type, file_name)
write.csv(table, "data_plot/cof_matrix_4.csv")

# words coefficient (NOT FINISHED) ======================================================================

#check: https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/
#check: https://stats.idre.ucla.edu/r/dae/logit-regression/

# LR
lr.test <- readRDS(file = "experiment/e57/e57_davidson_rds_4a_training_lr.rds")
lr.summary <- summary(lr.test)
lr.coef <- coef(lr.test)
View(lr.coef)

View(lr.summary$coefficients)


# SVM
svm.test <- readRDS(file = "experiment/e57/e57_davidson_rds_4c_training_svm.rds")
svm.coef <- svm.test$coefs
svm.coef
NROW(svm.coef)

# DT
dt.coef <- readRDS(file = "experiment/e57/e57_davidson_rds_4b_training_dt.rds")
dt.coef <- dt.coef$variable.importance
dt.coef <- dt.coef[1:20]
View(dt.coef)