# RUN FIRST MAIN.R to get library

# load from csv
svm <- readr::read_csv( 'experiment_results/summary_davidson_best_parameters_svm.csv')
dt <- readr::read_csv( 'experiment_results/summary_davidson_best_parameters_dt.csv')
lr <- readr::read_csv( 'experiment_results/summary_davidson_best_parameters_lr.csv')

summarize.all <- function(all.data){
  # convert to numbers
  all.data$acc <- as.numeric(as.character(all.data$acc))
  all.data$X0.rec <- as.numeric(as.character(all.data$X0.rec))
  all.data$X0.pre <-as.numeric(as.character(all.data$X0.pre))
  all.data$X1.rec <- as.numeric(as.character(all.data$X1.rec))
  all.data$X1.pre <- as.numeric(as.character(all.data$X1.pre))
  all.data$X2.rec <- as.numeric(as.character(all.data$X2.rec))
  all.data$X2.pre <- as.numeric(as.character(all.data$X2.pre))
  
  #replace na with 0
  all.data$X0.rec[is.na(all.data$X0.rec)] <- 0
  all.data$X0.pre[is.na(all.data$X0.pre)] <- 0
  all.data$X1.rec[is.na(all.data$X1.rec)] <- 0
  all.data$X1.pre[is.na(all.data$X1.pre)] <- 0
  all.data$X2.rec[is.na(all.data$X2.rec)] <- 0
  all.data$X2.pre[is.na(all.data$X2.pre)] <- 0
  
  # remove column X1 (the number)
  all.data <- within(all.data, rm("X1"))
  
  # average precision and recall
  all.data <- mutate(all.data, PRMean = rowMeans(select(all.data, starts_with(c("X0","X1","X2"))), na.rm = TRUE))
  all.data$PRMean <- round(all.data$PRMean,4)
  
  # average acc + precision and recall
  all.data <- mutate(all.data, TotalMean = rowMeans(select(all.data, starts_with(c("acc","PRMean"))), na.rm = TRUE))
  all.data$TotalMean <- round(all.data$TotalMean,4)
  
  return(all.data)
}

reformat_cell <- function(all.data){
  # reformat
  all.data$acc    <-  format(round(all.data$acc,2), nsmall = 2)
  all.data$X0.rec <-  format(round(all.data$X0.rec,2), nsmall = 2)
  all.data$X0.pre <-  format(round(all.data$X0.pre,2), nsmall = 2)
  all.data$X1.rec <-  format(round(all.data$X1.rec,2), nsmall = 2)
  all.data$X1.pre <-  format(round(all.data$X1.pre,2), nsmall = 2)
  all.data$X2.rec <-  format(round(all.data$X2.rec,2), nsmall = 2)
  all.data$X2.pre <-  format(round(all.data$X2.pre,2), nsmall = 2)
  
  all.data$PRMean <-  format(round(all.data$PRMean,4), nsmall = 4)
  all.data$TotalMean  <-  format(round(all.data$TotalMean,4), nsmall = 4)
  
  return(all.data)
}

lr <- summarize.all(lr)
dt <- summarize.all(dt)
svm <- summarize.all(svm)

#average all
all.combined <- data.frame(lr$TotalMean, dt$TotalMean, svm$TotalMean)
all.combined <- mutate(all.combined, grandMean = rowMeans(all.combined))
all.combined <- format(round(all.combined,4), nsmall = 4)

# reformat
lr <- reformat_cell(lr)
dt <- reformat_cell(dt)
svm <- reformat_cell(svm)

write.csv(lr, "experiment_results/summary_davidson_best_parameters_compare_lr.csv")
write.csv(dt, "experiment_results/summary_davidson_best_parameters_compare_dt.csv")
write.csv(svm, "experiment_results/summary_davidson_best_parameters_compare_svm.csv")
write.csv(all.combined, "experiment_results/summary_davidson_best_parameters_all.csv")

#experiment 57 is best