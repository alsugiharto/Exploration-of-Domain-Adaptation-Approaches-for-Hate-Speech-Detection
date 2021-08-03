# change the founta or wulczyn

summary_tl <- function(dataset.name){
 
  # load from csv
  svm <- readr::read_csv( paste0('experiment_results/tl_summary_daume_',dataset.name,'_svm.csv'))
  dt <- readr::read_csv( paste0('experiment_results/tl_summary_daume_',dataset.name,'_dt.csv'))
  lr <- readr::read_csv( paste0('experiment_results/tl_summary_daume_',dataset.name,'_lr.csv'))
  
  all <- rbind(svm,dt,lr)
  
  all2 <- all
  
  all$acc <- format(round(all$acc,2), nsmall = 2)
  all$X0.rec <- format(round(all$X0.rec,2), nsmall = 2)
  all$X0.pre <- format(round(all$X0.pre,2), nsmall = 2)
  all$X1.rec <- format(round(all$X1.rec,2), nsmall = 2)
  all$X1.pre <- format(round(all$X1.pre,2), nsmall = 2)
  all$X2.rec <- format(round(all$X2.rec,2), nsmall = 2)
  all$X2.pre <- format(round(all$X2.pre,2), nsmall = 2)
  
  write.csv(all, paste0("experiment_results/tl_summary_compare_all_tl_daume_",dataset.name,".csv"))
  
  # summarize
  all2 <- all2 %>%
    select('target.','acc')
  
  all2 <- all2 %>%
    group_by(target.) %>%
    summarize(Mean = mean(acc, na.rm=TRUE))
  
  all2$Mean <- format(round(all2$Mean ,4), nsmall = 4)

  write.csv(all2, paste0("experiment_results/tl_summary_compare_all_tl_daume_",dataset.name,"_final.csv"))
  
  #experiment 57 is best
}

summary_tl('founta')
summary_tl('wulczyn')

