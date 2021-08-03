summarize_per_experiment <- function(n, const.experiment.name,var.preprocess.tl.daume.target.sample.size,alg, full.alg){
  # load data from summary and sources
  e <- readRDS(file = paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.rds"))
  print(paste0("experiment/", const.experiment.name,"/",const.experiment.name,"_",const.source.domain,"_rds_3c_dtm.source.rds"))
  log <- readRDS(file = paste0("experiment/", const.experiment.name,"/",const.experiment.name,"_",const.source.domain,"_rds_3c_dtm.source.rds"))
  parameters <- readr::read_csv(paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@parameters.csv"))
  
  # limit to 2 digits only
  e <- format(round(e,2), nsmall = 2)

  # add all the valeus
  all.parameters[n, 'exp'] <<- const.experiment.name
  all.parameters[n, 'weight'] <<- parameters$save.parameters.values[parameters$save.parameters.names=='var.preprocess.weighting']
  all.parameters[n, 'minfreq'] <<- parameters$save.parameters.values[parameters$save.parameters.names=='var.preprocess.min.freq.words.number']
  all.parameters[n, 'alg'] <<- alg
  all.parameters[n, 'gram'] <<- parameters$save.parameters.values[parameters$save.parameters.names=='var.preprocess.ngram']
  all.parameters[n, 'target%'] <<- var.preprocess.tl.daume.target.sample.size
  all.results[n,] <<- unname(unlist(e[full.alg,]))
}

summarize_parameter_experiment <- function(dataset.name, alg, full.alg){
  
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  nrow.custom <- length(sizes)
  
  # prepare the tables to store values
  all.parameters <<- matrix(0, ncol = 6, nrow = nrow.custom)
  all.results <<- matrix(0, ncol = 7, nrow = nrow.custom)
  colnames(all.parameters) <<- c("alg", "exp", "gram", "weight", "minfreq", "target%")
  colnames(all.results) <<- c("acc", "0.rec", "0.pre", "1.rec", "1.pre", "2.rec", "2.pre")
  
  x <- 1
  for (size in sizes){
    summarize_per_experiment(x, paste0("tl57daume",dataset.name,paste0(size)), size,alg, full.alg)
    x <- x + 1
  }
  
  # save
  all <- data.frame(all.parameters, all.results)
  #saveRDS(all, file = paste0("experiment_results/tl_summary_daume_wulczyn_",alg,".rds")) 
  write.csv(all, file = paste0("experiment_results/tl_summary_daume_",dataset.name,"_",alg,".csv"))

}

summarize_complete_experiment <- function(dataset.name){
  
  const.source.domain <<- 'davidson'
  
  alg <- "svm"
  full.alg <- 'davidson svm'
  summarize_parameter_experiment(dataset.name, alg, full.alg)
  
  alg <- "lr"
  full.alg <- 'davidson logistic regression'
  summarize_parameter_experiment(dataset.name, alg, full.alg)
  
  alg <- "dt"
  full.alg <- 'davidson decision trees'
  summarize_parameter_experiment(dataset.name, alg, full.alg)
}

summarize_complete_experiment('founta')
summarize_complete_experiment('wulczyn')