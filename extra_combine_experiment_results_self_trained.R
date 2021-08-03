summarize_per_experiment <- function(n){
  # load data from summary and sources
  print(paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.rds"))
  e <- readRDS(file = paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.rds"))
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
  all.parameters[n, 'freq'] <<- log$train$log$n99afterpreprocessing$ncol
  all.results[n,] <<- unname(unlist(e[full.alg,]))
}

summarize_complete_experiment <- function(){
  
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  nrow.custom <- length(sizes)
  
  # prepare the tables to store values
  all.parameters <<- matrix(0, ncol = 6, nrow = nrow.custom)
  all.results <<- matrix(0, ncol = 7, nrow = nrow.custom)
  colnames(all.parameters) <<- c("alg", "exp", "gram", "weight", "minfreq", "freq")
  colnames(all.results) <<- c("acc", "0.rec", "0.pre", "1.rec", "1.pre", "2.rec", "2.pre")
  
  const.experiment.name <- ""
  n <- 1
  # loop summarize per experiment
  for(i in sizes){
    print(i)
    const.experiment.name <<- paste0("final57",const.source.domain,extra.custom.name,i)
    summarize_per_experiment(n)
    n <- n + 1
  }
  
  # save
  all <- data.frame(all.parameters, all.results)
  #saveRDS(all, file = paste0("experiment_results/summary_davidson_best_parameters_",alg,".rds")) 
  write.csv(all, file = paste0("experiment_results/summary_",const.source.domain,extra.custom.name,"_self_trained_",alg,".csv"))
}

extra.custom.name <- ""
const.source.domain <- 'founta';
alg <- "svm"
full.alg <- paste0(const.source.domain, ' svm')
summarize_complete_experiment()

alg <- "lr"
full.alg <- paste0(const.source.domain, ' logistic regression')
summarize_complete_experiment()

alg <- "dt"
full.alg <- paste0(const.source.domain, ' decision trees')
summarize_complete_experiment()

#===

extra.custom.name <- ""
const.source.domain <- 'wulczyn';
alg <- "svm"
full.alg <- paste0(const.source.domain, ' svm')
summarize_complete_experiment()

alg <- "lr"
full.alg <- paste0(const.source.domain, ' logistic regression')
summarize_complete_experiment()

alg <- "dt"
full.alg <- paste0(const.source.domain, ' decision trees')
summarize_complete_experiment()

#===

extra.custom.name <- "preprocesscustom"
const.source.domain <- 'wulczyn';
alg <- "svm"
full.alg <- paste0(const.source.domain, ' svm')
summarize_complete_experiment()

alg <- "lr"
full.alg <- paste0(const.source.domain, ' logistic regression')
summarize_complete_experiment()

alg <- "dt"
full.alg <- paste0(const.source.domain, ' decision trees')
summarize_complete_experiment()