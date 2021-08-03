summarize_per_experiment <- function(){
  # load data from summary and sources
  n <- as.numeric(substring(const.experiment.name, 2, 3))
  e <- readRDS(file = paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.rds"))
  print(paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.rds"))
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

  # prepare the tables to store values
  all.parameters <<- matrix(0, ncol = 6, nrow = 80)
  all.results <<- matrix(0, ncol = 7, nrow = 80)
  colnames(all.parameters) <<- c("alg", "exp", "gram", "weight", "minfreq", "freq")
  colnames(all.results) <<- c("acc", "0.rec", "0.pre", "1.rec", "1.pre", "2.rec", "2.pre")
  
  const.experiment.name <- ""
  # loop summarize per experiment
  for(i in 1:80){
    print(i)
    if(i < 10){
      const.experiment.name <<- paste0("e0",i)
    }else {
      const.experiment.name <<- paste0("e",i)
    }
    summarize_per_experiment()
  }
  
  # save
  all <- data.frame(all.parameters, all.results)
  #saveRDS(all, file = paste0("experiment_results/summary_davidson_best_parameters_",alg,".rds")) 
  write.csv(all, file = paste0("experiment_results/summary_davidson_best_parameters_",alg,".csv"))
}

const.source.domain <- 'davidson';
alg <- "svm"
full.alg <- 'davidson svm'
summarize_complete_experiment()

alg <- "lr"
full.alg <- 'davidson logistic regression'
summarize_complete_experiment()

alg <- "dt"
full.alg <- 'davidson decision trees'
summarize_complete_experiment()