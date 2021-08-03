# INSTRUCTION
# to run the bellow experiment and constant RUN FIRST THE main.R file
# main.R files consists of all the functions needed in the bellow experiment

# EXPERIMENT =================================

# PHASE 1
# non-tl different source and target domains
# training and testing on Davidson only to find the best parameters
# folder naming: "e(number).."
if(TRUE){
  
  const.source.domain <- 'davidson';
  
  # IMPORTANT!
  const.is.no.summary.file <- TRUE
  
  # run or not
  var.preprocess.train.size <- 1
  var.preprocess.is.exec.founta <- FALSE
  var.preprocess.is.exec.wulczyn <- FALSE
  var.preprocess.tl.daume <- FALSE
  var.preprocess.tl.daume.target <- ''
  var.preprocess.tl.daume.target.sample.size <- 1
  var.preprocess.custom.default <- TRUE
  var.training.is.exec.lr <- TRUE
  var.training.is.exec.dt <- TRUE
  var.training.is.exec.svm <- TRUE
  var.predict.is.exec.algorithm.lr <- TRUE
  var.predict.is.exec.algorithm.dt <- TRUE
  var.predict.is.exec.algorithm.svm <- TRUE
  var.predict.is.exec.dataset.davidson <- TRUE
  var.predict.is.exec.dataset.founta <- FALSE
  var.predict.is.exec.dataset.wulczyn <- FALSE

  # pre processing
  var.preprocess.sparsity <- 0
  var.preprocess.is.load.resuffle <- FALSE
  var.preprocess.is.save.resuffle <- TRUE
  var.preprocess.is.load.preprocess <- FALSE
  var.preprocess.is.save.preprocess <- TRUE
  
  # training
  var.training.is.load.training <- FALSE
  var.training.is.save.training <- TRUE
  var.training.lr.decay <- 0
  var.training.lr.maxnwts <- 1000000
  
  # predict
  var.predict.set.type <- "dev"
  var.predict.is.load.predict <- FALSE
  var.predict.is.save.predict <- TRUE
  
  #1
  const.experiment.name <- "e01"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #2
  const.experiment.name <- "e02"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #3
  const.experiment.name <- "e03"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #4
  const.experiment.name <- "e04"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #5
  #1.3 words
  const.experiment.name <- "e05"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #6
  #1.3 words
  const.experiment.name <- "e06"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #7
  const.experiment.name <- "e07"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #8
  const.experiment.name <- "e08"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #9
  const.experiment.name <- "e09"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #10
  const.experiment.name <- "e10"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #11
  const.experiment.name <- "e11"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #12
  const.experiment.name <- "e12"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #13
  const.experiment.name <- "e13"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #14
  const.experiment.name <- "e14"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #15
  const.experiment.name <- "e15"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #16
  const.experiment.name <- "e16"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #17
  const.experiment.name <- "e17"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #18
  const.experiment.name <- "e18"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #19
  const.experiment.name <- "e19"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #20
  const.experiment.name <- "e20"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #21
  const.experiment.name <- "e21"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #22
  const.experiment.name <- "e22"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #23
  const.experiment.name <- "e23"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #24
  const.experiment.name <- "e24"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #25
  const.experiment.name <- "e25"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #26
  const.experiment.name <- "e26"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #27
  const.experiment.name <- "e27"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #28
  const.experiment.name <- "e28"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #29
  const.experiment.name <- "e29"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #30
  const.experiment.name <- "e30"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #31
  const.experiment.name <- "e31"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #32
  const.experiment.name <- "e32"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #33
  const.experiment.name <- "e33"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #34
  const.experiment.name <- "e34"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #35
  const.experiment.name <- "e35"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #36
  const.experiment.name <- "e36"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #37
  const.experiment.name <- "e37"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #38
  const.experiment.name <- "e38"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #39
  const.experiment.name <- "e39"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #40
  const.experiment.name <- "e40"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #41
  const.experiment.name <- "e41"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #42
  const.experiment.name <- "e42"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #43
  const.experiment.name <- "e43"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #44
  const.experiment.name <- "e44"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #45
  const.experiment.name <- "e45"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #46
  const.experiment.name <- "e46"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #47
  const.experiment.name <- "e47"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #48
  const.experiment.name <- "e48"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #49
  const.experiment.name <- "e49"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #50
  const.experiment.name <- "e50"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #51
  const.experiment.name <- "e51"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #52
  const.experiment.name <- "e52"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #53
  const.experiment.name <- "e53"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #54
  const.experiment.name <- "e54"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #55
  const.experiment.name <- "e55"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #56
  const.experiment.name <- "e56"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #57
  const.experiment.name <- "e57"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #58
  const.experiment.name <- "e58"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #59
  const.experiment.name <- "e59"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #60
  const.experiment.name <- "e60"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #61
  const.experiment.name <- "e61"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #62
  const.experiment.name <- "e62"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #63
  const.experiment.name <- "e63"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #64
  const.experiment.name <- "e64"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #65
  const.experiment.name <- "e65"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #66
  const.experiment.name <- "e66"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #67
  const.experiment.name <- "e67"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #68
  const.experiment.name <- "e68"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #69
  const.experiment.name <- "e69"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #70
  const.experiment.name <- "e70"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #71
  const.experiment.name <- "e71"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #72
  const.experiment.name <- "e72"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #73
  const.experiment.name <- "e73"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #74
  const.experiment.name <- "e74"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #75
  const.experiment.name <- "e75"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #76
  const.experiment.name <- "e76"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #77
  const.experiment.name <- "e77"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #78
  const.experiment.name <- "e78"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #79
  const.experiment.name <- "e79"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #80
  const.experiment.name <- "e80"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict() 
}

# PHASE 2 
# folder naming:
# final57 = using 57 parameters training on davidson and trained on (davidson, founta & wulczyn)
if (TRUE){
  
  # ALL list of CONSTANT & default values =========================
  const.source.domain <- 'davidson';
  
  # IMPORTANT!
  const.is.no.summary.file <- TRUE
  
  # run or not
  var.preprocess.train.size <- 1
  var.preprocess.is.exec.founta <- TRUE
  var.preprocess.is.exec.wulczyn <- TRUE
  var.preprocess.tl.daume <- FALSE
  var.preprocess.tl.daume.target <- ''
  var.preprocess.tl.daume.target.sample.size <- 1
  var.preprocess.custom.default <- TRUE
  var.training.is.exec.lr <- TRUE
  var.training.is.exec.dt <- TRUE
  var.training.is.exec.svm <- TRUE
  var.predict.is.exec.algorithm.lr <- TRUE
  var.predict.is.exec.algorithm.dt <- TRUE
  var.predict.is.exec.algorithm.svm <- TRUE
  var.predict.is.exec.dataset.davidson <- TRUE
  var.predict.is.exec.dataset.founta <- TRUE
  var.predict.is.exec.dataset.wulczyn <- TRUE
  
  # pre processing
  var.preprocess.sparsity <- 0
  var.preprocess.is.load.resuffle <- FALSE
  var.preprocess.is.save.resuffle <- TRUE
  var.preprocess.is.load.preprocess <- FALSE
  var.preprocess.is.save.preprocess <- TRUE
  
  # training
  var.training.is.load.training <- FALSE
  var.training.is.save.training <- TRUE
  var.training.lr.decay <- 0
  var.training.lr.maxnwts <- 1000000
  
  # predict
  var.predict.set.type <- "test"
  var.predict.is.load.predict <- FALSE
  var.predict.is.save.predict <- TRUE
  
  # custom
  const.experiment.name <- "final57"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
}

# PHASE 3
# folder naming:
# final5founta using 57 parameters training on founta and trained on founta
# final5wulczyn using 57 parameters training on wulczyn and trained on wulczyn
# final57wulczypreprocesscustom using 57 parameters training on wulczyn and trained on wulczyn and preprocess wulczyn
if (TRUE){
  # IMPORTANT!
  const.is.no.summary.file <- TRUE
  
  # pre processing
  var.preprocess.sparsity <- 0
  var.preprocess.is.load.resuffle <- FALSE
  var.preprocess.is.save.resuffle <- TRUE
  var.preprocess.is.load.preprocess <- FALSE
  var.preprocess.is.save.preprocess <- TRUE
  var.preprocess.is.exec.founta <- FALSE # false because the source domain is founta
  var.preprocess.is.exec.wulczyn <- FALSE # false because the source domain is founta
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  
  # training
  var.training.is.load.training <- FALSE
  var.training.is.save.training <- TRUE
  var.training.lr.decay <- 0
  var.training.lr.maxnwts <- 1000000
  var.training.is.exec.lr <- TRUE
  var.training.is.exec.dt <- TRUE
  var.training.is.exec.svm <- TRUE
  
  # predict
  var.predict.is.exec.algorithm.lr <- TRUE
  var.predict.is.exec.algorithm.dt <- TRUE
  var.predict.is.exec.algorithm.svm <- TRUE
  var.predict.set.type <- "test"
  var.predict.is.load.predict <- FALSE
  var.predict.is.save.predict <- TRUE
  
  var.preprocess.tl.daume <- FALSE
  var.preprocess.tl.daume.target <- ''
  var.preprocess.tl.daume.target.sample.size <- 1
  
  # custom fouta
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  for (size in sizes){
    # custom founta
    var.preprocess.train.size <- size
    var.preprocess.custom.default <- TRUE
    const.experiment.name <- paste0("final57founta", size)
    const.source.domain <- 'founta'
    var.predict.is.exec.dataset.davidson <- FALSE
    var.predict.is.exec.dataset.founta <- TRUE
    var.predict.is.exec.dataset.wulczyn <- FALSE
    training_and_predict()
  }
  
  # custom wulczyn
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  for (size in sizes){
    # custom wulczyn
    var.preprocess.train.size <- size
    var.preprocess.custom.default <- TRUE
    const.experiment.name <- paste0("final57wulczyn",size)
    const.source.domain <- 'wulczyn'
    var.predict.is.exec.dataset.davidson <- FALSE
    var.predict.is.exec.dataset.founta <- FALSE
    var.predict.is.exec.dataset.wulczyn <- TRUE
    training_and_predict() 
  }
  
  # custom final57wulczynpreprocesscustom
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  for (size in sizes){
    # custom wulczyn preprocess
    var.preprocess.train.size <- size
    var.preprocess.custom.default <- FALSE
    const.experiment.name <- paste0("final57wulczynpreprocesscustom",size)
    const.source.domain <- 'wulczyn'
    var.predict.is.exec.dataset.davidson <- FALSE
    var.predict.is.exec.dataset.founta <- FALSE
    var.predict.is.exec.dataset.wulczyn <- TRUE
    training_and_predict() 
  }
}

# PHASE 4
# Transfer Learning Daume
# folder naming:
# tl57daumefounta[percentage of dataset used (how much from target domain being used for training)]
# tl57daumewulczyn[percentage of dataset used (how much from target domain being used for training)]
if (TRUE){
  const.source.domain <- 'davidson';

  # IMPORTANT!
  const.is.no.summary.file <- TRUE

  # pre processing
  var.preprocess.train.size <- 1
  var.preprocess.tl.daume <- TRUE
  var.preprocess.sparsity <- 0
  var.preprocess.is.load.resuffle <- FALSE
  var.preprocess.is.save.resuffle <- TRUE
  var.preprocess.is.load.preprocess <- FALSE
  var.preprocess.is.save.preprocess <- TRUE
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  var.preprocess.custom.default <- TRUE
  var.preprocess.custom.default <- TRUE

  # training
  var.training.is.load.training <- FALSE
  var.training.is.save.training <- TRUE
  var.training.lr.decay <- 0
  var.training.lr.maxnwts <- 1000000
  var.training.is.exec.lr <- TRUE
  var.training.is.exec.dt <- TRUE
  var.training.is.exec.svm <- TRUE

  # predict
  var.predict.set.type <- "test"
  var.predict.is.load.predict <- FALSE
  var.predict.is.save.predict <- TRUE
  var.predict.is.exec.algorithm.lr <- TRUE
  var.predict.is.exec.algorithm.dt <- TRUE
  var.predict.is.exec.algorithm.svm <- TRUE
  var.predict.is.exec.dataset.davidson <- TRUE
  var.predict.is.exec.dataset.founta <- FALSE
  var.predict.is.exec.dataset.wulczyn <- FALSE
  
  # for founta
  var.preprocess.is.exec.wulczyn <- FALSE
  var.preprocess.is.exec.founta <- TRUE
  var.preprocess.tl.daume.target <- 'founta'
  
  # custom fouta
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  for (size in sizes){
    const.experiment.name <- paste0("tl57daumefounta", size)
    var.preprocess.tl.daume.target.sample.size <- size
    training_and_predict()
  }
  
  # for wulczyn
  var.preprocess.is.exec.wulczyn <- TRUE
  var.preprocess.is.exec.founta <- FALSE
  var.preprocess.tl.daume.target <- 'wulczyn'
  
  # custom fouta
  sizes <- c(0.01,seq(0.02, 0.1, by = 0.02),seq(0.2, 1, by = 0.2))
  for (size in sizes){
    const.experiment.name <- paste0("tl57daumewulczyn", size)
    var.preprocess.tl.daume.target.sample.size <- size
    training_and_predict()
  }
  
}

# PHASE EXTRA
# overfitting test
# testing logistic regression tested on training dataset
# naming: all start with "q"
# PHASE EXTRA part1/2
# praparing the training results, no predicting
if(TRUE){
  
  const.source.domain <- 'davidson';
  
  # IMPORTANT!
  const.is.no.summary.file <- TRUE
  
  # run or not
  var.preprocess.train.size <- 1
  var.preprocess.is.exec.founta <- FALSE
  var.preprocess.is.exec.wulczyn <- FALSE
  var.preprocess.tl.daume <- FALSE
  var.preprocess.tl.daume.target <- ''
  var.preprocess.tl.daume.target.sample.size <- 1
  var.training.is.exec.lr <- TRUE
  var.training.is.exec.dt <- FALSE
  var.training.is.exec.svm <- FALSE
  var.predict.is.exec.algorithm.lr <- FALSE
  var.predict.is.exec.algorithm.dt <- FALSE
  var.predict.is.exec.algorithm.svm <- FALSE
  var.predict.is.exec.dataset.davidson <- FALSE
  var.predict.is.exec.dataset.founta <- FALSE
  var.predict.is.exec.dataset.wulczyn <- FALSE
  
  # pre processing
  var.preprocess.sparsity <- 0
  var.preprocess.is.load.resuffle <- FALSE
  var.preprocess.is.save.resuffle <- FALSE
  var.preprocess.is.load.preprocess <- FALSE
  var.preprocess.is.save.preprocess <- FALSE
  
  # training
  var.training.is.load.training <- FALSE
  var.training.is.save.training <- TRUE
  var.training.lr.decay <- 0
  var.training.lr.maxnwts <- 1000000
  
  # predict
  var.predict.set.type <- "dev"
  var.predict.is.load.predict <- FALSE
  var.predict.is.save.predict <- TRUE
  
  #1
  const.experiment.name <- "q01"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #2
  const.experiment.name <- "q02"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #3
  const.experiment.name <- "q03"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #4
  const.experiment.name <- "q04"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #5
  #1.3 words
  const.experiment.name <- "q05"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #6
  #1.3 words
  const.experiment.name <- "q06"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #7
  const.experiment.name <- "q07"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #8
  const.experiment.name <- "q08"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #9
  const.experiment.name <- "q09"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #10
  const.experiment.name <- "q10"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 10
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #11
  const.experiment.name <- "q11"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #12
  const.experiment.name <- "q12"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #13
  const.experiment.name <- "q13"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #14
  const.experiment.name <- "q14"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #15
  const.experiment.name <- "q15"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #16
  const.experiment.name <- "q16"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 50
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #17
  const.experiment.name <- "q17"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #18
  const.experiment.name <- "q18"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #19
  const.experiment.name <- "q19"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #20
  const.experiment.name <- "q20"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #21
  const.experiment.name <- "q21"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #22
  const.experiment.name <- "q22"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #23
  const.experiment.name <- "q23"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #24
  const.experiment.name <- "q24"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 5
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #25
  const.experiment.name <- "q25"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #26
  const.experiment.name <- "q26"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #27
  const.experiment.name <- "q27"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #28
  const.experiment.name <- "q28"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #29
  const.experiment.name <- "q29"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #30
  const.experiment.name <- "q30"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #31
  const.experiment.name <- "q31"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #32
  const.experiment.name <- "q32"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 15
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #33
  const.experiment.name <- "q33"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #34
  const.experiment.name <- "q34"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #35
  const.experiment.name <- "q35"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #36
  const.experiment.name <- "q36"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #37
  const.experiment.name <- "q37"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #38
  const.experiment.name <- "q38"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #39
  const.experiment.name <- "q39"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #40
  const.experiment.name <- "q40"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 20
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #41
  const.experiment.name <- "q41"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #42
  const.experiment.name <- "q42"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #43
  const.experiment.name <- "q43"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #44
  const.experiment.name <- "q44"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #45
  const.experiment.name <- "q45"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #46
  const.experiment.name <- "q46"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #47
  const.experiment.name <- "q47"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #48
  const.experiment.name <- "q48"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #49
  const.experiment.name <- "q49"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #50
  const.experiment.name <- "q50"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #51
  const.experiment.name <- "q51"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #52
  const.experiment.name <- "q52"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #53
  const.experiment.name <- "q53"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #54
  const.experiment.name <- "q54"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #55
  const.experiment.name <- "q55"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #56
  const.experiment.name <- "q56"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 25
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #57
  const.experiment.name <- "q57"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #58
  const.experiment.name <- "q58"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #59
  const.experiment.name <- "q59"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #60
  const.experiment.name <- "q60"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #61
  const.experiment.name <- "q61"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #62
  const.experiment.name <- "q62"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 30
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #63
  const.experiment.name <- "q63"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #64
  const.experiment.name <- "q64"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 35
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #65
  const.experiment.name <- "q65"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #66
  const.experiment.name <- "q66"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #67
  const.experiment.name <- "q67"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #68
  const.experiment.name <- "q68"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #69
  const.experiment.name <- "q69"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #70
  const.experiment.name <- "q70"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #71
  const.experiment.name <- "q71"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #72
  const.experiment.name <- "q72"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 40
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #73
  const.experiment.name <- "q73"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #74
  const.experiment.name <- "q74"
  var.preprocess.ngram <- 1
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #75
  const.experiment.name <- "q75"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #76
  const.experiment.name <- "q76"
  var.preprocess.ngram <- 2
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #77
  const.experiment.name <- "q77"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #78
  const.experiment.name <- "q78"
  var.preprocess.ngram <- 3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict()
  
  #79
  const.experiment.name <- "q79"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTf"
  training_and_predict()
  
  #80
  const.experiment.name <- "q80"
  var.preprocess.ngram <- 1:3
  var.preprocess.min.freq.words.number <- 45
  var.preprocess.weighting <- "weightTfIdf"
  training_and_predict() 
}
# PHASE EXTRA part2/2
# predicting using training set
if (TRUE){
  for(i in 1:80){
    const.experiment.name <- paste0("q",i)
    
    # run or not
    var.preprocess.is.exec.founta <- FALSE
    var.preprocess.is.exec.wulczyn <- FALSE
    var.training.is.exec.lr <- FALSE
    var.training.is.exec.dt <- FALSE
    var.training.is.exec.svm <- FALSE
    var.predict.is.exec.algorithm.lr <- TRUE
    var.predict.is.exec.algorithm.dt <- FALSE
    var.predict.is.exec.algorithm.svm <- FALSE
    var.predict.is.exec.dataset.davidson <- TRUE
    var.predict.is.exec.dataset.founta <- FALSE
    var.predict.is.exec.dataset.wulczyn <- FALSE
    var.preprocess.custom.default <- TRUE

    # pre processing
    var.preprocess.is.load.resuffle <- TRUE
    var.preprocess.is.save.resuffle <- FALSE
    var.preprocess.is.load.preprocess <- TRUE
    var.preprocess.is.save.preprocess <- FALSE
    
    # training
    var.training.is.load.training <- TRUE
    var.training.is.save.training <- FALSE
    
    # predict
    var.predict.set.type <- "train"
    var.predict.is.load.predict <- FALSE
    var.predict.is.save.predict <- TRUE
    
    training_and_predict() 
  }
}
