#SETTING & FUNCTIONS  =========================
if(TRUE){
  # SETTING ===================================================================================================================================================================
  #SETTING
  # turn off warning
  options(warn=-1)
  warn.conflicts = FALSE
  
  # INSTALL LIBRARY ===================================================================================================================================================================
  # install.packages('dplyr', version='0.8.5')
  # install.packages('readr', version='1.3.1')
  # install.packages('data.table', version='1.12.8')
  # install.packages('tm', version='0.7.7')
  # install.packages("rlang", version='0.4.10')
  # install.packages("tidyselect", version='1.1.0')
  # install.packages("recipes", version='0.1.15')
  # install.packages('caTools', version='1.17.1.1')
  # install.packages('lattice', version='0.20.40')
  # install.packages('ggplot2', version='3.3.3')
  # install.packages('caret', version='6.0.86')
  # install.packages('nnet', version='7.3.13')
  # install.packages('rpart', version='4.1.15')
  # install.packages('rattle', version='5.4.0')
  # install.packages('rpart.plot', version='3.0.9')
  # install.packages('RColorBrewer', version='1.1.2')
  # install.packages('randomForest', version='4.6.14')
  # install.packages('udpipe', version='0.8.3')
  # install.packages('wordcloud', version='2.6')
  # install.packages('crayon', version='1.3.4')
  # install.packages('benchmarkme', version='1.0.4')
  # install.packages('stringr', version=‘1.8.6’)
  # install.packages('e1071', version=‘1.7-3’)
  
  
  # LOAD LIBRARY ===================================================================================================================================================================
  # data manipulation
  library(dplyr)
  # filter of dplyr
  library(stringr)
  # load file
  library(readr)
  library(data.table)
  # tm dependencies
  library(NLP)
  # corpus preprocessing
  library(tm)
  # splitting dataset
  library(caTools)
  # caret dependencies
  library(lattice)
  library(ggplot2)
  library(rlang)
  library(tidyselect)
  library(recipes)
  # logistic regression algorithm
  library(caret)
  library(nnet)
  # decision tree algorithm
  library(rpart)
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)
  # SVM
  library(e1071)
  # remove sparse words frequency based
  library(udpipe)
  # cloud words
  library(wordcloud)
  # coloring text
  library(crayon)
  # benchmark computation power
  library("benchmarkme")
  # summary words frequency
  library(plyr)
  
  
  # SYSTEM CHECK ===================================================================================================================================================================
  # session info
  #sessionInfo()
  # # know RAM of the machine
  # benchmarkme::get_ram()
  # # check memory limit (only works on windows and linux)
  # memory.size(max=TRUE)
  # mem.limits(nsize=NA, vsize=NA)
  # # check all the objects
  # s()
  # # to clean garbage collection
  # gc()
  # # to clean environtment
  # m(list=ls())
  # # restart session
  # rs.restartR()
  # #memory.size()
  # check which 32 or 64 bit RStudio
  # Sys.info()[["machine"]]
  # #how to save all sessions to a file
  # save.image(file = "my_work_space.RData")
  # load("my_work_space.RData")
  #browseURL('https://www.deezer.com/us/track/121049640?autoplay=true')
  
  
  # FUNCTIONS FOR DATA LOAD ===================================================================================================================================================================
  
  # function to check whether a comment is labelled as more than one class
  # condition for a combined_class is vote_max bigger than 0 and and there are multiple vote_max between the attacking classes
  wiki_is_combined_class <- function(quoting_attack,recipient_attack,third_party_attack,other_attack){
    max_vote <- pmax(quoting_attack,recipient_attack,third_party_attack,other_attack)
    if (max_vote>0&(length(which(c(quoting_attack,recipient_attack,third_party_attack,other_attack)==max_vote))>1)){
      return(1)
    }else {
      return(0)  
    }
  }
  
  # function to classify comment
  wiki_classify_class <- function(combined_class,quoting_attack,recipient_attack,third_party_attack,other_attack){
    max_vote <- pmax(quoting_attack,recipient_attack,third_party_attack,other_attack)
    if(combined_class == 1){
      return(5)
    } else if (max_vote == 0){
      return(0)
    } else if (quoting_attack == max_vote){
      return(1)
    } else if (recipient_attack == max_vote){
      return(2)
    } else if (third_party_attack == max_vote){
      return(3)
    } else if (other_attack == max_vote){
      return(4)
    } else {
      return(6)
    }
  }
  
  # FUNCTIONS FOR PREPROCESSING ===================================================================================================================================================================
  
  # ngram tokenizer, unigram bigram and trigram
  GramTokenizer <- function(x)unlist(lapply(NLP::ngrams(NLP::words(x), var.preprocess.ngram), paste, collapse = ' '), use.names = FALSE)
  
  # print highlight
  print_highlight <- function(text){
    cat(crayon::yellow(text),"\n")
  }
  
  # print highlight
  print_bold_highlight <- function(text){
    cat(crayon::blue(text),"\n")
  }
  
  # print save&load
  print_save_load <- function(text){
    cat(crayon::green(text),"\n")
  }
  
  # print division distribution per set (dev, test, train)
  analyse_division_distribution <- function(dataset){
    for (i in names(dataset)){
      print_highlight(i)
      print(table(dataset[[i]][['class_label']]))
      print(prop.table(table(dataset[[i]][['class_label']])))
    }
  }
  
  # print_remove_doc
  print_remove_doc <- function(dataset.count.remove, before, after){
    return(paste0(dataset.count.remove, " documents have been removed. From ", before, " docs, now ", after, " docs"))
  }
  
  # shuffle dataset
  # input: dataset
  # output: shuffeled dataset
  # shuffle
  # devide to train, dev and set
  shuffle_train_dev_test <- function(dataset, dataset.name){
    set.seed(123)
    
    # split to train , dev and test
    inds <- caTools::sample.split(dataset$class_label, SplitRatio = 0.6)
    train <- dataset[inds,]
    dev_test  <- dataset[!inds,]
    
    inds <- caTools::sample.split(dev_test$class_label, SplitRatio = 0.5)
    dev <- dev_test[inds,]
    test  <- dev_test[!inds,]
    
    # get %percentage of train
    if(var.preprocess.train.size < 1){
      print_highlight(paste0("TRAIN SET IS BEING SMALLENED now being: ", (var.preprocess.train.size*100), "%"))
      inds <- caTools::sample.split(train$class_label, SplitRatio = var.preprocess.train.size)
      train <- train[inds,]
    }
    
    # add hate speech text if for wulczyn
    if (dataset.name == "wulczyn"){
      zulczyn.data.additional <- data.frame(text = c("stupid bad bitch here are"), class_label = c("0"))
      zulczyn.data.additional <- zulczyn.data.additional[rep(rownames(zulczyn.data.additional),2),]
      train <- rbind(train, zulczyn.data.additional)
      dev <- rbind(dev, zulczyn.data.additional) 
      test <- rbind(test, zulczyn.data.additional) 
    }
    
    results <- list()
    results$train <- train
    results$dev <- dev
    results$test <- test
    return(results)
  }
  
  # check corpus format make sure its dtm
  check_format <- function(dataset.analytic, highlight, is.dtm.format){
    print_highlight(highlight)
    
    if(is.dtm.format == 0){
      dataset.analytic <- DocumentTermMatrix(dataset.analytic)
    }
    
    return(dataset.analytic)
  }
  
  # analyse words frequency with table, barplot and words clouds
  analyse_words_freq_all <- function (dataset.name, dataset, top.words.number, cloud.words.number){
    print_highlight('START WORDS FREQUENCY CALCULATION')
    dataset.words.freq <- analyse_words_freq_list(dataset$log, top.words.number)
    print_highlight('START CREATING BARPLOT')
    analyse_top_words_barplot_list(dataset.name, dataset.words.freq, top.words.number)
    print_highlight('START CREATING WORDS CLOUD')
    analyse_words_cloud_list(dataset.name, dataset.words.freq, cloud.words.number) 
    print_highlight('FINISH')
  }
  
  # analyse words frequency for list of tdm
  # input list of tdm
  # output list of words frequency
  analyse_words_freq_list <- function(tdm_list, top.words.number){
    results <- list()
    for (i in names(tdm_list)){
      print_highlight(i)
      results[[i]] <- analyse_words_freq(tdm_list[[i]])
      print(head(results[[i]], top.words.number))
    }
    return(results)
  }
  
  # analyse barplot
  # input list of words frequency
  # output list of words frequency barplot
  analyse_top_words_barplot_list <- function(dataset.name, words.frequency, top.words.number){
    results <- list()
    for (i in names(words.frequency)){
      # store in result variable
      results[[i]] <- analyse_top_words_barplot(head(words.frequency[[i]], top.words.number), top.words.number)
      # store in a file
      jpeg(paste0("data_plot/words_barplot_", dataset.name,"_", i,".jpg"))
      print(results[[i]])
      dev.off()
    }
    return(results)
  }
  
  # analyse words cloud list
  # input list of words frequency
  # output list of words cloud
  analyse_words_cloud_list <- function(dataset.name, words.frequency, cloud.words.number){
    results <- list()
    for (i in names(words.frequency)){
      # store in result variable
      results[[i]] <- analyse_words_cloud(words.frequency[[i]], cloud.words.number)
      # store in a file
      jpeg(paste0("data_plot/words_cloud_", dataset.name,"_", i,".jpg"))
      print(results[[i]])
      dev.off()
    }
    return(results)
  }
  
  # get top words
  # input DocumentTermMatrix, top words number
  # output top words
  analyse_words_freq <- function(dataset.analytic){
    tryCatch(
      expr = {
        dataset.analytic <- plyr::numcolwise(sum)(dataset.analytic)
        dataset.analytic[2,] <- names(dataset.analytic)
        dataset.analytic <- data.table::transpose(dataset.analytic)
        dataset.analytic <- data.frame(word = dataset.analytic[,2], freq=as.numeric(dataset.analytic[,1]))
        return(dataset.analytic[order(-dataset.analytic$freq), ])
      },
      error = function(e){ 
        return(paste("error: ",e))
      }
    )
  }
  
  # get top words barplot
  # input data frame top words
  # output top words barplot
  analyse_top_words_barplot <- function(dataset.analytic, top.words.number){
    tryCatch(
      expr = {
        barplot(dataset.analytic[1:top.words.number,]$freq, las = 2, names.arg = dataset.analytic[1:top.words.number,]$word,
                col ="lightblue", main ="Most frequent words",
                ylab = "Word frequencies")
        return(recordPlot())
      },
      error = function(e){ 
        return(paste("error: ",e))
      }
    )
  }
  
  # get words cloud
  # input data frame words frequency
  # output words cloud
  analyse_words_cloud <- function(dataset.analytic, cloud.words.number){
    tryCatch(
      expr = {
        wordcloud(words = dataset.analytic$word, freq = dataset.analytic$freq, min.freq = 1,
                  max.words=cloud.words.number, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
        return(recordPlot())
      },
      error = function(e){ 
        return(paste("error: ",e))
      }
    )
  }
  
  # pre process dev, test and train
  # input: dev, test and train set
  # input: sparsity level for sparsity words remove
  # input: minimum frequency of words for sparsity words remove
  pre_process <- function(dataset.list, dataset.name, sparsity, min.freq.words.number){
    # create results
    results <- list()
    for (i in names(dataset.list)){
      # process preprocess
      print_bold_highlight(i)
      results[[i]] <- pre_process_per_set(dataset = dataset.list[[i]][['text']], 
                                          class = dataset.list[[i]][['class_label']],
                                          dataset.name = dataset.name,
                                          set.type = i,
                                          sparsity = sparsity,
                                          min.freq.words.number = min.freq.words.number
      )
      # pre process train (only for source domain)
      if (i == "train" & dataset.name == const.source.domain){
        # save here, so can be used for dtm source for test and dev
        save.rds(dataset.name, "3c", data = results ) 
      }
    }
    # return results
    return(results)
  }
  
  # # transform features for daume per set
  pre_process_daume <- function(source.set, target.set, sample.rate){
    
    # get target based on sample size
    sample.size <- length(target.set$class_label)*sample.rate
    target.set <- sample_n(target.set, sample.size)
    
    print_highlight(paste0('sample size is ', sample.rate, ' length of ', sample.size))

    # store label somewhere else
    save.source.label <- source.set$class_label
    save.target.label <- target.set$class_label
    
    # remove label for now
    source.set$class_label <- NULL
    target.set$class_label <- NULL
    
    # source
    source.set.1 <- source.set
    source.set.2 <- source.set
    source.set.3 <- source.set
    source.set.3[source.set.3>-1] <- 0
    
    names.source.set <- colnames(source.set)
    
    names.source.set.1 <- paste0(names.source.set, "_s")
    names.source.set.2 <- paste0(names.source.set, "_g")
    names.source.set.3 <- paste0(names.source.set, "_t")
    
    colnames(source.set.1) <- names.source.set.1
    colnames(source.set.2) <- names.source.set.2
    colnames(source.set.3) <- names.source.set.3
    
    # target
    target.set.1 <- target.set
    target.set.2 <- target.set
    target.set.3 <- target.set
    target.set.1[target.set.1>-1] <- 0
    
    names.target.set <- colnames(target.set)
    
    names.target.set.1 <- paste0(names.target.set, "_s")
    names.target.set.2 <- paste0(names.target.set, "_g")
    names.target.set.3 <- paste0(names.target.set, "_t")
    
    colnames(target.set.1) <- names.target.set.1
    colnames(target.set.2) <- names.target.set.2
    colnames(target.set.3) <- names.target.set.3
    
    # combine domains
    target <- data.frame(target.set.1, target.set.2, target.set.3)
    source <- data.frame(source.set.1, source.set.2, source.set.3)
    
    # add label
    target$class_label <- save.target.label
    source$class_label <- save.source.label
    
    # combine all domains
    final.combined <- rbind(target, source)
    
    return(final.combined)
  }
  
  # transform features for daume
  # replacing 3b (after the process, target and source are combined, therefore there are the same)
  complete_pre_process_daume <- function (source.name, target.name){
    # load
    d.source <- load.rds(source.name, "3b")
    d.target <- load.rds(target.name, "3b")
    
    # feature augumentation
    tosave <- {}
    print_bold_highlight('Feature transformation')
    print_highlight('train')
    tosave$train$dataset <- pre_process_daume(d.source$train$dataset, d.target$train$dataset, var.preprocess.tl.daume.target.sample.size)
    print_highlight('dev')
    tosave$dev$dataset <- pre_process_daume(d.source$dev$dataset, d.target$dev$dataset, 1)
    print_highlight('test')
    tosave$test$dataset <- pre_process_daume(d.source$test$dataset, d.target$test$dataset, 1)
    
    # save
    save.rds(source.name, "3b", data = tosave )
    save.rds(target.name, "3b", data = tosave )
  }
  
  # pre processing
  # input:
  # x
  # y
  # output:
  # logs
  # preprocessed dataset
  pre_process_per_set <- function(dataset, class, dataset.name, set.type, sparsity, min.freq.words.number){
    
    # init result list for return
    results <- list()
    
    # set weigting method
    if (var.preprocess.weighting == 'weightTf'){
      var.preprocess.weighting.object <- weightTf
    } else if (var.preprocess.weighting == 'weightTfIdf'){
      var.preprocess.weighting.object <- weightTfIdf
    }
    
    # formating to corpus for preprocessing
    dataset <- VCorpus(VectorSource(dataset))
    
    # log
    results$log$n1beforepreprocessing <- check_format(dataset, "START PRE PROCESSING", 0)
    
    #================= pre processing
    if (var.preprocess.custom.default == TRUE){
      print_highlight('DEFAULT pre processing starts')
      # stemming
      dataset <- tm_map(dataset, stemDocument)
      results$log$n2afterstemming <- check_format(dataset, "FINISH STEMMING", 0)
      # replace all @retweet
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "RT @[a-z,A-Z]*:", replacement = "tretweet")
      results$log$n3afterremoveretweet <- check_format(dataset, "FINISH REMOVE RETWEET", 0)
      # replace all @mention
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "@[a-z,A-Z]*", replacement = "tmention")
      results$log$n4afterremovemention <- check_format(dataset, "FINISH REMOVE MENTION", 0)
      # replace all @hashtags
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "#[a-z,A-Z]*", replacement = "thashtags")
      results$log$n5afterremovehashtags <- check_format(dataset, "FINISH REMOVE HASHTAGS", 0)
      # replace all @url
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", replacement = "turl")
      results$log$n6afterremoveurl <- check_format(dataset, "FINISH REMOVE URL", 0)
      # remove encoded punctuation
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\&\\w*", replacement = "")
      results$log$n7afterremoveencodedpunctuation <- check_format(dataset, "FINISH REMOVE ENCODED PUCTUATION", 0)
      # remove punctuation
      dataset <- tm_map(dataset, removePunctuation)
      results$log$n8afterremovepunctuation <- check_format(dataset, "FINISH REMOVE PUCTUATION", 0)
    } else {
      #================= pre processing
      print_highlight('CUSTOM pre processing starts')
      # stemming
      dataset <- tm_map(dataset, stemDocument)
      # remove new line
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "NEWLINE_TOKEN", replacement = "")
      # url
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", replacement = "turl")
      # remove images
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = '?=.*(.jpg|.gif)', replacement = "")
      # remove html styling
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "style=``.*?``", replacement = "")
      # # remove string that has "="
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\w*=+\\w*\\s*", replacement = "")
      # # remove string that has ":"
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\w*:+\\w*\\s*", replacement = "")
      # remove encoded puctuation
      dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\&\\w*", replacement = "")
      # remove punctuation
      dataset <- tm_map(dataset, removePunctuation)
    }
    
    
    
    # if a source datasetname and a train set, remove sparsity
    # if a target datasetname, make based on source
    if (dataset.name == const.source.domain & set.type=="train"){
      print_highlight("START AUTO SPARSE REMOVING")
      dataset <- DocumentTermMatrix(dataset, 
                                    control = list(weighting = var.preprocess.weighting.object,
                                                   tokenize= GramTokenizer
                                    )
      )

      #calcualte sparsity (calculate based on number of frequency if sparsity is not defined)
      sparsity <- 1-(min.freq.words.number/dataset$nrow)
      print(min.freq.words.number)
      print(dataset$nrow)
      print_highlight("SPARSITY:")
      print_highlight(sparsity) 
      
      # remove sparse terms
      dataset <- removeSparseTerms(dataset, sparsity)
      
    } else {
      
      # get the source.dtm
      print_highlight(paste0("USING ",const.source.domain, " as base"))
      source.log <- load.rds(const.source.domain, "3c")
      source.dtm <- source.log$train$log$n99afterpreprocessing
      print(source.dtm)
      
      print_highlight("CREATING DTM BASED ON TRAIN SET")
      #dictinonary = limit terms based on the learning source
      #weighting = weightTfIdf based
      #wordlength = document with less number than 1 word, will be removed
      #tokenize = n gram
      dataset <- DocumentTermMatrix(dataset, 
                                    control = list(dictionary=Terms(source.dtm), 
                                                   weighting = var.preprocess.weighting.object,
                                                   tokenize=GramTokenizer
                                    ) 
      )
    }
    
    # log
    results$log$n99afterpreprocessing <- check_format(dataset, "FINISH REMOVE SPARSE WORDS / ADAPT TO TRAIN SET", 1)
    
    #================= 
    # to data frame
    dataset <- as.data.frame(as.matrix(dataset))
    print_highlight("FINISH REFORMARING FROM CORPUS TO DATAFRAME")
    
    #================= fix variable names to be R language friendly
    colnames(dataset) <- make.names(colnames(dataset), unique=TRUE, allow_ = TRUE)
    
    #================= remove documents with 0 words (no words would be changed, due to only documents with 0 word)
    # add total number of words each document
    dataset <- dataset %>% 
      mutate(sum_x_total = rowSums(.))
    # add back the class
    dataset$class_label <- class
    dataset.before.remove <- dataset
    # remove all documents with 0 words
    dataset <- dataset %>% 
      filter(sum_x_total != 0) %>%
      select(-sum_x_total)
    # log removed words
    results$remdoc$before <- count(dataset.before.remove)
    results$remdoc$after <- count(dataset)
    dataset.count.remove <- results$remdoc$before - results$remdoc$after
    results$remdoc$total <- dataset.count.remove
    print_highlight(print_remove_doc(dataset.count.remove, results$remdoc$before,results$remdoc$after))
    print_highlight("FINISH REMOVING 0 WORD DOCUMENTS")
    
    #=================  return
    results$dataset <- dataset
    print_highlight("FINISH PRE PROCESSING")
    return(results)
  }
  
  # complete pre process (resuffle, pre process, loadsave resuffle, loadsave, pre process, analsis resuffle, analysys pre process)
  complete.preprocess <- function(dataset.name, is.load.resuffle, is.save.resuffle, is.load.preprocess, is.save.preprocess, preprocess.sparsity = 0, preprocess.min.freq.words.number = 0){
    
    print_bold_highlight(paste0(dataset.name, "PRE PROCESS IS STARTED ==================="))
    
    #================= resuffle
    dataset <- {}
    # load or new
    if (is.load.resuffle == TRUE){
      # load
      print_bold_highlight("START RESUFFLE LOAD")
      dataset <- load.rds(dataset.name, "3a")
    } else {
      print_bold_highlight("START RESUFFLE NEW")
      # shuffle to train,dev,test
      dataset <- load.rds(dataset.name, "1")
      dataset <- shuffle_train_dev_test(dataset, dataset.name)
      
      # check division distribution
      print_bold_highlight("ANALSE RESUFFLE DISTIBUTION")
      analyse_division_distribution(dataset)
      
      #save
      if (is.save.resuffle == TRUE){
        save.rds(dataset.name, "3a", data = dataset )
      } 
    }
    
    #================= pre processing
  
    # load or new
    if (is.load.preprocess){
      # load
      print_bold_highlight("START PRE PROCESSING LOAD")
      dataset <- load.rds(dataset.name, "3b")
    } else {
      
      print_bold_highlight("START PRE PROCESSING NEW")
      
      #pre processing
      dataset <- pre_process(
        dataset.list = dataset,
        dataset.name = dataset.name,
        sparsity = preprocess.sparsity,
        min.freq.words.number = preprocess.min.freq.words.number
      )
      
      #save
      if (is.save.preprocess == TRUE){
        save.rds(dataset.name, "3b", data = dataset )
      } 
      
    }
    
    #================= analysis
    # show number of term after each pre processing
    print_bold_highlight("ANALYSIS TRAIN PER PRE PROCESS")
    print(dataset$train$log)
    
    # show the similar number of terms between set
    print_bold_highlight("ANALYSIS PER DATASET")
    print_highlight('train')
    print(dataset$train$log$n99afterpreprocessing)
    print_highlight('tests')
    print(dataset$test$log$n99afterpreprocessing)
    print_highlight('dev')
    print(dataset$dev$log$n99afterpreprocessing)
    
  }
  
  # FUNCTIONS FOR TRAINING ====================================================================================================================
  
  #analyse model performance
  #input: results of predicted model
  #input: label
  # output: print of performance
  analyse_model_performance <- function(predict.dataset.name, trained.model.type, predicted.results, class.label){
    # confusion matrix
    confusionmatrix <- caret::confusionMatrix(predicted.results, as.factor(class.label))
    
    # set row title
    row.title <- paste(predict.dataset.name, trained.model.type)
    main.results <- load.summary.rds()
    
    # show accuracy
    print_highlight('accuracy')
    print(confusionmatrix$overall['Accuracy'])
    main.results[row.title, 'acc'] <<- confusionmatrix$overall['Accuracy']
    
    # show recall
    print_highlight('recall')
    print(confusionmatrix$byClass[, 'Recall'])
    main.results[row.title, '0.rec'] <<- confusionmatrix$byClass['Class: 0', 'Recall']
    main.results[row.title, '1.rec'] <<- confusionmatrix$byClass['Class: 1', 'Recall']
    main.results[row.title, '2.rec'] <<- confusionmatrix$byClass['Class: 2', 'Recall']
    
    # show precision
    print_highlight('precision')
    print(confusionmatrix$byClass[, 'Precision'])
    main.results[row.title, '0.pre'] <<- confusionmatrix$byClass['Class: 0', 'Precision']
    main.results[row.title, '1.pre'] <<- confusionmatrix$byClass['Class: 1', 'Precision']
    main.results[row.title, '2.pre'] <<- confusionmatrix$byClass['Class: 2', 'Precision']
  }
  
  save.summary.rds <- function(){
    print_highlight('SAVING SUMMARY')
    saveRDS(main.results, file = paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.rds")) 
    write.csv(main.results, paste0("experiment/", const.experiment.name,"/",const.experiment.name,"@summary.csv"))
  }
  
  load.summary.rds <- function(experiment.name = NULL){
    if(is.null(experiment.name)){
      experiment.name <- const.experiment.name
    }
    return(readRDS(file = paste0("experiment/", experiment.name,"/",experiment.name,"@summary.rds")))
  }
  
  save.rds <- function(dataset.name, part.number, data){
    save.or.load.rds("save", dataset.name, part.number, data)
  }
  
  load.rds <- function(dataset.name, part.number){
    save.or.load.rds("load", dataset.name, part.number)
  }
  
  # save, load or skip based on constant
  # 
  # input name of the dataset
  # input number showing which part
  # input name of the part
  # input data to save (only for saviong)
  save.or.load.rds <- function(command, dataset.name, part.number, data = 0) {
    
    # set part name
    if(part.number == "1"){
      part.name <- "load"
    } else if(part.number == "3a"){
      part.name <- "resuffle"
    } else if(part.number == "3b"){
      part.name <- "preprocess"
    } else if(part.number == "3c"){
      part.name <- "dtm.source"
    } else if(part.number == "4a"){
      part.name <- "training_lr"
    } else if(part.number == "4b"){
      part.name <- "training_dt"
    } else if(part.number == "4c"){
      part.name <- "training_svm"
    } else if(part.number == "5a"){
      part.name <- paste0("predict_lr_from_",const.source.domain)
    } else if(part.number == "5b"){
      part.name <- paste0("predict_dt_from_",const.source.domain)
    } else if(part.number == "5c"){
      part.name <- paste0("predict_svm_from_",const.source.domain)
    } else {
      stop("erong part number")
    }
    # set title
    title.file <- paste0("experiment/", const.experiment.name, "/", const.experiment.name, "_", dataset.name, "_rds_", part.number, "_", part.name, ".rds")
    # save or load or skip
    if (command == "save"){
      #save
      saveRDS(data, file = title.file)
      print_save_load(paste0('saving ',title.file))
      #return(data)
    } else if (command == "load"){
      # load
      print_save_load(paste0('loading ',title.file))
      return(readRDS(file = title.file))
    } else {
      # skip
      print_highlight('no saving nor loading')
      return(data)
    }
  }
  
  # get model number
  # input : model name
  get.model.number <- function(model.type, command){
    save.number <- 0
    
    if (command=="training"){
      save.number <- "4"
    } else if (command=="predict"){
      save.number <- "5"
    }
    
    if (model.type == "logistic regression"){
      # set save number
      save.number <- paste0(save.number,"a")
    } else if (model.type == "decision trees"){
      # set save number
      save.number <- paste0(save.number,"b")
    } else if (model.type == "svm"){
      # set save number
      save.number <- paste0(save.number,"c")
    } else {
      stop("wrong model type")
    }
    return(save.number)
  }
  
  
  # complete predict
  # input
  # trained.model.name
  # model.type
  # predict.dataset.name
  # set type (dev or test)
  complete.predict <- function(trained.model.name, trained.model.type, is.load.predict, is.save.predict, predict.dataset.name, predict.set.type){
    
    # load
    model.number <- get.model.number(trained.model.type, "training")
    
    dataset.training <- load.rds(trained.model.name, model.number)
    dataset.predict <- load.rds(predict.dataset.name, "3b")
    
    if (is.load.predict == TRUE){
      save.number <- get.model.number(trained.model.type, "predict")
      predict.result <- load.rds(predict.dataset.name, save.number)
      
    } else {
      # predict
      print(table(dataset.predict[[predict.set.type]]$dataset$class_label))
      predict.result <- predict(dataset.training, newdata = dataset.predict[[predict.set.type]]$dataset, type = "class" , decision.values = TRUE) 
    }
    
    # performance check
    analyse_model_performance(predict.dataset.name, trained.model.type, predict.result, dataset.predict[[predict.set.type]]$dataset$class_label)
    
    # save
    if (is.save.predict == TRUE){
      # save predict results
      save.number <- get.model.number(trained.model.type, "predict")
      save.rds(predict.dataset.name, save.number, data = predict.result ) 
      # save predict analysis results
      save.summary.rds()
    }
    
  }
  
  # complete training
  # input:
  # dataset.name
  complete.training <- function(dataset.name, is.load.training, is.save.training, model.type, parameters){
    
    dataset.training <- {}
    save.number <- get.model.number(model.type, "training")
    
    print_bold_highlight(model.type)
    
    # load
    dataset <- load.rds(dataset.name, "3b")
    
    if (is.load.training){
      print_bold_highlight("START TRAINING LOAD")
      dataset.training <- load.rds(dataset.name, save.number) 
    } else {
      print_bold_highlight("START TRAINING NEW")
      
      if (model.type == "logistic regression"){
        # train
        dataset.training <- nnet::multinom(class_label ~ . , data=dataset$train$dataset, MaxNWts=parameters$MaxNWts , decay = parameters$decay) 
      } else if (model.type == "decision trees"){
        # train
        dataset.training <- rpart::rpart(class_label ~ . , data = dataset$train$dataset, method = "class")
      } else if (model.type == "svm"){
        # train
        dataset.training <- e1071::svm(class_label ~ ., data = dataset$train$dataset, type = 'C-classification')
      } else {
        stop("wrong model type")
      }
      
      # save
      if (is.save.training == TRUE){
        save.rds(dataset.name, save.number, data = dataset.training ) 
      }
      
    }
    
  }
  

  # FUNCTIONS FOR INIT ===================================================================================================================================================================
  
  # save all parameters
  save.parameter <- function(){
    print_bold_highlight('SAVING PARAMETERS')
    if(is.integer(var.preprocess.ngram)){
      var.preprocess.ngram <- paste0("(", min(var.preprocess.ngram), ":", max(var.preprocess.ngram), ")")
    }
    
    save.parameters.values <- c(
      const.experiment.name,
      const.source.domain,
      const.is.no.summary.file,
      var.preprocess.train.size,
      var.preprocess.is.exec.founta,
      var.preprocess.is.exec.wulczyn,
      var.training.is.exec.lr,
      var.training.is.exec.dt,
      var.training.is.exec.svm,
      var.predict.is.exec.algorithm.lr,
      var.predict.is.exec.algorithm.dt,
      var.predict.is.exec.algorithm.svm,
      var.predict.is.exec.dataset.davidson,
      var.predict.is.exec.dataset.founta,
      var.predict.is.exec.dataset.wulczyn,
      var.preprocess.ngram,
      var.preprocess.weighting,
      var.preprocess.sparsity,
      var.preprocess.min.freq.words.number,
      var.preprocess.is.load.resuffle,
      var.preprocess.is.save.resuffle,
      var.preprocess.is.load.preprocess,
      var.preprocess.is.save.preprocess,
      var.training.is.load.training,
      var.training.is.save.training,
      var.training.lr.decay,
      var.training.lr.maxnwts,
      var.predict.set.type,
      var.predict.is.load.predict,
      var.predict.is.save.predict,
      var.preprocess.custom.default
    )
    
    
    save.parameters.names <- c(
      "const.experiment.name",
      "const.source.domain",
      "const.is.no.summary.file",
      "var.preprocess.train.size",
      "var.preprocess.is.exec.founta",
      "var.preprocess.is.exec.wulczyn",
      "var.training.is.exec.lr",
      "var.training.is.exec.dt",
      "var.training.is.exec.svm",
      "var.predict.is.exec.algorithm.lr",
      "var.predict.is.exec.algorithm.dt",
      "var.predict.is.exec.algorithm.svm",
      "var.predict.is.exec.dataset.davidson",
      "var.predict.is.exec.dataset.founta",
      "var.predict.is.exec.dataset.wulczyn",
      "var.preprocess.ngram",
      "var.preprocess.weighting",
      "var.preprocess.sparsity",
      "var.preprocess.min.freq.words.number",
      "var.preprocess.is.load.resuffle",
      "var.preprocess.is.save.resuffle",
      "var.preprocess.is.load.preprocess",
      'var.preprocess.is.save.preprocess',
      "var.training.is.load.training",
      "var.training.is.save.training",
      "var.training.lr.decay",
      "var.training.lr.maxnwts",
      "var.predict.set.type",
      "var.predict.is.load.predict",
      "var.predict.is.save.predict",
      "var.preprocess.custom.default"
    )
    
    save.parameters <- matrix(0, ncol = 2, nrow = 33)
    save.parameters <- data.frame(save.parameters.names, save.parameters.values)
    saveRDS(save.parameters, file = paste0("experiment/", const.experiment.name,"/", const.experiment.name, "@parameters.rds"))
    write.csv(save.parameters, paste0("experiment/", const.experiment.name, "/", const.experiment.name, "@parameters.csv"))
  }
  
  init_function <- function(){
    # create folder
    dir.create(paste0("experiment/", const.experiment.name))
    
    if (const.is.no.summary.file == TRUE){
      # create result data frame
      main.results <<- matrix(0, ncol = 7, nrow = 9)
      colnames(main.results) <<- c("acc", "0.rec", "0.pre", "1.rec", "1.pre", "2.rec", "2.pre")
      rownames(main.results) <<- c("davidson logistic regression", "davidson decision trees", "davidson svm", "founta logistic regression", "founta decision trees", "founta svm", "wulczyn logistic regression", "wulczyn decision trees", "wulczyn svm")
      save.summary.rds()
    } else {
      # or load from file
      main.results <<- load.summary.rds()
    } 
  }
}

#EXECUTIONS =========================
training_and_predict <- function(){
init_function()
# PART 1 ========================= LOAD DATA ===================================================================================================================
# twitter davidson load data ===============================================================================================================

# load data
davidson <- readr::read_csv("data/twitter.dataset.davidson/labeled_data.csv")

# select class & tweet and rename
davidson <- davidson %>%
  dplyr::select(tweet,class) %>%
  dplyr::rename(class_label = class,text = tweet)

#save
save.rds('davidson', "1", data = davidson )

#remove
remove(davidson)

# twitter founta load data ================================================================================================================

# load data
founta <- readr::read_csv2("data/twitter.dataset.founta/hatespeech_text_label_vote_updated.csv")

# select class & tweet and rename & remove records with spam label
founta <- founta %>%
  dplyr::filter(class != "spam")%>% 
  dplyr::mutate(class_label = recode(class, "hateful"=0, "abusive"=1,"normal"=2))%>% 
  dplyr::select(tweet,class_label) %>%
  dplyr::rename(text = tweet)

#make a similar dataset number with davidson
set.seed(123)
inds <- caTools::sample.split(founta$class_label, SplitRatio = 0.7117)
founta <- founta[!inds,]
remove(inds)

#save
save.rds("founta", "1", data = founta )

#remove
remove(founta)

# # wiki wulczyn load data ===================================================================================================================

# load data
wiki.label <- as.data.frame(fread("data/wikipedia.dataset.wulczyn/attack_annotated_comments.tsv"))
wiki.id <- as.data.frame(fread("data/wikipedia.dataset.wulczyn/attack_annotations.tsv"))

# decide for each record, which class wins the vote
wiki.id.class <- wiki.id %>%
  dplyr::select(-worker_id) %>%
  group_by(rev_id) %>%
  summarise(
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
  dplyr::mutate(class_label = recode(class_label, "0"="2", "1"="2", "2"="1", "3"="1", "4"="1", "5"="1"))%>%
  dplyr::select(comment,class_label) %>%
  dplyr::rename(text = comment)

#make a similar dataset number with davidson
set.seed(123)
inds <- caTools::sample.split(wulczyn$class_label, SplitRatio = 0.7861)
wulczyn <- wulczyn[!inds,]
remove(inds)

#save
save.rds("wulczyn", "1", data = wulczyn )

# remove unused variables
remove(wiki.label, wiki.id, wiki.id.class, wulczyn, zulczyn.data.additional)

# PART 2 ========================= DATA ANALYSIS  (MOVED TO ANALYTIC FILE)
# PART 3 ========================= PRE PROCESSING ==============================================================================================================

# twitter davidson ML ==========================================================

complete.preprocess(
  dataset.name = const.source.domain, 
  is.load.resuffle = var.preprocess.is.load.resuffle,
  is.save.resuffle = var.preprocess.is.save.resuffle,
  is.load.preprocess = var.preprocess.is.load.preprocess,
  is.save.preprocess = var.preprocess.is.save.preprocess,
  preprocess.sparsity = var.preprocess.sparsity, 
  preprocess.min.freq.words.number = var.preprocess.min.freq.words.number
)

# twitter founta ML ============================================================

if (var.preprocess.is.exec.founta == TRUE){
complete.preprocess(
  dataset.name = "founta",
  is.load.resuffle = var.preprocess.is.load.resuffle,
  is.save.resuffle = var.preprocess.is.save.resuffle,
  is.load.preprocess = var.preprocess.is.load.preprocess,
  is.save.preprocess = var.preprocess.is.save.preprocess
)
}

# wiki wulczyn ML ==========================================================================================================================

if (var.preprocess.is.exec.wulczyn == TRUE){
complete.preprocess(
  dataset.name = "wulczyn",
  is.load.resuffle = var.preprocess.is.load.resuffle,
  is.save.resuffle = var.preprocess.is.save.resuffle,
  is.load.preprocess = var.preprocess.is.load.preprocess,
  is.save.preprocess = var.preprocess.is.save.preprocess
) 
}

# daume transfer learning ==========================================================================================================================
if (var.preprocess.tl.daume){
  complete_pre_process_daume(const.source.domain, var.preprocess.tl.daume.target)
}

# PART 4 ========================= TRAINING ================================================================================================
# twitter davidson logistic regresion ======================================================================================================

if (var.training.is.exec.lr == TRUE){
# logistic regression
complete.training(
  dataset.name = const.source.domain,
  is.load.training = var.training.is.load.training,
  is.save.training = var.training.is.save.training,
  model.type = "logistic regression",
  parameters = list(MaxNWts = var.training.lr.maxnwts, decay = var.training.lr.decay)
) 
}

# twitter davidson decision trees ==========================================================================================================

if (var.training.is.exec.dt == TRUE){
complete.training(
  dataset.name = const.source.domain,
  is.load.training = var.training.is.load.training,
  is.save.training = var.training.is.save.training,
  model.type = "decision trees",
  parameters = list()
)
}

# twitter davidson SVM =====================================================================================================================

if (var.training.is.exec.svm == TRUE){
complete.training(
  dataset.name = const.source.domain,
  is.load.training = var.training.is.load.training,
  is.save.training = var.training.is.save.training,
  model.type = "svm",
  parameters = list()
)
}

# PART 5 ========================= PREDICT ================================================================================================
# logistic regresion ======================================================================================================

if (var.predict.is.exec.algorithm.lr == TRUE){

if (var.predict.is.exec.dataset.davidson){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "logistic regression",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "davidson",
  predict.set.type = var.predict.set.type
) 
}

if (var.predict.is.exec.dataset.founta){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "logistic regression",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "founta",
  predict.set.type = var.predict.set.type
)
}

if (var.predict.is.exec.dataset.wulczyn){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "logistic regression",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "wulczyn",
  predict.set.type = var.predict.set.type
) 
}

}

# decision tree ======================================================================================================

if (var.predict.is.exec.algorithm.dt == TRUE){

if (var.predict.is.exec.dataset.davidson){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "decision trees",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "davidson",
  predict.set.type = var.predict.set.type
)
}

if (var.predict.is.exec.dataset.founta){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "decision trees",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "founta",
  predict.set.type = var.predict.set.type
)
}

if (var.predict.is.exec.dataset.wulczyn){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "decision trees",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "wulczyn",
  predict.set.type = var.predict.set.type
)
}

}

# SVM =====================================================================================================================

if (var.predict.is.exec.algorithm.svm){

if (var.predict.is.exec.dataset.davidson){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "svm",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "davidson",
  predict.set.type = var.predict.set.type
)
}

if (var.predict.is.exec.dataset.founta){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "svm",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "founta",
  predict.set.type = var.predict.set.type
)
}

if (var.predict.is.exec.dataset.wulczyn){
complete.predict(
  trained.model.name = const.source.domain,
  trained.model.type = "svm",
  is.load.predict = var.predict.is.load.predict,
  is.save.predict = var.predict.is.save.predict,
  predict.dataset.name = "wulczyn",
  predict.set.type = var.predict.set.type
)
}

}

# Summary Results =====================================================================================================================
load.summary.rds()
save.parameter()
}


 