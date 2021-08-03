e1 <- readRDS(file = "e1/e1_davidson_rds_3c_dtm.source.rds")
r1 <- readRDS(file = "r1/r1_davidson_rds_3c_dtm.source.rds")
q1 <- readRDS(file = "q1/q1_davidson_rds_3c_dtm.source.rds")
w1 <- readRDS(file = "w1/w1_davidson_rds_3c_dtm.source.rds")

e1$train$log$n99afterpreprocessing
e1$train$remdoc
#==
r1$train$log$n99afterpreprocessing
r1$train$remdoc
#==
q1$train$log$n99afterpreprocessing
q1$train$remdoc
#==
w1$train$log$n99afterpreprocessing

const.experiment.name
dir.create("experiment/", const.experiment.name)


# checking size


sizes <- c(0.006, 0.008, 0.01, seq(0.02, 0.1, by = 0.02))
for (size in sizes){
    x <- readRDS(file = paste0("experiment/final57founta",size,"/final57founta",size,"_founta_rds_3b_preprocess.rds"))
    print(paste0("experiment/final57founta",size,"/final57founta",size,"_founta_rds_3b_preprocess.rds"))
    print(size)
    print(count(x$train$dataset))
}

sizes <- c(seq(0.2, 1, by = 0.2))
for (size in sizes){
  x <- readRDS(file = paste0("experiment/final57founta",size,"/final57founta",size,"_founta_rds_3b_preprocess.rds"))
  print(paste0("experiment/final57founta",size,"/final57founta",size,"_founta_rds_3b_preprocess.rds"))
  print(size)
  print(count(x$train$dataset))
}


