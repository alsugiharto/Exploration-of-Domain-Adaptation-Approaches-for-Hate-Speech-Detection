# load data
d.source <- tl57_davidson_rds_3b_preprocess$dev$dataset
d.target <- tl57_founta_rds_3b_preprocess$test$dataset

# store label somewhere else
save.source.label <- d.source$class_label
save.target.label <- d.target$class_label

# remove label for now
d.source$class_label <- NULL
d.target$class_label <- NULL

# source
d.source.1 <- d.source
d.source.2 <- d.source
d.source.3 <- d.source
d.source.3[d.source.3>-1] <- 0

names.d.source <- colnames(d.source)

names.d.source.1 <- paste0(names.d.source, "_s")
names.d.source.2 <- paste0(names.d.source, "_g")
names.d.source.3 <- paste0(names.d.source, "_t")

colnames(d.source.1) <- names.d.source.1
colnames(d.source.2) <- names.d.source.2
colnames(d.source.3) <- names.d.source.3

# target
d.target.1 <- d.target
d.target.2 <- d.target
d.target.3 <- d.target
d.target.1[d.target.1>-1] <- 0

names.d.target <- colnames(d.target)

names.d.target.1 <- paste0(names.d.target, "_s")
names.d.target.2 <- paste0(names.d.target, "_g")
names.d.target.3 <- paste0(names.d.target, "_t")

colnames(d.target.1) <- names.d.target.1
colnames(d.target.2) <- names.d.target.2
colnames(d.target.3) <- names.d.target.3

# combine domains
target <- data.frame(d.target.1, d.target.2, d.target.3)
source <- data.frame(d.source.1, d.source.2, d.source.3)

# add label
target$class_label <- save.target.label
source$class_label <- save.source.label

# combine all domains
final.combined.dev <- rbind(target, source)

tosave <- {}
tosave$train$dataset <- final.combined.train
tosave$dev$dataset <- final.combined.dev

saveRDS(tosave, file = "experiment/tlfinal57/tlfinal57_davidson_rds_3b_preprocess.rds")
