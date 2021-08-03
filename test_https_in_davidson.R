# testing https in davidson and founta
davidson <- readr::read_csv("data/twitter.dataset.davidson/labeled_data.csv")

test <- dplyr::filter(davidson, grepl('http', tweet))
table(test$class)

# ==

# load data
founta <- readr::read_csv2("data/twitter.dataset.founta/hatespeech_text_label_vote_updated.csv")

test2 <- dplyr::filter(founta, grepl('http', tweet))
table(test2$class)
