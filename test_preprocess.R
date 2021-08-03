# init result list for return
results <- list()

davidson <- readr::read_csv("data/twitter.dataset.davidson/labeled_data.csv")
dataset <- davidson$tweet

# formating to corpus for preprocessing
dataset <- Corpus(VectorSource(dataset))

#================= pre processing 


test123 <- function(){
  # retweet example
  print(dataset[[1]]$content)
  # url exmaple
  print(dataset[[404]]$content)
  # mention exmaple
  print(dataset[[10]]$content)
  # hashtags example
  print(dataset[[2223]]$content)
}

# converting to lower case
dataset <- tm_map(dataset, tolower)

# stemming
dataset <- tm_map(dataset, stemDocument)

# replace all @retweet
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "rt @[a-z,A-Z]*:", replacement = "tretweet")

# replace all @mention
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "@[a-z,A-Z]*", replacement = "tmention")

# replace all @hashtags
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "#[a-z,A-Z]*", replacement = "thashtags")

# replace all @url
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", replacement = "turl")

# remove encoded punctuation
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\&\\w*", replacement = "")

# remove punctuation
dataset <- tm_map(dataset, removePunctuation)

# remove stop words
#dataset <- tm_map(dataset, removeWords, c(stopwords("english")))

#=== another test

# Testing pre processing =====================================================================================================================

dataset <- c('I LOVES MAKING CAKE YES:::', 'I WANTING ICE CREAMS @halo #ini RT@keapa', 'https://www.goal.com/en')
dataset <- VCorpus(VectorSource(dataset))
dataset <- tm_map(dataset, stemDocument)
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "rt @[a-z,A-Z]*:", replacement = "tretweet")
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "@[a-z,A-Z]*", replacement = "tmention")
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "#[a-z,A-Z]*", replacement = "thashtags")
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", replacement = "turl")
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\&\\w*", replacement = "")
dataset <- tm_map(dataset, removePunctuation)
#dataset <- tm_map(dataset, stemDocument)
dataset <- TermDocumentMatrix(dataset, control = list(
  #tokenize = GramTokenizer,
  tolower = T
  )
)
inspect(dataset)

# testing wulczyn pre processing =====================================================================================================================

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
  dplyr::select(comment,class_label) %>%
  dplyr::rename(text = comment)

wulczyn <- dplyr::filter(wulczyn, grepl('style=', text))

dataset <- wulczyn[c(1),]
dataset <- VCorpus(VectorSource(dataset))
# remove new line
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "NEWLINE_TOKEN", replacement = "")
# remove new line
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", replacement = "turl")
# remove images
dataset <- tm_map(dataset, content_transformer(gsub), pattern = '?=.*(.jpg|.gif)', replacement = "")
# remove html styling
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "style=``.*?``", replacement = "")
# # remove string that has "="
 dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\w*=+\\w*\\s*", replacement = "")
# # remove string that has ":"
 dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\w*:+\\w*\\s*", replacement = "")
dataset <- tm_map(dataset, content_transformer(gsub), pattern = "\\&\\w*", replacement = "")
dataset <- tm_map(dataset, removePunctuation)
dataset <- TermDocumentMatrix(dataset, control = list(
  #tokenize = GramTokenizer,
  tolower = T
))
dataset$dimnames
