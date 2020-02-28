# load packages
library(dplyr)
library(tidyr)
library(car)
library(rio)
library(readtext)
library(quanteda)


## Load APSR data on economic policy/social policy/neither ----

data_readtext_EPcoaldebate <- readtext("tests/data_creation/data_epcoaldebate.zip",
                                         ignore_missing_files = TRUE, encoding = "utf-8")



# note: the dataset still appears to contain screener which are not marked as screeners
# remove them through the "sentence_text" start
data_readtext_EPcoaldebate <- data_readtext_EPcoaldebate %>%
    filter(!grepl('Code this sentence as', sentence_text)) %>%
    filter(screener != "TRUE") %>%
    filter(X_golden != "TRUE")


speeches_metadata <- rio::import("tests/data_creation/metadata_EPcoaldebate.xlsx")[, 1:12]

names(speeches_metadata) <- c("speechno", "name_first", "name_last", "title", "speech", "country", "party",
                 "ep_group", "X1134", "X1135", "X1136", "X1137")

## aggregate results data frame
#speeches_metadata <- speeches_metadata[, -which(names(speeches_metadata) %in% c("X1134", "X1135", "X1136", "X1137"))]
speeches_metadata$vote <- factor(speeches_metadata$X1137, levels=c(1,2,5), labels=c("For", "Against", "Abstain"))
speeches_metadata$vote_noabstain <- speeches_metadata$vote
speeches_metadata$vote_noabstain[which(speeches_metadata$vote == "Abstain")] <- NA


data_EPcoaldebate <- left_join(data_readtext_EPcoaldebate, speeches_metadata, by = "speechno")


## Create relevant variables ----

data_EPcoaldebate <- data_EPcoaldebate %>%
    rename(lang_abbr = language,
           coder_id = X_worker_id,
           coder_trust = X_trust)

data_EPcoaldebate <- data_EPcoaldebate %>%
    mutate(language = car::recode(lang_abbr, "'DE'='German';'EN'='English';'ES'='Spanish';
                                  'IT'='Italian';'PL'='Polish';'GR'='Greek'"))


data_EPcoaldebate <- data_EPcoaldebate %>%
    mutate(crowd_subsidy_label = ifelse(subsidy_scale == 0, "Neutral or inapplicable",
                                       ifelse(subsidy_scale == -1, "Anti-Subsidy",
                                              ifelse(subsidy_scale == 1, "Pro-Subsidy", NA))))

# get unique number for each sentence in each speech and language
dat_language_sentence <- data_EPcoaldebate %>%
  select(language, name_last, sentenceid) %>%
  group_by(language, name_last) %>%
  unique() %>%
  mutate(sentid = 1:n())

data_EPcoaldebate_subset <- data_EPcoaldebate %>%
    select(text = sentence_text, crowd_subsidy_label, sentenceid,
           language, lang_abbr, name_last, name_first, ep_group, country, vote, coder_id, coder_trust) %>%
  left_join(dat_language_sentence, by = c("language", "name_last", "sentenceid")) %>%
  group_by(language, name_last, name_first, sentid) %>%
  mutate(sentcodeid = 1:n()) %>%
  mutate(sentence_id = paste(language, name_last, sentid, sep = "_")) %>%
  mutate(doc_id = paste(lang_abbr, name_last, sentid, sentcodeid, sep = "_")) %>%
  ungroup() %>%
  select(sentence_id, everything(), -c(sentenceid, sentcodeid, sentid, lang_abbr))


data_EPcoaldebate_subset$crowd_subsidy_label <- factor(data_EPcoaldebate_subset$crowd_subsidy_label)
data_EPcoaldebate_subset$language <- factor(data_EPcoaldebate_subset$language)
data_EPcoaldebate_subset$vote <- as.factor(data_EPcoaldebate_subset$vote)
data_EPcoaldebate_subset$text <- as.character(data_EPcoaldebate_subset$text)
data_EPcoaldebate_subset$sentence_id <- as.character(data_EPcoaldebate_subset$sentence_id)
data_EPcoaldebate_subset$ep_group <- factor(data_EPcoaldebate_subset$ep_group)
data_EPcoaldebate_subset$coder_id <- as.character(data_EPcoaldebate_subset$coder_id)
data_EPcoaldebate_subset$country <- factor(data_EPcoaldebate_subset$country)
data_EPcoaldebate_subset$coder_trust <- as.numeric(data_EPcoaldebate_subset$coder_trust)

names(data_EPcoaldebate_subset)


## Create and save corpus object ----

data_corpus_EPcoaldebate <- corpus(data_EPcoaldebate_subset,
                                       docid_field = "doc_id")


meta(data_corpus_EPcoaldebate) <- list(
  description = "A multilingual text corpus of speeches from a European Parliament debate on coal subsidies in 2010, with individual crowd codings as the unit of observation. The sentences are drawn from officially translated speeches from a debate over a European Parliament debate concerning a Commission report proposing an extension to a regulation permitting state aid to uncompetitive coal mines.

Each speech is available in six languages: English, German, Greek, Italian, Polish and Spanish. The unit of observation is the individual crowd coding of each natural sentence. For more information on the coding approach see Benoit et al. (2016).",
  source = "Debate 'State aid to facilitate the closure of uncompetitive coal mines.' (23 November 2010).  European Parliament transcript.",
  url = "http://bit.ly/EP-Coal-Aid-Debate",
  author = "multiple MEPs",
  keywords = c("European Parliament", "debate", "transcript", "state aid", "coal mining", "subsidies"),
  title = "Crowd-labelled sentence corpus from a 2010 EP debate on coal subsidies"
)

# add corpus to package
usethis::use_data(data_corpus_EPcoaldebate, overwrite = TRUE)
