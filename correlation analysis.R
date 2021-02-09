
# analyse correlation of word appearance:
# time, performance, efficiency, safety and risk

library(tidyverse)
library(readxl)
library(corrplot)
library(data.table)

word_tor <- read_csv("Document Terms of Reference.csv") %>% 
  rename(tor = `Terms of Reference`)

tor_count <- word_tor %>% 
  group_by(tor) %>% 
  summarise(tor_count=n())

monogram <- read_csv("words combined separate files.csv")
bigram <- read_csv("nq phrases bigram separate files.csv")
trigram <- read_csv("nq phrases trigram separate files.csv")

combo <- monogram %>% 
  bind_rows(bigram, trigram) %>% 
  left_join(word_tor, by="doc_id") %>%   
  left_join(tor_count, by="tor") %>%   
  mutate(
    word = ifelse(ngram_num=='risks', 'risk', ngram_num),
    word = ifelse(ngram_num=='efficient', 'efficiency', ngram_num),
    word = ifelse(ngram_num=='safe', 'safety', ngram_num),
  ) %>% 
  group_by(word, tor) %>% 
  summarise(wc = round(sum(n)/min(tor_count),0)) %>% 
  ungroup %>% 
  pivot_wider(names_from = tor, values_from = wc) %>% 
  write_csv('correlation.csv')
  

word_vec <- read_csv("nq phrases trigram bigram separate files revised.csv") %>% 
  left_join(word_tor, by="doc_id")

words <- c("time", "performance", "efficiency", "safety", "risk")

word_cor <- word_vec %>%
  mutate(
    word = ifelse(ngram_num=='risks', 'risk', ngram_num),
    word = ifelse(ngram_num=='efficient', 'efficiency', ngram_num),
    word = ifelse(ngram_num=='safe', 'safety', ngram_num),
  ) %>% 
  # filter(word %in% words) %>% 
  group_by(word, tor) %>% 
  summarise(wc = sum(n)) %>% 
  ungroup %>% 
  pivot_wider(names_from = tor, values_from = wc) %>% 
  write_csv('correlation.csv')

# word_cor <- word_vec %>%
#   mutate(
#     word = ifelse(ngram_num=='risks', 'risk', ngram_num),
#     word = ifelse(ngram_num=='efficient', 'efficiency', ngram_num),
#     word = ifelse(ngram_num=='safe', 'safety', ngram_num),
#   ) %>% 
#   filter(word == 'time') %>% 
#   group_by(word, tor) %>% 
#   summarise(wc = sum(n)) %>% 
#   ungroup %>% 
#   pivot_wider(names_from = word, values_from = wc)
# 
# rownames(word_cor) <- word_cor$tor
# 
# word_cor <- select(word_cor, -tor)
# 
# for(x in word_cor$tor){
#   val <- filter(word_cor, tor==x)$time
#   col <- 
# }

interview_groups <- read_excel("correlation to peter 20200802_2.xlsx", sheet=3) %>% 
  rename_all(.funs = funs(gsub("\\s", "_", .))) %>% 
  group_by(Terms_of_Reference) %>% 
  summarise(interviews = length(unique(doc_id)))

rel1 <- read_excel("correlation to peter 20200802_2.xlsx", sheet=3) %>% 
  rename_all(.funs = funs(gsub("\\s", "_", .))) %>% 
  select(ngram_num, Relationship_1) %>%
  filter(!is.na(Relationship_1)) %>% 
  distinct(ngram_num, .keep_all = TRUE) %>% 
  mutate(Relationship_1 = tolower(Relationship_1),
         Relationship_1 = ifelse(Relationship_1=="manufactuer", "manufacturer", Relationship_1),
         Relationship_1 = ifelse(Relationship_1=="oganisational alignment", "organisational alignment", Relationship_1)
         )

combo1 <- monogram %>% 
  bind_rows(bigram, trigram) %>% 
  left_join(word_tor, by="doc_id") %>%   
  left_join(tor_count, by="tor") %>%
  mutate(
    word = ifelse(ngram_num=='risks', 'risk', ngram_num),
    word = ifelse(word=='efficient', 'efficiency', word),
    word = ifelse(word=='safe', 'safety', word),
    word = ifelse(word=='suppliers', 'supplier', word),
    word = ifelse(word=='low cost', 'cost', word),
    word = ifelse(word=='customers', 'customer', word),
    word = ifelse(word=='airlines', 'airline', word)
  ) %>% 
  left_join(rel1, by=c("word"="ngram_num")) %>%
  mutate(context = ifelse(is.na(Relationship_1), word, Relationship_1),
         number = as.numeric(context)) %>%
  filter(is.na(number))

tot_wc <- combo1 %>% 
  group_by(context) %>% 
  summarise(total = sum(n))

word_group <- c(c('market', 'people'), unique(rel1$Relationship_1))

combo2 <- combo1 %>% 
  mutate(avgn = n/tor_count) %>% 
  group_by(context, tor) %>% 
  summarise(word_count = sum(avgn)) %>% 
  ungroup %>% 
  pivot_wider(names_from = tor, values_from = word_count) %>% 
  left_join(tot_wc, by = "context") %>%
  arrange(desc(total)) %>% 
  filter(!grepl("'|’", context),
         context %in% word_group) %>% 
  write_csv('correlation2.csv', na="")

combo3 <- data.table::transpose(combo2)

combo3 <- combo3[2:length(combo3),]

names(combo3) <- combo2[,1][[1]]

combo3 <- combo3 %>% mutate_all(.funs=funs(as.numeric))

combo2[is.na(combo2)] <- 0
m <- cor(select(combo2, -total, -context))

library(RColorBrewer)
corrplot(m, method="color", type="full", outline = FALSE, addgrid.col = "darkgray",
         order="hclust", mar = c(2,0,2,0), addrect = 4, rect.col = "black",
         rect.lwd = 3, tl.col = "indianred4", tl.cex = 0.8, cl.cex = 0.9,
         col=colorRampPalette(c("red","white","darkgreen"))(100)
         )

corrplot(m, method="number", type="full", outline = FALSE, addgrid.col = "darkgray",
         order="hclust", mar = c(2,0,2,0), addrect = 4, rect.col = "black",
         rect.lwd = 3, tl.col = "indianred4", tl.cex = 0.8, cl.cex = 0.9,
         col=colorRampPalette(c("red","white","darkgreen"))(100)
)

corrplot.mixed(m, lower="number", upper="color", tl.pos="lt", diag="l")
