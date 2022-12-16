library(quanteda)
library(text2vec)
library(tidyverse)
library(hash)

#First we need some text
#let's make some fake tweets

text<-c("The capitol in westminster is surrounded by 1,000 corgis https://www.realurlwithcorgies.com",
        "there are a hundred people outside this arbys demanding hashbrowns smothered and covered and I don't know what to tell them",
        "$10 is a lot to ask for a box of nails",
        "I came here five times yesterday. Every time I did was a disappointment, but here I am again",
        "Tolkien wrote the entry for the word warlock in the OED")

### GET VOCABULARY FROM A CORPUS OF TEXT

#clean and tokenize
tokens<-text%>%
  tolower()%>%
  tokens(remove_url = T)%>%
  tokens_remove(pattern = c("@*", "#*", stopwords()))%>% #removes hashtags, usertags, and stopwords
  tokens(remove_punct = T,
         remove_symbols = T,
         remove_numbers = T)
#to dfm
dfm<-dfm(tokens)
#get vocab
vocab<-dfm@Dimnames$features 

### READ IN EMBEDDINGS 

#I'll use glove embedding here.
#the text file in this script is available at (https://nlp.stanford.edu/projects/glove/)

#data.table's fread() function reads the text file in quickly
glove<-data.table::fread('glove.twitter.27B.200d.txt', data.table = FALSE, encoding = 'UTF-8')

#convert to a matrix with named rows (to make it compatible with text2vecs functions)
names<-glove[[1]]
glove<-as.matrix(glove[,-1])
rownames(glove)<-names
rm(names)

#toy analogy text
berlin = glove["paris", , drop = FALSE] - 
  glove["france", , drop = FALSE] + 
  glove["germany", , drop = FALSE]  

#berlin should be near the top of the most similar vectors
cos_berlin <- sim2(x = glove, y = berlin , method = "cosine", norm = "l2")
head(sort(cos_berlin[,1], decreasing = TRUE), 20)

#get entries in vocabulary that have glove embeddings
vocab_embeds_index<-intersect(vocab, rownames(glove))

#get embeddings for words in vocabulary (cuts down on time for next step)
words_in<-glove[vocab_embeds_index,]

#remove tokens that are outside of the word embedding space and convert to list for later
toks_pruned<-tokens_select(
  tokens,
  rownames(words_in),
  selection = "keep",
  valuetype = "fixed"
)%>%
  as.list()

### CREATE HASH TABLE FOR EMBEDDING LOOKUP 

#(similar to dictionary in python) for embedding lookup

glove_hash<-hash(as.list(as.data.frame(t(words_in))))
#example of extracting embeddings from hash table
values(glove_hash, c("warlock", "corgis"))

### LOOKUP EMBEDDINGS FOR EACH DOCUMENT AND AVERAGE

#initialize list with document embeddings
doc_embeds<-list()
#loop that...
for(i in 1:length(toks_pruned)){
  x<-values(glove_hash,toks_pruned[[i]]) #looks up each embeddings for each doc
  n<-ncol(x) # gets token number for doc
  doc_embeds[[i]]<-rowSums(x)/n # averages embedding for doc
}

### GET REPRESENTATION FOR DESIRED CONSTRUCT

#need to still have the full glove embeddings in your working environment
#lets's create a construct representation for breakfast
words<-c("waffles", "bacon","eggs","toast","pancakes", "hashbrowns")
breakfast_d<-data.frame(matrix(ncol = 200, nrow = length(words)))
for(i in 1:length(words)){
  breakfast_d[i,]<-glove[words[[i]], , drop = FALSE]
}
breakfast<-colMeans(breakfast_d)%>%
  as.matrix()%>%
  t()
colnames(breakfast)<-colnames(glove)
#check out the nearest neighbors of our construct to make sure it looks like breakfast
breakfast_cos_sim = sim2(x = glove, y = breakfast, method = "cosine", norm = "l2")
head(sort(breakfast_cos_sim[,1], decreasing = TRUE), 100)

### NOW GET DDR SCORES TO SEE WHICH TWEET IS MOST CLOSELY RELATED TO BREAKFAST

#covert doc embeddings to matrix
doc_embeds_mat<-matrix(unlist(doc_embeds), byrow = T, ncol = 200)
#get scores
text_ddr_df<-tibble(text = text, 
                    breakfast_ddr = sim2(x = doc_embeds_mat, y = breakfast, method = "cosine", norm = "l2"))
#as we'd expect, our tweet discussing hashbrowns had the highest similarity to breakfast!
text_ddr_df


