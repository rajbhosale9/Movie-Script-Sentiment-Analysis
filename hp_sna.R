#########################################################################
#Group 2 MIS 612 Harry Potter Term Project
#Harry Potter and the Chamber of Secrets
#########################################################################

#Install Necessary Packages
install.packages("tidyverse")
install.packages("tm")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("tidytext")
install.packages("RWeka")
install.packages("reshape2")
install.packages("radarchart")
install.packages("circlize")
install.packages("dplyr")
install.packages("igraph")
install.packages("Matrix")

#Load Packages from Memory
library(dplyr)
library(tm)
library(textstem)
library(igraph)
library(Matrix)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(RWeka)
library(reshape2)
library(radarchart)
library(circlize)

#######################################################################
#SNA Process to create Social network Maps 
#######################################################################

#Input the CSV script and assign it to a variable called Book2
Book2<-read.csv("Book2Major.csv")

#Create File with Speakers and Listeners only
speaker2speaker<-Book2[,c("Speaker.1","Speaker.2")]

#Remove Blank Character Rows
speaker2speaker<-speaker2speaker%>% filter(Speaker.2 != "")
speaker2speaker<-speaker2speaker%>% filter(Speaker.1 != "")

#Count Speakers
length(unique(speaker2speaker$Speaker.1))
unique(speaker2speaker$Speaker.1)

#Count Listeners
length(unique(speaker2speaker$Speaker.2))
unique(speaker2speaker$Speaker.2)

#Count the number of times characters speak with each other
Dialogue<-speaker2speaker %>% group_by(Speaker.1, Speaker.2) %>% summarise(counts = n())

#Create a vector with character names
speakers<-c(as.character(Dialogue$Speaker.1), as.character(Dialogue$Speaker.2))
speakers<-unique(speakers)

#Assign values to Matrix connecting nodes (speakers) via edges (dialgoue)
g<-graph_from_data_frame(d=Dialogue, vertices=speakers, directed=TRUE)
g

#View nodes (characters)
V(g)$name

#View edges (dialogue)
E(g)

#Plot graph to view initial SNA Map
plot(g, vertex.label.color = "black")

#Assign weights based on number of conversations between each character
weights<-E(g)$counts

#Apply weights to the graph
plot(g, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(weights),  
     layout = layout_nicely(g))

#Delete any edges with weight < 2
g2<-delete_edges(g,E(g)[weights<2])
plot(g2,vertex.label.color="black",edge.width=3,vertex.size=8)

#Calculate Betweeness Score
g.b<-betweenness(g2)

#Plot with vertex size determined by betweenness score
plot(g2,vertex.label.color="black",
     edge.color="black",
     vertex.size=sqrt(g.b),
     edge.arrow.size=0.03,
     layout=layout_nicely(g2))

#Simplify Graph by Removing loops & Multiple Edges
g2<-simplify(g2)

#Plot the Simplified Graph
plot(g2,vertex.label.color="black",
     edge.color="black",
     vertex.size=sqrt(g.b),
     edge.arrow.size=0.03,
     layout=layout_with_fr(g2))

##########################################################################
#Sentiment Analysis Process
##########################################################################

# Read the Harry Potter movie script as a CSV and assign it to Scripts
scripts <- read.csv("HP.csv")

#Create File with Speakers and Listeners only
speaker2speaker<-Book2[,c("Speaker.1","Speaker.2")]

# Selecting top characters based on dialogues 
diag <- iconv(scripts$Narrative_Lemma)
char <- iconv(scripts$Speaker)
ndial = length(diag)
all_char <- as.data.frame(sort(table(scripts$Speaker), decreasing=TRUE))
top_char <- as.data.frame(sort(table(scripts$Speaker), decreasing=TRUE))[1:11,]
ntop = nrow(all_char)
all_char$Var1
char_diag = rep("", ntop)
for (i in 1:ntop) {
  char_diag[i] = paste(scripts$Narrative_Lemma[char == all_char$Var1[i]], collapse=" ")
}
names(char_diag) = all_char$Var1


# Create Corpus from character Dialogue 
diag_corpus = Corpus(VectorSource(char_diags))

# Text Pre-Processing of the Corpus 
diag_corpus = tm_map(diag_corpus, tolower)
diag_corpus = tm_map(diag_corpus, removeWords, 
                     c(stopwords("english"),"comlink"))
diag_corpus = tm_map(diag_corpus, removeNumbers)
diag_corpus = tm_map(diag_corpus, removePunctuation)
diag_corpus = tm_map(diag_corpus, stripWhitespace)

# Create Document Term Matrix from Corpus
diag_dtm = DocumentTermMatrix(diag_corpus)

diag_dtm
dim(diag_dtm)

# Convert the DTM to a Matrix
diag_mat = as.matrix(diag_dtm)

# Get word count
count_terms = colSums(diag_mat)
hist(count_terms, col="red")

# Most frequent words in the Script
which_mfw <- count_terms >= quantile(count_terms, probs=0.90)
sum(which_mfw)
# Top 20 terms
mfw <- count_terms[which_mfw]
barplot(head(sort(mfw, decreasing=TRUE), 20), 
        border=NA, las=2,col = "maroon")
title(c("Most frequent words in the script"), cex.main=0.9)
mfw1 <- head(sort(count_terms,decreasing=TRUE),20)

#Tokenize Sentiment of Script

tokens <- scripts %>%  
  mutate(dialogue=as.character(scripts$Narrative_Lemma)) %>%
  unnest_tokens(word, dialogue)

tokens <- tokens %>% select(Speaker, Listener, word)

# Find Main Sentiment of Script
sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 

# Plot the Frequency of each sentiment category
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw()


# For calculating sentiment of a specific character using Bing Lexicon
tokens %>%
  filter(tokens$Speaker %in% c( "Hermione")) %>%
  group_by(Speaker) %>% 
  inner_join(get_sentiments("bing"), by = 'word') %>%
  count(Speaker, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~Speaker, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw()  

# For calculating sentiment of a specific character using NRC
tokens %>%
  filter(tokens$Speaker %in% c( "Harry")) %>%
  group_by(Speaker) %>% 
  inner_join(get_sentiments("nrc"), by = 'word') %>%
  count(Speaker, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~Speaker, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw()  

# Top 10 terms for each sentiment
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Words") +
  coord_flip() +
  theme_bw() 





