###########################################
#### exploitation de la base theses.fr ####
###########################################

# load packages ----

library(readr)
library(udpipe)
library(xml2)
library(rvest)
library(ggplot2)
library(dplyr)


# load data ----

oneFile <- read_delim("DATA/resultat.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)
sourceXml <- read_xml("DATA/resultat.xml")

auteur <- html_nodes(sourceXml, xpath = "//str[@name = 'auteur']") %>% html_text()
auteurppn <- html_nodes(sourceXml, xpath = "//str[@name = 'auteurPpn']") %>% html_text()
directeur <- html_nodes(sourceXml, xpath = "//arr[@name = 'directeurThese']") %>% html_text()
directeurppn <- html_nodes(sourceXml, xpath = "//arr[@name = 'directeurThesePpn']") %>% html_text()
jury <- html_nodes(sourceXml, xpath = "//arr[@name = 'ppn']") %>% html_table()

# attention dans le jury il y a le candidat et il y a des problèmes d'ordre et de doublon si on compare avec "personne"


# apply model ----

ud_model <- udpipe_load_model(file = "DATA/french-ud-2.0-170801.udpipe")
x <- udpipe_annotate(ud_model, x = oneFile$Titre, doc_id = oneFile$`Identifiant de la these`)
x <- as.data.frame(x)


# explore results ----

# word function
stats <- txt_freq(x$upos)
ggplot(stats) +
  geom_bar(aes(x = key, y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

# nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$lemma) 
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = key, y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

## adjectives
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$lemma)
ggplot(stats[1:20, ]) +
  geom_bar(aes(x = key, y = freq_pct), stat = "identity") +
  coord_flip() + theme_bw()

