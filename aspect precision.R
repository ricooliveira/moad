library(dplyr)
library(data.table)
library(lsa)
library(nsga2R)
library(reshape)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
# address <- "/local/Scripts/word2vec/MOAD/"
address <- "/local/datasets/"

########################## DATA LOAD ##########################

artist.data <- fread(paste0(address,"experimento/artist.data.txt"), 
                     sep = ";", 
                     verbose = TRUE,
                     header = TRUE)
artist.data = as.data.frame(artist.data)

# Correcting debut year from Rod Stewart - wrong in MusicBrainz

artist.data[which(artist.data$Artist == "The_Pains_of_Being_Pure_at_Heart"),"last"] = 2017
artist.data[which(artist.data$Artist == "Rod_Stewart"),"debut"] = 1968

# Input 0 in NA values

col = c(3:6)
for(i in col){
  artist.data[which(is.na(artist.data[,i])),i] = 0
}

# Recommendations load

data.train <- fread(paste0(address,"experimento/LFM_train.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.train = as.data.frame(data.train)

data.test <- fread(paste0(address,"experimento/LFM_test.txt"), 
                   sep = "\t", 
                   verbose = TRUE,
                   header = TRUE)
data.test = as.data.frame(data.test)
data.test = data.test[,c("user-id", "artist-name")] %>% unique()
names(data.test) = c("user", "artistname")

recommendations.w2v <- fread("/local/Scripts/word2vec/MOAD/data/sample1000.nsga.word2vec.top10.pop10.gen20.txt", 
                   sep = ",", 
                   verbose = F,
                   header = F)
recommendations.w2v = as.data.frame(recommendations.w2v)
names(recommendations.w2v) = c("user","artistname")

recommendations.entropy <- fread("/local/Scripts/word2vec/MOAD/data/sample1000.nsga.entropy.top10.pop10.gen20.txt", 
                             sep = ",", 
                             verbose = F,
                             header = F)
recommendations.entropy = as.data.frame(recommendations.entropy)
names(recommendations.entropy) = c("user","artistname")

########################## ASPECT PRECISION - GENRE ##########################

melt.artists = melt(artist.data, id.vars = c("Artist"), measure.vars = c(7:ncol(artist.data)))
artists.by.genre = select(filter(melt.artists, value == 1), c(Artist,variable))
names(artists.by.genre) = c("artistname", "genre")

aspects.recommendations.w2v = merge(artists.by.genre,recommendations.w2v) %>% select(c(user,genre))
aspects.recommendations.entropy = merge(artists.by.genre,recommendations.entropy) %>% select(c(user,genre))
aspects.data.test = merge(artists.by.genre,data.test) %>% select(c(user,genre))

genre.matches.w2v = merge(aspects.recommendations.w2v, aspects.data.test) %>% unique()
genre.matches.group.by.users.w2v = group_by(genre.matches.w2v, user)
genre.precision.w2v = summarise(genre.matches.group.by.users.w2v, precision = n())
genre.precision.w2v$method = "w2v"

genre.matches.entropy = merge(aspects.recommendations.entropy, aspects.data.test) %>% unique()
genre.matches.group.by.users.entropy = group_by(genre.matches.entropy, user)
genre.precision.entropy = summarise(genre.matches.group.by.users.entropy, precision = n())
genre.precision.entropy$method = "entropy"

genre.precision = rbind(genre.precision.w2v, genre.precision.entropy)

boxplot(precision~method,genre.precision)

shapiro.test(genre.precision.entropy$precision)
shapiro.test(genre.precision.w2v$precision)

########################## ASPECT PRECISION - LOCALITY ##########################

artists.by.locality = artist.data[,c("Artist", "area")]
names(artists.by.locality) = c("artistname", "locality")

aspects.recommendations.w2v = merge(artists.by.locality,recommendations.w2v) %>% select(c(user,locality))
aspects.recommendations.entropy = merge(artists.by.locality,recommendations.entropy) %>% select(c(user,locality))
aspects.data.test = merge(artists.by.locality,data.test) %>% select(c(user,locality))

locality.matches.w2v = merge(aspects.recommendations.w2v, aspects.data.test) %>% unique()
locality.matches.group.by.users.w2v = group_by(locality.matches.w2v, user)
locality.precision.w2v = summarise(locality.matches.group.by.users.w2v, precision = n())
locality.precision.w2v$method = "w2v"

locality.matches.entropy = merge(aspects.recommendations.entropy, aspects.data.test) %>% unique()
locality.matches.group.by.users.entropy = group_by(locality.matches.entropy, user)
locality.precision.entropy = summarise(locality.matches.group.by.users.entropy, precision = n())
locality.precision.entropy$method = "entropy"

locality.precision = rbind(locality.precision.w2v, locality.precision.entropy)

boxplot(precision~method,locality.precision)

########################## ASPECT PRECISION - CONTEMPORANEITY ##########################

library(readr)
artists.by.contemporaneity = read_csv("/local/Scripts/word2vec/MOAD/data/artists_by_contemporaneity.csv")
names(artists.by.contemporaneity) = c("artistname", "contemporaneity")

aspects.recommendations.w2v = merge(artists.by.contemporaneity,recommendations.w2v) %>% select(c(user,contemporaneity))
aspects.recommendations.entropy = merge(artists.by.contemporaneity,recommendations.entropy) %>% select(c(user,contemporaneity))
aspects.data.test = merge(artists.by.contemporaneity,data.test) %>% select(c(user,contemporaneity))

contemporaneity.matches.w2v = merge(aspects.recommendations.w2v, aspects.data.test) %>% unique()
contemporaneity.matches.group.by.users.w2v = group_by(contemporaneity.matches.w2v, user)
contemporaneity.precision.w2v = summarise(contemporaneity.matches.group.by.users.w2v, precision = n())
contemporaneity.precision.w2v$method = "w2v"

contemporaneity.matches.entropy = merge(aspects.recommendations.entropy, aspects.data.test) %>% unique()
contemporaneity.matches.group.by.users.entropy = group_by(contemporaneity.matches.entropy, user)
contemporaneity.precision.entropy = summarise(contemporaneity.matches.group.by.users.entropy, precision = n())
contemporaneity.precision.entropy$method = "entropy"

contemporaneity.precision = rbind(contemporaneity.precision.w2v, contemporaneity.precision.entropy)

boxplot(precision~method,contemporaneity.precision)
