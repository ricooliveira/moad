library(readr)
library(data.table)
library(dplyr)
library(reshape2)
library(entropy)
library(ggplot2)
library(pbapply)

########################## CONSTANTS ##########################

address <- "/local/datasets/"
size_sample = 1000

########################## FUNCTION ##########################

# shannon<-function(data){
#   H<-numeric(nrow(data))
#   mult<-numeric(length(data[1,]))
#   for(i in 1:nrow(data)){
#     prop<-data[i,]/sum(data[i,])
#     for(j in 1:ncol(data)){
#       mult[j]<-prop[j]*log(prop[j])
#     }
#     H[i]<--sum(mult, na.rm=T)
#   }
#   plot.number<-1:nrow(data)
#   return(rbind(plot.number,H))
# }

genre.meltcast <- function(u){
  print(u)
  Iu = LFM[(which(LFM$userid == u)),]
  H = inner_join(Iu, artist.data, by = "artistname")
  meltH = melt(H, id.vars = c("userid"), measure.vars = c(9:ncol(H)))
  castH = dcast(meltH, userid ~ variable, sum)
}

decade <- function(year){
  # if((year > 1919) & (year < 2000))
  #   return(((floor(year / 10)) %% 10) * 10)
  # else
  return (floor(year / 10) * 10)
}

locality.entropy <- function(u){
  Iu = LFM[(which(LFM$userid == u)),]
  H = inner_join(Iu, artist.data, by = "artistname")
  H = H[,c("userid","area")]
  result = entropy(H$area,method = "ML")
  return(result)
}

contemporaneity.entropy <- function(u){
  Iu = LFM[(which(LFM$userid == u)),]
  H = inner_join(Iu, artist.data, by = "artistname")
  Hd = H[,c("userid","debut")]
  Hl = H[,c("userid","last")]
  result.debut = entropy(Hd$debut,method = "ML")
  result.last = entropy(Hl$last,method = "ML")
  return(mean(result.debut,result.last))
}


########################## DATA READ ##########################

LFM <- fread(paste0(address,"experimento/LFM.artists.available.txt"), 
             sep = "\t", 
             verbose = TRUE,
             na.strings = "")
LFM = as.data.frame(LFM)
names(LFM) = c("artistid", "userid", "timestamp", "country", "age", "gender", "playcount", "registered", "artistname")
LFM[,4:8] = NULL

artist.data <- fread(paste0(address,"experimento/artist.data.txt"), 
                     sep = ";", 
                     verbose = TRUE,
                     na.strings = "")
artist.data = as.data.frame(artist.data)
names(artist.data)[2] = c("artistname")

########################## ENTROPY GENRE ##########################

users = as.list(unique(LFM$userid))
# users.histories = as.data.frame(do.call(rbind, lapply(users, genre.meltcast)))
# fwrite(users.histories, 
#        paste0(address,"experimento/user.histories.genre.txt"),
#        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

users.histories = fread(paste0(address,"experimento/user.histories.genre.txt"), 
                        sep = "\t")

genre.entropy.result = cbind(users.histories[,1],as.data.frame(apply(users.histories[,2:ncol(users.histories)], 1, entropy)))
names(genre.entropy.result) = c("userid","entropy")

########################## ENTROPY CONTEMP ##########################

# Correct data 
artist.data[which(artist.data$artistname == "The Pains of Being Pure at Heart"),"last"] = 2017
artist.data[which(artist.data$artistname == "Rod Stewart"),"debut"] = 1968

artist.data.decades = artist.data[,c("id", "artistname", "debut", "last")]

# x = as.data.frame(do.call(rbind, apply(artist.data$debut, 2, decade)))
# x = do.call(decade,as.list(artist.data$debut))

# for(i in 1:nrow(artist.data.decades)){
#   artist.data.decades[i,"debut"] = decade(artist.data.decades[i,"debut"])
#   artist.data.decades[i,"last"] = decade(artist.data.decades[i,"last"])
# }


for(i in 1:nrow(artist.data)){
  artist.data[i,"debut"] = decade(artist.data[i,"debut"])
  artist.data[i,"last"] = decade(artist.data[i,"last"])
}

entropia = pbsapply(users, contemporaneity.entropy)
contemporaneity.entropy.result = cbind(users, entropia) %>% as_data_frame()

########################## ENTROPY LOCAL ##########################

entropia = pbsapply(users, locality.entropy)
locality.entropy.result = cbind(users, entropia) %>% as_data_frame()

########################## ENTROPY - ALL ##########################

users.entropy = cbind(contemporaneity.entropy.result, locality.entropy.result, genre.entropy.result)
users.entropy = users.entropy[,c(1,2,4,6)]
names(users.entropy) = c("user", "contemporaneity", "locality", "genre")

###################### MATRIX GENERATION ######################

users = users.entropy$user %>% unique() %>% as.integer()
nusers = users.entropy$user %>% unique() %>% length()
nartists = artist.data %>% nrow()
ratings.matrix = matrix(data = 0, nrow = nusers, ncol = nartists)

for (i in (1:nusers)){
  user.history = filter(LFM, userid == users[i])
  ratings.matrix[i, which(artist.data$artistname %in% user.history$artistname)] = 1
}

fwrite(ratings.matrix %>% as.data.frame(),
       "/local/Scripts/word2vec/MOAD/data/userItemMatrix.txt", 
       col.names = FALSE, row.names = FALSE)

###################### METRIC ######################

satisfaction.degree = function(artist){
  pos = which(artist.data$artistname == artist)
  users.rated = which(ratings.matrix[,pos] == 1)
  NRi = length(users.rated)
  S.contemporaneity = ((users.entropy[users.rated,"contemporaneity"]) %>% as.numeric() / (NRi * log(1 + NRi))) %>% sum()
  S.locality = ((users.entropy[users.rated,"locality"]) %>% as.numeric() / (NRi * log(1 + NRi))) %>% sum()
  S.genre = ((users.entropy[users.rated,"genre"]) %>% as.numeric() / (NRi * log(1 + NRi))) %>% sum()
  return(c(S.contemporaneity, S.locality, S.genre))
}

###################### RESULTS ######################

results.w2v <- fread("/local/Scripts/word2vec/MOAD/data/sample1000.nsga.word2vec.top10.pop10.gen20.txt", 
                     sep = ",", 
                     verbose = TRUE,
                     na.strings = "",
                     header = F)
results.w2v = as.data.frame(results.w2v)
names(results.w2v) = c("user","artist")

results.entropy <- fread("/local/Scripts/word2vec/MOAD/data/sample1000.nsga.entropy.top10.pop10.gen20.txt", 
                     sep = ",", 
                     verbose = TRUE,
                     na.strings = "",
                     header = F)
results.entropy = as.data.frame(results.entropy)
names(results.entropy) = c("user","artist")

users = unique(results.w2v$user)

generate.user.results = function(usr, results){
  user.recommendation = filter(results, user == usr)
  entropies = lapply(user.recommendation$artist, satisfaction.degree) %>% as.data.frame()
  entropies = apply(entropies, 1, mean)%>% t() %>% as.numeric()
  return(c(usr, entropies))
}

chinese.w2v = pbsapply(users, generate.user.results, results.w2v) %>% t() %>% as.data.frame()
chinese.entropy = pbsapply(users, generate.user.results, results.entropy) %>% t() %>% as.data.frame()

names(chinese.w2v) = c("user", "contemporaneity", "locality", "genre")
names(chinese.entropy) = c("user", "contemporaneity", "locality", "genre")

###################### PROPENSITY DATA ######################

propensity.w2v <- fread("/local/Scripts/word2vec/MOAD/data/user.propensity.word2vec.txt", 
                     sep = "\t", 
                     verbose = TRUE,
                     na.strings = "") %>% as.data.frame()
names(propensity.w2v) = c("user", "p_contemporaneity", "p_locality", "p_genre")

propensity.entropy <- fread("/local/Scripts/word2vec/MOAD/data/user.propensity.entropy.txt", 
                        sep = "\t", 
                        verbose = TRUE,
                        na.strings = "") %>% as.data.frame()
names(propensity.entropy) = c("user", "p_contemporaneity", "p_locality", "p_genre")

################################ PLOT ########################################

w2v.plot = merge(propensity.w2v, chinese.w2v, by = "user")
w2v.plot$method = "item2vec"
entropy.plot = merge(propensity.entropy, chinese.entropy, by = "user")
entropy.plot$method = "entropy"
chinese.plot = rbind(w2v.plot, entropy.plot)

plot.melt = melt(chinese.plot, id.vars = c("user", "p_contemporaneity", "p_locality", "p_genre", "method"), measure.vars = c("contemporaneity", "locality", "genre"))
plot.melt$propensity = -1

# This code is a shame
plot.melt[c(1:1994),"propensity"] = plot.melt[c(1:1994),"p_contemporaneity"]
plot.melt[c(1995:3988),"propensity"] = plot.melt[c(1995:3988),"p_locality"]
plot.melt[c(3989:5982),"propensity"] = plot.melt[c(3989:5982),"p_genre"]
plot.melt$p_contemporaneity = NULL
plot.melt$p_locality = NULL
plot.melt$p_genre = NULL
plot.melt$propensity = factor(plot.melt$propensity)
plot.melt$value = as.numeric(plot.melt$value)


f <- ggplot(plot.melt, aes(propensity, value, group = propensity, color = propensity)) + geom_boxplot() +
 facet_grid(method ~ variable) + theme_bw() + 
 theme(legend.position = "bottom", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
f
ggsave("/local/Scripts/word2vec/MOAD/data/chinese.metric.pdf", f)

# f <- ggplot(w2v.plot, aes(metodo, value, group = metodo, color = metodo)) + geom_boxplot() +
#   facet_grid(variable ~ cenario) + theme_bw() + theme(legend.position = "bottom", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
# f
# 
# ggsave("resultados_moad.pdf", f)
















