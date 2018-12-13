library(readr)
library(data.table)
library(dplyr)
library(reshape2)
library(entropy)
library(ggplot2)
library(pbapply)
library(stats)

########################## CONSTANTS ##########################

address <- "/local/datasets/"
size_sample = 1000

########################## FUNCTIONS ##########################

moda <- function(x) {
  z <- table(as.vector(x)) 
  y = names(z)[z == max(z)]
  y[1]
}

genre.entropy <- function(Iu){
  H = inner_join(Iu, artist.data, by = "artistname")
  meltH = melt(H, id.vars = c("userid", "timestamp"), measure.vars = c(10:ncol(H)))
  castH = dcast(meltH, timestamp ~ variable, sum)
  timestamps = castH$timestamp
  castH$timestamp = NULL
  result = apply(castH, 1, entropy, method = "ML")
  return(cbind(timestamps, result))
}

decade <- function(year){
  # if((year > 1919) & (year < 2000))
  #   return(((floor(year / 10)) %% 10) * 10)
  # else
  return (floor(year / 10) * 10)
}

locality.entropy <- function(Iu){
  H = inner_join(Iu, artist.data, by = "artistname")
  H = H[order(H$timestamp),c("userid", "area","timestamp")] 
  result = group_by(H, timestamp) %>% dplyr::summarise(entropy = entropy(area, method = "ML"))
  return(cbind(H$timestamp %>% unique(), result$entropy))
}

contemporaneity.entropy <- function(Iu){
  H = inner_join(Iu, artist.data, by = "artistname")
  Hd = H[,c("userid", "debut", "timestamp")]
  Hl = H[,c("userid", "last", "timestamp")]
  result.debut = group_by(H, timestamp) %>% dplyr::summarise(entropy = entropy(debut, method = "ML"))
  result.last = group_by(H, timestamp) %>% dplyr::summarise(entropy = entropy(last, method = "ML"))
  result = rowMeans(cbind(result.debut$entropy,result.last$entropy))
  return(cbind(H$timestamp %>% unique(),result))
}

user.entropy <- function(u){
  Iu = LFM[(which(LFM$userid == u)),]
  ce = contemporaneity.entropy(Iu) %>% as.data.frame()
  names(ce) = c("timestamps", "entropy")
  le = locality.entropy(Iu) %>% as.data.frame()
  names(le) = c("timestamps", "entropy")
  ge = genre.entropy(Iu) %>% as.data.frame()
  names(ge) = c("timestamps", "entropy")
  ce$aspect = "contemporaneity"
  le$aspect = "locality"
  ge$aspect = "genre"
  e = rbind(ce,le,ge)
  fwrite(e,
         paste0("/local/Scripts/word2vec/MOAD/data/user_entropy/user",u,".txt"),
         sep = "\t",
         row.names = FALSE, col.names = TRUE)
}

########################## DATA READ ##########################

LFM <- fread(paste0(address,"experimento/LFM.artists.available.txt"), 
             sep = "\t", 
             verbose = TRUE,
             na.strings = "")
LFM = as.data.frame(LFM)
names(LFM) = c("artistid", "userid", "timestamp", "country", "age", "gender", "playcount", "registered", "artistname")
LFM[,4:8] = NULL
LFM$timestamp = LFM$timestamp / (60*60*24) # timestamp em segundos transformado pra dias
LFM$timestamp = floor(LFM$timestamp)

artist.data <- fread(paste0(address,"experimento/artist.data.txt"), 
                     sep = ";", 
                     verbose = TRUE,
                     na.strings = "")
artist.data = as.data.frame(artist.data)
names(artist.data)[2] = c("artistname")

users = as.list(unique(LFM$userid))

########################## ALL USERS ENTROPY FUNCTION ##########################

# pblapply(users, user.entropy) # write in files all the daily entropy for each user

vec2dec = function(vec){
  Reduce(function(x,y) x*2+y, vec)  
}

daily.propension = function(day, entropy.user, cm, lm, gm){
  # day = 15394
  # cm = entropy.user.contemporaneity
  # cl = entropy.user.locality
  # cg = entropy.user.genre
  entropy.day = filter(entropy.user, timestamps == day)
  return(c(entropy.day[1,2] >= cm, entropy.day[2,2] >= lm, entropy.day[3,2] >= gm) %>% as.numeric() %>% vec2dec())
}

user.scenario = function(u){

  # u = 7687
  entropy.user = fread(paste0("/local/Scripts/word2vec/MOAD/data/user_entropy/user",u,".txt"),
      sep = "\t",
      na.strings = "")

  median.contemporaneity.entropy = entropy.user %>% filter(aspect == "contemporaneity") %>% select(entropy) %>% t() %>% median()
  median.locality.entropy = entropy.user %>% filter(aspect == "locality") %>% select(entropy) %>% t() %>% median()
  median.genre.entropy = entropy.user %>% filter(aspect == "genre") %>% select(entropy) %>% t() %>% median()
  
  days = entropy.user$timestamps %>% unique()

  user.daily.scenarios = lapply(days, daily.propension, entropy.user, median.contemporaneity.entropy, median.locality.entropy, median.genre.entropy) 
  return(c(u, user.daily.scenarios %>% as.numeric() %>% moda()))
}

scenarios.by.user = pblapply(users,user.scenario)

scenarios.by.user.2.write = scenarios.by.user %>% as.data.frame() %>% t() %>% as.data.frame()
names(scenarios.by.user.2.write) = c("user","scenario")

fwrite(scenarios.by.user.2.write, 
       "/local/Scripts/word2vec/MOAD/data/user.propensity.entropy.daily.txt",
       row.names = FALSE, col.names = TRUE,
       sep = "\t")

