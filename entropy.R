library(readr)
library(data.table)
library(dplyr)
library(reshape2)
library(entropy)
library(ggplot2)
library(pbapply)

########################## CONSTANTS ##########################

# address <- "/local/Scripts/moad/"
address <- "/home/ricardo/DataScience/moad/"
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

#LFM <- fread(paste0(address,"experimento/LFM.artists.available.txt"), 
LFM <- fread(paste0(address,"data/LFM_train.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    na.strings = "")
LFM = as.data.frame(LFM)
names(LFM) = c("albumid", "artistid", "userid", "timestamp", "country", "age", "gender", "playcount", "registered", "artistname", "albumname")
LFM = LFM %>% select(userid, timestamp, artistname)
#LFM[,4:8] = NULL

artist.data <- fread(paste0(address,"data/artist.data.txt"), 
             sep = ";", 
             verbose = TRUE,
             na.strings = "")
artist.data = as.data.frame(artist.data)
names(artist.data)[2] = c("artistname")

# Correct data 
artist.data[which(artist.data$artistname == "The Pains of Being Pure at Heart"),"last"] = 2017
artist.data[which(artist.data$artistname == "Rod Stewart"),"debut"] = 1968

# artist.data.decades = artist.data[,c("id", "artistname", "debut", "last")]

for(i in 1:nrow(artist.data)){
  artist.data[i,"debut"] = decade(artist.data[i,"debut"])
  artist.data[i,"last"] = decade(artist.data[i,"last"])
}

users = LFM$userid %>% unique()

########################## ENTROPY GENRE ##########################

# users.histories = as.data.frame(do.call(rbind, lapply(users, genre.meltcast)))
# fwrite(users.histories, 
#        paste0(address,"experimento/user.histories.genre.txt"),
#        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

users.histories = fread(paste0(address,"data/user.histories.genre.txt"), 
                        sep = "\t")

genre.entropy.result = cbind(users.histories[,1],as.data.frame(apply(users.histories[,2:ncol(users.histories)], 1, entropy)))
names(genre.entropy.result) = c("userid","entropy")

########################## ENTROPY CONTEMP ##########################

# x = as.data.frame(do.call(rbind, apply(artist.data$debut, 2, decade)))
# x = do.call(decade,as.list(artist.data$debut))

# for(i in 1:nrow(artist.data.decades)){
#   artist.data.decades[i,"debut"] = decade(artist.data.decades[i,"debut"])
#   artist.data.decades[i,"last"] = decade(artist.data.decades[i,"last"])
# }

entropia = pbsapply(users, contemporaneity.entropy)
contemporaneity.entropy.result = cbind(users, entropia) %>% as_data_frame()

########################## ENTROPY LOCAL ##########################

entropia = pbsapply(users, locality.entropy)
locality.entropy.result = cbind(users, entropia) %>% as_data_frame()

#locality.entropy.result = a %>% rowwise() %>% mutate(entropy = locality.entropy(users))
#locality.entropy.result = as.data.frame(do.call(rbind, lapply(users, locality.entropy)))

########################## BARPLOT ENTROPY ##########################

# locality.entropy.result$entropia = as.numeric(locality.entropy.result$entropy)
# contemporaneity.entropy.result$entropia = as.numeric(contemporaneity.entropy.result$entropy)

genre.plot = ggplot(data=genre.entropy.result, aes(x = reorder(userid, -entropy), y = entropy)) +
  geom_bar(stat="identity")
locality.plot = ggplot(data=genre.entropy.result, aes(x = reorder(userid, -entropia), y = entropia)) +
  geom_bar(stat="identity")
contemporaneity.plot = ggplot(data=genre.entropy.result, aes(x = reorder(userid, -entropia), y = entropia)) +
  geom_bar(stat="identity")


########################## BOXPLOT ENTROPY ##########################

genre.entropy.result = bind_cols(genre.entropy.result, as.data.frame(rep("genre", nrow(genre.entropy.result))))
names(genre.entropy.result) = c("userid", "entropy", "aspect")
contemporaneity.entropy.result = bind_cols(contemporaneity.entropy.result, as.data.frame(rep("contemporaneity", nrow(contemporaneity.entropy.result))))
names(contemporaneity.entropy.result) = c("userid", "entropy", "aspect")
locality.entropy.result = bind_cols(locality.entropy.result, as.data.frame(rep("locality", nrow(locality.entropy.result))))
names(locality.entropy.result) = c("userid", "entropy", "aspect")

entropy.results = rbind(genre.entropy.result, contemporaneity.entropy.result, locality.entropy.result)
boxplot.entropy = ggplot(entropy.results, aes(x = aspect, y = entropy, fill=aspect)) +
                    geom_boxplot()
genre.boxplot.entropy = ggplot(genre.entropy.result, aes(x = aspect, y = entropy, fill=aspect)) +
  geom_boxplot()

genre.entropy.distribution = ggplot(genre.entropy.result, aes(x = entropy)) + geom_density()
genre.entropy.sd = sd(genre.entropy.result$entropy)
genre.entropy.mean = mean(genre.entropy.result$entropy)

locality.entropy.distribution = ggplot(locality.entropy.result, aes(x = entropy)) + geom_density()
locality.entropy.sd = sd(locality.entropy.result$entropy)
locality.entropy.mean = mean(locality.entropy.result$entropy)

contemporaneity.entropy.distribution = ggplot(contemporaneity.entropy.result, aes(x = entropy)) + geom_density()
contemporaneity.entropy.sd = sd(contemporaneity.entropy.result$entropy)
contemporaneity.entropy.mean = mean(contemporaneity.entropy.result$entropy)

############################ SCENARIOS ############################

scenarios = data.frame(userid = users, contemporaneity = rep(0, length(users)), locality = rep(0, length(users)), 
                       genre = rep(0, length(users)))
medians = c(median(contemporaneity.entropy.result$entropy), 
            median(locality.entropy.result$entropy),
            median(genre.entropy.result$entropy))
names(medians) = c("contemporaneity", "locality", "genre")
for(i in 1:nrow(scenarios)){
  if(contemporaneity.entropy.result[i,2] > medians[1])
    scenarios[i,2] = 1
  if(locality.entropy.result[i,2] > medians[2])
    scenarios[i,3] = 1
  if(genre.entropy.result[i,2] > medians[3])
    scenarios[i,4] = 1
}

fwrite(scenarios, 
       paste0(address, "data/user.propensity.entropy.median.txt"),
       sep = "\t",
       row.names = F,
       col.names = T)

users.by.scenario = group_by(scenarios, contemporaneity, locality, genre)
users.scenarios = dplyr::summarise(users.by.scenario, total = n())
scenarios.label = as.data.frame(as.character(c(1:8)))
names(scenarios.label) = "scenario"

users.scenarios = as.data.frame(users.scenarios)
users.scenarios = cbind(users.scenarios, scenarios.label)

barplot.scenarios = ggplot(users.scenarios, aes(x = scenario, y = total, fill = scenario)) +
                      geom_bar(stat="identity")

barplot.scenarios

#############################################################################################
################################# PREPARE FOR DOC2VEC #######################################
#############################################################################################

LFM = LFM[order(LFM$userid, LFM$timestamp),]
H = inner_join(LFM, artist.data, by = "artistname")
meltH = melt(H, id.vars = c("userid", "area", "debut", "last", "timestamp"), measure.vars = c(9:ncol(H)))
meltH = meltH[which(meltH$value == 1), c(1:6)]

fwrite(meltH, 
       paste0(address,"experimento/data.for.word2vec.txt"), 
       row.names = FALSE, col.names = TRUE, sep = "\t", na = "")
