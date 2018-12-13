library(readr)
library(data.table)
library(dplyr)
library(stringr)

########################## CONSTANTS ##########################

address <- "/local/Scripts/word2vec/MOAD/"
size_sample = 1000

########################## FUNCTIONS ###########################

rewrite.lfm <- function(u){
  Iu = filter(LFM, userid == u)
  artists.line = as.list(t(Iu$artistname %>% unique()))
  fwrite(artists.line, 
          paste0(address,"data/LFM.byUser.txt"),
          row.names = FALSE, col.names = FALSE, sep = " ", quote = FALSE, append = TRUE)
}

########################## DATA READ ##########################

LFM <- fread(paste0(address,"data/LFM-treated.txt"),
             sep = ";",
             verbose = TRUE,
             na.strings = "")
LFM = as.data.frame(LFM)
LFM$artistname = str_replace_all(LFM$artistname, " ", "_")

########################## USER'S HISTORY ##########################

# Ordering - UID - Timestamp
LFM = LFM[order(LFM$userid, LFM$timestamp),]

users = unique(LFM$userid)

ptm <- proc.time()
lapply(users,rewrite.lfm)
time.elapsed = proc.time() - ptm
time.elapsed
