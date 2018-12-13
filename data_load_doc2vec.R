library(readr)
library(data.table)
library(dplyr)

########################## CONSTANTS ##########################

address <- "~/Documentos/experimento_doutorado/datasets/"
size_sample = 1000

###############################################################################
################################# LFM Load ####################################
###############################################################################

######################### USERS #########################

LFM_1b_users <- read_delim(paste0(address,"LFM/LFM-1b_users.txt"), 
                           "\t", escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE)

names(LFM_1b_users) = c("userid", "country", "age", "gender", "playcount", "registered timestamp")

nUsers = length(unique(LFM_1b_users$userid))

sample_indexes = floor(runif(size_sample, min=1, max=nUsers+1))

sample = LFM_1b_users[sample_indexes,"userid"]

sample = sort(sample$userid)

write.table(sample, paste0(address,"LFM/sample_users.txt"), sep = "\t", col.names = FALSE, row.names = FALSE)

######################### LEs #########################

# Geração do arquivo LEs_sample.txt com as interações dos usuários contidos no sample realizado em linha de comando linux
# awk -F'\t' 'NR==FNR{check[$0];next} $1 in check' sample_users.txt LFM-1b_LEs.txt > LEs_sample.txt

LEs_sample <- read_delim(paste0(address,"LFM/LEs_sample.txt"), 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

names(LEs_sample) = c("userid", "artistid", "albumid", "trackid", "timestamp")

LFM = merge(LEs_sample, LFM_1b_users, by = "userid")

######################### ARTISTS #########################

LFM_1b_artists <- read_delim(paste0(address,"LFM/LFM-1b_artists.txt"), 
                             "\t", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
names(LFM_1b_artists) = c("artistid", "artistname")

LFM = merge(LFM, LFM_1b_artists, by = "artistid")

######################### CLEANING & WRITING #########################

LFM$`track-id` = NULL

fwrite(LFM, "/local/Scripts/word2vec/MOAD/data/LFM-treated.txt",row.names = FALSE, col.names = TRUE, sep = ";")

