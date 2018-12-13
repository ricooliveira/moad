library(readr)

cenarios.decimal = function(cenarios){
  vec2dec = function(vec){
    vec = vec[-1]
    Reduce(function(x,y) x*2+y, vec)  
  }
  cenarios$scenario = cenarios %>% apply(1, vec2dec)
  return(cenarios)
}

setwd("/local/datasets/experimento/results_word2vec")

scen1 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario1.txt", sep = ",", na.strings = "", header = F)
scen2 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario2.txt", sep = ",", na.strings = "", header = F)
scen3 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario3.txt", sep = ",", na.strings = "", header = F)
scen4 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario4.txt", sep = ",", na.strings = "", header = F)
scen5 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario5.txt", sep = ",", na.strings = "", header = F)
scen6 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario6.txt", sep = ",", na.strings = "", header = F)
scen7 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario7.txt", sep = ",", na.strings = "", header = F)
scen8 = fread("sample1000.nsga.fixed.top10.pop10.gen20.scenario8.txt", sep = ",", na.strings = "", header = F)

scen1$scenario = 0
scen2$scenario = 1
scen3$scenario = 2
scen4$scenario = 3
scen5$scenario = 4
scen6$scenario = 5
scen7$scenario = 6
scen8$scenario = 7

setwd("/local/Scripts/word2vec/MOAD")
results.scenarios = rbind(scen1, scen2, scen3, scen4, scen5, scen6, scen7, scen8)
colnames(results.scenarios) = c("users", "artistname", "scenario")
results.scenarios$users = as.numeric(results.scenarios$users)
fwrite(results.scenarios,
       "data/sample1000.nsga.fixed.top10.pop10.gen20.allscenarios.txt",
       row.names = FALSE, col.names = FALSE,
       sep = ",")

# Test set
LFM.test = fread("/local/datasets/experimento/LFM_test.txt", sep = "\t", na.strings = "", header = T)
LFM.test = LFM.test[,c(3,10)] %>% unique()
colnames(LFM.test) = c("users", "artistname")

# Algorithm results
filename = "user.propensity.word2vec.diffentropy.gaussian.half"
user.propensity.w2v = fread(paste0("/local/Scripts/word2vec/MOAD/data/", filename, ".txt"), sep = "\t", na.strings = "")
colnames(user.propensity.w2v) = c("users", "contemporaneity", "locality", "genre")
#colnames(user.propensity.w2v) = c("users", "scenario")
user.propensity.w2v = cenarios.decimal(user.propensity.w2v)

results.w2v = inner_join(user.propensity.w2v, results.scenarios, by = c("users", "scenario")) %>% select(users, artistname, scenario)

# 1-call
one.call = inner_join(results.w2v, LFM.test, by = c("users", "artistname")) %>% select(users) %>% unique()
