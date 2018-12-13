library(readr)
library(data.table)
library(dplyr)

cenarios.decimal = function(cenarios){
  vec2dec = function(vec){
    vec = vec[-1]
    Reduce(function(x,y) x*2+y, vec)  
  }
  cenarios$scenario = cenarios %>% apply(1, vec2dec)
  return(cenarios)
}
setwd("/local/datasets/experimento/results_word2vec")

obj.scen1 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario1.txt", sep = ",", na.strings = "")
obj.scen2 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario2.txt", sep = ",", na.strings = "")
obj.scen3 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario3.txt", sep = ",", na.strings = "")
obj.scen4 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario4.txt", sep = ",", na.strings = "")
obj.scen5 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario5.txt", sep = ",", na.strings = "")
obj.scen6 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario6.txt", sep = ",", na.strings = "")
obj.scen7 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario7.txt", sep = ",", na.strings = "")
obj.scen8 = fread("objectives.sample1000.nsga.fixed.top10.pop10.gen20.scenario8.txt", sep = ",", na.strings = "")

obj.scen1$scenario = 0
obj.scen2$scenario = 1
obj.scen3$scenario = 2
obj.scen4$scenario = 3
obj.scen5$scenario = 4
obj.scen6$scenario = 5
obj.scen7$scenario = 6
obj.scen8$scenario = 7

objectives = rbind(obj.scen1, obj.scen2, obj.scen3, obj.scen4, obj.scen5, obj.scen6, obj.scen7, obj.scen8)
colnames(objectives) = c("users", "diversity", "affinity", "scenario")
fwrite(objectives,
       "objectives.sample1000.nsga.fixed.top10.pop10.gen20.allscenarios.txt",
       row.names = FALSE, col.names = FALSE,
       sep = ",")

best.scenario = function(scen, objectives, column){
  obj.scen = filter(objectives, scenario == scen)
  obj.scen = obj.scen[obj.scen[[column]] >= quantile(obj.scen[[column]])[4],]
  return(obj.scen$users)
}



scenarios = c(1:7)
best.diversity = lapply(scenarios, best.scenario, objectives, "diversity")


scenarios = c(0:6)
best.affinity = lapply(scenarios, best.scenario, objectives, "affinity")

users.best.affinity = do.call(c, best.affinity) %>% unique()
users.best.diversity = do.call(c, best.diversity) %>% unique()

users.best.scenario = list(best.affinity[[1]],
                           c(best.affinity[[2]],best.diversity[[1]]) %>% unique(),
                           c(best.affinity[[3]],best.diversity[[2]]) %>% unique(),
                           c(best.affinity[[4]],best.diversity[[3]]) %>% unique(),
                           c(best.affinity[[5]],best.diversity[[4]]) %>% unique(),
                           c(best.affinity[[6]],best.diversity[[5]]) %>% unique(),
                           c(best.affinity[[7]],best.diversity[[6]]) %>% unique(),
                           best.diversity[[7]]
                           )

###################################### Objectives for groups of users ###############################################

filename = "user.propensity.word2vec.diffentropy.daily.gaussian"
user.propensity.w2v = fread(paste0("/local/Scripts/word2vec/MOAD/data/", filename, ".txt"), sep = "\t", na.strings = "")
colnames(user.propensity.w2v) = c("users", "contemporaneity", "locality", "genre")
#colnames(user.propensity.w2v) = c("users", "scenario")
user.propensity.w2v = cenarios.decimal(user.propensity.w2v)

objectives.w2v = inner_join(user.propensity.w2v, objectives, by = c("users", "scenario")) %>% select(users, diversity, affinity, scenario)
fwrite(objectives.w2v, 
       paste0("/local/Scripts/word2vec/MOAD/data/objectives.sample1000.nsga.", filename, ".top10.pop10.gen20.txt"),
       row.names = FALSE, col.names = FALSE,
       sep = ",")

