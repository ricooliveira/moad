cenarios.decimal = function(cenarios){
  vec2dec = function(vec){
    vec = vec[-1]
    Reduce(function(x,y) x*2+y, vec)  
  }
  cenarios$scenario = cenarios %>% apply(1, vec2dec)
  return(cenarios)
}

precision = function(user, propensity){
  scenario.user = filter(propensity, users == user) %>% select(scenario)
  return(user %in% users.best.scenario[[scenario.user %>% as.numeric() + 1]])
}

filename = "user.propensity.word2vec.diffentropy.daily.gaussian"

propensity = fread(
  paste0("/local/Scripts/word2vec/MOAD/data/", filename, ".txt"), 
  sep = "\t",
  na.strings = "")
names(propensity) = c("users", "contemporaneity", "locality", "genre")
#names(propensity) = c("users", "scenario")

propensity = cenarios.decimal(propensity)

users = propensity$users

result = do.call(sum, lapply(users, precision, propensity))
result
