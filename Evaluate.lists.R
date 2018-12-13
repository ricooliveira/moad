library(dplyr)
library(data.table)
library(lsa)
library(nsga2R)
library(reshape)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
address <- "/local/"

########################################## Metric Functions ##########################################

# Intra List Diversity Metric (ILD)

similarity.function.genre = function(data){
  return(cosine(t(as.matrix(data))))
}

similarity.function.locality = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(data[i,"area"] == data[j,"area"]){
        m[i,j] = 1
      } else {  
        m[i,j] = 1 / (1 + log10(N/(as.numeric(freq.area[which(data[i,"area"] == freq.area$area),"total"]))) 
                      * log10(N/(as.numeric(freq.area[which(data[j,"area"] == freq.area$area),"total"]))))
      }
    }
  }
  return(m)
}

similarity.function.contemporaneity = function(data){
  artistas = rownames(data)
  data$artista = artistas
  artist_simi = combn(artistas, 2) %>% t() %>% as_data_frame()
  
  sim_between_artists = function(x, data){
    a = x[1]
    b = x[2]
    a_data =  data %>% filter(artista %in% a)
    b_data =  data %>% filter(artista %in% b)
    
    pc = min(a_data$debut, b_data$debut)
    ut = max(a_data$last, b_data$last)
    
    uc = max(a_data$debut, b_data$debut)
    pt = min(a_data$last, b_data$last)
    
    if (ut == pc)
      return(1)
    
    sim = (pt - uc)/(ut - pc)
    return((sim+1)/2)
  }
  
  sim = apply(artist_simi, 1, sim_between_artists, data)
  artist_simi$sim = sim
  artist_simi_replicated = artist_simi %>% select(V1=V2, V2=V1, sim)
  artists_identity = data.frame(V1 = artistas, V2 = artistas, sim = 1)
  artist_simi = bind_rows(artist_simi, artist_simi_replicated, artists_identity)
  
  matrix = artist_simi %>% cast(V1~V2, mean, value = "sim") %>% select(-V1)
  
  matrix = matrix %>% as.matrix()
  return(matrix)
}

ILD = function(data, similarity.function){
  k=nrow(data)
  if(k > 1){
    sum.dissimilarity = 0
    similarity.matrix = similarity.function(data)
    dissimilarity.matrix = 1 - similarity.matrix
    sum.dissimilarity = sum(colSums(dissimilarity.matrix))
    return(sum.dissimilarity/(k*(k-1)))
  }else{
    return(0)
  }
}

# Distance List History Metric (DLH)

distance.function.genre = function(data, history){
  centroid = genre.centroids[which(genre.centroids$`user-id` == u),]
  centroid$`user-id` = NULL
  distance = 0
  for(i in 1:nrow(data)){
    distance = distance + cosine(as.vector(t(data[i,])),as.vector(t(centroid)))
  }
  return(distance/nrow(data))
}

distance.function.locality = function(data, history){
  rec <- as.vector(t(data))
  h <- as.vector(t(history))
  
  rec.g <- sapply(rec, function(x) {freq.area[which(x == freq.area$area), ]$total})
  h.g <- sapply(h, function(x) {freq.area[which(x == freq.area$area), ]$total})
  
  f <- function(i, j) {
    ifelse(i == j, 1, 1 / (1 + log10(N/i) * log10(N/j)))
  }
  
  r <- outer(rec.g, h.g, f)
  return(mean(r))
}

distance.function.contemporaneity = function(data, history){
  
  n = nrow(data)
  m = nrow(history)
  data$index = c(1:n)
  history$index = c(1:m)
  # artist_simi = combn(artistas, 2) %>% t() %>% as_data_frame()
  
  sim_between_artists_history = function(x, data, history){
    a = x[1]
    b = x[2]
    a_data =  data %>% filter(index %in% a)
    b_history =  history %>% filter(index %in% b)
    
    pc = min(a_data$debut, b_history$debut)
    ut = max(a_data$last, b_history$last)
    
    uc = max(a_data$debut, b_history$debut)
    pt = min(a_data$last, b_history$last)
    
    if (ut == pc)
      return(1)
    
    sim = (pt - uc)/(ut - pc)
    
    return((sim+1)/2)
  }
  
  sum.contemporaneity = 0
  for(i in 1:n){
    for(j in 1:m){
      sum.contemporaneity = sum.contemporaneity + sim_between_artists_history(c(i,j), data, history)
    }
  }
  return(sum.contemporaneity / (m * n))
}

DLH = function(data, history, distance.function){
  return(distance.function(data, history))
}

# Pearson correlation

pearson.correlation = function(candidate,list.ubcf){
  soma = 0
  for(i in 1:nrow(list.ubcf)){
    soma = soma + cor.test(as.vector(list.ubcf[i,],mode = "numeric"),as.vector(candidate,mode = "numeric"),method = "pearson")$estimate
  }
  return(soma/nrow(list.ubcf))
}

########################################## NSGA2R Altered Functions ##########################################

nsga2R.altered = function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
                           upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
                           generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
                           MuDistIdx = 10, ubcf.index, td.index) 
{
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  cat("initializing the population")
  cat("\n")
  parent <- t(sapply(1:(popSize - 2), function(u) array(runif(length(lowerBounds), 
                                                              lowerBounds, upperBounds))))
  parent <- floor(parent)
  parent = rbind(ubcf.index, td.index, parent)
  parent <- cbind(parent, t(apply(parent, 1, fn)))
  cat("ranking the initial population")
  cat("\n")
  ranking <- fastNonDominatedSorting.altered(parent[, (varNo + 1):(varNo + 
                                                                     objDim)])
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  cat("crowding distance calculation")
  cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    cat("---------------generation---------------", iter, 
        "starts")
    cat("\n")
    cat("tournament selection")
    cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    cat("crossover operator")
    cat("\n")
    childAfterX <- boundedSBXover.altered(matingPool[, 1:varNo], 
                                          lowerBounds, upperBounds, cprob, XoverDistIdx)
    cat("mutation operator")
    cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    cat("evaluate the objective fns of childAfterM")
    cat("\n")
    childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                              1, fn)))
    cat("Rt = Pt + Qt")
    cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    cat("ranking again")
    cat("\n")
    ranking <- fastNonDominatedSorting.altered(parentNext[, (varNo + 
                                                               1):(varNo + objDim)])
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    cat("crowded comparison again")
    cat("\n")
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
                                  ]
    cat("environmental selection")
    cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    cat("---------------generation---------------", iter, 
        "ends")
    cat("\n")
    if (iter != generations) {
      cat("\n")
      cat("********** new iteration *********")
      cat("\n")
    }
    else {
      cat("********** stop the evolution *********")
      cat("\n")
    }
  }
  result = list(functions = fn, parameterDim = varNo, objectiveDim = objDim, 
                lowerBounds = lowerBounds, upperBounds = upperBounds, 
                popSize = popSize, tournamentSize = tourSize, generations = generations, 
                XoverProb = cprob, XoverDistIndex = XoverDistIdx, mutationProb = mprob, 
                mutationDistIndex = MuDistIdx, parameters = parent[, 
                                                                   1:varNo], objectives = parent[, (varNo + 1):(varNo + 
                                                                                                                  objDim)], paretoFrontRank = parent[, varNo + objDim + 
                                                                                                                                                       1], crowdingDistance = parent[, varNo + objDim + 
                                                                                                                                                                                       2])
  class(result) = "nsga2R"
  return(result)
}

nsga2R.altered.random = function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
                                  upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
                                  generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
                                  MuDistIdx = 10) 
{
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  cat("initializing the population")
  cat("\n")
  parent <- t(sapply(1:(popSize), function(u) array(runif(length(lowerBounds), 
                                                          lowerBounds, upperBounds))))
  parent <- floor(parent)
  #parent = rbind(ubcf.index, td.index, parent)
  parent <- cbind(parent, t(apply(parent, 1, fn)))
  cat("ranking the initial population")
  cat("\n")
  ranking <- fastNonDominatedSorting.altered(parent[, (varNo + 1):(varNo + 
                                                                     objDim)])
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  cat("crowding distance calculation")
  cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    cat("---------------generation---------------", iter, 
        "starts")
    cat("\n")
    cat("tournament selection")
    cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    cat("crossover operator")
    cat("\n")
    childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    cat("mutation operator")
    cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    cat("evaluate the objective fns of childAfterM")
    cat("\n")
    childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                              1, fn)))
    cat("Rt = Pt + Qt")
    cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    cat("ranking again")
    cat("\n")
    ranking <- fastNonDominatedSorting.altered(parentNext[, (varNo + 
                                                               1):(varNo + objDim)])
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    cat("crowded comparison again")
    cat("\n")
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
                                  ]
    cat("environmental selection")
    cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    cat("---------------generation---------------", iter, 
        "ends")
    cat("\n")
    if (iter != generations) {
      cat("\n")
      cat("********** new iteration *********")
      cat("\n")
    }
    else {
      cat("********** stop the evolution *********")
      cat("\n")
    }
  }
  result = list(functions = fn, parameterDim = varNo, objectiveDim = objDim, 
                lowerBounds = lowerBounds, upperBounds = upperBounds, 
                popSize = popSize, tournamentSize = tourSize, generations = generations, 
                XoverProb = cprob, XoverDistIndex = XoverDistIdx, mutationProb = mprob, 
                mutationDistIndex = MuDistIdx, parameters = parent[, 
                                                                   1:varNo], objectives = parent[, (varNo + 1):(varNo + 
                                                                                                                  objDim)], paretoFrontRank = parent[, varNo + objDim + 
                                                                                                                                                       1], crowdingDistance = parent[, varNo + objDim + 
                                                                                                                                                                                       2])
  class(result) = "nsga2R"
  return(result)
}

fastNonDominatedSorting.altered = function (inputData) 
{
  popSize = nrow(inputData)
  idxDominators = vector("list", popSize)
  idxDominatees = vector("list", popSize)
  for (i in 1:(popSize - 1)) {
    for (j in i:popSize) {
      if (i != j) {
        xi = inputData[i, ]
        xj = inputData[j, ]
        if (all(xi >= xj) && any(xi > xj)) {
          idxDominators[[j]] = c(idxDominators[[j]], 
                                 i)
          idxDominatees[[i]] = c(idxDominatees[[i]], 
                                 j)
        }
        else if (all(xj >= xi) && any(xj > xi)) {
          idxDominators[[i]] = c(idxDominators[[i]], 
                                 j)
          idxDominatees[[j]] = c(idxDominatees[[j]], 
                                 i)
        }
      }
    }
  }
  noDominators <- lapply(idxDominators, length)
  rnkList <- list()
  rnkList <- c(rnkList, list(which(noDominators == 0)))
  solAssigned <- c()
  solAssigned <- c(solAssigned, length(which(noDominators == 
                                               0)))
  while (sum(solAssigned) < popSize) {
    Q <- c()
    noSolInCurrFrnt <- solAssigned[length(solAssigned)]
    for (i in 1:noSolInCurrFrnt) {
      solIdx <- rnkList[[length(rnkList)]][i]
      hisDominatees <- idxDominatees[[solIdx]]
      for (i in hisDominatees) {
        noDominators[[i]] <- noDominators[[i]] - 1
        if (noDominators[[i]] == 0) {
          Q <- c(Q, i)
        }
      }
    }
    rnkList <- c(rnkList, list(sort(Q)))
    solAssigned <- c(solAssigned, length(Q))
  }
  return(rnkList)
}

boundedSBXover.altered =function (parent_chromosome, lowerBounds, upperBounds, cprob, 
                                  mu) 
{
  popSize = nrow(parent_chromosome)
  varNo = ncol(parent_chromosome)
  child <- parent_chromosome
  p <- 1
  for (i in 1:(popSize/2)) {
    if (runif(1) < cprob) {
      for (j in 1:varNo) {
        parent1 <- child[p, j]
        parent2 <- child[p + 1, j]
        yl <- lowerBounds[j]
        yu <- upperBounds[j]
        rnd = runif(1)
        if (rnd <= 0.5) {
          if (abs(parent1 - parent2) > 1e-06) {
            if (parent2 > parent1) {
              y2 <- parent2
              y1 <- parent1
            }
            else {
              y2 <- parent1
              y1 <- parent2
            }
            if ((y1 - yl) > (yu - y2)) {
              beta = 1 + (2 * (yu - y2)/(y2 - y1))
            }
            else {
              beta = 1 + (2 * (y1 - yl)/(y2 - y1))
            }
            alpha = 2 - (beta^(-(1 + mu)))
            rnd = runif(1)
            if (rnd <= 1/alpha) {
              alpha = alpha * rnd
              betaq = sign(alpha)*abs(alpha)^(1/(1 + mu))
            }
            else {
              alpha = alpha * rnd
              alpha = 1/(2 - alpha)
              
              betaq = sign(alpha)*abs(alpha)^(1/(1 + mu))
            }
            child1 = 0.5 * ((y1 + y2) - betaq * (y2 - 
                                                   y1))
            child2 = 0.5 * ((y1 + y2) + betaq * (y2 - 
                                                   y1))
          }
          else {
            betaq = 1
            y1 = parent1
            y2 = parent2
            child1 = 0.5 * ((y1 + y2) - betaq * (y2 - 
                                                   y1))
            child2 = 0.5 * ((y1 + y2) + betaq * (y2 - 
                                                   y1))
          }
          teste <- tryCatch({
            if (child1 > yu) {
              child1 = yu
            }
          },
          error = function(err) {
            print(paste("i",i))
            print(paste("j",j))
            print(paste("p",p))
            print(paste("y1",y1))
            print(paste("y2",y2))
            print(paste("rnd",rnd))
            print(paste("alpha",alpha))
            print(paste("betaq",betaq))
            print(parent_chromosome)
            print(child)
            #fwrite(parent_chromosome,"~/Documentos/experimento_doutorado/teste.txt",row.names = F,col.names = F)
          }
          )
          # if (is.na(child1)){
          #   print("breakpoint")
          # }
          if (child1 > yu) {
            print("aqui")
            child1 = yu
          }
          else if (child1 < yl) {
            child1 = yl
          }
          if (child2 > yu) {
            child2 = yu
          }
          else if (child2 < yl) {
            child2 = yl
          }
        }
        else {
          child1 = parent1
          child2 = parent2
        }
        child[p, j] <- child1
        child[p + 1, j] <- child2
      }
    }
    p <- p + 2
  }
  return(child)
}


########################################## DATA LOAD ##########################################

artist.data <- fread("/local/Scripts/word2vec/MOAD/data/artist.data.txt", 
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

ubcf.top10 <- fread(paste0(address,"datasets/experimento/sample1000.ubcf.top10.csv"), 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top10 = as.data.frame(ubcf.top10)

td.top10 <- fread(paste0(address,"datasets/experimento/sample1000.topic-diversification.top10.txt"),
                  sep = "\t",
                  verbose = TRUE,
                  header = FALSE,
                  col.names = c("user", "artist"))
td.top10 = as.data.frame(td.top10)


data.train <- fread(paste0(address,"datasets/experimento/LFM_train.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.train = as.data.frame(data.train)

data.test <- fread(paste0(address,"datasets/experimento/LFM_test.txt"), 
                   sep = "\t", 
                   verbose = TRUE,
                   header = TRUE)
data.test = as.data.frame(data.test)

genre.centroids <- fread(paste0(address,"datasets/experimento/user.history.centroids.txt"), 
                         sep = ",", 
                         verbose = TRUE,
                         header = TRUE)
genre.centroids = as.data.frame(genre.centroids)

#################### NEW - DAILY DATA TEST  ####################

colnames(data.test)[3] = "users"
colnames(data.test)[10] = "artistname"
data.test$timestamp = data.test$timestamp / (60*60*24) # timestamp em segundos transformado pra dias
data.test.grouped = select(data.test, "users", "timestamp", "artistname") %>% group_by(users)



#################### Calculate frequencies for IOF for Gender and Locality  ####################

by.area = group_by(artist.data,area)
freq.area = dplyr::summarise(by.area, total = n())

########################################## ASPECTS ##########################################

aspects <- vector(mode="list", length=3)
names(aspects) <- c("Contemporaneity", "Locality", "Genre")
aspects[[1]] <- c(5,6); aspects[[2]] <- 4; aspects[[3]] <- 7:ncol(artist.data)
similarity.functions <- vector(mode="list", length=3)
names(similarity.functions) <- c("Contemporaneity", "Locality", "Genre")
similarity.functions[[1]] <- similarity.function.contemporaneity; similarity.functions[[2]] <- similarity.function.locality
similarity.functions[[3]] <- similarity.function.genre

distance.functions <- vector(mode="list", length=3)
names(distance.functions) <- c("Contemporaneity", "Locality", "Genre")
distance.functions[[1]] <- distance.function.contemporaneity; distance.functions[[2]] <- distance.function.locality
distance.functions[[3]] <- distance.function.genre

################################ OBJECTIVE FUNCTION ################################

multi.objective = function(list.elements.index){
  size = length(list.elements.index)
  list.elements = as.data.frame(artist.data[list.elements.index,"Artist"])
  names(list.elements) = "artist"
  n = length(aspects.to.diversify)
  df = artist.data[artist.data$Artist %in% list.elements$artist,]
  sum.ild = 0
  
  for(i in aspects.to.diversify){
    df.aspect = df[(aspects[[i]])]
    sum.ild = sum.ild + ILD(data = df.aspect, similarity.function = similarity.functions[[i]])
  }
  y1 = (sum.ild / n)
  
  sum.dlh = 0
  for(i in aspects.not.to.diversify){
    df.aspect = df[(aspects[[i]])]
    sum.dlh = sum.dlh + DLH(data = df.aspect, history = data.train.user[(aspects[[i]])], distance.function = distance.functions[[i]])
  }
  y2 = (sum.dlh / length(aspects.not.to.diversify))
  
  return(c(y1, y2))
}

############################### Generate objective metrics ###############################

users.propensity <- fread("/local/Scripts/word2vec/MOAD/data/user.propensity.word2vec.txt", 
                          sep = "\t", 
                          header = TRUE)
users.propensity = as.data.frame(users.propensity)

#users.propensity = user.propensity

archive.name = "sample1000.nsga.fixed.top10.pop10.gen20.scenario4.txt"
results <- fread(paste0("/local/datasets/experimento/results_word2vec/",archive.name), 
                     sep = ",", 
                     verbose = TRUE,
                     header = FALSE)
results = as.data.frame(results)
names(results) = c("user","artist")

# results = results[-which(results$artist == ""),]

position = unlist(lapply(results$artist, function(x) which(artist.data$Artist == x)))
#teste = c(do.call("cbind",position))
results = cbind(results,position)

# results.grouped = group_by(results,user)
# results.summary = summarise(results.grouped, multi.objective(results.grouped$position))

start.time <- Sys.time()

aspects.all = c(1,2,3)
aspects.to.diversify = c(2,3)
N = nrow(artist.data)
users = unique(results$user)
user = 0
df.objectives = data.frame(matrix(0L,997,3))
for(u in users){
  user = user + 1; print(user)
  #aspects.to.diversify = users.propensity %>% filter(users %in% u) %>% t() 
  #aspects.to.diversify = which(aspects.to.diversify == 1) -1
  aspects.not.to.diversify = setdiff(aspects.all,aspects.to.diversify)
  
  artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
  artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
  data.train.user = artist.data[artist.data$Artist %in% artist.data.listenned,]
  df = multi.objective(results[results$user == u,]$position)
  df.objectives[user,1] = u; df.objectives[user,2] = df[1]; df.objectives[user,3] = df[2]
}
fwrite(df.objectives,
       paste0("/local/datasets/experimento/results_word2vec/objectives.",archive.name),
       col.names = FALSE, row.names = FALSE, quote = FALSE)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
