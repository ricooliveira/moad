if (!require("dplyr")) {
  install.packages("dplyr", repos="http://cran.rstudio.com/") 
}
if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
}
if (!require("nsga2R")) {
  install.packages("nsga2R", repos="http://cran.rstudio.com/") 
}
if (!require("reshape")) {
  install.packages("reshape", repos="http://cran.rstudio.com/") 
}
if (!require("lsa")) {
  install.packages("lsa", repos="http://cran.rstudio.com/") 
}

library(dplyr)
library(data.table)
library(nsga2R)
library(reshape)
library(lsa)

########################## CONSTANTS ##########################

ALPHA = 0.1
BETA = 0.1
GAMMA = 0.1
ETA = 0.1
K = 8 # Number of scenarios
TRIALS = 10 
S = 5 # Number of days in user history

TOPN = 10
TOPN_RERANK = 50
address <- "/local/Scripts/moad/"

# aspects setup
# aspects: 1 = "Contemporaneity", 2 = "Locality", 3 = "Genre")
#for(aspect.index in 1:4){
aspects.all = c(1,2,3)
aspects.not.to.diversify = c(3)
aspects.to.diversify = setdiff(aspects.all,aspects.not.to.diversify)

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
  centroid = genre.centroids[which(genre.centroids$`user-id` == user),]
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
    childAfterX <- floor(childAfterX)  # Added here
    cat("mutation operator")
    cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    childAfterM <- floor(childAfterM)
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

########################## DATA LOAD ############################

artist.data <- fread(paste0(address,"data/artist.data.txt"), 
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

data.train <- fread(paste0(address,"data/LFM_train.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.train = as.data.frame(data.train)

data.test <- fread(paste0(address,"data/LFM_test.txt"), 
                   sep = "\t", 
                   verbose = TRUE,
                   header = TRUE)
data.test = as.data.frame(data.test)

genre.centroids <- fread(paste0(address,"data/user.history.centroids.txt"), 
                         sep = ",", 
                         verbose = TRUE,
                         header = TRUE)
genre.centroids = as.data.frame(genre.centroids)

#################### Calculate frequencies for IOF for type and Locality  ####################

by.area = group_by(artist.data,area)
freq.area = dplyr::summarise(by.area, total = n())

################################ OBJECTIVE FUNCTION ################################

# This function will be executed for a user, with specific data.test, specified aspects to diversify and a determined 
# list size (TOPN)

multi.objective = function(list.elements.index){
  size = length(list.elements.index)
  list.elements = as.data.frame(artist.data.new[list.elements.index,"Artist"])
  names(list.elements) = "artist"
  n = length(aspects.to.diversify)
  df = artist.data.new[artist.data.new$Artist %in% list.elements$artist,]
  
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

########################### NSGA - MOAD + ShiftBand ##################################

#ShiftBand implemented as seen in
# AUER, Peter. Using confidence bounds for exploitation-exploration trade-offs. Journal of Machine Learning Research, v. 3, n. Nov, p. 397-422, 2002.
moad = function(user){

  # test
  # user = 7687
  
  random.probability.sort = function(p){
    total = sum(p)
    target = runif(1, 0, total)
    i = 1
    while(total - p[i] > target){
      total = total - p[i]
      i = i + 1
    }
    return(i)
  }

  set.probability = function(i, w, W){
    return((1 - GAMMA) * w[i] / W + GAMMA / K)
  }
  
  set.weights = function(i, w, W, p, x.exp){
    return(w[i] * exp(ETA * (x.exp[i] + ALPHA / (p[i] * sqrt(TRIALS * K / S))))) # S = number of days)
  }
  
  # setup ShiftBand
  w.diversity = rep(1,K)
  w.affinity = rep(1,K)
  
  #setup moad
  N = nrow(artist.data)
  artist.data.listenned = unique(data.train[which(data.train$`user-id` == user),]$`artist-name`)
  artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
  #ubcf.index = which(artist.data.new$Artist %in% ubcf.top10[which(ubcf.top10$user == user),"artist"])
  #td.index = which(artist.data.new$Artist %in% as.data.frame(td.top10[which(td.top10$user == user),"artist"])[1:10,]) # adapted to eliminate duplicated recommendation
  
  days.history = data.train[which(data.train$`user-id` == user),]
  days.history$timestamp = days.history$timestamp / (60*60*24) # timestamp in seconds switched to days
  days.history$timestamp = floor(days.history$timestamp)
  days.history.unique = days.history$timestamp %>% unique()
  S = length(days.history.unique) # Set S as the number of days
  TRIALS = S # considering each day as a trial, and also as a segment with different propension (bandit)
  
  one.day.moad = function(t){
  
    # test
    # t = days.history.unique[1]
    
    artist.data.listenned.dayt = days.history[days.history$timestamp == t,]
    data.train.user = artist.data[artist.data$Artist %in% artist.data.listenned.dayt$`artist-name`,]  
    
    # repeat for the user
    W.diversity = sum(w.diversity)
    W.affinity = sum(w.affinity)
    p.diversity = lapply(c(1:K), set.probability, w.diversity, W.diversity) %>% as.numeric()
    p.affinity = lapply(c(1:K), set.probability, w.affinity, W.affinity) %>% as.numeric()

    scenario = random.probability.sort((p.diversity + p.affinity)/2) # selects scenarios considering ShiftBand probabilities
    
    # determine aspects to diversify or not depending on the scenario selection
    aspects.all = c(1,2,3)
    aspects.to.diversify = c()
    if(scenario >= 4){
      aspects.to.diversify = c(aspects.to.diversify,1)
    }
    if(scenario == 3 | scenario == 4 | scenario == 7 | scenario == 8){
      aspects.to.diversify = c(aspects.to.diversify,2)
    }
    if(scenario %% 2 == 0){
      aspects.to.diversify = c(aspects.to.diversify,3)
    }
    
    aspects.not.to.diversify = setdiff(aspects.all,aspects.to.diversify)
    
    # Run NSGA to collect reward 
    results <- nsga2R.altered.random(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                                     upperBounds=rep(nrow(artist.data.new),TOPN), popSize=10, tourSize=2,
                                     generations=20, cprob=0.9, XoverDistIdx=20, mprob=0.1, MuDistIdx=10)
    
    nondominated = (fastNonDominatedSorting.altered(results$objectives))[[1]]
    best.solution = which.max(results$objectives[nondominated,1]+results$objectives[nondominated,2])
    
    if(length(nondominated) == 1){
      df = as.data.frame(artist.data.new[as.vector(t(as.data.frame(t(results$parameters[nondominated,]))[best.solution,])),"Artist"])
    }else{
      df = as.data.frame(artist.data.new[results$parameters[nondominated,][best.solution,],"Artist"])
    }
    df = bind_cols(as.data.frame(rep(user,TOPN)),
                   as.data.frame(rep(t,TOPN)),
                   as.data.frame(rep(scenario,TOPN)),
                   as.data.frame(rep(results$objectives[best.solution,1],TOPN)),
                   as.data.frame(rep(results$objectives[best.solution,2],TOPN)),
                   df)
    fwrite(df,
           paste0(address,"results/sample1000.nsga.shiftband.top10.pop10.gen20-exec.txt"), #replace unique for args[1] when parallelizing
           col.names = FALSE, row.names = FALSE, quote = TRUE, append = TRUE)
    
    xit.diversity = results$objectives[best.solution, 1]
    xit.affinity = results$objectives[best.solution, 2]
    
    # Ending the ShiftBand algorithm
    x.diversity.exp = rep(0,K)
    x.diversity.exp[scenario] = xit.diversity / p.diversity[scenario]
    x.affinity.exp = rep(0,K)
    x.affinity.exp[scenario] = xit.affinity / p.affinity[scenario]
    
    w.diversity = lapply(c(1:K), set.weights, w.diversity, W.diversity, p.diversity, x.diversity.exp) %>% as.numeric()
    w.affinity = lapply(c(1:K), set.weights, w.affinity, W.affinity, p.affinity, x.affinity.exp) %>% as.numeric()
    
  }
  lapply(days.history.unique, one.day.moad)
}

users = data.train$`user-id` %>% unique()
lapply(users, moad)
