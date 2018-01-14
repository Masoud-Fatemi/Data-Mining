getCenters <- function(data){
  centers <- c(mean(data)-sd(data), mean(data), mean(data)+sd(data))
  return(centers)
}
getMedium <- function(data){
  centers <- getCenters(data)
  medium <- c(centers[2]-sd(data)/2-mean(data)*0.1, centers[2]+sd(data)/2+mean(data)*0.1)
  return(medium)
}
getHigh<-function(data){
  centers <- getCenters(data)
  high <- c(centers[3]-sd(data)/2-mean(data)*0.1, max(data))
  return(high)
}
getLow<- function(data){
  centers <- getCenters(data)
  low <- c(min(data), centers[1]+sd(data)/2+mean(data)*0.1)
  return(low)
}
makefuzzyset<- function(data) 
{
  sets <- list(c(getLow(data)), c(getMedium(data)), c(getHigh(data)))
  class(sets) <- "fuzzyset"
  return(sets)
}
generateAllset <- function(alldata) {
  i <- 1
  allsets <- list()
  while(i <= ncol(alldata)) {
    col <- alldata[,i]
    sets <- makefuzzyset(col)
    allsets[[i]] <- sets
    i <- i + 1
  }
  return(allsets)
}
getIndividualMem <- function(x, sets) {
  membership <- c(0)                                            #indicate the membership vector
  length(membership) <- length(sets)
  k <- 1
  while(k <= length(membership)){
    membership[k] <- 0
    k <- k + 1
  }
  if(x >= sets[[length(sets)-1]][2]) {                         #check if x is a full member only of the last set
    membership[length(membership)] <- 1                      #if yes, save the number of the set and a membership of 1
  }
  else {                                                       #if it is not a full member of the last set
    j <- 1 
    while(j <= length(sets)) {
      if(x >= sets[[j]][1] && x < sets[[j]][2]) {          #check if x lies between two sets, i.e. shows partial membership
        if(x > sets[[j+1]][1]) {
          overlap <- sets[[j]][2] - sets[[j+1]][1]
          ma <- (sets[[j]][2]-x) / overlap             #compute the membership values
          mb <- (x-sets[[j+1]][1]) / overlap
          membership[j] <- ma                          #add the values to the vector
          membership[j+1] <- mb
          break
        }
        else {
          membership[j] <- 1                           #if it is a member of only one set, add a membership of 1
          break
        }
      }
      j <- j + 1
    }
  }
  return(membership)
}
GetMem <- function(data, sets){
  m <- matrix(data=0, nrow=length(data), ncol=length(sets))
  i <- 1
  while(i <= length(data)){                                   #cycle through datapoints
    m[i,] <- getIndividualMem(data[i], sets)            #add the computed membership values to the matrix
    i <- i + 1
  }
  return(m)
}
generateMinMtrx <- function(alldata, allsets){
  i <- 1
  while(i <= length(allsets)) {
    memmatrix <- GetMem(alldata[,i], allsets[[i]])
    if(i == 1) {
      m <- memmatrix
    } else {
      m <- cbind(m, memmatrix)                              #add the matrices together
    }
    i <- i + 1
  }
  return(m)  
}	

MtrixIndex <- function(alldata, allsets) {
  names <- colnames(alldata)
  i <- 1
  ind <- 1
  namelist <- list()
  while(i <= length(names)){
    if(i != 1) {
      ind <- ind + length(allsets[[i]])
    }
    namelist[[i]] <- c(names[i], ind)        #save the original name and the starting place in the new data
    i <- i + 1
  }
  return(namelist)
}
gensupp <- function(itemset, data, relative=FALSE) {
  i <- 1
  accsup <- 0
  while(i <= nrow(data)){
    thisrowvalues <- c()
    j <- 1
    while(j <= length(itemset)) {
      thisrowvalues <- c(thisrowvalues, data[i, itemset[j]])    #get the values of the items in the itemsets from one row
      j <- j + 1
    }
    accsup <- accsup + min(thisrowvalues)                        #add the minimum of th values to the support
    i <- i + 1
  }
  if(relative == TRUE) {
    accsup <- accsup/nrow(data)
  }
  return(accsup)
}
getRule <- function(lhs, rhs) {
  rule <- list(ant = lhs, con = rhs)
}
genConf <- function(rule, data) {
  i <- 1
  accconf1 <- 0
  accconf2 <- 0
  while(i <= nrow(data)){
    antvalues <- c()
    convalues <- c()
    j <- 1
    while(j <= length(rule$ant)){
      antvalues <- c(antvalues, data[i,rule$ant[j]])    #get values of items in the antecendent
      j <- j + 1
    }
    k <- 1
    while(k <= length(rule$con)){
      convalues <- c(convalues, data[i,rule$con[k]])    #get values of items in the consequent
      k <- k + 1
    }
    accconf1 <- accconf1 + min(antvalues, convalues)      #accumulate the values for later computation
    accconf2 <- accconf2 + min(antvalues)
    i <- i + 1
  }
  conf <- accconf1 / accconf2                               #compute fuzzy confidence
  return(conf)
}
SingleItemSupp <- function(data) {
  data.frame(place = 1:ncol(data), support = colSums(data)/nrow(data))  #save original place and support in a data frame
}
ProcessedIndex <- function(supp, minsup){
  
  ## min support
  take <- supp[,"support"]>minsup                             #choose the items with support greater than the minimum support
  supp <- supp[take,]
  
  ## sort
  supp <- supp[order(supp[,"support"], decreasing = TRUE),]   #sort by the support value, descending
  
  return(supp)
  
}
preProcessFP <- function(data, minsup) {
  
  supp <- SingleItemSupp(data)
  sortsupp <- ProcessedIndex(supp, minsup)
  
  data <- data[,sortsupp[,"place"]]
  
  data
}
genFPTree <- function(data){
  
  #generate root elements!
  tree <- matrix(data=0, ncol=3, nrow=ncol(data))
  ind <- 1
  while(ind<=ncol(data)){
    tree[ind,1] <- ind
    ind<-ind+1
  }
  
  
  i<-1
  while(i<=nrow(data)){                                               #cycle tuples of database
    j <- 1
    parent <- 0
    
    while(j<=ncol(data)){                                           #cycle through columns of one tuple
      #generate set of items that are not 0 in one row
      itemset <- c()
      u <- 1
      while(u<=ncol(data)){
        if(data[i,u]!=0){itemset<-c(itemset, data[i,u])}
        u <- u+1
      }
      
      if(length(itemset)<=1){break}
      
      rowvect<-c()                                                 #adds all elements that are not 0
      
      if(data[i,j]!=0){
        rowvect<-c(rowvect,data[i,j])
        
        #makes root elements entries
        if (parent == 0){
          parent <- j
          tree[j,3] <- tree[j,3]+min(rowvect)
        } else {
          existence <- 0
          n<-1
          while(n<=nrow(tree)){                              #cycle through tree nodes
            if(tree[n,1] == j && tree[n,2] == parent) {    #the node exists already: search, if node already exists (by column and parent)
              existence <- 1
              tree[n,3]<-tree[n,3]+min(rowvect)
              parent <- n
              break
            }
            n<-n+1
          }
          
          if(existence == 0) {                                #if no, create a new node
            tree <- rbind(tree, c(j, parent, min(rowvect)))
            parent <- nrow(tree)
          }
        }
      }
      j <- j+1
    }
    i <- i+1
  }
  return(tree)
}
powerSet <- function(x){
  if(length(x) == 0) return(vector(mode(x), 0))
  x <- sort(unique(x))
  K <- NULL
  for(m in x) K <- rbind(cbind(K, FALSE), cbind(K, TRUE))
  out <- apply(K, 1, function(x, s) s[x], s=x)[-1]
  names(out) <- NULL
  return(out) 
}
FPTreeExtrct <- function(tree, minsup){
  
  numlevels <- max(tree[,1])
  freqsets <- list()
  
  #generate possible itemsets
  level <- numlevels
  while(level > 0){                                          #cycle through levels
    
    newsets <- FPGrowth(tree, minsup, level, 0)            #call FPGrowth function for each level
    level <- level - 1
    m <- 1
    while(m <= length(newsets)){                           #fit together freqsets and newsets
      freqsets[[length(freqsets)+1]] <- newsets[[m]]
      m <- m + 1
    }
  }
  
  return(freqsets)
}
verifySupp <- function(itemsets, data, minsup) {
  i <- 1
  newsets <- list()
  while(i <= length(itemsets)){
    sup <- gensupp(itemsets[[i]][[1]], data)
    if(sup >= minsup){
      set <- itemsets[[i]]
      set[[2]] <- sup
      class(set) <- "itemset"
      newsets[[length(newsets)+1]] <- set
    }
    i <- i + 1
  }
  return(newsets)
}
makeRule <- function(freqsets){
  rules <- list()
  i <- 1
  while(i <= length(freqsets)){
    j <- 1
    while(j <= length(freqsets[[i]][[1]])){
      con <- freqsets[[i]][[1]][j]                    #take every item as a consequent once, the rest is the antecedent
      ant <- c()
      
      k <- 1
      while(k <= length(freqsets[[i]][[1]])){
        if(freqsets[[i]][[1]][k] != con){
          ant <- c(ant, freqsets[[i]][[1]][k])
        }
        k <- k + 1
      }
      rule <- getRule(ant, con)
      rule$sup <- freqsets[[i]][[2]]
      rules[[length(rules) +1 ]] <- rule
      j <- j + 1
    }
    i <- i + 1
  }
  return(rules)
}
evaluateRules <- function(rules, data, minconf) {
  i <- 1
  evalrules <- list()
  while(i <= length(rules)){
    singlerule <- rules[[i]]
    conf <- genConf(singlerule, data)
    if(conf >= minconf){
      singlerule$conf <- conf
      class(singlerule) <- "rule"
      evalrules[[length(evalrules)+1]] <- singlerule
    }
    i <- i + 1
  }
  return(evalrules)
}