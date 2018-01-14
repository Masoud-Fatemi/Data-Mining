trace <- function(rules, index, preind){
  newrules <- list()
  newrule<- list()
  i <- 1
  while(i <= length(rules)){
    j <- 1
    tempant <- c()
    while(j <= length(rules[[i]]$ant)){
      
      fuzzyset <- preind[rules[[i]]$ant[j],1]
      n <- 1
      while(n <= length(index)){
        if(n < length(index)) {
          if(fuzzyset >= as.numeric(index[[n]][2]) && fuzzyset < as.numeric(index[[n+1]][2])){
            origcol <- fuzzyset - as.numeric(index[[n]][2]) + 1
            tempant <- c(tempant, paste(index[[n]][1], origcol))
            break
          }
        }
        if(n == length(index)){
          origcol <- fuzzyset - as.numeric(index[[n]][2]) + 1
          tempant <- c(tempant, paste(index[[n]][1], origcol))
        }
        n <- n + 1
      }
      j <- j + 1
    }
    k <- 1
    tempcon <- c()
    while(k <= length(rules[[i]]$con)){
      fuzzyset <- preind[rules[[i]]$con[k],1]
      n <- 1
      while(n <= length(index)){
        if(n < length(index)) {
          if(fuzzyset >= as.numeric(index[[n]][2]) && fuzzyset < as.numeric(index[[n+1]][2])){
            origcol <- fuzzyset - as.numeric(index[[n]][2]) + 1
            tempcon <- c(tempcon, paste(index[[n]][1], origcol))
            break
          }
        }
        if(n == length(index)){
          origcol <- fuzzyset - as.numeric(index[[n]][2]) + 1
          tempcon <- c(tempcon, paste(index[[n]][1], origcol))
        }
        n <- n + 1
      }
      k <- k + 1
    }
    newrule$ant <- tempant
    newrule$con <- tempcon
    newrule$sup <- rules[[i]]$sup
    newrule$conf <- rules[[i]]$conf
    class(newrule) <- "rule"
    newrules[[length(newrules)+1]] <- newrule
    i <- i + 1
  }
  return(newrules)
}
