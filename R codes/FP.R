FPGrowth <- function(tree, minsup, level, count=0){
  if(max(tree[,3]) != 0) {                                                        #check if tree is empty
    
    freqsets <- list()                                                          #establish freqset list that will be returned in the end
    endnodes <- 0
    added <- 0
    
    i <- 1
    while(i <= nrow(tree)){                                                     #cycle tree
      #check, if node is not parent of any other node (check if the tree has only one single path)
      if(tree[i,3] != 0){                                                     #eliminates root nodes with no entries
        child <- 0
        j <- 1
        while(j <= nrow(tree)) {
          if(tree[j,2] == i){
            child <- 1
            break
          }
          j <- j + 1
        }
        if(child == 0){
          endnodes <- endnodes + 1
        }
      }
      i <- i + 1
    }                                                                           #end of procedure that checks if the tree has only one path
    #if it has one single path, endnodes == 1, else it is bigger
    
    
    if(endnodes == 1) {                                                         #procedure for single path tree
      
      freqsets <- list()
      items <- c()
      #support <- min(supplist)
      
      i <- 1
      while(i <= nrow(tree)) {
        if(tree[i,3] > 0){
          items <- c(items, tree[i,1]) 
        }
        i <- i + 1
      }
      
      itemsets <- powerSet(items)
      
      j <- 1
      while(j <= length(itemsets)){
        singleset <- list()
        
        #generate supplist
        supplist <- c() #for computing the minimum later
        t <- 1
        
        while(t <= nrow(tree)){
          v <- 1
          while (v <= length(itemsets[[j]])){
            if(tree[t,3] > 0 && tree[t,1] == itemsets[[j]][v]) {
              supplist <- c(supplist, tree[t,3])
            }
            v <- v + 1
          }
          t <- t + 1
        }
        if(min(supplist) >= minsup){
          singleset[[1]] <- c(itemsets[[j]], level)
          singleset[[2]] <- min(supplist)
          freqsets[[length(freqsets)+1]] <- singleset
        }
        j <- j + 1
      }
      
      return(freqsets)
    }
    
    
    if (endnodes >= 2){
      cpb <- list() #establish conditional pattern base
      
      i <- 1
      
      
      
      if(count == 0){ #check if it is the first iteration
        while(i <= nrow(tree)){ #cycle through old tree
          
          if(tree[i,1] == level && tree[i,3] != 0){
            
            #generate conditional pattern base!
            
            
            ancestor <- tree[i,2]
            patternnr <- length(cpb) + 1
            
            templist <- list(0)
            suppvect <- c(tree[i,3])
            
            while(ancestor != 0){
              if(templist[[1]][1] == 0){
                templist[[1]] <- c(tree[ancestor,1])
                suppvect <- c(tree[ancestor,3], suppvect)
                ancestor <- tree[ancestor,2]
              } else {
                templist[[1]] <- c(tree[ancestor,1], templist[[1]])
                suppvect <- c(tree[ancestor,3], suppvect)
                ancestor <- tree[ancestor,2]
              }
            }
            if(templist[[1]][1] != 0) {
              templist[[2]] <- tree[i,3]
              #templist[[2]] <- min(suppvect)
              cpb[[patternnr]] <- templist
              
              #add current level element to templist
              templist[[1]][length(templist[[1]])+1] <- level
              
              
              #check if support is met, if yes add to freqsets
              
              if(templist[[2]][1] >= minsup){
                freqsets[[length(freqsets)+1]] <- templist
              }
            }
          }
          i <- i + 1
        } 
      }
      
      
      
      
      if(count>=1) { # do this if it is not the first iteration
        while(i <= nrow(tree)){ #cycle through old tree
          status <- "more than one, not first iteration"
          test <- 0
          
          curlevel <- level-count
          
          if((tree[i,1] == curlevel && tree[i,3] != 0)) {
            
            ancestor <- tree[i,2]
            patternnr <- length(cpb)+1
            
            templist <- list(0)
            suppvect <- c(tree[i,3])
            
            while(ancestor != 0){
              if(templist[[1]][1] == 0){
                templist[[1]] <- c(tree[ancestor,1])
                suppvect <- c(tree[ancestor,3], suppvect)
                ancestor <- tree[ancestor,2]
              } else {
                templist[[1]] <- c(tree[ancestor,1], templist[[1]])
                suppvect <- c(tree[ancestor,3], suppvect)
                ancestor <- tree[ancestor,2]
              }
            }
            
            if(templist[[1]][1] !=0 ){
              templist[[2]] <- tree[i,3]
              #templist[[2]] <- min(suppvect)
              cpb[[patternnr]] <- templist
              
              
              #und wenns null ist...
              #add current level element to templist
              templist[[1]][length(templist[[1]])+1] <- level
              
              #check if support is met, if yes add to freqsets
              
              if(templist[[2]][1] >= minsup){
                freqsets[[length(freqsets)+1]] <- templist
              }
            }
          }
          i <- i + 1
        }
        
        
        #generate conditional FP-Tree
        
        #generate root elements!
        condfptree <- matrix(data=0, ncol=3, nrow=level)
        ind <- 1
        while(ind <= level){
          condfptree[ind,1] <- ind
          ind <- ind + 1
        }
        
        
        j <- 1
        while(j <= length(cpb)){ #cycle conditional pattern base
          itemlist <- cpb[[j]][[1]]
          k <- 1
          while(k <= length(itemlist)){ #cycle items
            if(k == 1){
              #add to root element
              condfptree[itemlist[k],3] <- condfptree[itemlist[k],3] + cpb[[j]][[2]][1]
              parent <- itemlist[k]
            } else {
              #check for existance, if exists add value, if not make new node 
              
              existence <- 0
              n <- 1
              while(n <= nrow(condfptree)){ #cycle through tree nodes
                if(condfptree[n,1] == itemlist[k] && condfptree[n,2] == parent) { #the node exists already: search, if node already exists (by column and parent)
                  existence <- 1
                  condfptree[n,3] <- condfptree[n,3]+cpb[[j]][[2]][1]
                  parent <- n
                  break
                }
                n <- n + 1
              }
              if(existence == 0) { #if no, create a new node
                condfptree <- rbind(condfptree, c(itemlist[k], parent, cpb[[j]][[2]][1]))
                parent <- nrow(condfptree)
              }
            }
            k <- k + 1
          }
          j <- j + 1
        }
        
        #conditional fp-tree constructed
        newsets <- FPGrowth(condfptree, minsup, curlevel)
        
        m <- 1
        while(m <= length(newsets)){
          #add level item
          newsets[[m]][[1]] <- c(newsets[[m]][[1]], level)
          freqsets[[length(freqsets)+1]] <- newsets[[m]]
          m <- m + 1
        }
        added <- 1
      }
      
      
      
      
      
      
      
      
      
      
      
      #generate conditional FP-Tree
      
      #generate root elements!
      condfptree <- matrix(data=0, ncol=3, nrow=level)
      ind <- 1
      while(ind <= level){
        condfptree[ind,1] <- ind
        ind <- ind + 1
      }
      
      
      j <- 1
      while(j <= length(cpb)){ #cycle conditional pattern base
        itemlist <- cpb[[j]][[1]]
        k <- 1
        while(k <= length(itemlist)){ #cycle items
          if(k == 1){
            #add to root element
            condfptree[itemlist[k],3] <- condfptree[itemlist[k],3] + cpb[[j]][[2]][1]
            parent <- itemlist[k]
          } else {
            #check for existance, if exists add value, if not make new node 
            existence <- 0
            n <- 1
            while(n <= nrow(condfptree)){ #cycle through tree nodes
              if(condfptree[n,1] == itemlist[k] && condfptree[n,2] == parent) { #the node exists already: search, if node already exists (by column and parent)
                existence <- 1
                condfptree[n,3] <- condfptree[n,3] + cpb[[j]][[2]][1]
                parent <- n
                break
              }
              n <- n + 1
            }
            if(existence == 0) { #if no, create a new node
              condfptree <- rbind(condfptree, c(itemlist[k], parent, cpb[[j]][[2]][1]))
              parent <- nrow(condfptree)
            }
          }
          k <- k + 1
        }
        j <- j + 1
      }
      
      #conditional fp-tree constructed
      
      count <- count + 1
      #call function again and add to itemsets, if minsup is met
      newsets<-FPGrowth(condfptree, minsup, level, count)
      
      #fit together freqsets and newsets
      m <- 1
      while(m <= length(newsets)){
        freqsets[[length(freqsets)+1]] <- newsets[[m]]
        m <- m + 1
      }
      
      finalsets <- list()
      f <- 1
      while(f <= length(freqsets)){
        if(f == length(freqsets)) {
          s <- freqsets[[f]]
          class(s) <- "itemset"
          finalsets[[length(finalsets) + 1]] <- s
        } else {
          g <- f + 1
          exists <- 0
          while(g <= length(freqsets)){
            if(length(freqsets[[f]][[1]]) == length(freqsets[[g]][[1]])){
              if(all(sort(freqsets[[f]][[1]]) == sort(freqsets[[g]][[1]]))){
                exists <- 1
                break
              }
            }
            g <- g + 1
          }
          if(exists == 0){
            s <- freqsets[[f]]
            class(s) <- "itemset"
            finalsets[[length(finalsets) + 1]] <- s
          }
        }
        f <- f + 1
      }
      return(finalsets)
      
    } #close of if-function from beginning
  } #check for empty tree
}
