rm(list=ls())
library(arules)
data("AdultUCI")
age <- AdultUCI[1:50,1]
fnlwgt <- AdultUCI[1:50,3]
hpw <- AdultUCI[1:50,13]
testdata <- cbind(age, fnlwgt, hpw)
testdata
sets <- makefuzzyset(testdata[,1])
sets
allsets <- generateAllset(testdata)
allsets
mem <- getIndividualMem(30, sets)
m <- GetMem(testdata[,1], sets)
m
mm <- generateMinMtrx(testdata, allsets)
mm
index <- MtrixIndex(testdata, allsets)
index
sup14 <- gensupp(c(1,4), mm)
sup14
relsup14 <- gensupp(c(1,4), mm, relative=TRUE)
relsup14
rule <- getRule(1, 4)
rule
genConf(rule, mm)
sisup <- SingleItemSupp(mm)
sisup
preind <- ProcessedIndex(sisup, 0.3)
preind
premm <- preProcessFP(mm, 0.3)
premm
tree <- genFPTree(premm)
tree
itemset
powerSet(itemset)
freqsets<-FPGrowth(tree, 1, 5, 0)
freqsets
freqsets <- FPTreeExtrct(tree, 1)
freqsets
verfreq <- verifySupp(freqsets, premm, 1)
verfreq
rules <- makeRule(verfreq)
rules
evalrules <- evaluateRules(rules, premm, 0.1)
evalrules
finalrules <- trace(evalrules, index, preind)
finalrules
