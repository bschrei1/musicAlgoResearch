setwd("/Users/benschreiber/Documents/musicAlgoResearch")
preResponses <- read.csv("binarySearchPre.csv")
postResponses <- read.csv("binarySearchPost.csv")
preResponses = preResponses[1:45,]

#renaming variables
colnames(preResponses)[2] <- "id"
colnames(postResponses)[2] <- "id"
colnames(preResponses)[7] <- "confidentSearch"
colnames(postResponses)[4] <- "confidentSearchPost"

colnames(preResponses)[8] <- "confidentBigO"
colnames(postResponses)[5] <- "confidentBigOPost"
#merge two tables together by id
mergedTable = merge(preResponses, postResponses, by = "id")

confidentSearch = mergedTable[,'confidentSearch']
confidentSearchPost = mergedTable[,'confidentSearchPost']
t.test(confidentSearchPost, confidentSearch, paired=TRUE)
t.test(mergedTable[,'confidentBigOPost'], mergedTable[,'confidentBigO'], paired=TRUE)
mergedTable["confidenceBigODiff"] = confidentSearchPost-confidentSearch


############# tests with bar graphs
joe = "Gender."
myT = table(mergedTable[joe]) #male and female counts
barplot(myT, main = "barplot")


#colnames(mergedTable)[3] = "class"
#colnames(mergedTable)[6] = "musicExperience"
#colnames(mergedTable)[9] = "progUnderstand"
#colnames(mergedTable)[10] = "timeComplexity"
#colnames(mergedTable)[15] = "timeComplexityPost"
#colnames(mergedTable)[16] = "simulationQ"
#colnames(mergedTable)[17] = "factualQ"


myBar <- function(columnVar, mTable, title){
  myT = table(mTable[columnVar]) #male and female counts
  myT2 <- myT
  barplot(myT, main = title)#, las = 2)
  return(myT2)
}
complexityTSearch = myBar('timeComplexity', mergedTable, "Time Complexity Responses")
experienceTSearch = myBar('musicExperience', mergedTable, "Musical Experience Scale 1-5")

###############
#testing for technical success

lengthTable = length(postResponses[,6]); #number of rows
femaleTotal = 0
numFemales = 0
maleTotal = 0
numMales = 0
numRightVec <- vector(mode="numeric", length=lengthTable)
malesVec = c()
femalesVec = c()
for (i in 1:lengthTable){
  numRight = 0
  if (postResponses[i,6] =="O(log2 n)  (base 2)" ){
    numRight = numRight+1
  }
  if (postResponses[i,7] == "set low to mid + 1 and let the high position be"){
    numRight = numRight+1
  }
  if (postResponses[i,8] == "It cuts the array in half till it finds the key's place"){
    numRight = numRight+1
  }
  gend = postResponses[i,19]
  if (gend == "Femal"||gend == "Female" || gend == "female"){
    femaleTotal =femaleTotal +numRight
    numFemales = numFemales +1
    femalesVec <- c(femalesVec, numRight)
  }
  if (gend == "M"||gend == "male" || gend == "Male" || gend == "man"){
    maleTotal =maleTotal +numRight
    numMales = numMales +1
    append(malesVec, numRight)
    malesVec <- c(malesVec, numRight)
  }
  numRightVec[i] <- numRight
  
}

#print(success)
postResponses["technicalSuccess"]<-numRightVec
maleAvg = maleTotal/numMales
femaleAvg = femaleTotal/numFemales
var.test(femalesVec,malesVec)
t.test(femalesVec,malesVec)


#####################testing attitude questions
meanVec = c()
varVec = c()
pValVec = c()
statisticVec = c()
confidenceIntVec = c()
namesVec =c()


namesVec =c(namesVec, "ConfidenceSearchPost") 
#meanVec = c(meanVec, mean(postResponses[,4]))
namesVec =c(namesVec, "ConfidenceBigOPost")
#meanVec = c(meanVec, mean(postResponses[,5]))
namesVec =c(namesVec, "RememberSongSearch") 
#meanVec = c(meanVec, mean(postResponses[,9]))


namesVec =c(namesVec, "DidntHelpSearch")
#meanVec = c(meanVec, mean(postResponses[,10]))
namesVec =c(namesVec, "MadeSenseSearch")
#meanVec = c(meanVec, mean(postResponses[,11]))
namesVec =c(namesVec, "TranslatedSearch")
#meanVec = c(meanVec, mean(postResponses[,12]))
namesVec =c(namesVec, "MoreFunSearch")
#meanVec = c(meanVec, mean(postResponses[,13]))

namesVec =c(namesVec, "LessIntimidatingSearch")
#meanVec = c(meanVec, mean(postResponses[,14]))
namesVec =c(namesVec, "NotWorthMyTimeSearch")
#meanVec = c(meanVec, mean(postResponses[,15]))
namesVec =c(namesVec, "StudyResourceSearch")
#meanVec = c(meanVec, mean(postResponses[,16]))
namesVec =c(namesVec, "WantMoreVideosSearch")
"
meanTechnicalSuccessSearch = mean(postResponses[,22])
varConfidenceSearchPost =  var(postResponses[,4]) 
varConfidenceBigOPost =  var(postResponses[,5]) 
varRememberSongSearch =  var(postResponses[,9]) 
varConfidenceBigOPostSearch =  var(postResponses[,5]) 
varRememberSongSearch =  var(postResponses[,9]) 
varDidntHelpSearch =  var(postResponses[,10]) 
varMadeSenseSearch=  var(postResponses[,11]) 
varTranslatedSearch =  var(postResponses[,12]) 
varMoreFunSearch =  var(postResponses[,13]) 
varLessIntimidatingSearch =  var(postResponses[,13]) 
varLessIntimidatingSearch =  var(postResponses[,14]) 
varNotWorthMyTimeSearch =  var(postResponses[,15]) 
varStudyResourceSearch =  var(postResponses[,15]) 
varStudyResourceSearch =  var(postResponses[,16]) 
varWantMoreVideosSearch =  var(postResponses[,17]) 
varTechnicalSuccessSearch =  var(postResponses[,22])

tConfidenceSearchPost =  t.test(postResponses[,4], mu=3) 
tConfidenceBigOPost =  t.test(postResponses[,5], mu=3) 
tRememberSongSearch =  t.test(postResponses[,9], mu=3) 
tConfidenceBigOPostSearch =  t.test(postResponses[,5], mu=3) 
tRememberSongSearch =  t.test(postResponses[,9], mu=3) 
tDidntHelpSearch =  t.test(postResponses[,10], mu=3) 
tMadeSenseSearch=  t.test(postResponses[,11], mu=3) 
tTranslatedSearch =  t.test(postResponses[,12], mu=3) 
tMoreFunSearch =  t.test(postResponses[,13], mu=3) 
tLessIntimidatingSearch =  t.test(postResponses[,13], mu=3) 
tLessIntimidatingSearch =  t.test(postResponses[,14], mu=3) 
tNotWorthMyTimeSearch =  t.test(postResponses[,15], mu=3) 
tStudyResourceSearch =  t.test(postResponses[,15], mu=3) 
tStudyResourceSearch =  t.test(postResponses[,16], mu=3) 
tWantMoreVideosSearch =  t.test(postResponses[,17], mu=3) 
tTechnicalSuccessSearch =  t.test(postResponses[,22], mu=0)

"
mergedTable["confidenceBigODiff"] = mergedTable["confidentBigOPost"]- mergedTable["confidentBigO"]
for (i in 4:17){
  if (i<=5 || i>=9){
    meanVec = c(meanVec, mean(postResponses[,i]))
    
    varVec = c(varVec, var(postResponses[,i]))
    tTest = t.test(postResponses[,i], mu=3) 
    pValVec = c(pValVec, tTest$p.value)
    statisticVec = c(statisticVec, tTest$statistic)
    #confidenceIntVec = c(confidenceIntVec,c(tTest$conf.int[1], tTest$conf.int[2]) )
    
  }
}
attitudeTableSearch = data.frame(namesVec, meanVec, varVec, pValVec, statisticVec)#, confidenceIntVec)
#     setNames.times..height.
mergedTable["confidenceBigODiff"] = mergedTable["confidentBigOPost"]- mergedTable["confidentBigO"]
mergedTable["confidenceSearchDiff"] = mergedTable["confidentSearchPost"]- mergedTable["confidentSearch"]
computeMFDiff<-function(myVec, mergedTable){
  manVec = c()
  womanVec = c()
  for (i in 1:length(myVec)){
    gend = mergedTable[i,"Gender."]
    if (gend == 'Femal' || gend == 'Female' || gend == 'female'){
      womanVec = c(womanVec,myVec[i])
    }
    if (gend == 'Male' || gend == 'male' || gend == 'M'|| gend == 'man'){
      manVec = c(manVec,myVec[i])
    }
  }
  print("line 193")
  print( mean(manVec) )
  
  gendTest = t.test(manVec,womanVec)
  return(gendTest)
}
OConfDiffTestSearch = computeMFDiff(mergedTable[,"confidenceBigODiff"],mergedTable)
codeConfDiffTestSearch = computeMFDiff(mergedTable[,"confidenceSearchDiff"],mergedTable)