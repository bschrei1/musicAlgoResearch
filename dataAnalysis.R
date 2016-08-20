setwd("/Users/benschreiber/Documents/musicAlgoResearch")
preResponses <- read.csv("selectionSortPre.csv")
postResponses <- read.csv("selectionSortPost.csv")
colnames(preResponses)[2] <- "id"
colnames(postResponses)[2] <- "id"
colnames(preResponses)[7] <- "confidentSort"
colnames(postResponses)[4] <- "confidentSortPost"

colnames(preResponses)[8] <- "confidentBigO"
colnames(postResponses)[5] <- "confidentBigOPost"

mergedTable = merge(preResponses, postResponses, by = "id")
#truncated = mergedTable['id'!='']
mergedTable = mergedTable[19:69,]
confidentSort = mergedTable[,'confidentSort']
confidentSortPost = mergedTable[,'confidentSortPost']
#truncated = mergedTable[complete.cases(mergedTable),] tried getting rid of null rows
t.test(confidentSort,confidentSortPost, paired=TRUE)
t.test(mergedTable[,'confidentBigO'],mergedTable[,'confidentBigOPost'], paired=TRUE)
genderFrame <- postResponses[,"Gender."]
i <- 1


joe = "Gender."
myT = table(mergedTable[joe]) #male and female counts
barplot(myT, main = "barplot")
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
  if (postResponses[i,6] =="O(n^2)" ){
    numRight = numRight+1
  }
  if (postResponses[i,7] == "The smallest element is at the start of the list."){
    numRight = numRight+1
  }
  if (postResponses[i,8] == "With each pass (i.e., outer loop iteration), the number of iterations of the inner loop decreases."){
    numRight = numRight+1
  }
  gend = postResponses[i,19]
  if (gend == "Femal"||gend == "Female" || gend == "female"|| gend == "Woman"){
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

#variances of attitude responses
varConfidenceSortPost =  var(postResponses[,4]) 
varConfidenceBigOPost =  var(postResponses[,5]) 
varRememberSongSort =  var(postResponses[,9]) 
varConfidenceBigOPostSort =  var(postResponses[,5]) 
varRememberSongSort =  var(postResponses[,9]) 
varDidntHelpSort =  var(postResponses[,10]) 
varMadeSenseSort=  var(postResponses[,11]) 
varTranslatedSort =  var(postResponses[,12]) 
varMoreFunSort =  var(postResponses[,13]) 
varLessIntimidatingSort =  var(postResponses[,13]) 
varLessIntimidatingSort =  var(postResponses[,14]) 
varNotWorthMyTimeSort =  var(postResponses[,15]) 
varStudyResourceSort =  var(postResponses[,15]) 
varStudyResourceSort =  var(postResponses[,16]) 
varWantMoreVideosSort =  var(postResponses[,17]) 
varTechnicalSuccessSort =  var(postResponses[,22]) #not attitude

meanVec = c()
varVec = c()
pValVec = c()
statisticVec = c()
confidenceIntVec = c()
namesVec =c()


namesVec =c(namesVec, "ConfidenceSortPost") 

namesVec =c(namesVec, "ConfidenceBigOPost")

namesVec =c(namesVec, "RememberSongSort") 



namesVec =c(namesVec, "DidntHelpSort")

namesVec =c(namesVec, "MadeSenseSort")

namesVec =c(namesVec, "TranslatedSort")

namesVec =c(namesVec, "MoreFunSort")


namesVec =c(namesVec, "LessIntimidatingSort")

namesVec =c(namesVec, "NotWorthMyTimeSort")

namesVec =c(namesVec, "StudyResourceSort")

namesVec =c(namesVec, "WantMoreVideosSort")

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
attitudeTableSort = data.frame(namesVec, meanVec, varVec, pValVec, statisticVec)#, confidenceIntVec)
#     setNames.times..height.
mergedTable["confidenceBigODiff"] = mergedTable["confidentBigOPost"]- mergedTable["confidentBigO"]
mergedTable["confidenceSortDiff"] = mergedTable["confidentSortPost"]- mergedTable["confidentSort"]
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
codeConfDiffTestSearch = computeMFDiff(mergedTable[,"confidenceSortDiff"],mergedTable)
