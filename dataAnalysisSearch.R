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
joe = "Gender."
myT = table(mergedTable[joe]) #male and female counts
barplot(myT, main = "barplot")


colnames(mergedTable)[3] = "class"
colnames(mergedTable)[6] = "musicExperience"
colnames(mergedTable)[9] = "progUnderstand"
colnames(mergedTable)[10] = "timeComplexity"

myBar <- function(columnVar, mTable, title){
  myT = table(mTable[columnVar]) #male and female counts
  myT2 <- myT
  barplot(myT, main = title)#, las = 2)
  return(myT2)
}
complexityTSearch = myBar('timeComplexity', mergedTable, "Time Complexity Responses")
experienceTSearch = myBar('musicExperience', mergedTable, "Musical Experience Scale 1-5")
