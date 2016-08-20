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
truncated = mergedTable[19:69,]
confidentSort = truncated[,'confidentSort']
confidentSortPost = truncated[,'confidentSortPost']
#truncated = mergedTable[complete.cases(mergedTable),]
t.test(confidentSort,confidentSortPost, paired=TRUE)
t.test(truncated[confidentSort],confidentSortPost, paired=TRUE)