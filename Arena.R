library(reshape2)

#Import File
#The First Parameter is the file in your working directory
#Set your working directory to setwd(your current directory where the csv file resides)
ArtistList=read.csv("Artist_lists_small.txt",quote="",na.strings=c("", "NA"),header=FALSE,encoding = "UTF-8")


#Get RowNames
ArtistList = cbind(ArtistList,as.numeric(rownames(ArtistList)))

#Give ColumnNames
names (ArtistList) = c(1:50,"ListNumber")

#Unpivot Data
UnpivotList=melt(ArtistList,id = "ListNumber",na.rm = TRUE)

#Group counts by Album
SongCount <- aggregate(UnpivotList$ListNumber ~ UnpivotList$value,data = UnpivotList, FUN = length)
names(SongCount) = c("Album","Total")


#Filter down to counts having equal or more than 50
x <-SongCount[SongCount$Total >=50,]

#Create function to find all possible combinations from a given index and calculate to see
#if inner join equates to at least 50 rows.
FindCombinations <- function(Albums)
{
  
  x <- 2
  AlbumCombinations <- c()
  
  while (x<=length(Albums))
  {
    
    z <- UnpivotList[UnpivotList$value==Albums[1],]
    y <- UnpivotList[UnpivotList$value==Albums[x],]
    zy <- merge (z,y,by ="ListNumber")
    
    if(nrow(zy) > 49)
    {
      AlbumCombinations <- c(AlbumCombinations,paste(zy$value.x[1],',',zy$value.y[1]))
      
    }
    x <- x + 1
  }
  
  return (AlbumCombinations)
  
} 

counter <- 1
AllCombinations <- c()

#Iterate thru Album List to get all combinations
#ie if a vector is c(1,2,3,4) loop thru at each index
#after the index is complete, resize to set the next item as the start
#for example 1st run to the FindCombinations starts at item 1 (calcuate all combinations
#with 1 (1,2),(1,3),(1,4)), the next iteration the
#start is at item 2 with all combinations (2,3),(2,4))

while (counter < length(x$Album))
  
{
   AlbumResize <- x$Album[counter:length(x$Album)]
   AllCombinations <- c(AllCombinations,FindCombinations(AlbumResize))
   counter <- counter + 1
}

write.table(AllCombinations,"Solution.csv",row.names = FALSE,col.names = FALSE)
