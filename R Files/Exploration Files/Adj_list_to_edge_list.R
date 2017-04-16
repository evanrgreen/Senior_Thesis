setwd("/Users/evangreen/Desktop/Senior\ Thesis/Grade\ Data")
collab <- read.csv("CollabPS2.csv",as.is=T)

#turn dan's raw files into edge lists
#cutoff can be used to reject peopel who listed too many people 
create_edge_list <- function(raw_file,cutoff = 20){
  #get rid
  raw_file$collabs <- unlist(sapply(collab$collabs,function(x)gsub("\\[|\\]", "", x)))
  edges <- as.data.frame(matrix(NA,ncol=2,nrow=100))
  colnames(edges) <- c("Source","Sink")
  edge_num <- 1
  over_reports <- 0 
  for(student in unique(raw_file$student)){
    student_entries <- which(raw_file$student==student)
    sources <- raw_file[student_entries[length(student_entries)], "collabs"]
    sources <- strsplit(sources,split=", ")[[1]]
    num_collabs <- length(sources)
    if( num_collabs > cutoff){
      over_reports <- over_reports + 1
      next
    }else{
      for(elt in unique(sources)){
        if (elt != student & elt != 0 & student != 0){
          edges[edge_num, "Source"]<- elt
          edges[edge_num, "Sink"] <- student
          edge_num <- edge_num + 1 
        }
      }
    }
  }
  edges <- edges[!is.na(edges$Source),]
  
  cat("There were", over_reports, "Over reports with a cutoff of", cutoff,"\n")
  return(edges)
}
edges <- create_edge_list(collab)
