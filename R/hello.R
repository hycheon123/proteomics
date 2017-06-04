#' Reformatting the original proteomics csv file
#'
#' @param data Original csv file.
#' @param k The number of observations.
#' @export
#' @return reformatted csv file.
#' @examples
#' setwd("C:/Users/kbs/Desktop/RA_Pig")
#' # data = original data in .csv format 
#' data<-read.csv("LI-A-2.csv")

reformat1<-function(data,k){
  # data = original data
  # k = the number of observations
  # data2 = protein peptide part only
  data2<-data[data$Master%in%c("Master Protein","FALSE"),]
  N<-nrow(data2)
  # Save the result in temp
  temp<-data.frame(matrix(NA,nrow=N,ncol=(k+3)))
  colnames(temp)<-c("Group","Names",paste0("obs",c(1:k)),"Ions score")
  for(i in 1:N){
    if(as.logical(1-is.na(data2$Checked[i]))){temp[i,]<-c("Protein",as.character(data2[i,3]),as.numeric(as.matrix(data2[i,c(30:(30+k-1))])),NA)}
    if(is.na(data2$Checked[i])){temp[i,]<-c("Peptide",as.character(data2[i,4]),as.numeric(as.matrix(data2[i,c(22:(22+k-1))])))}
  }
  return(temp)
}


