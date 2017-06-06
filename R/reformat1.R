#' Reformatting the original proteomics csv file
#'
#' @param data Original csv file.
#' @param k The number of observations.
#' @param s1 The starting column number of protein abundance.
#' @param s2 The starting column number of peptide abundance.
#' @export
#' @return reformatted csv file.
#' @examples
#' setwd("C:/Users/kbs/Desktop/RA_pig2/Liver_samples")
#' data<-read.csv("Infected-4.csv")
#' temp<-reformat1(data,12,50,50)

reformat1<-function(data,k,s1,s2){
  # data = original data
  # k = the number of observations
  # data2 = protein peptide part only
  data2<-data[data$Master%in%c("Master Protein","FALSE"),]
  N<-nrow(data2)
  # Save the result in temp
  temp<-data.frame(matrix(NA,nrow=N,ncol=(k+3)))
  colnames(temp)<-c("Group","Names",paste0("obs",c(1:k)),"Ions score")
  for(i in 1:N){
    #Be careful about the column name.
    if(as.logical(1-is.na(data2$Checked[i]))){temp[i,]<-c("Protein",as.character(data2[i,3]),as.numeric(as.matrix(data2[i,c(s1:(s1+k-1))])),NA)}
    if(is.na(data2$Checked[i])){temp[i,]<-c("Peptide",as.character(data2[i,4]),as.numeric(as.matrix(data2[i,c(s2:(s2+k))])))}
  }
  return(temp)
}
