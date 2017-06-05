#' Reformatting the original proteomics csv file
#'
#' @param data Original csv file.
#' @param k The number of observations.
#' @export
#' @return reformatted csv file.
#' @examples
#' setwd("C:/Users/kbs/Desktop/RA_Pig")
#' # data = original data in .csv format 
#' data<-read.csv("HN-A-2.csv")
#' # result will be "HN-A.csv"

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
    #Be careful about the column name.
    if(as.logical(1-is.na(data2$Checked[i]))){temp[i,]<-c("Protein",as.character(data2[i,3]),as.numeric(as.matrix(data2[i,c((14+2*k):(13+3*k))])),NA)}
    if(is.na(data2$Checked[i])){temp[i,]<-c("Peptide",as.character(data2[i,4]),as.numeric(as.matrix(data2[i,c((14+k):(14+2*k))])))}
  }
  return(temp)
}