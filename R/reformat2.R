#' Reformatting the original proteomics csv file
#'
#' @param data reformat1 output csv file:reformat1 output contains protein and peptide in the same row.
#' @export
#' @return protein info and peptide info are separated.
#' @examples
#' setwd("C:/Users/kbs/Desktop/RA_pig2/Liver_samples")
#' data<-read.csv("Control_Liver.csv")
#' data2<-reformat2(data)

reformat2<-function(data){
  name1<-unique(as.character(data[data$Group=="Protein",]$Names))
  leng1<-length(name1)
  tleng<-nrow(data)
  data2<-data.frame()
  
  for(i in 1:leng1){
    if(i!=leng1){
      loc1<-c(1:tleng)[data$Names==name1[i]]
      loc2<-c(1:tleng)[data$Names==name1[i+1]]
      temp2<-data[loc1:loc2,]
      temp2$Protein<-name1[i]
      data2<-rbind(data2,temp2)
    }else{
      loc1<-c(1:tleng)[data$Names==name1[i]]
      loc2<-tleng
      temp2<-data[loc1:loc2,]
      temp2$Protein<-name1[i]
      data2<-rbind(data2,temp2)    
    }
  }
  
  t1<-data2[data2$Group=="Peptide",]
  t1<-t1[!t1$Protein%in%paste0("PRTC-",1:30),]
  t2<-data[data$Names%in%paste0("PRTC-",1:30),]
  t2$Protein<-t2$Names
  data3<-rbind(t1,t2)
  return(data3)
}

#devtools::document() 

