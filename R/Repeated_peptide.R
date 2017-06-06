#' A set of Repeated peptide names
#'
#' @param data Reformatted csv file from the reformat1 function.
#' @param first_col The first column of observations.
#' @param last_col The last column of observations.
#' @export
#' @return A set of Repeated peptides names.
#' @examples
#' # warning: I assumed that a vector of obserations are the same if they have the same observations.??
#' setwd("C:/Users/kbs/Desktop/RA_pig2")
#' Control_Liver<-read.csv("Control_Liver.csv")
#' Infected_Liver<-read.csv("Infected_Liver.csv")
#' uname<-unique(c(Repeated_peptide(Control_Liver,3,14),Repeated_peptide(Infected_Liver,3,14)))
#'

Repeated_peptide1<-function(data,first_col,last_col){
  peptide<-data[data$Group=="Peptide",]
  missing<-apply(peptide[,-c(1,2,ncol(peptide))],1,function(x)sum(is.na(x))/(ncol(peptide)-3))
  peptide<-peptide[missing!=1,]

  # A set of peptide names: name.
  name<-as.character(unique(peptide$Names))
  # There might exist missing Ions.score
  # peptide<-peptide[!duplicated(peptide$Ions.score),]
  name2<-names(table(peptide$Names)[table(peptide$Names)>=2])
  # Repeated peptide name (name2) does not have enough information.
  # Need one more step.
  temp2<-NULL
  # Need to check peptide by peptide
  
  for(i in 1:length(name2)){
    temp<-peptide[peptide$Names==name2[i],]
    temp<-temp[!duplicated(temp[,c(first_col:last_col)]),]
    temp2<-rbind(temp2,temp)
  }
  # A set of repeated peptides: name3.
  name3<-names(table(temp2$Names)[table(temp2$Names)>=2])
  temp3<-temp2[temp2$Names%in%name3,]
  return(name3)
}


#' A set of Repeated peptides abundances.
#'
#' @param data Reformatted csv file from the reformat1 function.
#' @param first_col The first column of observations.
#' @param last_col The last column of observations.
#' @export
#' @return A set of Repeated peptides abundances.
#' @examples
#' # warning: I assumed that a vector of obserations are the same if they have the same observations.??
#' setwd("C:/Users/kbs/Desktop/RA_pig2")
#' Control_Liver<-read.csv("Control_Liver.csv")
#' Infected_Liver<-read.csv("Infected_Liver.csv")
#' Repeated_peptide2(Control_Liver,3,14)
#' 


Repeated_peptide2<-function(data,first_col,last_col){
  peptide<-data[data$Group=="Peptide",]
  missing<-apply(peptide[,-c(1,2,ncol(peptide))],1,function(x)sum(is.na(x))/(ncol(peptide)-3))
  peptide<-peptide[missing!=1,]
  
  # A set of peptide names: name.
  name<-as.character(unique(peptide$Names))
  # There might exist missing Ions.score
  # peptide<-peptide[!duplicated(peptide$Ions.score),]
  name2<-names(table(peptide$Names)[table(peptide$Names)>=2])
  # Repeated peptide name (name2) does not have enough information.
  # Need one more step.
  temp2<-NULL
  # Need to check peptide by peptide
  
  for(i in 1:length(name2)){
    temp<-peptide[peptide$Names==name2[i],]
    temp<-temp[!duplicated(temp[,c(first_col:last_col)]),]
    temp2<-rbind(temp2,temp)
  }
  # A set of repeated peptides: name3.
  name3<-names(table(temp2$Names)[table(temp2$Names)>=2])
  temp3<-temp2[temp2$Names%in%name3,]
  return(temp3)
}


#' Save all repeated peptide names and abundances.
#'
#' @param name File name.
#' @param first_col The first column of observations.
#' @param last_col The last column of observations.
#' @export
#' @return Write a set of Repeated peptides abundances in csv format.(_repeated.csv)
#' @examples
#' setwd("C:/Users/kbs/Desktop/RA_pig2")
#' W_Repeated_peptide(Control_Liver)
#' 

W_Repeated_peptide<-function(name,first_col,last_col){
  read.csv(paste0(name,".csv")) %>%
    Repeated_peptide2(first_col,last_col) %>%
      write.csv(paste0(name,"_Repeated.csv"), row.names=F)
}

# setwd("C:/Users/kbs/Desktop/proteomics")