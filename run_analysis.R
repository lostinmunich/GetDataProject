
setwd("./")

library(data.table)
library(dplyr)

##Reading data files into R
Xtest<-fread("./X_test.txt")
Xtrain<-fread("./X_train.txt")
Xval<-rbind(Xtest,Xtrain)

ytest<-fread("./y_test.txt")
ytrain<-fread("./y_train.txt")
yval<-rbind(ytest,ytrain)

subtest<-fread("./subject_test.txt")
subtrain<-fread("./subject_train.txt")
subval<-rbind(subtest,subtrain)

##Reading variable names
name<-fread("./features.txt")
namevec<-as.vector(name$V2)
`colnames<-`(Xval,namevec)

##Splitting the names to find those with mean() or std()
test=strsplit(namevec,"\\-")
test<-as.data.table(test)

colindex<-vector()
colname<-vector()

i=1
j=1

for (i in 1:561){
  if(test[[i]][[2]]=="mean()"||test[[i]][[2]]=="std()"){
    colindex[i]=TRUE 
    colname[j]=name[[2]][[i]]
    j<-j+1
  }
  
  else {
    colindex[i]=FALSE
  }
  i<-i+1
}


##Filtering the variables with desired names
i=1
counter=0

for (i in 1:561){
  if(colindex[i]==TRUE){
    if(counter==0){
      Xfinal<-Xval[[i]]
    }
    else{
      Xfinal<-cbind(Xfinal,Xval[[i]])
      
    }
        counter<-counter+1
  }
  i<-i+1
}


Xfinal<-as.data.table(Xfinal)

##Combining y and subject to X
wholeset<-cbind(Xfinal,yval)
wholeset<-cbind(wholeset,subval)


##Renaming y and subject
colname[ncol(Xfinal)+1]<-"action"
colname[ncol(Xfinal)+2]<-"subject"

`colnames<-`(wholeset,colname)

##Factorize the action labels
moves<-c("Walking","Walkin Upstairs","Walking Downstairs","Sitting","Standing","Laying")

wholeset$action<-factor(wholeset$action,labels = moves)


##Making variable names more readable
namesofset<-names(wholeset)
beginning<-substr(namesofset,1,1)

i=1

for (i in 1:ncol(wholeset)){
  if(beginning[i]=="t"){
    namesofset[i]<-paste0("time",namesofset[i]) 
  }
  
  else if(beginning[i]=="f"){
    namesofset[i]<-paste0("fastfouriertransform",namesofset[i]) 
  }
  
  i<-i+1
}


namesofset<-tolower((namesofset))
namesofset<-gsub("-","",namesofset)
namesofset<-gsub("mean","meanof",namesofset)
namesofset<-gsub("std","standarddeviation",namesofset)
namesofset<-gsub("acc","accelerometer",namesofset)
namesofset<-gsub("gyro","gyroscope",namesofset)
namesofset<-gsub("mag","magnitude",namesofset)
namesofset<-gsub("bodybody","body",namesofset)

names(wholeset)<-namesofset


##Calculating the mean of variables
counter<-0

for(i in 1: 30){
  for(j in 1: 6){
    subtable<-wholeset[wholeset$action==moves[j] & wholeset$subject==i]
    
    tempmean<-sapply(subtable,mean)
    tempmean[ncol(Xfinal)+1]<-j
    tempmean[ncol(Xfinal)+2]<-i
    
    if(counter==0){
      outputmean<-tempmean
      counter<-counter+1
    }
    else{
    outputmean<-rbind(outputmean,tempmean)
    }
    j<-j+1
  }
    
  i<-i+1
}

outputmean<-as.data.table(outputmean)

outputmean$action<-factor(outputmean$action,labels = moves)

write.table(outputmean,file="./outputmean.txt",row.names = FALSE)