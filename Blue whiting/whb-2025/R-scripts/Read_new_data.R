
read_Intercatch_data<-function(root='..') {
  readD<-function(infile='canum.txt') {
    a<-readLines(con=file.path(root,infile))
    ages<<-scan(text=a[5])
    a<-a[8]
    
    a<-gsub(',','',a)
    scan(text=a)
  }
  
  canum<-readD('canum.txt')/1000
  weca<-readD('weca.txt')/1000 
  str(canum)
  str(weca)
  canum
  weca
  
  caton<-readD('caton.txt')
  SOP<-canum*weca
  
  sum(SOP)
  caton # they should approximately be the same
  
  #delete age 0 and make 10+ group 
  if (ages[1]==0 & ages[2]==15) {
    canumOut<-c(canum[2:10],sum(canum[11:16]))  # del age 0 and use 10+
    wecaOut<-c(weca[2:10],sum(SOP[11:16])/sum(canum[11:16]))
  } else if (ages[1]==1 & ages[2]==15) {
    canumOut<-c(canum[1:9],sum(canum[10:15]))  # use 10+
    wecaOut<-c(weca[1:9],sum(SOP[10:15])/sum(canum[10:15]))
  }  else if (ages[1]==0 & ages[2]==10) {
    canumOut<-c(canum[2:10],sum(canum[11:11]))  # use 10+
    wecaOut<-c(weca[2:10],sum(SOP[11:11])/sum(canum[11:11]))
  } else if (ages[1]==1 & ages[2]==10) {
    canumOut<-c(canum[1:9],sum(canum[10:10]))  # use 10+
    wecaOut<-c(weca[1:9],sum(SOP[10:10])/sum(canum[10:10]))
  }

  
  sum(canumOut*wecaOut) # same as Caton minus catch weight of 0-group
  canumOut<-round(canumOut,1)
  
  wecaOut<-round(wecaOut,5)
  
  return(list(canum=canumOut,weca=wecaOut))
}
