#---------------HUMAN RESOURCES----------------------------------


hr_train=read.csv("C:/Users/sr/Downloads/hr_train.csv")
hr_test=read.csv("C:/Users/sr/Downloads/hr_test.csv")
View(hr_train)
View(hr_test)


hr_test$left=NA
hr_train$data="train"
hr_test$data="test"
hr_all=rbind(hr_train,hr_test)


View(hr_all)


#creating dummies for categorical variables
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

  

names(hr_all)[sapply(hr_all,function(x) is.character(x))]
  char_cols=c("sales" ,"salary","data" )
  
  char_cols=char_cols[!(char_cols %in% c('data','left'))]
  
  char_cols=c("sales","salary")

  
  for(cat in char_cols){
    hr_all=CreateDummies(hr_all,cat,50)
  }  
  
  
  
  for(col in names(hr_all)){
    if(sum(is.na(hr_all[,col]))>0 & !(col %in% c("data","left"))){
      hr_all[is.na(hr_all[,col]),col]=mean(hr_all[hr_all$data=='train',col],na.rm=T)
    }
  }


library(dplyr)
sum(is.na(hr_train1))
glimpse(hr_train1)



#dividing data
hr_train=hr_all %>% filter(data=="train") %>% select(-data)
hr_test=hr_all %>% filter(data=="test") %>% select(-data,-left)

set.seed(2)
s=sample(1:nrow(hr_all),0.8*nrow(hr_all))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]
#--------------------------------------Linear Regression---------------
fit=lm(left~.-sales_sales,data=hr_train1)
library(car)
vif(fit)
sort(vif(fit),decreasing = T)[1:3]
fit=step(fit)
formula(fit)


fit=lm(formula(fit),data=hr_train1)
train_predictions=predict(fit,data=hr_train2)
pROC::roc(hr_train2$left,train_predictions)$auc

fit.final=lm(left~.,data=hr_train1)
fit.final=step(fit.final)

predicted_test=predict(fit.final,newdata = hr_test)
predicted_test
# for only probability scores 
#if we want to classes[(0,1),(yes,no)] we need to find cutoff...



#model
library(tree)
hr.tree=tree(left~.,data=hr_train1)
hr.tree
plot(hr.tree)
text(hr.tree)
val.ir=predict(hr.tree,newdata=hr_train2,type="vector")
val.ir
library(pROC)
pROC::roc(hr_train2$left,val.ir)$auc



hr.tree.final=tree(left~.,data=hr_train)
test.score=predict(hr.tree.final,newdata = hr_test,type="vector")
test.score


write.csv(test.score,"my_submission_p4_left(2).csv",row.names = F)


#---------------Random forest--------------------
param=list(mtry=c(5,10,15,20,30,35),
           ntree=c(50,100,150,200,250,300),
           maxnodes=c(5,10,15,20,25,30),
           nodesize=c(1,2,5,10))



my_cost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}


subset_params=function(full_list_params,n=10){
  all_comb=expand.grid(full_list_params)
  s=sample(1:nrow(all_comb),n)
  sub_set_params=all_comb[s,]
  return(sub_set_params)
}



num_trails=50
my_params=subset_params(param,num_trails)
my_params



myauc=0
library(cvTools)
library(randomForest)



for (i in 1:num_trails){
  print(paste('string iteration:',i))
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~.,data = hr_train,tuning = params,
             folds = cvFolds(nrow(hr_train),K=10,type = 'random'),
             cost = my_cost_auc,seed = 2,predictArgs = list(type='prob'))
  
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
    best_params=params
  }
  print('done')
}



myauc
best_params



hr.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,data=hr_train)


test.score=predict(hr.rf.final,newdata = hr_test)
test.score




#--------------
randomForest(formula=left~.,data=hr_train,mtry=best_params$mtry,
             ntree=best_params$ntree)
#Explains how much the model perfoms


#---------------------Variable importance------------
d=importance(hr.rf.final)
d=as.data.frame(d)
d$variableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))
