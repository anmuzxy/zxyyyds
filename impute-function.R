#* writed by zhangxinyu from AHMU*#
#* the r code is coping for data which is not completed*#

delete=function(data){
  data=na.omit(data)
  return(data)
}


junshu=function(data){
  for (i in 1:ncol(data)) {
    data[is.na(data[,i]),i]=mean(data[,i],na.rm=T) 
  }
  return(data)
}

zhongweishu=function(data){
  for (i in 1:ncol(data)) {
    data[is.na(data[,i]),i]=median(data[,i],na.rm=T) 
  }
  return(data)
}

mode=function(v){
  tab=table(v) 
  return(names(tab)[which.max(tab)]) 
}


getmode=function(data){
  for (i in 1:ncol(data)) {
    data[is.na(data[,i]),i]=mode(data[,i]) 
  }
  return(data)
}


suijisenlin=function(data){
  dat2=missForest(data)
  return(dat2$ximp)
}

knn=function(data){
  data2=knnImputation(data,meth="weighAvg")
  return(data2)
}

m_impute=function(data,id_junshu=NULL,id_zhongweishu=NULL,id_zhongshu=NULL){
  if(length(id_junshu)==1){
    
    data[,id_junshu][is.na(data[,id_junshu])]=mean(data[,id_junshu],na.rm=T)
  } else if(length(id_junshu)>1)
  {data[,which(names(data)%in%id_junshu)]=apply(data[,which(names(data)%in%id_junshu)],2,function(x){x[which(is.na(x))]=mean(x,na.rm=T);return(x)})}
  
  if(length(id_zhongweishu)==1){
    data[,id_zhongweishu][which(is.na(data[,id_zhongweishu]))]=median(data[,id_zhongweishu],na.rm=T)
  } else if(length(id_zhongweishu)>1)
  {data[,which(names(data)%in%id_zhongweishu)]=apply(data[,which(names(data)%in%id_zhongweishu)],2,function(x){x[which(is.na(x))]=median(x,na.rm=T);return(x)})}
  
  if(length(id_zhongshu)==1){
    data[,id_zhongshu][which(is.na(data[,id_zhongshu]))]=mode(data[,id_zhongshu])
  } else if(length(id_zhongshu)>1)
  {data[,which(names(data)%in%id_zhongshu)]=apply(data[,which(names(data)%in%id_zhongshu)],2,function(x){x[which(is.na(x))]=mode(x);return(x)})}
  return(data)
}

find_missing=function(df) {
  missing_rows=which(apply(df, 1, function(x) any(is.na(x))))
  return(missing_rows) 
}

pre_data=function(data){
  number=ncol(data)
  impute_matrix=diag(1,number)
  impute_matrix_new=impute_matrix==0
  pre_matrix=data.frame(rep("pmm",number),impute_matrix_new,row.names=names(data))
  names(pre_matrix)=c("插入方法",names(data))
  return(pre_matrix)
}



pre_columns=function(data){
  preduct_na=function(data){
    source=list(NA,c("pmm", "logreg", "polyreg", "polr"))
    for(i in 1:ncol(data)){
      source=append(source,NA)
    }
    return(source)
  }
  title=c("变量名","插补方法",names(data))
  type=c("text","dropdown",rep("checkbox",ncol(data)))
  width=c(500,500,rep(300,ncol(data)))
  source=I(preduct_na(data))
  return(data.frame(title,type,width,source))
}



logistic_regression=function(response,x,predictors=NULL, data) {
  dd=datadist(data)
  options(datadist=dd)
  if(length(predictors)==0){
    formula=as.formula(paste(response, "~",paste("rcs(",x,",3)")))
  } else{
    formula=as.formula(paste(response, "~",paste("rcs(",x,",3)+"),paste(predictors, collapse = "+")))
  }
  model=lrm(formula,data = data,x=TRUE,y=TRUE)
  return(model)
} #返回logistic_regression

to_factor_data=function(var,data){
  if(length(var)==1){
    data[,var]=as.factor(data[,var])
  } else if(is.null(var)){
    data=data
  }
  else{
    data[,which(names(data)%in%var)]=apply( data[,which(names(data)%in%var)],2,function(x){x=as.factor(x)})
  }
  return(data)
}#data转化为因子变量


regression=function(response,x,predictors=NULL,data) {
  dd=datadist(data)
  options(datadist=dd)
  if(length(predictors)==0){
    formula=as.formula(paste(response, "~",paste("rcs(",x,",3)")))
  } else{
    formula=as.formula(paste(response, "~",paste("rcs(",x,",3)+"),paste(predictors, collapse = "+")))
  }
  model=ols(formula,data = data)
  return(model)
}#返回regression









