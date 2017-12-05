library(caret)
library(ggplot2)

#ロジスティック回帰 ホームワーク
setwd("C:/Users/t-sako/datamix/20171120_Rによる統計モデリング実践実践/")
## まずデータを読み込みます
cam_data<-read.csv("homework_data/bank_marketing_test.csv")
summary(cam_data$month)

###事前クレンジング
##目的変数の処理 Yes⇒1、No⇒0
cam_data$y<-ifelse(cam_data$y=="yes", 1, 0)
summary(cam_data$y) #平均加入率は7.4%程度

#質的変数をダミー変数に変換してみる
tmp <- dummyVars(~., data=cam_data)
cam_data.dummy <- as.data.frame(predict(tmp, cam_data))
#- が入るとエラーが出たため列名を変更
names(cam_data.dummy)[3]<-"job.bluecolloar"
names(cam_data.dummy)[8]<-"job.selfemployed"

head(cam_data.dummy)

sim_persona <- function(cam_data.dummy){
  
  #学習用とテスト用にデータを分ける
  set.seed(1234)  # コードの再現性を保つためseedを固定
  num_rows<-dim(cam_data.dummy)[1]
  idx<-c(1:num_rows)
  train_idx<-sample(idx, size = num_rows*0.7 )
  train_data<-cam_data.dummy[train_idx, ]
  valid_data<-cam_data.dummy[-train_idx, ]
  
  summary(train_data)
  
  #モデルを作成開始
  #年齢とjob,学歴でモデルを作ってみます
  model_vol1<-glm(y~age+job.admin.+job.bluecolloar+job.entrepreneur+job.housemaid+job.management+job.retired+job.selfemployed+job.services+job.technician+marital.married+education.illiterate+education.professional.course+education.university.degree+loan.no+default.no
                  ,data = train_data, family = "binomial")
  summary(model_vol1)
  
  #Step関数でAICが最小になるようにモデルを作る
  model_vol2<-step(model_vol1)
  summary(model_vol2)
  ## オッズ比の計算
  exp(model_vol2$coefficients)
}
#sim_persona(cam_data.dummy = cam_data.dummy)


sim_roi <- function(cam_data.dummy){
  
  #学習用とテスト用にデータを分ける
  #set.seed(1234)  # コードの再現性を保つためseedを固定
  #num_rows<-dim(cam_data.dummy)[1]
  ##idx<-c(1:num_rows)
  #train_idx<-sample(idx, size = num_rows*0.7 )
  #train_data<-cam_data.dummy[train_idx, ]
  #valid_data<-cam_data.dummy[-train_idx, ]
  
  #summary(train_data)
  
  #モデルを作成開始
  #年齢とjob,学歴でモデルを作ってみます
  model_vol3<-glm(y~age+job.admin.+job.bluecolloar+job.entrepreneur+job.housemaid+job.management+job.retired+job.selfemployed+job.services+job.technician+marital.married+education.illiterate+education.professional.course+education.university.degree+loan.yes+default.no+month.apr+month.aug+month.jul+month.may+month.jun+poutcome.success
                  ,data = cam_data.dummy, family = "binomial")
  summary(model_vol3)
  
  #Step関数でAICが最小になるようにモデルを作る
  model_vol4<-step(model_vol3)
  summary(model_vol2)
  ## オッズ比の計算
  exp(model_vol4$coefficients)
  
  ###アタックリスト
  #作成したモデルを検証用データに適用し、
  #マーケティングキャンペーンにリアクションする確率を求めます
  score<-predict(model_vol2, valid_data, type = "response")
  summary(score)
  
  #scoreの閾値設定
  L <- 0.01 #min(score)
  m <- 85
  d <- 0.01 #(max(socre)-L)/50
  for(i in 1:m){
    ypred_flag<-ifelse(score >  L, 1, 0)
    #confusion matrix
    conf_mat<-table(valid_data$y, ypred_flag)
    
    # conf_mat[3](表の右上)とconf_mat[4](表の右下)
    attack_num<-conf_mat[3] + conf_mat[4]
    get_num <-conf_mat[4]
    
    #コスト
    pre_cost <- attack_num * 500
    #print(pre_cost)
    
    #売上
    pre_sale <- get_num * 2000
    #print(pre_sale)
    
    #利益
    pre_revenue = pre_sale - pre_cost
    #print(pre_revenue)
    L <- L + d
  }
  return()
}

sim_roi(cam_data.dummy = cam_data.dummy)

