library(caret)
library(ggplot2)

#���W�X�e�B�b�N��A �z�[�����[�N
setwd("C:/Users/t-sako/datamix/20171120_R�ɂ�铝�v���f�����O���H���H/")
## �܂��f�[�^��ǂݍ��݂܂�
cam_data<-read.csv("homework_data/bank_marketing_test.csv")
summary(cam_data$month)

###���O�N�����W���O
##�ړI�ϐ��̏��� Yes��1�ANo��0
cam_data$y<-ifelse(cam_data$y=="yes", 1, 0)
summary(cam_data$y) #���ω�������7.4%���x

#���I�ϐ����_�~�[�ϐ��ɕϊ����Ă݂�
tmp <- dummyVars(~., data=cam_data)
cam_data.dummy <- as.data.frame(predict(tmp, cam_data))
#- ������ƃG���[���o�����ߗ񖼂�ύX
names(cam_data.dummy)[3]<-"job.bluecolloar"
names(cam_data.dummy)[8]<-"job.selfemployed"

head(cam_data.dummy)

sim_persona <- function(cam_data.dummy){
  
  #�w�K�p�ƃe�X�g�p�Ƀf�[�^�𕪂���
  set.seed(1234)  # �R�[�h�̍Č�����ۂ���seed���Œ�
  num_rows<-dim(cam_data.dummy)[1]
  idx<-c(1:num_rows)
  train_idx<-sample(idx, size = num_rows*0.7 )
  train_data<-cam_data.dummy[train_idx, ]
  valid_data<-cam_data.dummy[-train_idx, ]
  
  summary(train_data)
  
  #���f�����쐬�J�n
  #�N���job,�w���Ń��f��������Ă݂܂�
  model_vol1<-glm(y~age+job.admin.+job.bluecolloar+job.entrepreneur+job.housemaid+job.management+job.retired+job.selfemployed+job.services+job.technician+marital.married+education.illiterate+education.professional.course+education.university.degree+loan.no+default.no
                  ,data = train_data, family = "binomial")
  summary(model_vol1)
  
  #Step�֐���AIC���ŏ��ɂȂ�悤�Ƀ��f�������
  model_vol2<-step(model_vol1)
  summary(model_vol2)
  ## �I�b�Y��̌v�Z
  exp(model_vol2$coefficients)
}
#sim_persona(cam_data.dummy = cam_data.dummy)


sim_roi <- function(cam_data.dummy){
  
  #�w�K�p�ƃe�X�g�p�Ƀf�[�^�𕪂���
  #set.seed(1234)  # �R�[�h�̍Č�����ۂ���seed���Œ�
  #num_rows<-dim(cam_data.dummy)[1]
  ##idx<-c(1:num_rows)
  #train_idx<-sample(idx, size = num_rows*0.7 )
  #train_data<-cam_data.dummy[train_idx, ]
  #valid_data<-cam_data.dummy[-train_idx, ]
  
  #summary(train_data)
  
  #���f�����쐬�J�n
  #�N���job,�w���Ń��f��������Ă݂܂�
  model_vol3<-glm(y~age+job.admin.+job.bluecolloar+job.entrepreneur+job.housemaid+job.management+job.retired+job.selfemployed+job.services+job.technician+marital.married+education.illiterate+education.professional.course+education.university.degree+loan.yes+default.no+month.apr+month.aug+month.jul+month.may+month.jun+poutcome.success
                  ,data = cam_data.dummy, family = "binomial")
  summary(model_vol3)
  
  #Step�֐���AIC���ŏ��ɂȂ�悤�Ƀ��f�������
  model_vol4<-step(model_vol3)
  summary(model_vol2)
  ## �I�b�Y��̌v�Z
  exp(model_vol4$coefficients)
  
  ###�A�^�b�N���X�g
  #�쐬�������f�������ؗp�f�[�^�ɓK�p���A
  #�}�[�P�e�B���O�L�����y�[���Ƀ��A�N�V��������m�������߂܂�
  score<-predict(model_vol2, valid_data, type = "response")
  summary(score)
  
  #score��臒l�ݒ�
  L <- 0.01 #min(score)
  m <- 85
  d <- 0.01 #(max(socre)-L)/50
  for(i in 1:m){
    ypred_flag<-ifelse(score >  L, 1, 0)
    #confusion matrix
    conf_mat<-table(valid_data$y, ypred_flag)
    
    # conf_mat[3](�\�̉E��)��conf_mat[4](�\�̉E��)
    attack_num<-conf_mat[3] + conf_mat[4]
    get_num <-conf_mat[4]
    
    #�R�X�g
    pre_cost <- attack_num * 500
    #print(pre_cost)
    
    #����
    pre_sale <- get_num * 2000
    #print(pre_sale)
    
    #���v
    pre_revenue = pre_sale - pre_cost
    #print(pre_revenue)
    L <- L + d
  }
  return()
}

sim_roi(cam_data.dummy = cam_data.dummy)
