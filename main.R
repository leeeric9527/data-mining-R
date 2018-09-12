setwd("E:\\R_workspace\\数据挖掘大作业")
# 安装包
# install.packages("openxlsx")
library(xlsx)  
library(openxlsx)
#文件名+sheet的序号
data_OR<- read.xlsx("口红.xlsx", sheet = 1)
View(data_OR)
data_OR<-data.frame(data_OR)
data<-data_OR[,c(-6,-11,-14,-15,-18)]
View(data)


# ###########################################################
# 数据清洗
# ###########################################################
# 对价格进行处理，将 ￥~￥的记录，取平均值
x_price<-data[,6]
x_price=data.frame(x_price)
View(x_price)
x_price<-apply(x_price,1,function(i){
  flag=grepl(pattern="~",x=i)
  if(flag){
    # 转换为字符串
    y=as.character(i)
    # 字符分割
    n=strsplit(y,split = "~",fixed = T)
    # 字符截取
    n<-sapply(n, function(x){
      substring(x, 2, nchar(x))
    })
    # 转换为数字
    n=apply(n,2,as.numeric)
    # 求平均
    n_mean=apply(n,2,mean)
    i=n_mean
    j="¥"
    i=paste(j, i, sep = "")
  }else{
    i=i
  }
  
})
data[,6]<-x_price
View(data)
View(data[,6])
# 去除价格的"￥"
data[,6]<-sapply(data[,6], function(x){
  substring(x, 2, nchar(x))
})

View(data)
# 查看结构信息
summary(data)
# 将字符串转换为数值
# dflme1[,2:60]<-lapply(dflme1[,2:60],as.numeric)
data[,2:8]<-lapply(data[,2:8], as.numeric)
summary(data)
# 将字符串转换为因子
data[,c(1,9:13)]<-lapply(data[,c(1,9:13)], as.factor)
summary(data)
View(data)
# typeof(data$国家)
# data$国家
# data$是否进口

#########################################################
# 缺失值处理
########################################################
# 缺失值的可视化
library(mice)
# 查看缺失数据的情况，各行各列统计
md_data=md.pattern(data)
# 可以观测到，缺失的数据，都是非数值的变量
fix(md_data)

# 缺失值的缺失行的记录
data.lack<-which(!complete.cases(data))
# 转换为 数据框
data.lack<-data.frame(data.lack)
# 缺失值的总记录数
data.lack.num<-nrow(data.lack)
data.lack.num
data<-data.frame(data)
summary(data)
# 把缺失的记录全部删除
data_clean<-na.omit(data)
# 重新查看--缺失值的总个数
n=sum(is.na(data_clean))
n
# 非空数据的行数
(nrow(data_clean))

# # 求NA和的函数
# na.count<-function(x){
#   sum(is.na(x))
# }
# # 缺失值的属性超过所有属性数的30%
# (idx<-which(apply(data,1,na.count)>=ncol(data2)*0.3))
# # 去除缺失30%属性的记录
# data_clean_01<-data[-idx,]
# 
# 
# # 去除防晒缺失的数据和进口缺失的数据
# idx_Sunscreen<-which((!complete.cases(data_clean_01[,11])))
# idx_Imported<-which((!complete.cases(data_clean_01[,13])))
# data_clean_02<-data_clean_01[c(-idx_Sunscreen,-idx_Imported),]
# 
# # 数据清洗完毕
# data_clean<-data_clean_02
# # 查看缺失的数据
# which(!complete.cases(text_deal))

########################################################
# 文件操作
#######################################################
# 写入文件
write.csv(data_clean,file = "mydata.csv",row.names = F)
# 读文件，共1540条记录
data_handle<-read.table("mydata.csv",header=T, sep=",")
data<-data_handle
View(data)


#############################################################
# 查看喜欢---颜色----的词云
#############################################################
library(Rwordseg)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(rJava)
# 分词包
# install.packages("jiebaR")
library(jiebaR)
library(jiebaRD) 

x_color<- data[,9]
x_color<- data.frame(x_color)
View(x_color)
# 导出 .txt 数据格式
# 这里的col.names=F是不保存列名，col.names=T 为保存列名
# 空格分隔
write.table(x_color, file = "color_ciyun.txt",sep ="\t",
            col.name=F,row.names = F, quote = F) 
# 读文件
# 解决：line 3 did not have 3 elements
# 加入参数：blank.lines.skip=F
color_text<-read.table("color_ciyun.txt",head=FALSE,
                       blank.lines.skip=F,quote = "")
View(color_text)


#读入数据分隔符是‘\n’，字符编码是‘UTF-8’，what=''表示以字符串类型读入
f_color <- scan('color_ciyun.txt',sep='\n',what='',encoding="GB2312",fileEncoding='GB2312')
#将数据字符串化
f_color <-as.character(f_color) 
View(f_color)
# 造工厂
wk<-worker()
word_color<-c('朱红色','粉红色','粉色','玫红','樱桃红','西瓜红','姨妈色','玫紫','经典红','胭脂红','豆沙色','橘子红','哑光',
              '草莓红','酒红色','玫瑰红','暖橙红','中国红','砖红色','复古红','双色','小样','PBF','M0','80','NO.',
              'SE0','RD0','CR0','PK0','OR','PBG','豆沙红','姨妈','复合','CR','水光','MHS','裸红','RD','CR','1#',
              '2#','3#','4#','5#','6#','7#','8#','9#','10#','11#','12#','13#','14#','15#','#1','#2','#3','#4','#5',
              '#6','#7','#8','#9','#10','#11','#12','#13','#14','#15','#20','#30','#40','#50','#60','#70','#80','#90','M0')
# 使用new_user_word函数
# R语言worker()参数user不起作用
new_user_word(wk,word_color)
seg_color<-wk[f_color]

#统计词频
seg_color <- table(seg_color)
#去除单个没有意义的字：‘色’
seg_color <- seg_color[!grepl('色',names(seg_color))] 
#去除单个没有意义的字：‘号’
seg_color <- seg_color[!grepl('号',names(seg_color))] 
#去除NA
seg_color <- seg_color[!grepl('NA',names(seg_color))] 
# seg_color
#查看处理完后剩余的词数
length(seg_color) 
#降序排序，并提取出现次数最多的前100个词语
seg_color<- sort(seg_color, decreasing = TRUE)[1:100]
#查看100个词频最高的
seg_color 
View(seg_color)
data_color=data.frame(seg_color)
# 制作词云
wordcloud(data_color$seg_color , data_color$Freq, colors = rainbow(100), random.order=F)

# ######################################################
# 查看喜欢的功效词云
# ######################################################
x_save<-data[,10]
x_save<-data.frame(x_save)
View(x_save)
nrow(x_save)
# 导出 .txt 数据格式
# 这里的col.names=F是不保存列名，col.names=T 为保存列名
# 不输出列名、行名:col.names = FALSE,row.names = FALSE
# 代表字符串的双引号——加参数：quote = FALSE
# 空格分隔
write.table(x_save, file = "effect_ciyun.txt",sep ="\t",
            col.name=F,row.names = F, quote = F) 

# 读文件
effect_text<-read.table("effect_ciyun.txt",head=FALSE,sep="\t")
View(effect_text)

#读入数据分隔符是‘\n’，字符编码是‘UTF-8’，what=''表示以字符串类型读入
effect_f <- scan('effect_ciyun.txt',sep='\n',what='',encoding="GB2312",fileEncoding='GB2312')
#将数据字符串化
effect_f <-as.character(effect_f) 
# 造工厂
wk<-worker()
word_fenci<-c("易上色","易卸妆","不沾杯","不脱妆","防脱色","非小样唇膏","均匀肤色","防脱妆","不掉色",
              "双色口红","雾面哑光","不脱色","温和卸妆","不粘杯","姨妈色","咬唇妆","提亮肤色","均匀肤色","清爽不油腻",
              "哑光唇","液体唇蜜","粘杯唇膏","唇部去死皮","哑光豆沙色","其他功效","哑光口红","按钮唇膏","唇颊两用",
              "豆沙色","植物成份")
# 使用new_user_word函数
# R语言worker()参数user不起作用
new_user_word(wk,word_fenci)
effect_seg<-wk[effect_f]

#去除字符长度小于2的词语
effect_seg <- effect_seg[nchar(effect_seg)>1] 
#统计词频
effect_seg <- table(effect_seg)
#去除数字
effect_seg <- effect_seg[!grepl('[0-9]+',names(effect_seg))] 
#去除字母
effect_seg <- effect_seg[!grepl('[a-zA-Z]{2}',names(effect_seg))] 
effect_seg
#查看处理完后剩余的词数
length(effect_seg) 
#降序排序，并提取出现次数最多的前100个词语
effect_seg <- sort(effect_seg, decreasing = TRUE)[1:100]
#查看100个词频最高的
effect_seg
View(effect_seg)
data_Effect=data.frame(effect_seg)
# 制作词云
wordcloud(data_Effect$effect_seg , data_Effect$Freq, colors = rainbow(100), random.order=F)

########################################################
# 对sales_num进行分类处理
########################################################
# 查看 总销量的分布图
data3<-data
summary(data3)
nrow(data3)
# 转化为英文字段
names(data3) <- c('name','describe_score','price_score','quality_score',
                  'service_score','price','evaluate_num','sales_num',
                  'color','effect','sunScreen','country','imported')
View(data3)
# 原始数据的统计直方图
hist(data3$sales_num)
# 查看销量的排名
sales_num_sorted<-sort(data3[,8],decreasing = TRUE)
View(sales_num_sorted)
# ###################################################
# 对数据进行分级处理
# 将数据分为A/B/C三个等级
# 设置中间变量对处理后的向量进行临时存储
grade=0
num_sales=nrow(data3)
for(i in 1:num_sales){
  if(data3[i,8]>100){
    grade[i]="A"
  }
  else if(data3[i,8]>10){
    grade[i]="B"
  }
  else{
    grade[i]="C"
  }
}
# 将字符型变量转化为含有因子的变量并复制给数据集data3
grade
data3[,8]=factor(grade)
# 查看三个等级数据的数量
summary(data3$sales_num)
# A   B   C 
# 467 566 507 
View(data3)

############################################################
# 文本处理：功效
#############################################################
#读入数据分隔符是‘\n’，字符编码是‘UTF-8’，what=''表示以字符串类型读入
text_deal<-scan('effect_ciyun.txt',sep='\n',what='',encoding="GB2312",fileEncoding='GB2312')
# 将数据字符串化
text_deal<-as.character(text_deal)
text_deal<-data.frame(text_deal)
# which(!complete.cases(text_deal))
# # 把缺失的记录全部删除
# text_deal2<-na.omit(text_deal)
# View(data3)
# nrow(data3)
# text_deal<-data3
n=sum(is.na(text_deal))
n
View(text_deal)
# 判断行数
num_text<-nrow(text_deal)
# 设置中间变量对处理后的向量进行临时存储
weight_save=0
# 计算权重函数
value_weight=function(x){
  temp=0
  if(grepl("易上色",x)==TRUE){
    temp=temp+4
  }
  if(grepl("滋润",x)==TRUE){
    temp=temp+5
  }
  if(grepl("保湿",x)==TRUE){
    temp=temp+5
  }
  if(grepl("防脱色",x)==TRUE){
    temp=temp+4
  }
  if(grepl("易卸妆",x)==TRUE){
    temp=temp+4
  }
  if(grepl("补水",x)==TRUE){
    temp=temp+5
  }
  if(grepl("温和卸妆",x)==TRUE){
    temp=temp+5
  }
  if(grepl("均匀肤色",x)==TRUE){
    temp=temp+4
  }
  if(grepl("不掉色",x)==TRUE){
    temp=temp+3
  }
  if(grepl("防水",x)==TRUE){
    temp=temp+4
  }
  if(grepl("不沾杯",x)==TRUE){
    temp=temp+3
  }
  if(grepl("哑光",x)==TRUE){
    temp=temp+2
  }
  if(grepl("咬唇妆",x)==TRUE){
    temp=temp+4
  }
  if(grepl("不脱色",x)==TRUE){
    temp=temp+3
  }
  if(grepl("提亮肤色",x)==TRUE){
    temp=temp+5
  }
  if(grepl("持久",x)==TRUE){
    temp=temp+4
  }
  if(grepl("防晒",x)==TRUE){
    temp=temp+3
  }
  if(grepl("其他功能",x)==TRUE){
    temp=temp+1
  }
  return(temp)
}

# 依次计算每条记录的权重
for(i in 1:num_text){
  weight_save[i]=value_weight(text_deal[i,])
}
weight_save
# 赋予权重
data3[,10]<-weight_save
# 查看信息
summary(data3[,10])
View(data3)
View(data3[,10])

########################################################
# 数据归一化方法: 最大最小值方法
########################################################
scale_norma=function(x){
  # 提取预处理样本集中特征变量个数
  ncol=dim(x)[2]
  # 提取预处理样本的样本总量
  nrow=dim(x)[1]
  # 建立用于保存新样本集的矩阵
  new=matrix(0,nrow,ncol)
  
  for(i in 1:ncol){
    # 提取每个变量的最大值
    max=max(x[,i])
    # 提取每个变量的最小值
    min=min(x[,i])
    # 对每个变量的所有样本数据进行归一化处理
    for(j in 1:nrow){
      new[j,i]=(x[j,i]-min)/(max-min)
    }
  }
  new
}

# data9<-data3
# 进行归一化
data3[,c(2:7,10)]<-scale_norma(data3[,c(2:7,10)])
View(data3)


########################################################
# 文件操作
#######################################################
# 写入文件
write.csv(data3[,-9],file = "mydata_clean.csv",row.names = F)
# 读文件
data_handle2<-read.table("mydata_clean.csv",header=T, sep=",")
data3<-data_handle2
View(data3)


##################################################################
# 数据抽样，生成训练集和测试集
###################################################################
# 没有缺失值的数据
View(data3)
# 对数据进行分组抽样
set.seed(50)
num_sales=nrow(data3)
num_sales
# 分层抽样：sampling ，3:1的规则，
library(sampling)
# 保存分组抽取的个数
a=round(1/4*sum(data3$sales_num=="A"))
b=round(1/4*sum(data3$sales_num=="B"))
c=round(1/4*sum(data3$sales_num=="C"))
# 查看分布
a;b;c;
# 分层抽样
samp=strata(data3,stratanames = "sales_num",size=c(a,b,c),method="srswor")
# View(samp)
# 生成训练集
Train_data=data3[-samp$ID_unit,]
# 生成测试集
Test_data=data3[samp$ID_unit,]
# 查看训练集和测试集的个数
nrow(Train_data);nrow(Test_data)
# 1154 386
# 查看维度
dim(Train_data);dim(Test_data)
# 查看抽取样本的信息
View(Train_data)
View(Test_data)
# head(Train_data3)
# 提取训练集和测试集的行号
Train_data_num<-sample(1:nrow(Train_data),nrow(Train_data))
Test_data_num<-sample(1:nrow(Test_data),nrow(Test_data))
# 1540--1154--386
View(Train_data_num)
View(Test_data_num)


####################################################
# 模型构建：判别分析----朴素贝叶斯
#####################################################
library(klaR)
# 模型训练
# 对样本进行预处理，去掉 店名name 
data6<-data3[Train_data_num,-1]
View(data6)
# 生成判别规则
data6_Bayes<-NaiveBayes(sales_num~.,data6)
# names(data6_Bayes)
# data6_Bayes$tables
# data6_Bayes$levels
# data6_Bayes$call
# data6_Bayes$usekernel
# data6_Bayes$varnames

############################################################
# 各类别下变量密度可视化
# 'sunScreen','country','imported'
# 对是否防晒各类别绘制密度图
plot(data6_Bayes,vars="describe_score",main="describe_score--密度图",n=20,lwd = 2,col=c("DeepPink","#D55E00","DarkTurquoise","#0033FF","#000000","#009900")) 
# 对价格各类别绘制密度图
plot(data6_Bayes,vars = "price",main="price--密度图",n=50,lwd = 2,col = c("DeepPink","#D55E00","DarkTurquoise","#0033FF","#000000","#009900")) 
# 对是否进口各类别绘制密度图
plot(data6_Bayes,vars = "imported",main="imported--密度图",n=50,col = c("DeepPink","#D55E00","RDarkTurquoise","#0033FF","#000000","#009900")) 

# #############################################
# 预测
# 准备测试集
data7<-data3[Test_data_num,-1]
View(data7)
pred_Bayes<-predict(data6_Bayes,data7)
# pred_Bayes
# 混淆矩阵
table(data7$sales_num,pred_Bayes$class)

# 计算贝叶斯判别预测错误概率
error_Bayes<-sum(as.numeric(as.numeric(pred_Bayes$class)
                            !=as.numeric(data7$sales_num)))/nrow(data7)

# 不满足各变量之间的独立条件
error_Bayes
# 0.5310881
library(ggplot2)
library(corrplot)
cor_Bayes<-cor(data3[,c(2:7,9)])
corrplot(cor_Bayes,method="number")
corrplot(cor_Bayes,method="pie")

################################################################
# 模型构建：集成学习
################################################################
library(adabag)
library(rpart)

# 模型训练
# 对样本进行预处理,训练集
data4<-data3[Train_data_num,-1]
View(data4)
# 模型构建
boost=boosting(sales_num~.,data4,boos = TRUE,mfinal = 500)
# print(boost)

# 查看boost所生成的输出项名称
names(boost)
# [1] "formula"    "trees"      "weights"    "votes"      "prob"       "class"      "importance"
# [8] "terms"      "call" 
# 查看树的构成
boost$trees[20]
# 投票情况
boost$votes[200:215,]
# 预测类别
boost$class[200:215]
# "A" "B" "C" "B" "C" "B" "C" "B" "C" "A" "A" "C" "A" "A" "A" "B"
# 模型boost各输入变量的相对重要性
sort(boost$importance,decreasing = TRUE)

# 通过control参数控制基分类树的复杂度
boost2=boosting(sales_num~.,data4,boos = TRUE,mfinal = 500,
                control = rpart.control(maxdepth = 7))
# 查看树的构成
boost2$trees[20]
################################################
# 预测
data5<-data3[Test_data_num,-1]
# View(data5)
# 预测
pred_boost=predict(boost2,data5)
# 查看测试集的混淆矩阵
pred_boost$confusion
#                 Observed Class
# Predicted Class   A   B   C
#               A 209  18   5
#               B  16  74  15
#               C   3  12  34

# 查看测试集的错误率
pred_boost$error
# 0.1787565
# (p=sum(as.numeric(pre_data3!=Test_data$sales_num))/nrow(Test_data))
pred_boost$class<-as.factor(pred_boost$class)
error_boost<-sum(as.numeric(as.numeric(pred_boost$class)
                            !=as.numeric(data5$sales_num)))/nrow(data5)

error_boost
# 0.1787565

############################################################
# 模型构建：随机森林的判别模型
############################################################
# 把缺失的记录全部删除
# data2<-na.omit(data3)
# View(data2)
# md_data2=md.pattern(data2)
# fix(md_data2)

# data3<-data2[,-c(9,10)]
# View(data3)

# 查看变量的重要值
library(randomForest)
set.seed(50)
num_sales=nrow(data3)

set.seed(111)
# 总销量数：sales_num
# 构建决策树为500棵的随机森林模型
data.rf=randomForest(sales_num~.-name,data=data3,ntree=500,
                     importance=TRUE,proximity=TRUE,subset=Train_data_num)
# 展示所构建的随机森林模型
print(data.rf)
# OOB estimate of  error rate: 29.12%

# 提取随机森林模型中的重要值
importance(data.rf)
# 提取随机森林中以第一种度量标准得到的重要值
# importance(data.rf,type=1)
# 对重要值进行排序并取值
data.rf_importance<-data.frame(sort(importance(data.rf,type=1)[,1],decreasing = TRUE))
names(data.rf_importance)<-'importance_sorted'
data.rf_importance
# 调用varlmPlot函数绘制变量重要性曲线
varImpPlot(data.rf)
########################################################
# 随机森林：模型优化
# 自变量的个数，出去店的名字
num_var=ncol(data3)-1
num_var
# 设置模型误判率向量的初始值
rate=1
# 依次逐个增加节点所选变量个数
for(i in 1:num_var){
  set.seed(200)
  # mtry:用来决定随机森林中决策树的每次分支时所选的变量个数
  model=randomForest(sales_num~.-name,data=data3,mtry=i,importance=TRUE,
                     ntree=1000,proximity=TRUE,subset=Train_data_num)
  # 计算基于OOB数据的模型误判率均值
  rate[i]=mean(model$err.rate)
  # 展示模型简要信息
  print(model)
}
# 展示所有模型误判率的均值
rate
# 选择节点数为的作为模型的优化
set.seed(222)
model=randomForest(sales_num~.-name,data=data3,mtry=5,importance=TRUE,ntree=1000,
                   proximity=TRUE,subset=Train_data_num)
# 绘制模型误差与决策树数量的关系图
plot(model,col=1:6)

# 为图像添加图例topright
legend(800,0.180,"A",cex=0.9,bty="n")
legend(800,0.352,"B",cex=0.9,bty="n")
legend(800,0.394,"C",cex=0.9,bty="n")

# 当决策树大于之后，模型趋于稳定，选择决策树的数量为，进行优化
set.seed(230)
model2=randomForest(sales_num~.-name,data=data3,mtry=5,importance=TRUE,ntree=400,
                    proximity=TRUE,subset=Train_data_num)
# 展示模型的简要信息
print(model2)
# 绘制相应的柱状图，展示随机森林模型中每棵决策树的节点数
hist(treesize(model2))
# 随机森林模型的可视化,注意，要有一个临近矩阵
# 不同类别，使用的符号不一样
MDSplot(data.rf,data3$sales_num,palette = rep(1,6),pch=as.numeric(data3$sales_num))
varImpPlot(model2)

#################################################
# 对测试集进行目标变量预测
#################################################
pre_data3=predict(model2,Test_data,type="class")
# 显示预测结果
pre_data3
# 获取混淆矩阵
table(Test_data$sales_num,pre_data3)
# 计算错误
error_randomForest=sum(as.numeric(pre_data3!=Test_data$sales_num))/nrow(Test_data)
error_randomForest
# 错误率为: 0.06994819





