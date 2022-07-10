#########设置工作目录##########

setwd("E:/运动水平")

rm(list = ls()) #1.清理工作环境


#########引用分析包##########
library(nhanesA)
library(tidyverse)
library(knitr)


#########定义函数##########
downloadtranslate<-function(x)
{
  xdata<-nhanes(x)
  xvars<-names(xdata)
  y<-nhanesTranslate(x,xvars,data = xdata)
  return(y)
}
#########设定筛选文件及关键词##########

dllist <- list("HDL_H",'DEMO_H',"DIQ_H","BPQ_H","BMX_H","BPX_H")

dllist <-rownames(read.csv(file = ,header = TRUE))
varlist <- c("SEQN","LBDHDD","LBDHDDSI","SDDSRVYR","RIDSTATR")


#########下载翻译合并##########
 

mergedata<-downloadtranslate(dllist[1])##下载设置初始融合数据

mergedata <- right_join(mergedata,downloadtranslate(dllist[2]),by="SEQN")

mergedata <- right_join(mergedata,downloadtranslate(dllist[3]),by="SEQN")

mergedata <- right_join(mergedata,downloadtranslate(dllist[4]),by="SEQN")

mergedata <- right_join(mergedata,downloadtranslate(dllist[5]),by="SEQN")

mergedata <- right_join(mergedata,downloadtranslate(dllist[6]),by="SEQN")

#########筛选关键变量并重命名列名##########
#1.筛选研究变量
usedata <- mergedata[,colnames(mergedata)%in% varlist]

#1.重命名研究变量
usedata <- rename(usedata, c(RIAGENDR = "Gender"
                          ,RIDAGEYR ="Age"
                          ,RIDRETH3 = "Race"
                          ,DMDEDUC2 = "Education"
                          ,SMQ020 = "Smoking"
                          ,DIQ010='Diabetes'
                          ,BPQ020 ='hipertension'
                          ,BMXBMI ='BMI' 
                          ,BPXSY1 = 'SBP'
                          ,BPXDI1 = 'DBP'
                          ,LBDSCRSI ='Creatinine'
                          ,LBDSUASI ='Uricacid'
                          ,LBXSASSI = 'AST'
                          ,LBXSATSI = 'ALT'
                          ,LBDSPHSI = 'Phosphorus'
                          ,LBDSCASI = 'calcium'
                          ,LBDB12SI = 'B12'
                          ,LBDTCSI = 'Cholesterol'
                          ,LBXVIDMS = 'VD'
                          ,LBXBCD = 'cadmium'
                          ,LBXBPB = 'lead'
                          ,DXXAAC24 ="AAC"
                          ,LBDHDD="HDL"
))

#########清洗数据##########

selectdata <-usedata %>%drop_na(lead,AAC,HDL)#删除关键变量缺失值数据

selectdata<-selectdata[(selectdata$Age>40),]#筛选大于，小于，或者非等于数据
selectdata<-selectdata[(selectdata$Age==40),]#筛选大于，小于，或者非等于数据


selectdata<-subset(selectdata,LBDHDD != 10)#筛选不等于某值法，但是NA值自动筛除

selectdata<-selectdata[-which(selectdata$LBDHDD == 18),]#筛选不等于某值法，但是NA值不筛除

#########确定下阶段分析数据##########

aa <- selectdata

#########存储原始数据##########
save(usedata,file="usedata.Rdata")
write.csv(usedata,'usedata.txt',sep='\t')

save(mergedata,file="mergedata.Rdata")
write.csv(usedata,'mergedata.txt',sep='\t')

save(selectdata,file="selectdata.Rdata")
write.csv(usedata,'selectdata.txt',sep='\t')

save(aa,file="aa.Rdata")
write.csv(usedata,'aa.txt',sep='\t')











