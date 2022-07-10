#########设置工作目录及基本环境##########

setwd("E:/运动水平")
rm(list = ls()) #1.清理工作环境
aa<- load("aa.RData") #读入数据

#########引用分析包##########
library(nhanesA)
library(tidyverse)
library(knitr)
library(tableone)

#########分析数据划分等级##########

#取3分位法，或者4分位置法，该为3分位
quantile(selectdata[,22],c(0.3333,0.6666))


#某个变量值分等级
#1.例1
bu<-selectdata$lead
selectdata$lead3[bu<0.96]<-"Tertile1"
selectdata$lead3[bu>=0.96&bu<1.58]<-"Tertile2"
selectdata$lead3[bu>=1.58]<-"Tertile3"

#1.例2
bu<-selectdata$HDL
selectdata$HDL2[bu<=49]<-"HDL ≤ 49 (mg/dl)"
selectdata$HDL2[bu>49]<-"HDL > 49 (mg/dl)"




#########数据因子转换##########


head(aa)
str(aa) #查看数据数据性质


summary(aa$Race)
summary(aa$Education)


levels(aa$Race)[levels(aa$Race)=="Non-Hispanic Asian"] <- "Other Races"
levels(aa$Race)[levels(aa$Race)=="Other Race - Including Multi-Rac"] <- "Other Races"
levels(aa$Education)[levels(aa$Education)=="Don't Know"] <- "<High school"
levels(aa$Education)[levels(aa$Education)=="Less than 9th grade"] <- "<High school"
levels(aa$Education)[levels(aa$Education)=="9-11th grade (Includes 12th grad"] <- "High school"
levels(aa$Education)[levels(aa$Education)=="High school graduate/GED or equi"] <- "High school"

levels(aa$Education)[levels(aa$Education)=="Some college or AA degree"] <- ">High school"
levels(aa$Education)[levels(aa$Education)=="College graduate or above"] <- ">High school"




names(aa) #提取变量的名字
shapiro.test(aa$Age) #p＞0.05才符合正态分布
shapiro.test(aa$lead) #p＞0.05才符合正态分布


#输入Table 1的条件
#条件1# myVars的（）中输入想要在Table 1出现的变量【英文引号以英文逗号隔开】


myVars <-c (names(aa)[2:24])  #变量

#条件2# catVars的（）内指明上述中哪些是分类变量

catVars <- c("Gender","Race","Education","Smoking","Diabetes","hipertension","lead3")#14个分类变量

#条件3# tableone包默认输出均数+标准差，所以nonvar的（）输入那些连续变量是非正态分布的（输出中位数+四分位数）


nonvar <- c("Age")  # 指定哪些变量是非正态分布变量




#5、构建Table 函数 CreateTableOne（）
table<- CreateTableOne(vars = myVars,#条件1
                       factorVars = catVars,#条件2
                       data = aa, #源数据
                       addOverall = TRUE) #增加overall列
#6、输出结果

table1 <- print(table, #构建的table函数（包括条件1.2）
                showAllLevels=TRUE, #显示所有变量
                nonnormal = nonvar) #条件3
#===============================================================================
#三、tableone包制作Table 1（4列）

#5、构建Table函数

##构建table 函数，加入条件4，strata = " " 。英文引号内填入需要分列的变量，例如本研究想探索放疗对预后的影响，则为strata = "rt"

table <- CreateTableOne(vars = myVars, #条件1
                          factorVars = catVars,#条件2
                          strata = "lead3", #条件4
                          data = aa, #原始数据
                        addOverall = TRUE
);table




###条件3不变 指定非正态分布连续变量变量

nonvar <- c("Age")   
#条件5新加入 假如有T<5变量应使用Fisher精确检验，本文数量大，无需Fisher精确检验

exactvars <- c("a", "b")
#附加细节：catDigits = 2, contDigits = 3, pDigits = 4,修改连续变量小数位数为2位,分类变量百分比位数为3位,调整小数位数为4位；

#6、输出结果

#把构建的table+条件1-5+附加细节条件放入print（）函数

table1<- print(table, #构建的table函数（带条件1.2.3）
               #nonnormal = nonvar,#条件4
               #exact = exactvars, #条件5
               catDigits = 2,contDigits = 3,pDigits = 4, #附加条件
               showAllLevels=TRUE, #显示所有变量
               quote = FALSE, # 不显示引号
               noSpaces = TRUE, # #删除用于对齐的空格
               printToggle = TRUE
               ) #展示输出结果*`
write.csv(table1, file = "table1.csv")
 
