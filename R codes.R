##############################
###### 1. Preprocessing ######
##############################

#1.A Downloading Data and Packages

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(dslabs)
library(readxl)
library(ggpubr)

################################
#Attribute Information:
  
#This research employed a binary variable, default payment (Yes = 1, No = 0), as the response variable. This study reviewed the literature and used the following 23 variables as explanatory variables: 
#X1: Amount of the given credit (NT dollar): it includes both the individual consumer credit and his/her family (supplementary) credit. 
#X2: Gender (1 = male; 2 = female). 
#X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others). 
#X4: Marital status (1 = married; 2 = single; 3 = others). 
#X5: Age (year). 
#X6 - X11: History of past payment. We tracked the past monthly payment records (from April to September, 2005) as follows: X6 = the repayment status in September, 2005; X7 = the repayment status in August, 2005; . . .;X11 = the repayment status in April, 2005. The measurement scale for the repayment status is: -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months; . . .; 8 = payment delay for eight months; 9 = payment delay for nine months and above. 
#X12-X17: Amount of bill statement (NT dollar). X12 = amount of bill statement in September, 2005; X13 = amount of bill statement in August, 2005; . . .; X17 = amount of bill statement in April, 2005. 
#X18-X23: Amount of previous payment (NT dollar). X18 = amount paid in September, 2005; X19 = amount paid in August, 2005; . . .;X23 = amount paid in April, 2005. 
################################

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00350/default%20of%20credit%20card%20clients.xls", dl, cacheOK=TRUE)


#1.B Importing Data into data frame 
library(readxl)
xlsx_example <- read_excel(dl, range = cell_rows(c(2, NA)))
df = as.data.frame(xlsx_example)



#1.C Changing columns from numeric to factors
sapply(df, class)
df$ID<-as.factor(df$ID)
df$SEX<-as.factor(df$SEX)
df$EDUCATION<-as.factor(df$EDUCATION)
df$MARRIAGE<-as.factor(df$MARRIAGE)
df$PAY_0<-as.factor(df$PAY_0)
df$PAY_2<-as.factor(df$PAY_2)
df$PAY_3<-as.factor(df$PAY_3)
df$PAY_4<-as.factor(df$PAY_4)
df$PAY_5<-as.factor(df$PAY_5)
df$PAY_6<-as.factor(df$PAY_6)
names(df)[25]<-"default"
df$default<-as.factor(df$default)
sapply(df, class)

#1.D partitioning the dateset into test set and train set
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = df$default, times = 1, p = 0.1, list = FALSE)
train <- df[-test_index,]
test <- df[test_index,]

##################################
##### 2. Basic Investigation #####
##################################

#2.A Calculating Basic Statistics
summary(train)
nrow(train)
total_default<-sum(train$default==1) # total default number
total_n_default<-sum(train$default==0) #total non-default number
total_pop<-total_default+total_n_default #total population
default_prop<-total_default/(total_default+total_n_default) #ratio of default
default_prop

#2.B Identifying and Investigating Undocumented Data
train%>% group_by(EDUCATION) %>% summarize(n=n(), Q_1=quantile(LIMIT_BAL,0.25), median = median(LIMIT_BAL), mean = mean(LIMIT_BAL), Q_3=quantile(LIMIT_BAL,0.75)) #No clear pattern for the undocuemnted Education
train%>% group_by(MARRIAGE) %>% summarize(n=n(), Q_1=quantile(LIMIT_BAL,0.25), median = median(LIMIT_BAL), mean = mean(LIMIT_BAL), Q_3=quantile(LIMIT_BAL,0.75))#No clear pattern for the undocuemnted Education
barchart(train$PAY_0, ylab="PAY_0", col ="cornflowerblue")
barchart(train$PAY_2, ylab="PAY_2", col ="cornflowerblue")

table(train$PAY_0, train$PAY_2)
table(train$PAY_2, train$PAY_3)
#status 0 and 2 are not documented
#It appears most -2 are coming from -2 or -1, except 3 cases where it jumps from 2.  

train%>%filter(ID%in%c(110,122, 143, 149,574))

#ID 110 seems to suggest BILL_AMT_x corresponds to PAY_AMT_x+1  
#ID 143 and 149 Pay status can jump from 0 to 2, which as said before should be against the explanation of documentation
#ID 149 Pay status can descrease from 3 to 2 and 2 to 1, which is against the explanation in the documentation
#ID 122 suggests the person can default even if BILL_AMT1 =0.
#ID 574 seems to suggest PAY_4 relates to BILL_AMT3 and PAY_AMT4

train%>%filter(BILL_AMT4!=0)%>%mutate(ratio=PAY_AMT3/BILL_AMT4)%>%group_by(PAY_3)%>% summarise(median=median(ratio),mean=mean(ratio), Q_10=quantile(ratio,0.1),Q_25=quantile(ratio,0.25),Q_75=quantile(ratio,0.75))
#this shows generally status 0 pays at least certain minimal balance of the bill and is better than 1


#2.C Check Independence
chisq.test(train$SEX, train$default)$p.value
chisq.test(train$SEX, train$EDUCATION)$p.value
chisq.test(train$SEX, train$MARRIAGE)$p.value
chisq.test(train$SEX, train$AGE)$p.value
chisq.test(train$SEX, train$PAY_0)$p.value
chisq.test(train$SEX, train$LIMIT_BAL)$p.value
chisq.test(train$EDUCATION, train$PAY_0)$p.value
chisq.test(train$EDUCATION, train$MARRIAGE)$p.value
chisq.test(train$PAY_0, train$PAY_2)$p.value


############################
##### 3. Visualisation #####
############################
# This section requires to ggpubr library
library(ggpubr)

#3.A Charting Demographic Data vs Default
#3.A.i SEX vs default
sex_vs_default<-train%>% select(SEX, default) %>% group_by(SEX) %>% mutate(n2=n()) %>% ungroup() %>% group_by(SEX, default) %>% summarize(prop=n()/mean(n2), n= mean(n2)/total_pop)
chart_sex<-ggplot(sex_vs_default) +
  geom_col(aes(x = SEX, y = prop, fill = default), width=0.5)+ 
  geom_line(aes(x=SEX, y=n, group=1))+
  geom_point(aes(x=SEX, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different sex") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.A.ii EDUCATION vs default
edu_vs_default<-train%>% select(EDUCATION, default) %>% group_by(EDUCATION) %>% mutate(n2=n()) %>% ungroup() %>% group_by(EDUCATION, default) %>% summarize(prop=n()/mean(n2), n= mean(n2)/total_pop)
chart_edu<-ggplot(edu_vs_default) +
  geom_col(aes(x = EDUCATION, y = prop, fill = default), width=0.5)+ 
  geom_line(aes(x=EDUCATION, y=n, group=1))+
  geom_point(aes(x=EDUCATION, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different sex") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  scale_x_discrete(breaks=c("0", "1","2", "3", "4", "5", "6"), labels=c("Unknown_0", "graduate school", "university", "high school", "others", "unknown_5", "unknown_6"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.A.iii MARRIAGE vs default
marriage_vs_default<-train%>% select(MARRIAGE, default) %>% group_by(MARRIAGE) %>% mutate(n2=n()) %>% ungroup() %>% group_by(MARRIAGE, default) %>% summarize(prop=n()/mean(n2), n= mean(n2)/total_pop)
chart_marriage<-ggplot(marriage_vs_default) +
  geom_col(aes(x = MARRIAGE, y = prop, fill = default), width=0.5)+ 
  geom_line(aes(x=MARRIAGE, y=n, group=1))+
  geom_point(aes(x=MARRIAGE, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different sex") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  scale_x_discrete(breaks=c("0", "1","2", "3"), labels=c("unknown_0", "married", "single", "others"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.A.iv Charting Age vs Default
age_vs_default<-train%>% select(AGE, default) %>% group_by(AGE) %>% mutate(n2=n()) %>% ungroup() %>% group_by(AGE, default) %>% summarize(prop=n()/mean(n2), n= mean(n2)/total_pop)
chart_age<-ggplot(age_vs_default) +
  geom_col(aes(x = AGE, y = prop, fill = default), width=0.5)+ 
  geom_line(aes(x=AGE, y=7*n, group=1))+
  geom_point(aes(x=AGE, y=7*n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different Age") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/7 , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")+ 
  xlim(20, 80)

#3.A.v Combine Chart(i) (may need to load twice depending on computing power)
chart_personal<-ggarrange(ggarrange(chart_sex, chart_marriage, ncol = 2,nrow=1, align = "h",common.legend = TRUE, legend = "bottom"), chart_edu, chart_age,ncol=1, nrow = 3, 
  align = "v",common.legend = TRUE, legend = "bottom")

annotate_figure(chart_personal,
                top = text_grob("Default status \nacross Demographical Data", color = "red", face = "bold", size = 30),
                left = text_grob("Default Proportion",  rot = 90, size =20),
                right = text_grob("Population",  size =20),
)

#3.A.vi Combine Chart(ii) (may need to load twice depending on computing power)
personal_vs_default<-train%>% select(SEX, EDUCATION, MARRIAGE, default) %>% group_by(SEX, EDUCATION, MARRIAGE) %>% mutate(n2=n()) %>% ungroup() %>% 
  group_by(SEX, EDUCATION, MARRIAGE, default) %>% summarize(prop=n()/mean(n2), n= mean(n2)/total_pop)%>% filter((MARRIAGE==1|MARRIAGE==2)&(EDUCATION==1|EDUCATION==2|EDUCATION==3))

levels(personal_vs_default$MARRIAGE)<-c("unknown_0", "married", "single", "others")
levels(personal_vs_default$EDUCATION)<-c("Unknown_0", "graduate school", "university", "high school", "others", "unknown_5", "unknown_6")

ggplot(personal_vs_default) +
  geom_col(aes(x = SEX, y = prop, fill = default), width=0.5)+ 
  geom_line(aes(x=SEX, y=n, group=1))+
  geom_point(aes(x=SEX, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different sex") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name ="Default Proportion",
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")+
  facet_grid(MARRIAGE ~ EDUCATION)


#3.B Charting Pay status vs Default
#3.B.i PAY_0 vs default
pay0_vs_default<-train%>% select(PAY_0, default) %>% group_by(PAY_0) %>% mutate(n2=n()) %>% ungroup() %>% group_by(PAY_0, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_pay0<- ggplot(pay0_vs_default) +
  geom_col(aes(x = PAY_0, y = prop, fill = default))+ 
  geom_line(aes(x=PAY_0, y=n, group=1))+
  geom_point(aes(x=PAY_0, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.B.ii PAY_2 vs default
pay2_vs_default<- pay2_vs_default<-train%>% select(PAY_2, default) %>% group_by(PAY_2) %>% mutate(n2=n()) %>% ungroup() %>% group_by(PAY_2, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_pay2<-ggplot(pay2_vs_default) +
  geom_col(aes(x = PAY_2, y = prop, fill = default))+ 
  geom_line(aes(x=PAY_2, y=n, group=1))+
  geom_point(aes(x=PAY_2, y=n, group=1))+
    theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_2 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")  

#3.B.iii PAY_3 vs default
pay3_vs_default<-train%>% select(PAY_3, default) %>% group_by(PAY_3) %>% mutate(n2=n()) %>% ungroup() %>% group_by(PAY_3, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_pay3<- ggplot(pay3_vs_default) +
  geom_col(aes(x = PAY_3, y = prop, fill = default))+ 
  geom_line(aes(x=PAY_3, y=n, group=1))+
  geom_point(aes(x=PAY_3, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_3 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")  

#3.B.iv PAY_4 vs default
pay4_vs_default<-train%>% select(PAY_4, default) %>% group_by(PAY_4) %>% mutate(n2=n()) %>% ungroup() %>% group_by(PAY_4, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_pay4<- ggplot(pay4_vs_default) +
  geom_col(aes(x = PAY_4, y = prop, fill = default))+ 
  geom_line(aes(x=PAY_4, y=n, group=1))+
  geom_point(aes(x=PAY_4, y=n, group=1))+
   theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_4 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")  

#3.B.v PAY_5 vs default
pay5_vs_default<-train%>% select(PAY_5, default) %>% group_by(PAY_5) %>% mutate(n2=n()) %>% ungroup() %>% group_by(PAY_5, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_pay5<- ggplot(pay5_vs_default) +
  geom_col(aes(x = PAY_5, y = prop, fill = default))+ 
  geom_line(aes(x=PAY_5, y=n, group=1))+
  geom_point(aes(x=PAY_5, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_5 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")  

#3.B.vi PAY_6 vs default
pay6_vs_default<-train%>% select(PAY_6, default) %>% group_by(PAY_6) %>% mutate(n2=n()) %>% ungroup() %>% group_by(PAY_6, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_pay6<- ggplot(pay6_vs_default) +
  geom_col(aes(x = PAY_6, y = prop, fill = default))+ 
  geom_line(aes(x=PAY_6, y=n, group=1))+
  geom_point(aes(x=PAY_6, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_6 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")  

#3.B.vii Combine Chart (may need to run again depending on the computing power)
chart_pay<-ggarrange(chart_pay0, chart_pay2,chart_pay3, chart_pay4, chart_pay5, chart_pay6, heights = c(2, 2, 2),ncol = 2, nrow = 3, 
          align = "v",common.legend = TRUE, legend = "bottom")

annotate_figure(chart_pay,
                top = text_grob("Default status \nacross different PAY status", color = "red", face = "bold", size = 30),
                left = text_grob("Default Proportion",  size =20, rot = 90),
                right = text_grob("Population",  size =20, rot=270)
)


#3.C Charting LIMIT_BAL vs default
LIMIT_BAL_vs_default<-train%>% select(LIMIT_BAL, default) %>% group_by(LIMIT_BAL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(LIMIT_BAL, default) %>% summarize(prop=n()/mean(n2), n= mean(n2)/total_pop)
ggplot(LIMIT_BAL_vs_default) +
  geom_col(aes(x = LIMIT_BAL, y = prop, fill = default))+ 
  geom_line(aes(x=LIMIT_BAL, y=n*8, group=1))+
  geom_point(aes(x=LIMIT_BAL, y=n*8, group=1))+
  theme(legend.position = "bottom") + 
  labs(title = "Default status \nacross different Limit Balance") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000, 800000),label = c(0, 2, 4, 6, 8), name = "Limit Balance (100k NTD)")+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/8 , name = "Population"))+
   geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
 geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")
 
#3.D Charting Bill Amount vs default
#3.D.i BILL_AMT1 vs default
BILL1_vs_default<-train%>% mutate(BILL=round(BILL_AMT1/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL1<- ggplot(BILL1_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n*2.5, group=1))+
  geom_point(aes(x=BILL, y=n*2.5, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/2.5 , name = "Population"))+
  scale_x_continuous(limits = c(0, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.D.ii BILL_AMT2 vs default
BILL2_vs_default<-train%>% mutate(BILL=round(BILL_AMT2/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL2<- ggplot(BILL2_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n*2.5, group=1))+
  geom_point(aes(x=BILL, y=n*2.5, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/2.5 , name = "Population"))+
  scale_x_continuous(limits = c(0, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.D.iii BILL_AMT3 vs default
BILL3_vs_default<-train%>% mutate(BILL=round(BILL_AMT3/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL3<- ggplot(BILL3_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n*2.5, group=1))+
  geom_point(aes(x=BILL, y=n*2.5, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/2.5 , name = "Population"))+
  scale_x_continuous(limits = c(0, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.D.iv BILL_AMT4 vs default
BILL4_vs_default<-train%>% mutate(BILL=round(BILL_AMT4/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL4<- ggplot(BILL4_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n*2.5, group=1))+
  geom_point(aes(x=BILL, y=n*2.5, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/2.5 , name = "Population"))+
  scale_x_continuous(limits = c(0, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.D.v BILL_AMT5 vs default
BILL5_vs_default<-train%>% mutate(BILL=round(BILL_AMT5/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL5<- ggplot(BILL5_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n*2.5, group=1))+
  geom_point(aes(x=BILL, y=n*2.5, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/2.5 , name = "Population"))+
  scale_x_continuous(limits = c(0, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.D.v BILL_AMT6 vs default
BILL6_vs_default<-train%>% mutate(BILL=round(BILL_AMT6/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL6<- ggplot(BILL6_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n*2.5, group=1))+
  geom_point(aes(x=BILL, y=n*2.5, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop/2.5 , name = "Population"))+
  scale_x_continuous(limits = c(0, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")



#3.D.v Combine BILL_AMT chart (may need to run again depending on the computing power)
chart_bill<-ggarrange(chart_BILL1, chart_BILL2,chart_BILL3, chart_BILL4, chart_BILL5, chart_BILL6, labels=c("Bill_1","Bill_2","Bill_3","Bill_4","Bill_5","Bill_6" ),heights = c(2, 2, 2),ncol = 2, nrow = 3, 
                     align = "v",common.legend = TRUE, legend = "bottom")
annotate_figure(chart_bill,
                top = text_grob("Default status \nacross different Bill Amount", color = "red", face = "bold", size = 30),
                left = text_grob("Default Proportion",  size =20, rot = 90),
                right = text_grob("Population",  size =20, rot=270),
                bottom = text_grob("Bill Amount (NTD 10k)", color = "blue", face = "bold", size = 25),
)


#3.E Charting Pay Amount vs default
#3.E.i PAY_AMT1 vs default
PAYAMT1_vs_default<-train%>% mutate(BILL=round(PAY_AMT1/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_PAYAMT1<- ggplot(PAYAMT1_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(limits = c(-5, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.E.ii PAY_AMT2 vs default
PAYAMT2_vs_default<-train%>% mutate(BILL=round(PAY_AMT2/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_PAYAMT2<- ggplot(PAYAMT2_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(limits = c(-5, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.E.iii PAY_AMT3 vs default
PAYAMT3_vs_default<-train%>% mutate(BILL=round(PAY_AMT3/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_PAYAMT3<- ggplot(PAYAMT3_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(limits = c(-5, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.E.iv PAY_AMT4 vs default
PAYAMT4_vs_default<-train%>% mutate(BILL=round(PAY_AMT4/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_PAYAMT4<- ggplot(PAYAMT4_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(limits = c(-5, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.E.v PAY_AMT5 vs default
PAYAMT5_vs_default<-train%>% mutate(BILL=round(PAY_AMT5/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_PAYAMT5<- ggplot(PAYAMT5_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(limits = c(-5, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.E.vi PAY_AMT6 vs default
PAYAMT6_vs_default<-train%>% mutate(BILL=round(PAY_AMT6/10000,0)) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_PAYAMT6<- ggplot(PAYAMT6_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(limits = c(-5, 60))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")


#3.E.vii Combine PAY_AMT chart (may need to run again depending on the computing power)
chart_payamt<-ggarrange(chart_PAYAMT1, chart_PAYAMT2,chart_PAYAMT3, chart_PAYAMT4, chart_PAYAMT5, chart_PAYAMT6, labels=c("Pay_1","Pay_2","Pay_3","Pay_4","Pay_5","Pay_6" ),heights = c(2, 2, 2),ncol = 2, nrow = 3, 
                      align = "v",common.legend = TRUE, legend = "bottom")
annotate_figure(chart_payamt,
                top = text_grob("Default status \nacross different Pay Amount", color = "red", face = "bold", size = 30),
                left = text_grob("Default Proportion",  size =20, rot = 90),
                right = text_grob("Population",  size =20, rot=270),
                bottom = text_grob("Pay Amount (NTD 10k)", color = "blue", face = "bold", size = 25),
)



#3.F Charting Pay Credit Limit ratio vs default
#3.F.i BILL_AMT1 LIMIT_BAL ratio vs default
BILL1Limit_vs_default<-train%>% mutate(BILL=round(BILL_AMT1/LIMIT_BAL/0.05)*0.05) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL1Limit<- ggplot(BILL1Limit_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 2))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.F.ii BILL_AMT2 LIMIT_BAL ratio vs default
BILL2Limit_vs_default<-train%>% mutate(BILL=round(BILL_AMT2/LIMIT_BAL/0.05)*0.05) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL2Limit<- ggplot(BILL2Limit_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 2))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.F.iii BILL_AMT3 LIMIT_BAL ratio vs default
BILL3Limit_vs_default<-train%>% mutate(BILL=round(BILL_AMT3/LIMIT_BAL/0.05)*0.05) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL3Limit<- ggplot(BILL3Limit_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 2))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.F.iv BILL_AMT4 LIMIT_BAL ratio vs default
BILL4Limit_vs_default<-train%>% mutate(BILL=round(BILL_AMT4/LIMIT_BAL/0.05)*0.05) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL4Limit<- ggplot(BILL4Limit_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 2))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.F.v BILL_AMT5 LIMIT_BAL ratio vs default
BILL5Limit_vs_default<-train%>% mutate(BILL=round(BILL_AMT5/LIMIT_BAL/0.05)*0.05) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL5Limit<- ggplot(BILL5Limit_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 2))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")

#3.F.v BILL_AMT6 LIMIT_BAL ratio vs default
BILL6Limit_vs_default<-train%>% mutate(BILL=round(BILL_AMT6/LIMIT_BAL/0.05)*0.05) %>% select(BILL, default) %>% group_by(BILL) %>% mutate(n2=n()) %>% ungroup() %>% group_by(BILL, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)
chart_BILL6Limit<- ggplot(BILL6Limit_vs_default) +
  geom_col(aes(x = BILL, y = prop, fill = default))+ 
  geom_line(aes(x=BILL, y=n, group=1))+
  geom_point(aes(x=BILL, y=n, group=1))+
  theme(legend.position = "bottom") + 
  #labs(title = "Default status \nacross different PAY_0 status") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-0.1, 2))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")


#3.F.vi Combine BILL_AMT6 LIMIT_BAL ratio chart (may need to run again depending on the computing power)
chart_billlimit<-ggarrange(chart_BILL1Limit, chart_BILL2Limit,chart_BILL3Limit, chart_BILL4Limit, chart_BILL5Limit, chart_BILL6Limit, labels=c("Bill_1/Limit","Bill_2/Limit","Bill_3/Limit","Bill_4/Limit","Bill_5/Limit","Bill_6/Limit" ),heights = c(2, 2, 2),ncol = 2, nrow = 3, 
                      align = "v",common.legend = TRUE, legend = "bottom")
annotate_figure(chart_billlimit,
                top = text_grob("Default status \nacross different Bill Amount divided by Limit Balance", color = "red", face = "bold", size = 30),
                left = text_grob("Default Proportion",  size =20, rot = 90),
                right = text_grob("Population",  size =20, rot=270),
                bottom = text_grob("Bill Amount/Credit Balance", color = "blue", face = "bold", size = 25),
)


#3.G Charting BILL Limit BALANCE ratio vs PAY status
BILL1Limit_vs_PAY<-train%>% mutate(BILLAMT1_LIMIT_ratio=round(BILL_AMT1/LIMIT_BAL/0.05)*0.05) %>% select(BILLAMT1_LIMIT_ratio, PAY_0, default)
scatter_BILL1Limit_Pay<- ggplot(BILL1Limit_vs_PAY) +
  geom_point(aes(x=BILLAMT1_LIMIT_ratio, y=PAY_0, color= default))
scatter_BILL1Limit_Pay


# PAY_0_vs_PAY1<-train%>% mutate(BILL=round(BILL_AMT1/LIMIT_BAL/0.05)*0.05) %>% select(BILL, PAY_0, PAY_2, default)
# scatter_PAY0_PAY1<- ggplot(PAY_0_vs_PAY1) +
#   geom_point(aes(x=PAY_2, y=PAY_0, size= sum(default==1)))
# scatter_PAY0_PAY1
# ggplot(PAY_0_vs_PAY1) +
#   geom_count(aes(x=PAY_0,y=PAY_2, color= default))


#3.H Charting Pay status vs Default
All_pay_vs_default<-train%>% select(PAY_0,PAY_2, PAY_3, PAY_4, default) %>% group_by(PAY_0,PAY_2, PAY_3, PAY_4) %>% mutate(n2=n()) %>% ungroup() %>% 
  group_by(PAY_0,PAY_2, PAY_3, PAY_4, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)%>% filter(default==1)
ggplot(All_pay_vs_default, aes(PAY_3, PAY_4, fill = prop))+ 
  geom_tile(colour = "white") + 
  facet_grid(PAY_0 ~ PAY_2) + 
  scale_fill_gradient(low="green", high="red") +
  labs(x="PAY_0",
       y="PAY_2",
       title = "Default Rate Heat Map", 
       subtitle="vs different pay status", 
       fill="Close")

#3.I Charting Index vs Default

alpha<-0.4
lambda<-0.1
pay_index<-train %>% 
  mutate(index1=(as.numeric(paste(PAY_0))+alpha*as.numeric(paste(PAY_2))+alpha^2*as.numeric(paste(PAY_3))+alpha^3*as.numeric(paste(PAY_4))+alpha^4*as.numeric(paste(PAY_5))+alpha^5*as.numeric(paste(PAY_6))),
         index2=(BILL_AMT1+alpha*BILL_AMT2+alpha^2*BILL_AMT3+alpha^3*BILL_AMT4+alpha^4*BILL_AMT5+alpha^5*BILL_AMT6)/LIMIT_BAL,
         index=0.5*round(index1+lambda*index2)/0.5) %>% group_by(index) %>% mutate(n2=n()) %>% ungroup() %>% 
  group_by(index, default) %>% summarize(prop=n()/mean(n2), n=mean(n2)/total_pop)


chart_index<- ggplot(pay_index) +
  geom_col(aes(x = index, y = prop, fill = default))+ 
  geom_line(aes(x=index, y=n, group=1))+
  geom_point(aes(x=index, y=n, group=1))+
  theme(legend.position = "bottom") + 
  labs(title = "Default status vs Index (alpha=0.4, lambda=0.1)") +
  theme(plot.title = element_text(hjust = 0.5),  axis.title.y = element_blank()) + 
  #scale_x_discrete(breaks=c("1","2"), labels=c("Male", "Female"))+
  scale_fill_discrete(name = "Default status", labels = c("No Default", "Default"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     sec.axis = sec_axis(~ . * total_pop , name = "Population"))+
  geom_hline(yintercept=default_prop, linetype="dashed", color = "purple")+
  geom_text(aes(0,default_prop,label = "Overall default rate"), vjust = -1, hjust=0, color ="purple")


chart_index


##########################
######  4. Modeling ######
##########################

##########  Running the following codes related to KNN and random forest below may take hours or even days ##########

library(caret)
#4.A Proprietary Model
Prop_Model<- function(alpha, threshold, lambda){
p<-train %>%  
  mutate(index1=(as.numeric(paste(PAY_0))+alpha*as.numeric(paste(PAY_2))+alpha^2*as.numeric(paste(PAY_3))+alpha^3*as.numeric(paste(PAY_4))+alpha^4*as.numeric(paste(PAY_5))+alpha^5*as.numeric(paste(PAY_6))), 
         index2=(BILL_AMT1+alpha*BILL_AMT2+alpha^2*BILL_AMT3+alpha^3*BILL_AMT4+alpha^4*BILL_AMT5+alpha^5*BILL_AMT6)/LIMIT_BAL,
         p=ifelse(index1+index2*lambda>=threshold, 1,0)) %>%
pull(p)
c(alpha, threshold, lambda, confusionMatrix(data=as.factor(p), reference=train$default)$overall["Accuracy"])
}

Prop_Model_Prediction<- function(dataset, alpha, threshold, lambda){
  p<-dataset%>% 
    mutate(index1=(as.numeric(paste(PAY_0))+alpha*as.numeric(paste(PAY_2))+alpha^2*as.numeric(paste(PAY_3))+alpha^3*as.numeric(paste(PAY_4))+alpha^4*as.numeric(paste(PAY_5))+alpha^5*as.numeric(paste(PAY_6))), 
           index2=(BILL_AMT1+alpha*BILL_AMT2+alpha^2*BILL_AMT3+alpha^3*BILL_AMT4+alpha^4*BILL_AMT5+alpha^5*BILL_AMT6)/LIMIT_BAL,
           p=ifelse(index1+index2*lambda>=threshold, 1,0)) %>%
    pull(p)
  as.factor(p)
#  confusionMatrix(data=as.factor(p), reference=dataset$default)
}

v1<-rep(seq(0,1,0.1),121)
v2<-rep(rep(seq(0,5,0.5),each=11), time=11)
v3<-rep(seq(0,1,0.1), each = 121)

P_Model_Calibration<-mapply(Prop_Model,v1, v2, v3)
P_Model_Calibration[, which(P_Model_Calibration[4,] == max(P_Model_Calibration[4,]), arr.ind = TRUE)]
P_train<-Prop_Model_Prediction(train, 0.2,1.5,0)
confusionMatrix(data=P_train, reference=train$default)

#4.B naive bayseian
Sys.time()
model_nb = train(train[ ,2:24], train$default,'nb',trControl=trainControl(method='cv',number=10))
Sys.time()
#y_hat_nb <- predict(model_nb, train, type = "class")
model_nb
y_hat_nb_train <- predict(model_nb, train, type = "raw")
confusionMatrix(data=y_hat_nb_train, reference=train$default)
varImp(model_nb)

#4.C knn (takes days to complete)
Sys.time()
set.seed(7, sample.kind = "Rounding") 
model_knn <- train(train[ ,2:24], train$default, method = "knn", tuneGrid = data.frame(k = seq(1,60)))
model_knn
Sys.time()
y_hat_knn_train <- predict(model_knn, train)
varImp(model_knn)


#4.D random forest
Sys.time()
set.seed(9, sample.kind = "Rounding") 
model_rf <- train(train[ ,2:24], train$default, method = "rf",tuneGrid = data.frame(mtry = seq(3, 9, 2)),importance=TRUE)
model_rf
Sys.time()
varImp(model_rf)
y_hat_rf_train <- predict(model_rf, train)

#4.E glm
Sys.time()
model_glm = train(train[ ,2:24], train$default,'glm')
Sys.time()
y_hat_glm_train <- predict(model_glm, test, type = "raw")
model_glm

#4.F Ensemble
E_Prediction<-function(threshold, p_1,p_2,p_3){
  if(missing(p_3)){
    p<-ifelse(as.numeric(paste(p_1))+as.numeric(paste(p_2))>threshold, 1,0)
    as.factor(p)} 
  else{
  p<-ifelse(as.numeric(paste(p_1))+as.numeric(paste(p_2))+as.numeric(paste(p_3))>threshold, 1,0)
as.factor(p)}
}

#E_train<-E_Prediction(y_hat_nb_train, y_hat_rf_train, P_train)
y_hat_E_train<-E_Prediction(1,y_hat_rf_train, P_train)
confusionMatrix(data=y_hat_E_train, reference=train$default)

#########################
######  5. Testing ######
#########################
#5.A Proprietary Model
P_test<-Prop_Model_Prediction(test, 0.2,1.5,0)
confusionMatrix(data=P_test, reference=test$default)

#5.B naive bayseian
y_hat_nb <- predict(model_nb, test, type = "raw")
confusionMatrix(data=y_hat_nb, reference=test$default)

#5.C knn
y_hat_knn <- predict(model_knn, test)
confusionMatrix(data = y_hat_knn, reference = test$default)

#5.D random forest
y_hat_rf <- predict(model_rf, test)
confusionMatrix(data = y_hat_rf, reference = test$default)

#5.E glm
y_hat_glm <- predict(model_glm, test)
confusionMatrix(data = y_hat_glm, reference = test$default)

#6.F Ensemble
y_hat_E_test<-E_Prediction(1,y_hat_rf, P_test)
confusionMatrix(data=y_hat_E_test, reference=test$default)

################################################
######  APPENDIX. Models that do not work ######
################################################

#LDA
# Sys.time()
# model_lda = train(train[ ,2:24], train$default,'lda')
# Sys.time()
# y_hat_lda <- predict(model_lda, test, type = "class")
# confusionMatrix(data=y_hat_lda, reference=test$default)$overall[["Accuracy"]]
# 
# #QDA
# Sys.time()
# #model_qda = train(train[ ,2:24], train$default,'qda')
# model_qda = train(as.data.frame(train$PAY_0), train$default,'qda')
# Sys.time()
# y_hat_qda <- predict(model_qda, test, type = "class")
# confusionMatrix(data=y_hat_qda, reference=test$default)$overall[["Accuracy"]]
# 
# #loess
# Sys.time()
# model_loess = train(train[ ,2:24], train$default,'gamLoess')
# Sys.time()
# y_hat_loess <- predict(model_loess, test, type = "class")
# confusionMatrix(data=y_hat_loess, reference=test$default)$overall[["Accuracy"]]






