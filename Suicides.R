
# -----------------Suicides in India----------------



#library files
library(caret)
library(dplyr)
library(plyr)
library(ggplot2)
library(formattable)

#importing data

data=read.csv("E:/life R/projects/suicides.csv")
View(data)
str(data)
glimpse(data)
sapply(data,class)
#renaming column X3 as State/UT
names(data)[1]<-"State/UT"

# Analyzing faxtor variables
levels(data$Gender)
levels(data$Age_group)
levels(data$Type_code)
levels(data$Type) 
table(levels(data$Type_code))
table(data$`State/UT`,data$Age_group)
table(data$`State/UT`,data$Gender)

# n levels returns the count of levels

# Analysis or Prinicpal component Analysis

'Questions to be arised?

1.which state has maximum suicides?
2.which gender suicides more?
3.which age group suicides more?
4.what are the major causes of suicides?
5.which type of cause effects more? 
6.In which Year which state is having maximum suicides?
'

# Removing non suicide data
data<-filter(data,Total!=0)

#Seperating states and count of suicides

data_state<-aggregate(data$Total, by=list(State=data$`State/UT`), FUN=sum)
data_state<-data_state[-c(32,33,34),]
glimpse(data_state)

#reordering states on the basis of suicides
data_state<-data_state[order(data_state$x,decreasing=TRUE),]

View(data_state)

top<-data.frame(data_state$State[1:10],data_state$x[1:10])
names(top)[1]<-"state"
names(top)[2]<-"count"

View(top)


ggplot(top, aes(state,count)) +geom_bar(fill="green",stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 


#which gender suicides more? 


data_gender<-aggregate(data$Total,by=list(Gender=data$Gender),FUN=sum)

data_gender<-mutate(data_gender,count_pct=percent(data_gender$x/ sum(data_gender$x),1))

ggplot(data_gender,aes(Gender,x))+geom_bar(fill="pink",stat="Identity")+
  xlab("Gender")+ylab("Count")+geom_text(aes(label=count_pct),vjust=-0.5,hjust=1)

#Conclusion
#Male Suicides are more'''


#Which age group suicides more?

data_agegroup<-aggregate(data$Total,by=list(Age=data$Age_group),FUN=sum)
View(data_agegroup)
data_agegroup<-data_agegroup[-1,]
data_agegroup<-mutate(data_agegroup,count_pct =percent(data_agegroup$x/ sum(data_agegroup$x),1))

View(data_agegroup)


ggplot(data_agegroup,aes(Age,x))+geom_bar(fill="yellow",stat="Identity")+
ylab("Count")+geom_text(aes(label=count_pct), vjust=-0.1, size=3.5)

#15-29 Age Group Suicides more  



#what are the major causes of suicides?

data_causes<-aggregate(data$Total,by=list(Causes=data$Type_code),FUN=sum)

View(data_causes)


data_causes=mutate(data_causes,count_pct=percent((data_causes$x)/ sum(data_causes$x),1))


ggplot(data_causes,aes(Causes,x))+geom_bar(fill="violet",stat="Identity")+
geom_text(aes(label=count_pct),vjust=-0.1, size=3.5)+
  theme(axis.text.x=element_text(angle = 45,hjust=1))

#Education Status and Social status comprises of almost 68% suicides

#Word Cloud

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

data_Type<-aggregate(data$Total, by=list(Type=data$Type), FUN=sum)

#### Generate the Word cloud

set.seed(1234)
wordcloud(words = data_Type$Type, freq = data_Type$x, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# which state suicides are more 
   ' 1.what are the causes 
     2.what are the types
     3.which gender suicides more'



View(data_state)
top_state<-data_state[1,]

names(top_state)[1]<-"state"
names(top_state)[2]<-"count"
top_state

#top state data i.e Maharastra
top_statedata<-filter(data,data$`State/UT`==top_state$state)

#checking which gender suicides more in this state

top_stategender<-aggregate(top_statedata$Total,by=list(Gender=top_statedata$Gender),FUN=sum)

View(top_stategender)

top_stategender<-mutate(top_stategender,countpct=percent(top_stategender$x/ sum(top_stategender$x),1))

ggplot(top_stategender, aes(Gender,x)) +geom_bar(fill="green",stat="identity")+
  xlab("Gender Suicides in Maharastara")+ylab("Count")+geom_text(aes(label=countpct),hjust=1,vjust=-0.5) 

'Male suicides are more in Maharastra'

# Mal



















