library("ggplot2")
library("tidyr")
library("ggthemes")
library("dplyr")
library("Cairo")
library("showtext")

font.add("myfontl","msyhl.ttc")
font.add("myfont","msyh.ttc")
font.add("myfzhzh","方正正粗黑简体.TTF")

setwd("C:\Users\YAN\Documents\Codes\R\circle_bubble_环形图")

circle_bubble <- read.csv("circle_bubble.csv", stringsAsFactors = FALSE, check.names = FALSE)

circle_bubble$Government <- -circle_bubble$Government
level <- circle_bubble[, "State"]
circle_bubble$State <- factor(circle_bubble$State, levels = level, ordered = TRUE)
circle_bubble$State


circle_data <- circle_bubble %>%select(State,Government,Family,Yourself) %>%gather(Class, Value, -State)
circle_data$Class<-factor(circle_data$Class,levels=c("Government","Yourself","Family"),ordered=TRUE)


circle_point<-circle_bubble[,c("State","General","Very","Raito")]
circle_point_data<-gather(circle_point,Class,Value,-State)
circle_point_data$Class<-factor(circle_point_data$Class,levels=c("General","Very","Raito"),order=TRUE)


circle_point_data <- within(circle_point_data,{
  mood_y <- NA
  mood_y[Class=="General"]<--150
  mood_y[Class=="Very"]<--100
  mood_y[Class=="Raito"]<-140})
circle_point_data$Value_y<-ifelse(circle_point_data$Class=="Raito",circle_point_data$Value,0.3*circle_point_data$Value)

circle_data <- within(circle_data,{
  label_y <- NA
  label_y[Class=="Government"]<-circle_bubble$Government/2
  label_y[Class=="Family"]<-circle_bubble$Family/2
  label_y[Class=="Youself"]<-circle_bubble$Family+circle_bubble$Youself-circle_bubble$Youself/2})

ggplot()+
  geom_linerange(data=circle_bubble,aes(x=State,ymin=-150,ymax=140),size=.25,color="#D8E5F2",alpha=0.8)+
  geom_col(data=circle_data,aes(x=State,y=Value,fill=Class),width=1,colour="white",size=.25)+
  coord_polar()

ggplot()+
  geom_linerange(data=circle_bubble,aes(x=State,ymin=-150,ymax=140),size=.25,color="#D8E5F2",alpha=0.8)+
  geom_col(data=circle_data,aes(x=State,y=Value,fill=Class),width=1,colour="white",size=.25)+
  geom_point(data=circle_bubble,aes(State,y=140),size=40,colour="#FEFCD3",alpha=.8)+
  geom_point(data=circle_point_data,aes(State,y=mood_y,size=Value_y,colour=Class))+
  geom_text(data=circle_data,aes(x=State,y=label_y,group=Class,label=abs(Value)),family="myfont",colour="white",size=5)+
  geom_text(data=filter(circle_point_data,mood_y==-150),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#AA1A66",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==-100),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#A31067",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==140),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#F29E4A",size=5)+
  coord_polar()



ggplot()+
  geom_linerange(data=circle_bubble,aes(x=State,ymin=-150,ymax=140),size=.25,color="#D8E5F2",alpha=0.8)+
  geom_col(data=circle_data,aes(x=State,y=Value,fill=Class),width=1,colour="white",size=.25)+
  geom_point(data=circle_bubble,aes(State,y=140),size=40,colour="#FEFCD3",alpha=.8)+
  geom_point(data=circle_point_data,aes(State,y=mood_y,size=Value_y,colour=Class))+
  geom_text(data=circle_data,aes(x=State,y=label_y,group=Class,label=abs(Value)),family="myfont",colour="white",size=5)+
  geom_text(data=filter(circle_point_data,mood_y==-150),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#AA1A66",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==-100),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#A31067",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==140),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#F29E4A",size=5)+
  scale_fill_manual(values=c("#FA844F","#BDCCD4","#00C4B5"))+
  scale_colour_manual(values=c("#FAC0A1","#F35C57","#FBEB1B"))+
  labs(title="各国人民\n对于养老的态度")+
  scale_size_area(max_size=34.8)+
  coord_polar()




CairoPNG(file="circle_bubble.png",width=1000,height=1000)
showtext.begin()
ggplot()+
  geom_linerange(data=circle_bubble,aes(x=State,ymin=-150,ymax=140),size=.25,color="#D8E5F2",alpha=0.8)+
  geom_col(data=circle_data,aes(x=State,y=Value,fill=Class),width=1,colour="white",size=.25)+
  geom_point(data=circle_bubble,aes(State,y=140),size=40,colour="#FEFCD3",alpha=.8)+
  geom_point(data=circle_point_data,aes(State,y=mood_y,size=Value_y,colour=Class))+
  geom_text(data=circle_data,aes(x=State,y=label_y,group=Class,label=abs(Value)),family="myfont",colour="white",size=5)+
  geom_text(data=filter(circle_point_data,mood_y==-150),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#AA1A66",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==-100),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#A31067",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==140),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#F29E4A",size=5)+
  scale_fill_manual(values=c("#FA844F","#BDCCD4","#00C4B5"))+
  scale_colour_manual(values=c("#FAC0A1","#F35C57","#FBEB1B"))+
  labs(title="各国人民\n对于养老的态度")+
  scale_size_area(max_size=34.8)+
  coord_polar()+
  ylim(-250,150)+
  guides(colour=FALSE,size=FALSE,fill=FALSE)+
  theme_map() %+replace%theme(plot.title=element_text(family="myfzhzh",size=50,hjust=0,lineheight=1.2))
showtext.end()
dev.off()



ggplot()+
  geom_linerange(data=circle_bubble,aes(x=State,ymin=-150,ymax=140),size=.25,color="#D8E5F2",alpha=0.8)+
  geom_col(data=circle_data,aes(x=State,y=Value,fill=Class),width=1,colour="white",size=.25)+
  geom_point(data=circle_bubble,aes(State,y=140),size=40,colour="#FEFCD3",alpha=.8)+
  geom_point(data=circle_point_data,aes(State,y=mood_y,size=Value_y,colour=Class))+
  geom_text(data=circle_data,aes(x=State,y=label_y,group=Class,label=abs(Value)),family="myfont",colour="white",size=5)+
  geom_text(data=filter(circle_point_data,mood_y==-150),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#AA1A66",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==-100),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#A31067",size=4)+
  geom_text(data=filter(circle_point_data,mood_y==140),aes(x=State,y=mood_y,label=Value),family="myfontl",colour="#F29E4A",size=5)+
  scale_fill_manual(values=c("#FA844F","#BDCCD4","#00C4B5"))+
  scale_colour_manual(values=c("#FAC0A1","#F35C57","#FBEB1B"))+
  labs(title="各国人民\n对于养老的态度")+
  scale_size_area(max_size=34.8)+
  coord_polar()+
  ylim(-250,150)+
  guides(colour=FALSE,size=FALSE,fill=FALSE)+
  theme_map() %+replace%theme(plot.title=element_text(family="myfzhzh",size=50,hjust=0,lineheight=1.2))
