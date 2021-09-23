library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
tuesdata$nominees->nominees
glimpse(nominees)
library(cowplot)
library(jpeg)
library(png)



nominees%>%filter(title=="Friends")%>%
  distinct(category,type,.keep_all = TRUE)%>%select(year,type)%>%
  group_by(year)%>%
  summarise(lost=sum(type=="Nominee"), won=sum(type=="Winner"), allnominations=lost+won)->friends
friends
colnames(friends)<-c("Year", "Lost","Won","Nominations")

nominees%>%filter(title=="Friends")%>%select(category,type,year)%>%
  group_by(year)%>%count(type)

friends$Year<-as.character(friends$Year)

ggplot(friends, aes(x=Nominations, y=Year, label=Nominations))+
  geom_col(width=0.8, fill="#fe8a71", color="#0e9aa7")+
  scale_y_discrete(limits=rev)+
  geom_text(color="black", fontface="bold", hjust=2, size=4)+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_text(color = "#f6cd61", face="italic",size=16))+
  theme(plot.background = element_rect(colour = "black",fill = "black"),
        panel.background = element_rect(colour = "black",fill="black"),
        panel.grid = element_blank())+
  labs(title="EMMY NOMINATIONS FOR FRIENDS", caption = "Data: Emmys.com via Tidy Tuesday, Design:@annapurani93")+
  theme(plot.title = element_text(size=30,hjust=.5,colour = "#f6cd61", face="italic"),
        plot.caption = element_text(size=16,colour = "#f6cd61", face="italic"))->ff
ff


friendslogo <- readPNG("C:/Users/Annapurani/Desktop/friends.png")
emmy<-readJPEG("C:/Users/Annapurani/Desktop/emmy.jpg")
ggdraw(ff) +
  draw_image(friendslogo, x = .21, y =-.30, scale = .25)->ff1
ff1
  ggdraw(ff1)+
  draw_image(emmy, x = .20, y =.035, scale = .60)->ff2

ff2
ggsave("ff2.png", ff2,
       width = 25, height = 12, dpi=600) 
