library(ggplot2)
library(dplyr)
library(RODBC)
library(RColorBrewer)
library(reshape2)
library(grid)
channel=odbcConnectExcel2007("D:/My study/Peking/My works/Nexus analysis/Global Lnad-Water nexus/Data and calculation/Country data.xlsx")
sqlTables(channel)
countrydata=sqlFetch(channel,"figures")
landdata=sqlFetch(channel,"table s4")
waterdata=sqlFetch(channel,"table s5")
fig3=sqlFetch(channel,"Fig3")
exportimport=sqlFetch(channel,"fig2_pre (2)")
close(channel)
fig3_df <- tbl_df(fig3)


# 提取前十的数据 净进口 净出口 土地
p<-ggplot(data = fig3, mapping = aes(x = DL_per_capita, y = GDP_per_capita, size=abs(land_imbalance), colour= tu))+
  geom_point(alpha=0.5)+xlim(0,20)+ylim(0,120000)
p<-p+labs(x="DL per capita (ha/capita)", y="GDP per capita (USD/capita)", size="Net values (Mha)", colour=NULL)
p<-p+theme(legend.position=c(0.7,0.85))
p<-p+theme(legend.background=element_rect(fill = "transparent",colour = NA))
p<-p+theme(legend.title=element_text(size=10))
p<-p+theme(legend.direction="horizontal")
p<-p+geom_vline(xintercept = 0.693798,linetype="dashed")+geom_hline(yintercept = 10484.37351,linetype="dashed")
p<-p+annotate("text", label = "Global average", x = 2.8, y = 118000, size = 3.5, colour = "black")
p<-p+annotate("text", label = "DL per capita", x = 2.8, y = 111000, size = 3.5, colour = "black")
p<-p+annotate("text", label = "Global average", x = 18.5, y = 22000, size = 3.5, colour = "black")
p<-p+annotate("text", label = "GDP per capita", x = 18.5, y = 15000, size = 3.5, colour = "black")
p<-p+geom_text(aes(label=ISO_tu),size=3,colour = 'black', hjust=1)
p
#去掉淡灰底色和网格线
#p+ theme_set(theme_bw())
#p+theme(panel.grid.major=element_line(colour=NA))

# 提取前十的数据 净进口 净出口 水资源
p2<-ggplot(data = fig3, mapping = aes(x = DW_per_capita, y = GDP_per_capita, size=abs(water_imbalance), colour= shui))+
  geom_point(alpha=0.5)+xlim(0,2000)+ylim(0,120000)
p2<-p2+labs(x="DW per capita (m3/capita)", y="GDP per capita (USD/capita)", size="Net Values (billion m3)", colour=NULL)
p2<-p2+theme(legend.position=c(0.6,0.85))
p2<-p2+theme(legend.background=element_rect(fill = "transparent",colour = NA))
p2<-p2+theme(legend.title=element_text(size=10))
p2<-p2+theme(legend.direction="horizontal")
p2<-p2+geom_vline(xintercept = 383.7147131,linetype="dashed")+geom_hline(yintercept = 10484.37351,linetype="dashed")
p2<-p2+annotate("text", label = "Global average", x = 200, y = 118000, size = 3.5, colour = "black")
p2<-p2+annotate("text", label = "DW per capita", x = 200, y = 111000, size = 3.5, colour = "black")
p2<-p2+annotate("text", label = "Global average", x = 1900, y = 23000, size = 3.5, colour = "black")
p2<-p2+annotate("text", label = "GDP per capita", x = 1900, y = 16000, size = 3.5, colour = "black")
p2<-p2+geom_text(aes(label=ISO_shui),size=3,colour = 'black', hjust=1)
p2

##进出口贸易对比
p3<-ggplot()+
   geom_bar(data=exportimport[exportimport$value>0,],aes(x=country,y=value,fill=sector),stat="identity",alpha=0.8)+
   geom_bar(data=exportimport[exportimport$value<0,],aes(x=country,y=value,fill=sector),stat="identity",alpha=0.8)
  
p3<-p3+theme(legend.title=element_blank(),legend.position="top",
             legend.direction="horizontal",legend.key.size=unit(0.4,"cm"))+
  scale_x_discrete(limits=c("Pakistan","India","Egypt","Myanmar","Chile","France","Russia","Germany","USA","Japan"))
   #scale_x_discrete(limits=c("Australia","Mongolia","Argentina","Madagascar","Ethiopia",	"Russia","Germany","USA","China","Japan"))
p3<-p3+geom_hline(yintercept = 0)
p3<-p3+coord_flip()+labs(x="",y="Freshwater exports (billion m3)         Freshwater imports (billion m3)")
#+scale_fill_brewer(palette = "Paired")
p3
