library(data.table);library(ggplot2)

ua=\(x,y,...){u=unique(x);y(u,...)[match(x,u)]}
age=\(x,y){class(x)=class(y)=NULL;(y-x-(y-789)%/%1461+(x-789)%/%1461)%/%365}

rec=fread("~/GitHub/Czech/data/CR_records.csv",showProgress=F)

d=data.table(dose=rep(1:7,each=nrow(rec)),death=rec$DatumUmrti)
set.seed(0);d$birth=ua(paste0(rec$Rok_narozeni,"-1-1"),as.Date)+sample(0:364,nrow(d),T)
d$date=rec[,unlist(.SD,,F),.SDcols=patterns("Datum_")]
d$type=rec[,unlist(.SD,,F),.SDcols=patterns("Latka_")]
d=d[!is.na(date)]

name1=unique(d$type);name2=rep("Other",length(name1))
name2[name1==""]=""
name2[grep("comirnaty",ignore.case=T,name1)]="Pfizer"
name2[grep("spikevax",ignore.case=T,name1)]="Moderna"
name2[grep("nuvaxovid",ignore.case=T,name1)]="Novavax"
name2[name1=="COVID-19 Vaccine Janssen"]="Janssen"
name2[name1=="VAXZEVRIA"]="AstraZeneca"
d$type=name2[match(d$type,name1)]

p=d[dose<=4&type!="Novavax"&type!="Other"&type!="",.(pop=.N,dead=sum(!is.na(death))),.(age=age(birth,date),type,dose)]
p[,survival:=(1-(dead/pop))*100]

xstart=0;xend=max(p$age);xstep=10;ystart=0;yend=100;ystep=10

p$type=factor(p$type,p[,sum(as.double(pop)),type][order(-V1)]$type)
color=hsv(c(240,355,330,204,36)/360,c(.94,.75,.8,.67,.64),c(.76,.82,.5,.87,.95))
pointsize=c(1e4,3e4,1e5,3e5)

sub="People remain included under p dose after subsequent doses. The age at vaccination was calculated by assigning a random day of birth to each person in the dataset."|>stringr::str_wrap(73)

ggplot(p,aes(x=age,y=survival))+
facet_wrap(~dose,ncol=1,scales="free_x",strip.position="top")+
geom_vline(xintercept=c(xstart,xend),linewidth=.3,lineend="square")+
geom_hline(yintercept=c(ystart,yend),linewidth=.3,lineend="square")+
geom_point(aes(color=type),size=1.5,stroke=0)+
geom_line(aes(color=type),linewidth=.4)+
geom_label(data=data.frame(dose=unique(p$dose)),aes(label=paste0("\n    Dose ",dose,"    \n")),x=xstart,y=0,lineheight=.7,hjust=0,vjust=0,size=2.5,fill=alpha("white",1),label.r=unit(0,"lines"),label.padding=unit(0,"lines"),label.size=.25)+
labs(title="Czech record-level data: Probability of survival up to end of 2022",x="Age at vaccination",y="Probability of survival up to end of 2022",subtitle=sub)+
scale_x_continuous(limits=c(xstart,xend),breaks=seq(xstart,xend,xstep))+
scale_y_continuous(labels=\(x)paste0(x,"%"))+
scale_color_manual(values=color,name="Type")+
coord_cartesian(clip="off",expand=F)+
theme(axis.text=element_text(size=8,color="black"),
  axis.ticks=element_line(linewidth=.3),
  axis.ticks.length=unit(3,"pt"),
  axis.title=element_text(size=8,face="bold"),
  axis.title.x=element_text(margin=margin(4)),
  axis.title.y=element_text(margin=margin(,2,,1)),
  legend.background=element_rect(fill=alpha("white",0)),
  legend.key.height=unit(10,"pt"),
  legend.key.width=unit(20,"pt"),
  legend.key=element_rect(fill="white"),
  legend.margin=margin(0,0,7,0),
  legend.direction="horizontal",
  legend.box.just="right",
  legend.justification=c(1,.5),
  legend.position="top",
  legend.box.margin=margin(),
  legend.spacing.x=unit(2,"pt"),
  legend.box.spacing=unit(0,"pt"),
  legend.spacing=unit(0,"pt"),
  legend.text=element_text(size=8),
  # legend.title=element_text(size=8,face="bold",margin=margin(0,2,0,0)),
  legend.title=element_blank(),
  panel.background=element_rect(fill="white"),
  panel.grid.major=element_line(linewidth=.3,color="gray90"),
  panel.spacing=unit(3,"pt"),
  strip.background=element_blank(),
  strip.text=element_blank(),
  plot.margin=margin(5,9,5,5),
  plot.subtitle=element_text(size=8,margin=margin(,,5)),
  plot.title=element_text(size=8.8,face="bold",margin=margin(2,,4)))
ggsave("1.png",width=4.5,height=6,dpi=380)
