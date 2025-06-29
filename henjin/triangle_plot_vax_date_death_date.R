library(data.table)

ua=\(x,y,...){u=unique(x);y(u,...)[match(x,u)]}

kimi=\(x){e=floor(log10(ifelse(x==0,1,abs(x))));e2=pmax(e,0)%/%3+1
  x[]=ifelse(abs(x)<10,round(x),paste0(round(x/1e3^(e2-1),ifelse(e%%3==0,1,0)),c("","k","M","B","T")[e2]));x}

type1="Moderna";type2="Pfizer"

buck=fread("http://sars2.net/f/czbucketskeep.csv.gz")[month>="2020-12"&dose<=1]
t=buck[,.(dead=sum(dead),alive=as.numeric(sum(alive))),.(age=pmin(age,100),month,vaxmonth,type)]
t=merge(t[type!=""],t[,.(base=sum(dead)/sum(alive)),.(age,month)])[,base:=base*alive]
t=rbind(t,t[,.(dead=sum(dead),alive=sum(alive),base=sum(base),month="Total"),.(age,vaxmonth,type)])
t=rbind(t,t[,.(dead=sum(dead),alive=sum(alive),base=sum(base),vaxmonth="Total"),.(age,month,type)])
t=t[,.(rat=sum(dead)/sum(base),alive=sum(alive)),.(type,vaxmonth,month)]

a=tapply(t$rat,t[,1:3],c)
m1=a[type1,,];m2=a[type2,,]
m=(m1-m2)/ifelse(m1>m2,m2,m1)
disp=m1/m2;disp=ifelse(disp>=10,sprintf("%.1f",disp),sprintf("%.2f",disp))
disp[lower.tri(disp)&row(disp)!=nrow(disp)]=""
exp=.9;m=abs(m)^exp*sign(m)
maxcolor=4^exp;m[is.infinite(m)]=-maxcolor

pheatmap::pheatmap(m,filename="mort.png",display_numbers=disp,
  gaps_col=ncol(m)-1,gaps_row=nrow(m)-1,cluster_rows=F,cluster_cols=F,legend=F,
  cellwidth=19,cellheight=19,fontsize=9,fontsize_number=8,border_color=NA,na_col="white",
  number_color=ifelse(!is.na(m)&abs(m)>.5*maxcolor,"white","black"),
  breaks=seq(-maxcolor,maxcolor,,256),
  colorRampPalette(hsv(rep(c(7/12,0),4:5),c(.9,.75,.6,.3,0,.3,.6,.75,.9),c(.4,.65,1,1,1,1,1,.65,.4)))(256))

rec=fread("../Czech/data/CR_records.csv",showProgress=F)
d=rec[,.N,.(month=ua(Datum_1,substr,1,7),type=OckovaciLatka_1)]
name1=unique(d$type);name2=rep("Other",length(name1))
name2[name1==""]=""
name2[grep("comirnaty",ignore.case=T,name1)]="Pfizer"
name2[grep("spikevax",ignore.case=T,name1)]="Moderna"
name2[grep("nuvaxovid",ignore.case=T,name1)]="Novavax"
name2[name1=="COVID-19 Vaccine Janssen"]="Janssen"
name2[name1=="VAXZEVRIA"]="AstraZeneca"
d$type=name2[match(d$type,name1)]

pop=xtabs(N~month+type,d)[,c(type1,type2)]
pop=rbind(pop,Total=colSums(pop))[rownames(m),]

exp2=.6;maxcolor2=max(pop[-nrow(pop),])

pheatmap::pheatmap(pop^exp2,filename="pop.png",display_numbers=kimi(pop),
  gaps_row=nrow(m)-1,cluster_rows=F,cluster_cols=F,legend=F,
  cellwidth=19,cellheight=19,fontsize=9,fontsize_number=8,border_color=NA,na_col="white",
  number_color=ifelse(pop^exp2>maxcolor2^exp2*.45,"white","black"),
  breaks=seq(0,maxcolor2^exp2,,256),sapply(seq(1,0,,256),\(i)rgb(i,i,i)))

system("magick mort.png \\( pop.png -splice x4 \\) +append 0.png;w=`identify -format %w 0.png`;pad=22;magick -pointsize 46 -font Arial-Bold -interline-spacing -2 \\( -size $[w-pad*2]x caption:'Czech record-level data: Moderna-Pfizer mortality ratio from day of first dose until end of 2022' -splice $[pad]x20 \\) \\( -pointsize 42 -font Arial caption:'The y-axis shows the month of vaccination and the x-axis shows the month of death. The two columns on the right show the number of people who got vaccinated each month. Only first doses included.' -splice $[pad]x14 \\) 0.png -append 1.png")
# system("magick -gravity north mort.png \\( -splice x4 pop.png \\) +append 0.png;w=`identify -format %w 0.png`;pad=48;convert \\( -pointsize 46 -font Arial-Bold -size $[w-pad]x caption:'Czech record-level data: Moderna-Pfizer mortality ratio from day of first dose until end of 2022' \\( -gravity northwest -splice x12 -pointsize 42 -font Arial -interline-spacing -2 caption:'The y-axis shows the month of vaccination and the x-axis shows the month of death. The two columns on the right show the number of people who got vaccinated each month. Only first doses included.' \\) -extent $[w-pad]x -gravity center \\) \\( -gravity north 0.png -shave x6 \\) -append -gravity north -splice x20 1.png")
