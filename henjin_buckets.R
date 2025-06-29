t=fread("../Czech/data/henjin_timeseries.csv")

ages=c(1800,seq(1930,1960,5),seq(1970,1990,10))
t[,age:=ages[findInterval(t$born,ages)]]

a=t[,.(dead=sum(dead),pop=sum(pop)),.(age,dose=pmin(dose,3),week=obsweek)]
a=merge(a,a[week==97,sum(pop),age][,.(age,std=V1/sum(V1))]) # 97 is week 1 of 2022
a[,.(asmr=sum(dead/pop/7*365e5*std)),,.(week,dose)]
