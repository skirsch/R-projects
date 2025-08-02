library(data.table);library(ggplot2)

# Load and prepare data
t=fread("../czech/data/vax_24.csv")
t=t[RokNarozeni>="1940"]
t[Umrti!=""&DatumUmrtiLPZ=="",DatumUmrtiLPZ:=Umrti]
t[Umrti!=""&DatumUmrtiLPZ!=Umrti,Umrti:=DatumUmrtiLPZ]
t=t[!(DatumUmrtiLPZ!=""&Datum_Prvni_davka>DatumUmrtiLPZ)]
t[,vax:=Datum_Prvni_davka!=""&Datum_Prvni_davka<"2021-24"]
t[,born:=ifelse(RokNarozeni>="2000","2000+",RokNarozeni)]
t[Datum_Prvni_davka>="2021-24",Datum_Prvni_davka:=""]

t2=t[Infekce%in%c(NA,1)]
dead=t2[,.(unvaxdead=sum(!vax),vaxdead=sum(vax)),.(week=DatumUmrtiLPZ,born)]
coviddead=t[,.(unvaxcoviddead=sum(!vax),vaxcoviddead=sum(vax)),.(week=Umrti,born)]
vax=t2[,.(newvax=.N),.(week=Datum_Prvni_davka,born)]

a=merge(dead,merge(coviddead,vax,all=T),all=T)[week!=""]
a=merge(do.call(CJ,lapply(a[,1:2],unique)),a,all=T);a[is.na(a)]=0

a=merge(t2[,.(startpop=.N),born],a)
a[,unvaxpop:=startpop-cumsum(unvaxdead)-cumsum(newvax),born]
a[,vaxpop:=cumsum(newvax)-cumsum(vaxdead),born]

l=a[,.(week,dead=c(unvaxdead,vaxdead),covid=c(unvaxcoviddead,vaxcoviddead),pop=c(unvaxpop,vaxpop),dose=rep(1:2,each=.N),born)]
l=rbind(l,l[,.(dead=sum(dead),pop=sum(pop),covid=sum(covid),dose=0),.(born,week)])
l=l[week>="2021-24"]

l=merge(l,l[week=="2022-01",sum(pop),born][,.(std=V1/sum(V1),born)])

l2=l[,.(dead=sum(dead),covid=sum(covid),pop=sum(pop)),.(week,dose)]
p1=l2[,.(y=c(dead,dead-covid),type=1:2,facet=1),.(week,dose)]
p2=l2[,.(y=c(dead,dead-covid)/pop/7*365e5,type=1:2,facet=2),.(week,dose)]
p3=l[,.(y=c(sum(dead/pop*std),sum((dead-covid)/pop*std))/7*365e5,type=1:2,facet=3),.(week,dose)]

cum=l[,.(dead=cumsum(dead),covid=cumsum(covid),pop=cumsum(pop),week),.(born,dose,std)]
c1=cum[,.(y=c(sum(dead),sum(dead-covid)),type=1:2,facet=1),.(week,dose)]
c2=cum[,.(y=c(sum(dead),sum(dead-covid))/sum(pop)/7*365e5,type=1:2,facet=2),.(week,dose)]
c3=cum[,.(y=c(sum(dead/pop*std/7*365e5),sum((dead-covid)/pop*std/7*365e5)),type=1:2,facet=3),.(week,dose)]

p=rbind(rbind(c1,c2,c3)[,cum:=2],rbind(p1,p2,p3)[,cum:=1])

p[,dose:=factor(dose,,c("Total","Unvaccinated","Vaccinated"))]
p[,type:=factor(type,,c("All causes","Not COVID"))]
p[,facet:=factor(facet,,c("Deaths","CMR","ASMR"))]
p[,cum:=factor(cum,,c("Not cumulative","Cumulative"))]

iso=as.Date(0:1e4*7,"1970-1-1");names(iso)=format(iso,"%G-%V")
p[,x:=iso[week]]

# Filter to only rows with valid data
p <- p[!is.na(x) & is.finite(y) & !is.na(facet) & !is.na(cum)]

# Drop facet-cum combinations with no data
tab <- p[, .N, by = .(facet, cum)][N > 0]
p <- merge(p, tab, by = c("facet", "cum"))

# Only plot one panel at a time: e.g., CMR cumulative
# p <- p[facet == "CMR" & cum == "Cumulative"]

# Extra safeguard: remove non-finite rows and compute valid y-range
yrange <- range(p$y[is.finite(p$y)], na.rm = TRUE)
p <- p[is.finite(y) & !is.na(y)]

# Ensure date-related variables are Date type
xstart <- as.Date("2021-01-01")
xend <- as.Date("2025-01-01")
xbreak <- seq(xstart + 182, xend, by = "year")
xlab <- format(xbreak, "%Y")

# Abort if data invalid
if (nrow(p) == 0 || diff(yrange) == 0 || any(!is.finite(yrange))) {
  stop("Filtered data is empty or y-range invalid — skipping plot.")
}

# Hardcoded y-axis for testing to avoid viewport bug
ymin <- 0
ymax <- max(p$y, na.rm = TRUE)
ybreaks <- pretty(c(ymin, ymax), 4)

library(openxlsx)

# Create a clean exportable table of week-level data
export_data <- p[, .(week, date=as.character(x), dose=as.character(dose), type=as.character(type),
                     facet=as.character(facet), cum=as.character(cum), value=y)]

# Write to Excel
write.xlsx(export_data, file = "henjin/ASMR_CMR_by_week.xlsx", asTable = TRUE)

# exit now since plot won't work
q()

# Generate the plot
plot <- ggplot(p) +
  facet_null() +
  geom_vline(xintercept = xbreak, color = "gray87", linewidth = 0.4, lineend = "square") +
  geom_vline(xintercept = iso["2021-24"] - 3.5, color = "gray87", linewidth = 0.4, linetype = "22") +
  geom_line(aes(x = x, y = y, color = dose, alpha = type), linewidth = 0.6) +
  labs(x = NULL, y = NULL, title = "Czech Republic, people born in 1940 or later") +
  scale_x_date(limits = c(xstart, xend), breaks = xbreak, labels = xlab) +
  scale_y_continuous(
    limits = c(ymin, ymax),
    breaks = ybreaks,
    labels = function(x) ifelse(x == max(x), "", ifelse(x >= 1e3, paste0(x / 1e3, "k"), x))
  ) +
  scale_color_manual(values = c("black", "#6666ff", "#ff6666")) +
  scale_alpha_manual(values = c(1, 0.4)) +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(
    axis.text = element_text(size = 11, color = "gray50", margin = margin(2, 2, 2, 2)),
    axis.ticks.length = unit(0, "pt"),
    legend.background = element_rect(color = "gray87", linewidth = 0.4),
    legend.box = "vertical",
    legend.box.just = "center",
    legend.box.margin = margin(NA, NA, 1),
    legend.box.spacing = unit(0, "pt"),
    legend.direction = "horizontal",
    legend.key = element_blank(),
    legend.key.height = unit(13, "pt"),
    legend.key.width = unit(26, "pt"),
    legend.margin = margin(2.5, 5.5, 2.5, 2.5),
    legend.position = "top",
    legend.spacing.x = unit(3, "pt"),
    legend.spacing.y = unit(0, "pt"),
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4, color = "gray87"),
    panel.spacing = unit(2, "pt"),
    plot.title = element_text(size = 11, face = 2, hjust = 0.5, margin = margin(1, NA, 4)),
    strip.background = element_blank(),
    strip.text = element_text(size = 11, margin = margin(3, 3, 3, 3))
  )

# Save to file (bypasses broken viewport in RStudio)
#ggsave("cmr_cumulative_plot.png", plot, width = 9, height = 6, dpi = 300)
png("cmr_cumulative_plot.png", width = 9, height = 6, units = "in", res = 300)
print(plot)
dev.off()
