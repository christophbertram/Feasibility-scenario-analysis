# Script to calculate model-specific regional average governance scores (gov),
# carbon price scales (sca), maximum carbon prices (max),
# maximum emission reduction rates (red), and a theoretical limit to fastest possible relative decarbonization (lim)


library(dplyr)
library(tidyr)
library(magclass)
library(madrat)
library(ggplot2)
library(grid) #grid.arrange
library(gridExtra) #grid.arrange
library(cowplot)
library(png) #include png in plot

filepath <- "data/gov_ssp2_country_04_04_2023.csv"
data <- read.csv(filepath)

# remove all rows with gov_norm == NA
data <- data[!is.na(data$gov_norm), ]

# convert to magpie
m <- data %>%
  pivot_longer(cols = c("gov_norm", "gdppc", "pop"), names_to = "variable", values_to = "value") %>%
  select(c(region = "iso3c", year = "Year", "variable", "value")) %>%
  as.magpie()

# TKM misses data for 2020-2025 - remove the whole country to avoid incomplete data
# (alternative: remove 2020-2025)
m <- m["TKM", , invert= TRUE]

# weigh with population
w <- m[, , "pop"]
## start of model for loop ----
 for(mod in c("ME","RE","PO","IM","GE","GC","WI","AI","CO","r10")){ # each model identified by first two letters
  # for(mod in c("ME")){ #alternative start of for loop to only run one model

## calculation of region governance indicator ----
rmap <- madrat::toolGetMapping(file.path('mappings',paste0('iso_',mod,'.csv')), type = "region") %>%
  select(c(region = "iso", H12 = "model"))
rmap$region <- toupper(rmap$region)
data_agg <- madrat::toolAggregate(m[,,"gov_norm"], rel = rmap, from = "region", to = "H12", weight = w, partrel = TRUE)

#duplicate region list: original name for the dynamic projections, with _f suffix for frozen case
data_frozen <-data_agg
# original region name for dynamic projections
nfroz <- getRegions(data_agg)
#region names with f for frozen governance levels
froz <- paste0(nfroz,"_f")
getRegions(data_frozen) <- froz
#make governance constant for 
data_frozen[,c(seq(2021,2029),seq(2030,2095,5)),] <- data_frozen[,2020,]
#put dynamic and frozen dataset together
data_agg <- mbind(data_agg,data_frozen)
#write out files both in long and wide format
write.magpie(collapseDim(data_agg,3),file.path('output',paste0("gov_",mod,".csv")),comment="Population-weighted average governance indicator across regions and time, based on Andrijevic et al.")
write.magpie(collapseDim(data_agg,3),file.path('output',paste0("gov_",mod,".cs4r")),comment="Population-weighted average governance indicator across regions and time, based on Andrijevic et al.")


# #scaling of carbon prices: starting from factor 10 differentiation, going to 2 in 2050 ----
scale <- data_agg
for(it in c(1,2)){
  if(it == 1){regs<- froz} else {regs <- nfroz}
for(year in getYears(scale, as.integer = TRUE)){
for(reg in getRegions(scale[regs,,])){
#minimum carbon price as ratio of maximum carbon price, going from 0.1 in 2025 to 0.5 in 2050 and beyond
scale[reg,year,] <- min(0.5,0.1+(year-2025)/25*0.4)+
#this is thei value for regions with the minimum governance level in each time step, else linearly scaled, but all th the squared value of the relative position in terms of governance to increase the difference between low- and high governance regions, top 20% regions all get the same carbon price.
#this then also means that the largest changes in carbon price happen in the range from 0.6-0.8 of governance index where also the treshold approach (below) sees most change.
  (min(1,((data_agg[reg,year]-min(data_agg[regs,year,]))/(0.8*(max(data_agg[regs,year,])-min(data_agg[regs,year,]))))**2)*max(0.5,0.9-(year-2025)/25*0.4))
}}}
write.magpie(collapseDim(scale[,seq(2025,2095,5),],3),file.path('output',paste0("gov_sca_",mod,".csv")),comment="Relative regional carbon price expressed in terms of maximum in each period")
write.magpie(collapseDim(scale[,seq(2025,2095,5),],3),file.path('output',paste0("gov_sca_",mod,".cs4r")),comment="Relative regional carbon price expressed in terms of maximum in each period")


#emission reductions:
#threshold values of governance levels
trs1 <- 0.65
trs2 <- 0.70
trs3 <- 0.75
#decadal maximum reduction rates (maximal reduction per decade period)
red1 <- 0.2
red2 <- 0.25
red3 <- 0.4
red4 <- 0.7

## maximum reduction over time in five year time steps ----
#overall lower limit of emissions over time, expressed in terms of 2020 emissions
red <- data_agg
lim <- data_agg
red[,,] <-0
lim[,,] <-0
for(year in seq(2025,2095,5)){
for(reg in getRegions(scale)){
if(year ==2025){
if(data_agg[reg,year,]<trs1){
  #countries below first threshold
  lim[reg,year,]<-(1-red1)**(5/10)
  red[reg,year,]<-(1-red1)**(5/10)
} else if (data_agg[reg,year,]<trs2){
  #countries below second threshold
  lim[reg,year,]<-(1-red2)**(5/10)
  red[reg,year,]<-(1-red2)**(5/10)
} else if(data_agg[reg,year,]<trs3){
  #countries below third threshold
  lim[reg,year,]<-(1-red3)**(5/10)
  red[reg,year,]<-(1-red3)**(5/10)
} else {
  #countries above third threshold
lim[reg,year,]<- (1-red4)**(5/10)
red[reg,year,]<- (1-red4)**(5/10)
}
}else{
if(data_agg[reg,year,]<trs1){
  lim[reg,year,]<-lim[reg,year-5,]*(1-red1)**(5/10)
  red[reg,year,]<-(1-red1)**(5/10)
} else if (data_agg[reg,year,]<trs2){
  lim[reg,year,]<-lim[reg,year-5,]*(1-red2)**(5/10)
  red[reg,year,]<-(1-red2)**(5/10)
} else if (data_agg[reg,year,]<trs3){
  lim[reg,year,]<-lim[reg,year-5,]*(1-red3)**(5/10)
  red[reg,year,]<-(1-red3)**(5/10)
} else {
lim[reg,year,]<- lim[reg,year-5,]*(1-red4)**(5/10)
red[reg,year,]<- (1-red4)**(5/10)
}
}
}
}
#check if lim is below 8%, then jump to negative emissions is possible in next time step
for(year in seq(2025,2095,5)){
for(reg in getRegions(scale)){
if(lim[reg,year,]<0.08){
lim[reg,year,] <- -1
red[reg,year,] <- -1
}
}
}

#write out files in wide and long format
write.magpie(collapseDim(lim[,seq(2025,2095,5),],3),file.path('output',paste0("gov_lim_",mod,".csv")),comment="limits on mimimum regional emissions on trajectory that is always at maximum reduction rate (2020=1)")
write.magpie(collapseDim(lim[,seq(2025,2095,5),],3),file.path('output',paste0("gov_lim_",mod,".cs4r")),comment="limits on mimimum regional emissions on trajectory that is always at maximum reduction rate (2020=1)")
write.magpie(collapseDim(red[,seq(2025,2095,5),],3),file.path('output',paste0("gov_red_",mod,".csv")),comment="maximum reduction rate compared to previous 5-year time step (previous emissions = 1)")
write.magpie(collapseDim(red[,seq(2025,2095,5),],3),file.path('output',paste0("gov_red_",mod,".cs4r")),comment="maximum reduction rate compared to previous 5-year time step (previous emissions = 1)")


## maximum carbon price allowed for low governance region ----
max1 <- 30 #below 0.65 governance
max2 <- 40 #below 0.7 governance
max3 <- 60 #below 0.75 governance
max4 <- 100#first time step (5 years) above 0.75 governance for smoother trajectories
max5 <- 180#second time step (10 years) above 0.75 governance for smoother trajectories
max6 <- 100#above 0.75 governance 2025
max7 <- 300#above 0.75 governance 2030
rate <- 3 # growth rate for governance 1,2, and 3 levels
rate2 <- 7 # growth rate for governance 4 level (>0.75)
max_price <- data_agg[,seq(2025,2095,5),]
for(year in getYears(max_price, as.integer = TRUE)){
  for(reg in getRegions(max_price)){
   if(year == 2025){
    if(data_agg[reg,year,]<trs1){
      max_price[reg,year,]<-max1/2
    } else if (data_agg[reg,year,]<trs2){
      max_price[reg,year,]<-max2/2
    } else if(data_agg[reg,year,]<trs3){
      max_price[reg,year,]<-max3/2
    } else {
      max_price[reg,year,]<-max6
    }
   } else {
     if(data_agg[reg,year,]<trs1){
       max_price[reg,year,]<-max1*(1+rate/100)^(year-2030)
     } else if (data_agg[reg,year,]<trs2){
       max_price[reg,year,]<-max2*(1+rate/100)^(year-2030)
     } else if(data_agg[reg,year,]<trs3){
       max_price[reg,year,]<-max3*(1+rate/100)^(year-2030)
     } else {
       if(data_agg[reg,year-5,]<trs3){#first time step after crossing threshold
         max_price[reg,year,]<-max4*(1+rate/100)^(year-2030)
       } else {
         if(data_agg[reg,year-10,]<trs3){#second time step after crossing threshold
           max_price[reg,year,]<-max5*(1+rate/100)^(year-2030)
         } else {
           if(year > 2030 && data_agg[reg,year-15]<trs3){#third time step after crossing threshold: intermediate step towards (faster growing) high governance trajectory
             max_price[reg,year,]<-(max5*(1+rate/100)^(year-2030)+max7*(1+rate2/100)^(year-2030))/2
           } else {
       max_price[reg,year,]<-max7*(1+rate2/100)^(year-2030)
           }
       }
     }
   }}
  }}

#write out files in both long and wide format
write.magpie(collapseDim(max_price,3),file.path('output',paste0("gov_maxCO2_",mod,".csv")),comment="* maximum carbon price in 2020$/tCO2")
write.magpie(collapseDim(max_price,3),file.path('output',paste0("gov_maxCO2_",mod,".cs4r")),comment="* maximum carbon price in 2020$/tCO2")



## plot data for R10----
if (mod=="r10"){

#convert to data.frame
dat <- rbind(as.data.frame(red) |> select(-Cell) |> rename_with(tolower) |> rename(variable=data1) |> filter(year %in% seq(2020,2100,5)) |> mutate(variable="Maximum reduction (%)"),
             as.data.frame(scale) |> select(-Cell) |> rename_with(tolower) |> rename(variable=data1) |> filter(year %in% seq(2020,2100,5)) |> mutate(variable="Scale (%)"),
             as.data.frame(max_price) |> select(-Cell) |> rename_with(tolower) |> rename(variable=data1) |> filter(year %in% seq(2020,2100,5)) |> mutate(variable="Maximum carbon price ($/t CO2)"),
             as.data.frame(data_agg) |> select(-Cell) |> rename_with(tolower) |> rename(variable=data1) |> filter(year %in% seq(2020,2100,5)) |> mutate(variable="Governance indicator (1)"))



data <- left_join(data,rmap,by=c("iso3c"="region"))
data$H12 <- gsub("R10","",data$H12)
dat$region <- as.character(dat$region)
dat$region <- gsub("R10","",dat$region)
dat$year <- as.numeric(as.character(dat$year))
froz <-gsub("R10","",froz)
nfroz <-gsub("R10","",nfroz)

#plot exact location for all countries with population of more than 25 Mio in 2020
plotd <- data|> filter(Year==2020)|> filter(pop >25) |> arrange(gov_norm)
plotd$jit <- c(rep(c(1,2,3,4),12),1)+rep(c(0,0,0,0,0.5,0.5,0.5,0.5),5)
# plotd[plotd$iso3c %in% c("ZAF","USA","GBR"),]$pop <- c(59.00900,330.93900,67.55793)
# plotd[plotd$iso3c %in% c("ARG","VNM","SAU"),]$pop <- c(59.00900,330.93900,67.55793)

cbPal <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')

color_scale <- scale_color_manual(values=c("NORTH_AM"=cbPal[1],"PAC_OECD"=cbPal[2],"EUROPE"=cbPal[3],"CHINA+"=cbPal[4],"REST_ASIA"=cbPal[5],
                 "INDIA+"=cbPal[6],"LATIN_AM"=cbPal[7],"MIDDLE_EAST"=cbPal[8],"REF_ECON"=cbPal[9],"AFRICA"=cbPal[10]))

p1 <- ggplot()+
  geom_line(data=dat |> filter(variable=="Governance indicator (1)",region %in% froz),aes(x=year,y=value,alpha=region),color="grey",linewidth=.5)+
  scale_alpha_manual(values=rep(1,11))+
  geom_hline(aes(yintercept=c(0.65,0.7,0.75)),linetype="dashed",color="grey")+
  geom_line(data=dat |> filter(variable=="Governance indicator (1)",!region %in% froz),aes(x=year,y=value,color=region))+
  color_scale+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  geom_text(data=plotd,aes(x=2023-jit*5,y=gov_norm,label=iso3c,color=H12),size=2,fontface="bold")+
  xlab("")+ylab("Governance indicator (1)")+ theme(legend.position="none")+xlim(2000,2100)


p2 <-
  ggplot()+
  geom_line(data=dat |> filter(variable=="Scale (%)",region %in% froz),aes(x=year,y=value*100,alpha=region),color="grey",linewidth=.5)+
  scale_alpha_manual(values=rep(1,11))+
  geom_line(data=dat |> filter(variable=="Scale (%)",!region %in% froz),aes(x=year,y=value*100,color=region))+
  color_scale+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  geom_text(data=dat|>filter(variable=="Scale (%)",value==1,year==2030,!region %in% froz),
           aes(x=2030,y=c(104,94,84),label=region,color=region))+xlim(2000,2100)+
  xlab("")+ylab("Relative carbon price \n scaling (%)")+ theme(legend.position="none")


p3 <- ggplot()+
  geom_line(data=dat |> filter(variable=="Maximum carbon price ($/t CO2)",region %in% froz),aes(x=year,y=value,alpha=region),color="grey",linewidth=.5)+
  scale_alpha_manual(values=rep(1,11))+
  geom_line(data=dat |> filter(variable=="Maximum carbon price ($/t CO2)",!region %in% froz),aes(x=year,y=value,color=region))+
  color_scale+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim=c(0,500)) + 
  geom_text(data=dat |> filter(variable=="Maximum carbon price ($/t CO2)",year==2025,value>90,!region %in% froz),
           aes(x=2025,y=c(225,185,145),label=region,color=region))+xlim(2000,2100)+
  xlab("")+ylab("Maximum carbon price \n ($/t CO2)")+ theme(legend.position="none")


plotd4 <- dat |> filter(variable=="Maximum reduction (%)",year>2020)
plotd4$value <- (1-plotd4$value^(1/5))*100
plotd4[is.na(plotd4$value),]$value <- 100

p4 <- ggplot()+
  geom_line(data=plotd4 |> filter(region %in% froz),aes(x=year,y=value,alpha=region),color="grey",linewidth=.5)+
  scale_alpha_manual(values=rep(1,11))+
  geom_line(data=plotd4|> filter(!region %in% froz),aes(x=year,y=value,color=region))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim=c(0,20)) + 
  geom_text(data=dat |> filter(variable=="Maximum reduction (%)",year==2025,value<0.6,!region %in% froz),
                                                           aes(x=2030,y=c(15.5,14,12.5),label=region,color=region))+
  geom_text(data=dat |> filter(variable=="Maximum reduction (%)",year==2075,value>0.5,value<0.7,!region %in% froz),
            aes(x=2075,y=c(7.5,6),label=region,color=region))+ guides(color="none",alpha="none")+
  color_scale+
  xlab("")+ylab("Maximum relative yearly \n reduction (%)") + xlim(2000,2100)

onlyleg <- ggplot(plotd4|> filter(region %in% nfroz), aes(variable, fill = region)) +
  scale_fill_manual(values=c("NORTH_AM"=cbPal[1],"PAC_OECD"=cbPal[2],"EUROPE"=cbPal[3],"CHINA+"=cbPal[4],"REST_ASIA"=cbPal[5],
                              "INDIA+"=cbPal[6],"LATIN_AM"=cbPal[7],"MIDDLE_EAST"=cbPal[8],"REF_ECON"=cbPal[9],"AFRICA"=cbPal[10]),
                    breaks=c('NORTH_AM','PAC_OECD','EUROPE','CHINA+','REST_ASIA','INDIA+','LATIN_AM','MIDDLE_EAST','REF_ECON','AFRICA'))+
  geom_bar()

#plot legend only
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(onlyleg)

## paper Figure 1 ----
#use the plot elements defined above, and arrange them together with additional
#layouting elements
arr_text <-
  ggplot()+
  geom_line(data=data.frame(x=c(0,1.1,0,1.1),y=c(0,1,0,-1),z=c("1","1","2","2")),
            aes(x=x,y=y,group=z,linetype=z),arrow=arrow(length=unit(0.2,"cm")))+
  scale_linetype_manual(values=c("solid","dashed"),guide="none")+
  geom_text(aes(x=2.5,y=0.8,label="Price constraint:"),fontface="bold",hjust = "righ")+
  geom_text(aes(x=2.7,y=-0.8,label="Quantity constraint:"),fontface="bold",hjust = "righ")+
  geom_text(aes(x=5.2,y=0.7,label="differentiated and capped \n carbon prices"),hjust = "righ",size=3.5,lineheight = 1)+
  geom_text(aes(x=5.2,y=-0.6,label="emission reduction \n constraints"),hjust = "righ",size=3.5,lineheight = 1)+
  xlim(0,7)+
  theme_void()

img <- readPNG("plots/Inst.png")
inst_img <- rasterGrob(img, interpolate=TRUE)

A_inst <- ggplot()+
  geom_text(aes(x=0,y=-3,label = "A"),size=5,fontface="bold")+
  annotation_custom(inst_img, xmin=2, xmax=6, ymin=-3, ymax=0) +
  ylim(-3,0)+xlim(0,6)+
  theme_void()

B <- ggplot()+  geom_text(aes(x=0,y=0,label = "B \n \n \n \n"),size=5,fontface="bold")+  theme_void()
C <- ggplot()+  geom_text(aes(x=0,y=0,label = "C \n \n \n \n"),size=5,fontface="bold")+  theme_void()
D <- ggplot()+  geom_text(aes(x=0,y=0,label = "D \n \n"),size=5,fontface="bold")+  theme_void()


lay <- rbind(c(1 ,3 ,6 ,8 ,9 ),
             c(2 ,NA,6 ,NA,9 ),
             c(2 ,4 ,4 ,4 ,10),
             c(2 ,5 ,7 ,NA,10),
             c(NA,NA,7 ,NA,10))
h_sc <- 1.2
h_1<- 3  *h_sc
h_2<- 1.5*h_sc
h_3<- 1.5*h_sc
h_4<- 1.5*h_sc
h_5<- 3  *h_sc
w_1<- 7  *h_sc
w_2<- 1  *h_sc
w_3<- 7  *h_sc
w_4<- 1  *h_sc
w_5<- 7  *h_sc
comb <- grid.arrange(A_inst,p1,B,arr_text,D,p2,p4,C,p3,legend,
                     heights=unit(c(h_1,h_2,h_3,h_4,h_5),c("cm","cm","cm","cm","cm")),
                     widths=unit(c(w_1,w_2,w_3,w_4,w_5),
                                 c("cm","cm","cm","cm","cm")),layout_matrix=lay)
# gpl <- arrangeGrob(yhist,con,t,xhist,nrow=2,widths = 1:2,heights=2:1)
ggsave(file.path('plots','Fig1.png'),comb,height=h_1+h_2+h_3+h_4+h_5,width=w_1+w_2+w_3+w_4+w_5,unit="cm")
ggsave(file.path('plots','Fig1.pdf'),comb,height=h_1+h_2+h_3+h_4+h_5,width=w_1+w_2+w_3+w_4+w_5,unit="cm")

### Extended Data Figure 2 ----------

p5 <- ggplot()+
  geom_text(data=data |> filter(Year==2020,pop>25),aes(x=gdppc,y=gov_norm,label=iso3c,color=H12))+
  geom_text(data=data |> filter(Year==2020,pop<=25),aes(x=gdppc,y=gov_norm,label=iso3c,color=H12),size=2)+
  scale_x_log10(minor_breaks=c(seq(1000,10000,1000),seq(10000,100000,10000)))+xlab("GDP per capita in PPP ($)")+ylab("Governance indicator (1)")+
  color_scale + theme_bw() + theme(legend.position="none") 

comb2 <- grid.arrange(p5,legend,widths=2:1)
ggsave(file.path('plots','ExDataFig2.png'),comb2,height=11,width=17,unit="cm")
ggsave(file.path('plots','ExDataFig2.pdf'),comb2,height=11,width=17,unit="cm")

}
}

