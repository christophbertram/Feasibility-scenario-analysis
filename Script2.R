library(conflicted)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('lag', 'dplyr', quiet = TRUE)

library(tidyverse)
library(arrow)
library(zoo)
library(ggnewscale)
library(readxl)
library(grid) #grid.arrange
library(gridExtra) #grid.arrange
library(png) #include png in plot

## data loading and preparation ----

# Load snapshot files (to be downloaded from https://data.ece.iiasa.ac.at/engage/#/downloads
# see also zenodo entry 10.5281/zenodo.11562539
  data <- read_xlsx(file.path('data','10.5281_zenodo.11562539_feasibility_of_peak_temperature_targets_in_light_of_institutional_constraints.xlsx')) |>
  pivot_longer(cols = seq(6,29),names_to="year")|>
  mutate(year=as.numeric(gsub("X","",year)))|>
  rename(region=Region,variable=Variable,unit=Unit,model=Model,scenario=Scenario) |>
  filter(!is.na(value))



#get rid of spaces in scenario names:
data$scenario <- gsub(" ", "",data$scenario)

# Carbon budget
data <- data |>
  mutate(cbudget = as.numeric(str_extract(scenario, "^T34_(\\d+)", 1)))

# scenario variant
data <- data |>
  mutate(scen = str_extract(scenario, "^T34_\\d+_([a-zA-Z_]+)$", 1))

# rename long model name variable
data <- data |> rename(mo = model)

# model short name
data <- data |>
  mutate(Model = case_when(
    str_detect(mo, "MESSAGE") ~ "MESSAGE",
    str_detect(mo, "AIM") ~ "AIM",
    str_detect(mo, "GEM-E3") ~ "GEM-E3",
    str_detect(mo, "COFFEE") ~ "COFFEE",
    str_detect(mo, "IMAGE") ~ "IMAGE",
    str_detect(mo, "POLES") ~ "POLES",
    str_detect(mo, "REMIND") ~ "REMIND",
    str_detect(mo, "WITCH") ~ "WITCH",
    .default = mo))

# Overall filter
data <- data |>
  filter(year >= 2005 & year <= 2100) |>
  filter(!is.na(value)) #|>
#filter(region=="World" | str_detect(region,"\\|"))
# |> # Only Model regions and world
#   filter(!str_detect(region, "10R Map")) # Remove aggregate POLES region

# Shorten region name: get rid of model name
data <- data |>
  mutate(region = ifelse(region != "World",str_extract(region,"\\|(.*)$",1), region)) |>
  filter(region != "OECD90+EU")

# shorten scenario name: get rid of initial "T34_"
data <- data |> mutate(scenario = substr(scenario,5,50))

data <- as_tibble(data)

#overview of scens
scens <- unique(data$scenario)
scen_ord <- c("ref","govpr","govem","biop","techb","bitb_ref","bitb_pr","bitb_em","enab_pr","enab_em","feas_ref","feas_pr","feas_em","feaDAC_ref","feaDAC_pr")
scen_ord2 <- c("ref","govpr","govqu","govem","biop","techb","bitb_ref","bitb_pr","bitb_qu","bitb_em","enab_ref","enab_pr","enab_qu","enab_em","feas_ref","feas_pr","feas_qu","feas_em","feaDAC_ref","feaDAC_pr","feaDAC_qu","feaDAC_em")
mods <- unique(data$Model)

#add unique run variable
data$run <- paste0(data$Model,"-",data$scenario)
runs <- unique(data$run)

#list and order of first protocol table
tab1 <- c("NPi2100","NPi2100_ld","NDC2100","1000_ref","1000_govpr","1000_govem","1000_biop","1000_techb","1000_bitb_ref","1000_bitb_pr","1000_bitb_em","1000_enab_pr","1000_enab_em","1000_feas_ref","1000_feas_pr","1000_feas_em","1000_feaDAC_ref","1000_feaDAC_pr")
#list and order of second protocol table
tab2 <- c("m550_ref","m550_govpr","m550_govem","m550_biop","m550_techb","m550_bitb_ref","m550_bitb_pr","m550_bitb_em","m550_enab_pr","m550_enab_em","m550_feas_ref","m550_feas_pr","m550_feas_em","m550_feaDAC_ref","m550_feaDAC_pr")


theme_set(theme_bw())
theme_update(axis.text=element_text(size=11),axis.title=element_text(size=13))

#calculate net zero years:
var <- 'Emissions|CO2'
emi <- data |>
  filter(variable == var,year>=2015) |>
  select(-unit,-variable,-run)
netz <- emi |>
  filter(region == "World") |>
  select(-region,-mo,-cbudget,-scen) |>
  group_by(Model,scenario) |>
  complete(year = 2015:2100) |>
  mutate(value = na.approx(value)) |>
  mutate(dummy = ifelse(value<250,year,2100)) |> #mark all years with emissions below 0.25 Gt
  summarize(netzeroyear = min(dummy)) #|>
# pivot_wider(names_from = scenario,values_from = netzeroyear)

#disregard emissions after net-zero year for peak budget calculation
for(imod in unique(data$Model)){
  for(iscen in unique(data[data$Model==imod,]$scenario)){
    data[data$region=="World"&data$variable==var&data$Model==imod&data$scenario==iscen&data$year>
           netz[netz$Model==imod&netz$scenario==iscen,]$netzeroyear,]$value<-0
  }
}


#|label: overview1
scen1 <- c("ref","bitb_ref","govem","bitb_em","enab_em","feas_em")
scen2 <- c("ref","govpr","govem","bitb_ref","bitb_pr","bitb_em","enab_ref","enab_pr","enab_em","feas_ref","feas_pr","feas_em")
scen2 <- c("ref","govpr","govem","bitb_pr","bitb_em","enab_pr","enab_em","feas_pr","feas_em")

# images and text plots for figures
img1 <- readPNG("plots/oo-oo-oo2.png")
img2 <- readPNG("plots/Tech-oo-oo2.png")
img3 <- readPNG("plots/oo-oo-Inst2.png")
img4 <- readPNG("plots/Tech-oo-Inst2.png")
img5 <- readPNG("plots/oo-Enab-Inst2.png")
img6 <- readPNG("plots/Tech-Enab-Inst2.png")
p1 <- rasterGrob(img1, interpolate=TRUE)
p2 <- rasterGrob(img2, interpolate=TRUE)
p3 <- rasterGrob(img3, interpolate=TRUE)
p4 <- rasterGrob(img4, interpolate=TRUE)
p5 <- rasterGrob(img5, interpolate=TRUE)
p6 <- rasterGrob(img6, interpolate=TRUE)
sizelab <- 3 #font size of x-tick labels
t1 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Cost \n effective \n"),aes(x=1,y=1,label=label),size=sizelab)
t2 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Tech \n \n "),aes(x=1,y=1,label=label),size=sizelab)
t3 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Institutional \n \n"),aes(x=1,y=1,label=label),size=sizelab)
t4 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Tech & \n Institutional \n"),aes(x=1,y=1,label=label),size=sizelab)
t5 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Enablers & \n Institutional \n"),aes(x=1,y=1,label=label),size=sizelab)
t6 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Tech & \n Enablers & \n Institutional"),aes(x=1,y=1,label=label),size=sizelab)

#color-blind palette for plots that do not include COFFEE model
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#333333", "#0072B2", "#D55E00", "#CC79A7","#F0E442") #,
#color-blind palette for plots that do include COFFFE model (using the least visible color F0E442 for model that is not on all figures)
cbCPalette <- c("#E69F00","#F0E442", "#56B4E9", "#009E73","#333333", "#0072B2", "#D55E00", "#CC79A7") #,


## Main paper figure 4 (min-max rel carbon) ----

#Figure 4
scen1 <- c("ref","bitb_ref","govem","bitb_em","enab_em","feas_em")
per <- 2050

cpri <- data |> filter(variable=="Price|Carbon",year==per) |> group_by(Model,scenario) |> summarize(value=max(value)) |> ungroup()
cpri1000 <- cpri |> filter(scenario %in% grep("1000",scens,value=T)) |>  mutate(scenario=substr(scenario,6,55)) |> filter(scenario %in% scen1)
cpri1000r <- NULL
for(mod in unique(cpri1000$Model)){
  ref <- cpri1000 |> filter(Model==mod, scenario=="ref") |> select(value)
  cpri1000m <- cpri1000 |> filter(Model==mod) |> mutate(rel = value/as.numeric(ref))
  # cpri1000m$rel <- cpri1000m$rel/ref
  cpri1000r <- rbind(cpri1000r,cpri1000m)
}
cpri1000$scenario <- factor(cpri1000$scenario,levels=scen1)
cpri1000r$scenario <- factor(cpri1000$scenario,levels=scen1)

mincpri <- data |> filter(variable=="Price|Carbon",year==per) |> group_by(Model,scenario) |> summarize(value=min(value)) |> ungroup()
mincpri1000 <- mincpri |> filter(scenario %in% grep("1000",scens,value=T)) |>  mutate(scenario=substr(scenario,6,55)) |> filter(scenario %in% scen1)
mincpri1000r <- NULL
for(mod in unique(mincpri1000$Model)){
  ref <- mincpri1000 |> filter(Model==mod, scenario=="ref") |> select(value)
  mincpri1000m <- mincpri1000 |> filter(Model==mod) |> mutate(rel = value/as.numeric(ref))
  # mincpri1000m$rel <- mincpri1000m$rel/ref
  mincpri1000r <- rbind(mincpri1000r,mincpri1000m)
}
mincpri1000$scenario <- factor(mincpri1000$scenario,levels=scen1)
mincpri1000r$scenario <- factor(mincpri1000$scenario,levels=scen1)

pl1 <- ggplot()+
  geom_hline(aes(yintercept = 1))+
  geom_boxplot(data=cpri1000r,aes(x=scenario,y=rel),fill="red",alpha=0.3)+
  geom_point(data=cpri1000r,aes(x=scenario,y=rel,color=Model),size=4,shape=1)+
  geom_text(aes(x="bitb_em",y=7.75,label="ME: 12.4"),size=2)+
  geom_boxplot(data=mincpri1000r,aes(x=scenario,y=rel),fill="yellow",alpha=0.3)+
  geom_point(data=mincpri1000r,aes(x=scenario,y=rel,color=Model),size=4,shape=1)+ylab("relative carbon price (Cost-effective = 1)")+
  geom_text(aes(x="bitb_ref",y=4.5,label="Higher carbon \n prices in \n regions with "),color="red",size=3)+
  geom_text(aes(x="bitb_ref",y=3.4,label="highest \ institutional \n capacity)"),color="red",fontface="bold",size=3)+
  geom_text(aes(x="bitb_ref",y=0.3,label="Lowest capacity"),color="#cccc22",fontface="bold",size=3)+
  geom_line(data=data.frame(x=c("bitb_ref","govem"),y=c(1,0.6),z=c("A","A")),aes(x=x,y=y,group = z ),arrow=arrow(length=unit(0.2,"cm")),color="#cccc22")+
  geom_line(data=data.frame(x=c("bitb_ref","govem"),y=c(2,3),z=c("A","A")),aes(x=x,y=y,group=z),arrow=arrow(length=unit(0.2,"cm")),color="red")+
  geom_line(data=data.frame(x=rep("bitb_em",2),y=c(0.8,1.2)),aes(x=x,y=y),arrow=arrow(length=unit(0.2,"cm")))+
  geom_line(data=data.frame(x=rep("bitb_em",2),y=c(7.9,8.3)),aes(x=x,y=y),arrow=arrow(length=unit(0.2,"cm")))+
  scale_shape_manual(values = c(seq(1,8)))+scale_color_manual(values=cbCPalette)+
  theme (plot.margin=margin(b=-12,l=5,r=5,t=5))+scale_y_continuous(limits = c(0, 8.301), breaks = seq(0,8))+
  xlab("")+scale_x_discrete(labels=NULL)

print(p1)

pl2 <- ggplot()+
  geom_polygon(data=data.frame(x=c(0,0,0.25,0.25),y=c(0,3,3,0)),aes(x=x,y=y),fill="red",alpha=0.5)+
  geom_polygon(data=data.frame(x=c(0,0,0.25,0.25),y=c(0,3,3,0)),aes(x=x,y=y),fill="yellow",color="black",alpha=0.5,size=0.2)+
  geom_polygon(data=data.frame(x=c(0.3,0.3,0.55,0.55),y=c(0,1.4,1.4,0)),aes(x=x,y=y),fill="yellow",color="black",alpha=0.5,size=0.2)+
  geom_polygon(data=data.frame(x=c(0.3,0.3,0.55,0.55),y=c(1.6,3,3,1.6)),aes(x=x,y=y),fill="red",color="black",alpha=0.5,size=0.2)+
  geom_text(aes(x=0.7,y=2.4,label="Highest \n capacity"),size=3,hjust = "left")+
  geom_text(aes(x=0.7,y=0.6,label="Lowest \n capacity"),size=3,hjust = "left")+
  geom_point(aes(x=3,y=3),color="white")+
  geom_point(aes(x=-1,y=3),color="white")+
  theme_void()

lay <- rbind(c(1 ,1 ,1 ,1 ,1 ,1 ,1 ,1),
             c(NA,2 ,3 ,4 ,5 ,6 ,7 ,14),
             c(NA,8 ,9 ,10,11,12,13,14))
h_plot<- 9
h_lab <- 1.5
h_pic <- 1.5
w_first <- 1.2
w_cell <- 184/144*h_pic
w_end <- 3.3
comb <- grid.arrange(pl1,t1,t2,t3,t4,t5,t6,p1,p2,p3,p4,p5,p6,pl2,heights=unit(c(h_plot,h_lab,h_pic),c("cm","cm","cm")),
                     widths=unit(c(w_first,w_cell,w_cell,w_cell,w_cell,w_cell,w_cell,w_end),
                                 c("cm","cm","cm","cm","cm","cm","cm","cm")),layout_matrix=lay)
# gpl <- arrangeGrob(yhist,con,t,xhist,nrow=2,widths = 1:2,heights=2:1)
ggsave(file.path('plots','Fig4.png'),comb,height=h_plot+h_lab+h_pic,width=w_first+6*w_cell+w_end,unit="cm")
ggsave(file.path('plots','Fig4.pdf'),comb,height=h_plot+h_lab+h_pic,width=w_first+6*w_cell+w_end,unit="cm")



## Policy Brief: Carbon budget until net-zero (Gt CO2) ----


#reference likelihood data from Joeri / Foster et al 2023 https://essd.copernicus.org/articles/15/2295/2023/

all_prob <- read.csv("data/budget_normal_IGCC_multiple_percentiles_zeroZEC_5pcincr.csv")
prob_1p5 <- data.frame(x=rep(0,dim(all_prob)[2]-2),y=as.numeric(all_prob[5,seq(3,21)]),prob=rev(c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)),
                       lab=c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%"))
prob_1p6 <- data.frame(x=rep(0,dim(all_prob)[2]-2),y=as.numeric(all_prob[6,seq(3,21)]),prob=rev(c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)),
                       lab=c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%"))
prob_1p8 <- data.frame(x=rep(0,dim(all_prob)[2]-2),y=as.numeric(all_prob[8,seq(3,21)]),prob=rev(c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)),
                       lab=c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%"))
prob_2p0 <- data.frame(x=rep(0,dim(all_prob)[2]-2),y=as.numeric(all_prob[10,seq(3,21)]),prob=rev(c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95)),
                       lab=c("5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%","70%","75%","80%","85%","90%","95%"))

#simple version for policy brief
var <- 'Emissions|CO2'
# var <- 'Emissions|CO2|Energy and Industrial Processes'
emi <- data |>
  filter(variable == var) |>
  select(-unit,-variable,-run)

cum_emis <- emi |>
  filter(scenario %in% c(tab2,"m550_bitb_em_f","m550_enab_em_f","m550_feas_em_f")) |>
  filter(region == "World") |>
  group_by(Model,scenario,region,mo,cbudget,scen) |>
  mutate(value = ifelse(value>0,value * 1e-3,0)) |> # get rid of negative numbers
  complete(year = 2015:2100) |>
  mutate(value = na.approx(value)) |>
  filter(year>2022) |> #only count emission from 2023 onward, as this is the budget in Forster
  mutate(value = cumsum(value)) |>
  filter(year==2100) |>
  mutate(scenario=substr(scenario,6,55)) |>
  ungroup()

#simple version for policy brief
cum_emi <- cum_emis |> filter(scenario %in% c("ref","bitb_em","feas_em"))
cum_emi$scenario <- factor(cum_emi$scenario,levels=c("ref","bitb_em","feas_em","dummy"))
cum_emi1 <- cum_emi
cum_emi1$scenario <- as.numeric(cum_emi1$scenario)

cum_emi1 <- cum_emi1 |> mutate(scen = case_when(
  scenario==1~"1",
  scenario==2~"2",
  scenario==3~"3"
))
pb <- ggplot()+
  geom_hline(aes(yintercept=1000-200),color="#444444")+#originally defined budgets of 1000 and 550 Gt for 2C and 1.5C are from 2018 onward, so reduce by 200 to account for 5 years of ~40 Gt between 2018 and 2023
  geom_hline(aes(yintercept=550-200),color="#444444")+
  geom_polygon(data=data.frame(x=c(3.3,3.3,4,4),y=c(300,1265,1265,300)),aes(x=x,y=y),fill="red",alpha=0.2)+ geom_text(aes(x=3.6,y=1175,label="<1.6C \n prob.:"),size=3)+
  geom_segment(data=prob_1p6 |> filter(prob %in% seq(30,90,10)),aes(x=3.4,xend=3.5,y=y,yend=y))+
  geom_text(data=prob_1p6|> filter(prob %in% seq(30,90,10)),aes(x=3.7,y=y,label=lab),size=3)+
  geom_point(data=cum_emi1,aes(x=scenario,y=value,size=5, stroke=1,color=scen,alpha=0.8),size=3)+scale_shape_manual(values = c(seq(1,3),1,seq(4,6),4))+ylim(300,1265)+
  scale_color_manual(values=c("#0072B2","#009E73","#CC79A7"))+ylab(bquote("cumulative emis. 2023 - net zero Gt CO"[2       ]))+
  scale_x_continuous(breaks=seq(1,6),minor_breaks = F,labels = c("Lowest \n cost","With \n constraints","With constraints \n and enablers","","",""))+xlab("")+theme_minimal()+theme(legend.position="none")
# +theme (axis.text.x = element_text (angle = 90, vjust = 0.5, hjust=1))

write.csv(cum_emi1,file.path('plots','policy_brief1.csv'),quote = F,row.names = F)
write.csv(prob_1p6,file.path('plots','policy_brief2.csv'),quote = F,row.names = F)
lay <- rbind(c(1 ,1 ,1 ,1 ,1 ,1 ,1 ),
             c(NA,2 ,NA,3 ,NA,4 ,NA))
h_plot<- 9
h_pic <- 1.5
w_first <- 1.2
w_gap <- 0.7
w_cell <- 184/144*h_pic
w_end <- 2.1
comb <- grid.arrange(pb,p1,p4,p6,heights=unit(c(h_plot,h_pic),c("cm","cm")),
                     widths=unit(c(w_first,w_cell,w_gap,w_cell,w_gap,w_cell,w_end),
                                 c("cm","cm","cm","cm","cm","cm","cm")),layout_matrix=lay)
# gpl <- arrangeGrob(yhist,con,t,xhist,nrow=2,widths = 1:2,heights=2:1)
ggsave(file.path('plots','FigPolicyBrief.png'),comb,height=h_plot+h_pic,width=w_first+3*w_cell+2*w_gap+w_end,unit="cm")
ggsave(file.path('plots','FigPolicyBrief.pdf'),comb,height=h_plot+h_pic,width=w_first+3*w_cell+2*w_gap+w_end,unit="cm")



## Main paper Figure 5: carbon budgets ----

#Paper version with more detail

var <- 'Emissions|CO2'
emi <- data |>
  filter(variable == var) |>
  select(-unit,-variable,-run)

cum_emis <- emi |>
  filter(scenario %in% c(tab2,"m550_govem_f","m550_bitb_em_f","m550_enab_em_f","m550_feas_em_f")) |>
  filter(region == "World") |>
  group_by(Model,scenario,region,mo,cbudget,scen) |>
  mutate(value = ifelse(value>0,value * 1e-3,0)) |> # get rid of negative numbers
  complete(year = 2015:2100) |>
  mutate(value = na.approx(value)) |>
  filter(year>2022) |>
  mutate(value = cumsum(value)) |>
  filter(year==2100) |>
  mutate(scenario=substr(scenario,6,55)) |>
  ungroup()

cum_emi <- cum_emis |> filter(scenario %in% scen1)
cum_emi$scenario <- factor(cum_emi$scenario,levels=scen1)

cum_emi$scenario <- factor(cum_emi$scenario,levels=c(scen1,"dummy"))
cum_emi1 <- cum_emi
cum_emi1$scenario <- as.numeric(cum_emi1$scenario)

cum_emi2 <- cum_emis |> filter(scenario %in% scen2)
cum_emi2$scenario <- factor(cum_emi2$scenario,levels=scen2)
cum_emi2$scenario <- as.numeric(cum_emi2$scenario)

scen3 <- c("ref","bitb_ref","govpr","govem","govem_f","bitb_pr","bitb_em","bitb_em_f","enab_pr","enab_em","enab_em_f","feas_pr","feas_em","feas_em_f")
cum_emi3 <- cum_emis |> filter(scenario %in% scen3) |> select(-scen)
cum_emi3 <- cum_emi3 |> rename(scen=scenario)
cum_emi3$scenario <- 0
cum_emi3[cum_emi3$scen=="ref",]$scenario <- 27
cum_emi3[cum_emi3$scen=="bitb_ref",]$scenario <- 22
cum_emi3[cum_emi3$scen=="govpr",]$scenario <- 14
cum_emi3[cum_emi3$scen=="govem",]$scenario <- 8
cum_emi3[cum_emi3$scen=="govem_f",]$scenario <- 2
cum_emi3[cum_emi3$scen=="bitb_pr",]$scenario <- 13
cum_emi3[cum_emi3$scen=="bitb_em",]$scenario <- 7
cum_emi3[cum_emi3$scen=="bitb_em_f",]$scenario <- 1
cum_emi3[cum_emi3$scen=="enab_pr",]$scenario <- 16
cum_emi3[cum_emi3$scen=="enab_em",]$scenario <- 10
cum_emi3[cum_emi3$scen=="enab_em_f",]$scenario <- 4
cum_emi3[cum_emi3$scen=="feas_pr",]$scenario <- 15
cum_emi3[cum_emi3$scen=="feas_em",]$scenario <- 9
cum_emi3[cum_emi3$scen=="feas_em_f",]$scenario <- 3

cum_emi3$shape <- "1"
for(i in seq(7,10)){
  cum_emi3[cum_emi3$scenario==i,]$shape<-"19"
}

cumemi3s <- cum_emi3 |> ungroup() |> select(-scen,-cbudget,-shape) |> pivot_wider(names_from=scenario)

img <- readPNG("plots/Inst.png")
inst_img <- rasterGrob(img, interpolate=TRUE)

f4a <- ggplot()+
  ggtitle("A        Lowest feasible budget")+
  geom_segment(data=cumemi3s,aes(x=2+1,xend=2+7,y=`1`,yend=`7`,color=Model),alpha=0.5,linetype="dotted")+
  geom_segment(data=cumemi3s,aes(x=2+7,xend=2+13,y=`7`,yend=`13`,color=Model),alpha=0.3,linetype="dashed")+
  geom_segment(data=cumemi3s,aes(x=2+9,xend=2+15,y=`9`,yend=`15`,color=Model),alpha=0.3)+
  geom_segment(data=cumemi3s,aes(x=2+8,xend=2+14,y=`8`,yend=`14`,color=Model),alpha=0.3,linetype="dashed")+
  geom_segment(data=cumemi3s,aes(x=2+10,xend=2+16,y=`10`,yend=`16`,color=Model),alpha=0.3)+
  geom_point(data=cum_emi3|>filter(scenario>20),aes(x=2+scenario,y=value,color=Model,shape=shape),size=3)+
  # geom_hline(aes(yintercept=390),color="#444444")+geom_hline(aes(yintercept=840),color="#444444")+
  geom_point(data=cum_emi3,aes(x=2+scenario,y=value,color=Model,shape=shape),size=3)+
  geom_text(aes(x=2+0.5,y=1520,label="Pessim."),fontface="bold",hjust = "left",size=3)+
  geom_text(aes(x=2+6.5,y=1520,label="Default"),fontface="bold",hjust = "left",size=3)+
  geom_text(aes(x=2+12.5,y=1520,label="Optimistic"),fontface="bold",hjust = "left",size=3)+
  geom_text(aes(x=2+0.5,y=1460,label="Frozen \n institut. \n capacity"),hjust = "left",vjust="top",size=3)+
  geom_text(aes(x=2+6.5,y=1460,label="Price & \n quantity"),hjust = "left",vjust="top",size=3)+
  geom_text(aes(x=2+12.5,y=1460,label="Only price \n differ."),hjust = "left",vjust="top",size=3)+
  annotation_custom(inst_img, xmin=2+6.4, xmax=2+10.6, ymin=1190, ymax=1340) +
  scale_shape_manual(values = c(1,16),guide="none")+
  coord_cartesian(ylim = c(340,1540),xlim=c(2+0.5,2+28))+
  geom_vline(aes(xintercept=2+5.5),linetype="dashed")+
  geom_vline(aes(xintercept=2+11.5),linetype="dashed")+
  geom_vline(aes(xintercept=2+19))+
  theme (plot.title.position = "plot",plot.margin=margin(b=-10,l=5,r=0,t=5))+
  ylab(bquote("cumulative emis. 2023 - net zero Gt CO"[2]))+scale_color_manual(values=cbbPalette)+
  scale_x_continuous(breaks=2+c(1,2,3,4,7,8,9,10,13,14,15,16,22,27),minor_breaks = F,labels = NULL)+xlab("")

#interpolate for finer display of color gradient
prob_1p5_i <- prob_1p5 |> select(y,prob) |>
  complete(prob=5:95) |> mutate(y = na.approx(y))
prob_1p6_i <- prob_1p6 |> select(y,prob) |>
  complete(prob=5:95) |> mutate(y = na.approx(y))
prob_1p8_i <- prob_1p8 |> select(y,prob) |>
  complete(prob=5:95) |> mutate(y = na.approx(y))
prob_2p0_i <- prob_2p0 |> select(y,prob) |>
  complete(prob=5:95) |> mutate(y = na.approx(y))

# Figure 5b
f4b <-  ggplot()+
  ggtitle("B 5-95% probability ranges")+
  geom_line(data=prob_1p6_i,aes(x=1,y=y,color=100-prob),linewidth=4)+
  geom_line(data=prob_1p8_i,aes(x=2,y=y,color=100-prob),linewidth=4)+
  geom_line(data=prob_2p0_i,aes(x=3,y=y,color=100-prob),linewidth=4)+
  geom_segment(data=prob_1p6 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+1,xend=x+0.3+1,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p6 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+1,xend=x+0.1+1,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p8 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+2,xend=x+0.3+2,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p8 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+2,xend=x+0.1+2,y=y,yend=y),color="white")+
  geom_segment(data=prob_2p0 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+3,xend=x+0.3+3,y=y,yend=y),color="white")+
  geom_segment(data=prob_2p0 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+3,xend=x+0.1+3,y=y,yend=y),color="white")+
  scale_color_gradient2(low = "red",mid="white",high="blue",midpoint=50,minor_breaks=seq(5,95,1),name="Limit \n likelihood")+
  coord_cartesian(ylim = c(340,1540))+guides(color = guide_colorbar(reverse = TRUE))+
  theme (plot.title.position = "plot",plot.margin=margin(b=-10,l=0,r=5,t=5))+
  xlab("")+ylab("")+xlim(c(1,3))+
  scale_x_continuous(breaks=seq(1,3),minor_breaks = NULL,labels=NULL)#c("1.6","1.8","2.0"))

# Alternative version of Figure 4b with additional 1.5 likelihoods.
f4c <-  ggplot()+
  ggtitle("B 5-95% probability ranges")+
  geom_line(data=prob_1p5_i,aes(x=1,y=y,color=100-prob),linewidth=4)+
  geom_line(data=prob_1p6_i,aes(x=2,y=y,color=100-prob),linewidth=4)+
  geom_line(data=prob_1p8_i,aes(x=3,y=y,color=100-prob),linewidth=4)+
  geom_line(data=prob_2p0_i,aes(x=4,y=y,color=100-prob),linewidth=4)+
  geom_segment(data=prob_1p5 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+1,xend=x+0.3+1,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p5 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+1,xend=x+0.1+1,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p6 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+2,xend=x+0.3+2,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p6 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+2,xend=x+0.1+2,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p8 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+3,xend=x+0.3+3,y=y,yend=y),color="white")+
  geom_segment(data=prob_1p8 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+3,xend=x+0.1+3,y=y,yend=y),color="white")+
  geom_segment(data=prob_2p0 |> filter(prob%in%c(25,50,75)),aes(x=x-0.3+4,xend=x+0.3+4,y=y,yend=y),color="white")+
  geom_segment(data=prob_2p0 |> filter(!prob%in%c(25,50,75)),aes(x=x-0.1+4,xend=x+0.1+4,y=y,yend=y),color="white")+
  scale_color_gradient2(low = "red",mid="white",high="blue",midpoint=50,minor_breaks=seq(5,95,1),name="Limit \n likelihood")+
  coord_cartesian(ylim = c(340,1540))+guides(color = guide_colorbar(reverse = TRUE))+
  theme (plot.title.position = "plot",plot.margin=margin(b=-10,l=0,r=5,t=5))+
  xlab("")+ylab("")+xlim(c(1,4))+
  scale_x_continuous(breaks=seq(1,4),minor_breaks = NULL,labels=NULL)#c("1.6","1.8","2.0"))


lines <- ggplot()+
  geom_line(data=data.frame(x=c(1.4,4.71,4.2,5.23,6.8,5.77,9.6,6.29),y=c(1,2,1,2,1,2,1,2),z=c(1,1,2,2,3,3,4,4)),
            aes(x=x,y=y,group=z))+
  geom_point(aes(x=c(0,11),y=c(1,1)),color="white")+theme_void()
temps <- ggplot()+
  geom_text(aes(x=0,y=0,label = "1.6°C 1.8°C 2.0°C         \n"),size=3)+
  # geom_point(aes(x=c(0.1),y=c(-0.01)),color="red")+
  theme_void()
temps2 <- ggplot()+
  geom_text(aes(x=c(1,2,3,4),y=rep(0,4),label = c("1.5°C","1.6°C","1.8°C","2.0°C")),size=3,angle=45)+
  # geom_point(aes(x=c(0.1),y=c(-0.01)),color="red")+
  theme_void()+xlim(c(0-1.4,5+4.6))
lay <- rbind(c(1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,16),
             c(NA,14,14,14,14,NA,NA,NA,NA,15),
             c(NA,2 ,3 ,4 ,5 ,NA,6 ,7 ,NA,NA),
             c(NA,8 ,9 ,10,11,NA,12,13,NA,NA))
h_plot<- 9
h_sec <- 0.8
h_lab <- 1.2
h_pic <- 1.25
w_first <- 1.33
w_gap <- 0.1
w_cell <- 184/144*h_pic
w_end <- 2.6
w_sec <- 6
# comb <- grid.arrange(f4a,t4,t3,t6,t5,t2,t1, p4,p3,p6,p5,p2,p1,lines,temps,f4b,
#                      heights=unit(c(h_plot,h_sec,h_lab,h_pic),c("cm","cm","cm","cm")),
#                      widths=unit(c(w_first,w_cell,w_cell,w_cell,w_cell,w_gap,w_cell,w_cell,w_end,w_sec),
#                                  c("cm","cm","cm","cm","cm","cm","cm","cm","cm","cm")),layout_matrix=lay)
comb <- grid.arrange(f4a,t4,t3,t6,t5,t2,t1, p4,p3,p6,p5,p2,p1,lines,temps2,f4c,
                     heights=unit(c(h_plot,h_sec,h_lab,h_pic),c("cm","cm","cm","cm")),
                     widths=unit(c(w_first,w_cell,w_cell,w_cell,w_cell,w_gap,w_cell,w_cell,w_end,w_sec),
                                 c("cm","cm","cm","cm","cm","cm","cm","cm","cm","cm")),layout_matrix=lay)
# gpl <- arrangeGrob(yhist,con,t,xhist,nrow=2,widths = 1:2,heights=2:1)
ggsave(file.path('plots','Fig5.png'),comb,height=h_plot+h_sec+h_lab+h_pic,width=w_first+6*w_cell+w_end+w_gap+w_sec,unit="cm")
ggsave(file.path('plots','Fig5.pdf'),comb,height=h_plot+h_sec+h_lab+h_pic,width=w_first+6*w_cell+w_end+w_gap+w_sec,unit="cm")





## Extended Data Figure 1: Absolute Carbon Prices ----

per <- c(2030,2050)
cpri <- data |> filter(variable=="Price|Carbon",year %in% per) |> group_by(Model,scenario,year) |> summarize(value=max(value)) |> ungroup()
mincpri <- data |> filter(variable=="Price|Carbon",year %in% per) |> group_by(Model,scenario,year) |> summarize(value=min(value)) |> ungroup()
cpri1000 <- cpri |> filter(scenario %in% grep("1000",scens,value=T)) |>  mutate(scenario=substr(scenario,6,55)) |> filter(scenario %in% scen1)
mincpri1000 <- mincpri |> filter(scenario %in% grep("1000",scens,value=T)) |>  mutate(scenario=substr(scenario,6,55)) |> filter(scenario %in% scen1)
cpri1000$Region <- "Highest capacity"
mincpri1000$Region <- "Lowest capacity"
cpri1000 <- rbind(cpri1000,mincpri1000)
cpri1000$scenario <- factor(cpri1000$scenario,levels=scen1)
g <- ggplot()+
  geom_hline(data=data.frame(year=2030),aes(yintercept=185),linetype="dashed",color="black")+#Rennert et al.
  geom_hline(data=data.frame(year=2030),aes(yintercept=190),linetype="solid",color="black")+#EPA 
  geom_hline(data=data.frame(year=2030),aes(yintercept=467),linetype="dotted",color="black")+#Moore et al. 
  geom_point(data=cpri1000,aes(x=scenario,y=value,color=Model,shape=Region,size=Region))+
  scale_shape_manual(values=c(19,1))+scale_size_manual(values=c(3,5))+
  theme (plot.margin=margin(b=-12,l=5,r=5,t=5))+ylab(bquote("Carbon Price $/t CO"[2]))+scale_color_manual(values=cbCPalette)+facet_grid(year~.,scales = "free_y")+
  xlab("")+scale_x_discrete(labels=NULL)

#combine various plots into one figure: https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
lay <- rbind(c(1 ,1 ,1 ,1 ,1 ,1 ,1 ,1),
             c(NA,2 ,3 ,4 ,5 ,6 ,7 ,NA),
             c(NA,8 ,9 ,10,11,12,13,NA))
h_plot<- 12
h_lab <- 1.5
h_pic <- 1.5
w_first <- 1.9
w_cell <- 184/144*h_pic
w_end <- 4.7
comb <- grid.arrange(g,t1,t2,t3,t4,t5,t6,p1,p2,p3,p4,p5,p6,heights=unit(c(h_plot,h_lab,h_pic),c("cm","cm","cm")),
                     widths=unit(c(w_first,w_cell,w_cell,w_cell,w_cell,w_cell,w_cell,w_end),
                                 c("cm","cm","cm","cm","cm","cm","cm","cm")),layout_matrix=lay)
# gpl <- arrangeGrob(yhist,con,t,xhist,nrow=2,widths = 1:2,heights=2:1)
ggsave(file.path('plots','ExDataFig1.png'),comb,height=h_plot+h_lab+h_pic,width=w_first+6*w_cell+w_end,unit="cm")
ggsave(file.path('plots','ExDataFig1.pdf'),comb,height=h_plot+h_lab+h_pic,width=w_first+6*w_cell+w_end,unit="cm")

## Extended Data Figure 3: Electrification ----

var <- c('Final Energy','Final Energy|Electricity')#,'Final Energy|Hydrogen')
emi <- data |>
  filter(variable %in% var,year %in% c(2020,2030,2050)) |>
  select(-unit,-run) |>
  filter(scenario %in% c(tab1)) |>
  filter(region == "World") |>
  mutate(scenario=substr(scenario,6,55))

emi <- emi |> filter(scenario %in% scen1)
emi$scenario <- factor(emi$scenario,levels=scen1)

figs3 <- ggplot()+
  geom_hline(data=emi|> filter(year==2020,scenario=="ref")|>select(-year),aes(yintercept=value,color=Model,linetype=Model))+
  geom_point(data=emi|> filter(year %in% c(2030,2050)),aes(x=scenario,y=value,color=Model),shape=19,size=3)+scale_shape_manual(values = c(seq(1,3),1,seq(4,6),4))+scale_color_discrete()+#ylim(700,1400)+
  theme (plot.margin=margin(b=-12,l=5,r=5,t=5))+ylab("Global energy use (EJ/yr)") +
  facet_grid(year~variable)+scale_color_manual(values=cbCPalette) +
  scale_x_discrete(labels=NULL)+xlab("")+ylim(0,500)

sizelab <- 2 #font size of x-tick labels
t1 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Cost \n effective \n"),aes(x=1,y=1,label=label),size=sizelab)
t2 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Tech \n \n "),aes(x=1,y=1,label=label),size=sizelab)
t3 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Institutional \n \n"),aes(x=1,y=1,label=label),size=sizelab)
t4 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Tech & \n Institutional \n"),aes(x=1,y=1,label=label),size=sizelab)
t5 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Enablers & \n Institutional \n"),aes(x=1,y=1,label=label),size=sizelab)
t6 <- ggplot()+theme_void()+geom_text(data=data.frame(label="Tech & \n Enablers & \n Institutional"),aes(x=1,y=1,label=label),size=sizelab)


lay <- rbind(c(1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1 ,1),
             c(NA,2 ,3 ,4 ,5 ,6 ,7 ,NA,14 ,15 ,16 ,17 ,18 ,19 ,NA),
             c(NA,8 ,9 ,10,11,12,13,NA,20 ,21 ,22,23,24,25,NA))
h_plot<- 12
h_lab <- 0.9
h_pic <- 0.9
w_first <- 1.55
w_cell <- 184/144*h_pic
w_gap <- 0.3
w_end <- 3.9
comb <- grid.arrange(figs3,t1,t2,t3,t4,t5,t6,p1,p2,p3,p4,p5,p6,t1,t2,t3,t4,t5,t6,p1,p2,p3,p4,p5,p6,
                     heights=unit(c(h_plot,h_lab,h_pic),c("cm","cm","cm")),
                     widths=unit(c(w_first,w_cell,w_cell,w_cell,w_cell,w_cell,w_cell,w_gap,w_cell,w_cell,w_cell,w_cell,w_cell,w_cell,w_end),
                                 c("cm","cm","cm","cm","cm","cm","cm","cm","cm","cm","cm","cm","cm","cm","cm")),
                     layout_matrix=lay)
# gpl <- arrangeGrob(yhist,con,t,xhist,nrow=2,widths = 1:2,heights=2:1)
ggsave(file.path('plots','ExDataFig3.png'),comb,height=h_plot+h_lab+h_pic,width=w_first+12*w_cell+w_gap+w_end,unit="cm")
ggsave(file.path('plots','ExDataFig3.pdf'),comb,height=h_plot+h_lab+h_pic,width=w_first+12*w_cell+w_gap+w_end,unit="cm")


## data checks for citation in paper ----


#check AIM electricity results
data |> filter(Model=="AIM",region=="World",year%in%c(2020),variable=="Secondary Energy|Electricity") |> summarize(max=max(value),min=min(value))
data |> filter(Model=="AIM",region=="World",year%in%c(2030),variable=="Secondary Energy|Electricity") |> summarize(max=max(value),min=min(value))
(167.57-102.2)*272/10 # yearly increase in TWh in scenario with fastest growth
(154.56-102.2)*272/10 # yearly increase in TWh in scenario with slowest growth
data |> filter(Model=="AIM",region=="World",year%in%c(2020,2030),variable=="Secondary Energy|Electricity")
data |> filter(scenario=="m550_ref",region=="World",year%in%c(2030),variable=="Secondary Energy|Electricity")

