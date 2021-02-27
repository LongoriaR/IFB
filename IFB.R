#Inversión Fija Bruta


rm(list = ls())
options(warn = 1)

library(pacman)


pacman::p_load(dplyr,RCurl,gdata,readxl,tidyverse,lubridate,readxl, highcharter,
               tidyquant,timetk,tibbletime,quantmod,PerformanceAnalytics,scales,
               tidyverse,highcharter,jsonlite, curl,httr, broom,ggplot2,forecast,
               gcookbook,plyr, ggfortify,tbl2xts,htmltools,tidyverse,lubridate,readxl,
               highcharter,tidyquant,timetk,tibbletime, reshape2,dplyr,viridis,rvest,stringi,
               plsgenomics,pheatmap,lubridate)



source("C:/Users/J342546/Documents/R/Generic Programs/General_Inegi_Functions.R")
source("C:/Users/J342546/Documents/R/Generic Programs/Paleta_Colores_Principal.R")

#==============================================================================================
#functions and information

plot_index<-function(data_plot,name1,title,color_plot,y_option){
  #option y_option=1 #index
  #option y_option=2 #valor in percentages
  #option y_option=3 #valor in percentages MoM, barchart
  if(y_option==1 ){
    plot_ifb<- ggplot(subset(data_plot,description==name1), aes(x=date, y=value)) +
      geom_line(color=color_plot)   +
      ggtitle(title) +
      xlab("Year")  + ylab("Index")+
      scale_x_date(labels=date_format ("%y"), breaks=date_breaks("1 year")) +
      theme(plot.title = element_text(lineheight=.8, face="bold", size = 14)) +
      theme(text = element_text(size=18))+ 
      theme_classic()}
  
  if(y_option==2 ){
    plot_ifb<- ggplot(subset(data_plot,description==name1), aes(x=date, y=value)) +
      geom_line(color=color_plot)   +
      ggtitle(title) +
      xlab("Year")  +  ylab("%")+
      scale_x_date(labels=date_format ("%y"), breaks=date_breaks("1 year")) +
      theme(plot.title = element_text(lineheight=.8, face="bold", size = 14)) +
      theme(text = element_text(size=18))+ 
      theme_minimal()+scale_y_continuous(labels=scales::percent)}
    
    if(y_option==3 ){
      plot_ifb<- ggplot(subset(data_plot,description==name1), aes(x=date, y=value)) +
        geom_bar(stat="identity", na.rm = TRUE, fill=color_plot)   +
        ggtitle(title) +
        xlab("")  +  ylab("")+
        scale_x_date(labels=date_format ("%b"), breaks=date_breaks("1 month")) +
        theme(plot.title = element_text(lineheight=58, face="bold", size=10)) +
        theme(text = element_text(size=10))+ 
        theme_minimal()+
        geom_text(aes(label = round(value,3)*100), vjust = -0.2, size =2.5,
                  position = position_dodge(0.9))+
        scale_y_continuous(labels=scales::percent) }
  
  return(plot_ifb)
}


catalog<-read.csv("C:/Users/J342546/Documents/Quant_Project/AWS/INEGI/AWS_Inegi_BM_Catalog.csv", encoding = "UTF-8") %>% 
  select(.,c("vendor","symbol","symbol_name","symbol_description_local","Group")) 

IFB_series<-catalog %>% subset(.,vendor=="INEGI" & Group=="Inversion Fija Bruta")





#==============================================================================================


#-------------------- Inversión Fija Brita --------------------


# select fields for Retail sale
sub_base<- IFB_series %>%  select(.,c("symbol","symbol_description_local"))


#Download all Gross Fixed Investment from INEGI


for(i in 1:(dim(sub_base)[1])){
  x<-GET_INEGI_2.0.BIE(as.numeric(sub_base$symbol[i]))
  xx<-x %>% back_as_ts(.,type=1) %>% window(.,start="1993-01-01")
  if(i==1){ xts_base<-xx } else{  xts_base<-cbind.xts(xts_base,xx)}
  
}

colnames(xts_base)<-sub_base$symbol
colnames(xts_base)<-gsub("Inversión fija bruta, base 2013; ","",sub_base$symbol_description_local)



#--------------------  Plot Indexes ------------------------

IFB_Index <- melt(t(xts_base),"date")
colnames(IFB_Index)<-c("description","date","value")
IFB_Index$date<-as.Date(IFB_Index$date)

names<-levels(IFB_Index$description)


Gross_fixed_investment<-plot_index(data_plot=IFB_Index,names[1],title="Inverisión Fija Bruta",color_plot=Orange[[1]],y_option=1)
GFI_Mach_Tot<-  plot_index(data_plot=IFB_Index,names[2],title="Maquinaria y equipo; Total",color_plot=Orange[[1]],y_option=1)
GFI_Mach_Nac<-plot_index(data_plot=IFB_Index,names[3],title="Maquinaria y equipo; Nacional",color_plot=Orange[[1]],y_option=1)
GFI_Cons_Tot<-plot_index(data_plot=IFB_Index,names[9],title="Construcción; Total",color_plot=Orange[[1]],y_option=1)

grid.arrange(Gross_fixed_investment, GFI_Mach_Tot,GFI_Cons_Tot, ncol=3)
 


#Plot charts

IFB_Index <- melt(t(window(xts_base,start="2018-01-01")),"date")
colnames(IFB_Index)<-c("description","date","value")
IFB_Index$date<-as.Date(IFB_Index$date)

names<-levels(IFB_Index$description)


Gross_fixed_investment<-plot_index(data_plot=IFB_Index,names[1],title="Inverisión Fija Bruta",color_plot=Orange[[1]],y_option=1)
GFI_Mach_Tot<-  plot_index(data_plot=IFB_Index,names[2],title="Maquinaria y equipo; Total",color_plot=Turq[[1]],y_option=1)
GFI_Mach_Nac<-plot_index(data_plot=IFB_Index,names[3],title="Maquinaria y equipo; Nacional",color_plot=Turq[[1]],y_option=1)
GFI_Mach_Nac_Trans<-plot_index(data_plot=IFB_Index,names[4],title="Nacional; Equipo de transporte",color_plot=Turq[[1]],y_option=1)
GFI_Mach_Nac_equip<-plot_index(data_plot=IFB_Index,names[5],title="Nacional; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=1)
GFI_Mach_Imp<-plot_index(data_plot=IFB_Index,names[6],title="Maquinaria y equipo; Importado",color_plot=Turq[[1]],y_option=1)
GFI_Mach_Imp_Trans<-plot_index(data_plot=IFB_Index,names[7],title="Importado; Equipo de transporte",color_plot=Turq[[1]],y_option=1)
GFI_Mach_Imp_equip<-plot_index(data_plot=IFB_Index,names[8],title="Importado; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=1)
GFI_Cons_Tot<-plot_index(data_plot=IFB_Index,names[9],title="Construcción; Total",color_plot=Teal[[1]],y_option=1)
GFI_Cons_Res<-plot_index(data_plot=IFB_Index,names[10],title="Construcción; Residencial",color_plot=Teal[[1]],y_option=1)
GFI_Cons_NonRes<- plot_index(data_plot=IFB_Index,names[11],title="Construcción; No residencial",color_plot=Teal[[1]],y_option=1)

grid.arrange(Gross_fixed_investment, GFI_Mach_Tot,GFI_Cons_Tot,
             GFI_Mach_Nac,GFI_Mach_Nac_Trans,GFI_Mach_Nac_equip,
             GFI_Mach_Imp,GFI_Mach_Imp_Trans,GFI_Mach_Imp_equip,
             GFI_Cons_Res,GFI_Cons_NonRes, ncol=3)




#--------------------  Plot YoY --------------------

IFB<-xts_base
IFB_MoM<-Return.calculate(IFB,method="discrete") %>% na.omit()
IFB_YoY<-lapply(IFB,FUN=YoY_rate) %>% do.call(cbind,.) 

#plot


#Plot charts

IFB_Index_YoY <- melt(t(window(IFB_YoY,start="2020-01-01")),"date")
colnames(IFB_Index_YoY)<-c("description","date","value")
IFB_Index_YoY$date<-as.Date(IFB_Index_YoY$date)

names<-levels(IFB_Index_YoY$description)


Gross_fixed_investment<-plot_index(data_plot=IFB_Index_YoY,names[1],title="Inverisión Fija Bruta",color_plot=Orange[[1]],y_option=3)
GFI_Mach_Tot<-  plot_index(data_plot=IFB_Index_YoY,names[2],title="Maquinaria y equipo; Total",color_plot=Turq[[1]],y_option=3)
GFI_Mach_Nac<-plot_index(data_plot=IFB_Index_YoY,names[3],title="Maquinaria y equipo; Nacional",color_plot=Turq[[1]],y_option=3)
GFI_Mach_Nac_Trans<-plot_index(data_plot=IFB_Index_YoY,names[4],title="Nacional; Equipo de transporte",color_plot=Turq[[1]],y_option=3)
GFI_Mach_Nac_equip<-plot_index(data_plot=IFB_Index_YoY,names[5],title="Nacional; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=3)
GFI_Mach_Imp<-plot_index(data_plot=IFB_Index_YoY,names[6],title="Maquinaria y equipo; Importado",color_plot=Turq[[1]],y_option=3)
GFI_Mach_Imp_Trans<-plot_index(data_plot=IFB_Index_YoY,names[7],title="Importado; Equipo de transporte",color_plot=Turq[[1]],y_option=3)
GFI_Mach_Imp_equip<-plot_index(data_plot=IFB_Index_YoY,names[8],title="Importado; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=3)
GFI_Cons_Tot<-plot_index(data_plot=IFB_Index_YoY,names[9],title="Construcción; Total",color_plot=Teal[[1]],y_option=3)
GFI_Cons_Res<-plot_index(data_plot=IFB_Index_YoY,names[10],title="Construcción; Residencial",color_plot=Teal[[1]],y_option=3)
GFI_Cons_NonRes<- plot_index(data_plot=IFB_Index_YoY,names[11],title="Construcción; No residencial",color_plot=Teal[[1]],y_option=3)

grid.arrange(Gross_fixed_investment, GFI_Mach_Tot,GFI_Cons_Tot,
             GFI_Mach_Nac,GFI_Mach_Nac_Trans,GFI_Mach_Nac_equip,
             GFI_Mach_Imp,GFI_Mach_Imp_Trans,GFI_Mach_Imp_equip,
             GFI_Cons_Res,GFI_Cons_NonRes, ncol=3)

IFB_YoY_bar



# line
IFB_YoY<-lapply(IFB,FUN=YoY_rate) %>% do.call(cbind,.)
IFB_Index_YoY <- melt(t(window(IFB_YoY,start="2018-01-01")),"date")
colnames(IFB_Index_YoY)<-c("description","date","value")
IFB_Index_YoY$date<-as.Date(IFB_Index_YoY$date)

names<-levels(IFB_Index_YoY$description)


Gross_fixed_investment<-plot_index(data_plot=IFB_Index_YoY,names[1],title="Inverisión Fija Bruta",color_plot=Orange[[1]],y_option=2)
GFI_Mach_Tot<-  plot_index(data_plot=IFB_Index_YoY,names[2],title="Maquinaria y equipo; Total",color_plot=Turq[[1]],y_option=2)
GFI_Mach_Nac<-plot_index(data_plot=IFB_Index_YoY,names[3],title="Maquinaria y equipo; Nacional",color_plot=Turq[[1]],y_option=2)
GFI_Mach_Nac_Trans<-plot_index(data_plot=IFB_Index_YoY,names[4],title="Nacional; Equipo de transporte",color_plot=Turq[[1]],y_option=2)
GFI_Mach_Nac_equip<-plot_index(data_plot=IFB_Index_YoY,names[5],title="Nacional; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=2)
GFI_Mach_Imp<-plot_index(data_plot=IFB_Index_YoY,names[6],title="Maquinaria y equipo; Importado",color_plot=Turq[[1]],y_option=2)
GFI_Mach_Imp_Trans<-plot_index(data_plot=IFB_Index_YoY,names[7],title="Importado; Equipo de transporte",color_plot=Turq[[1]],y_option=2)
GFI_Mach_Imp_equip<-plot_index(data_plot=IFB_Index_YoY,names[8],title="Importado; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=2)
GFI_Cons_Tot<-plot_index(data_plot=IFB_Index_YoY,names[9],title="Construcción; Total",color_plot=Teal[[1]],y_option=2)
GFI_Cons_Res<-plot_index(data_plot=IFB_Index_YoY,names[10],title="Construcción; Residencial",color_plot=Teal[[1]],y_option=2)
GFI_Cons_NonRes<- plot_index(data_plot=IFB_Index_YoY,names[11],title="Construcción; No residencial",color_plot=Teal[[1]],y_option=2)

IFB_YoY_line<-grid.arrange(Gross_fixed_investment, GFI_Mach_Tot,GFI_Cons_Tot,
             GFI_Mach_Nac,GFI_Mach_Nac_Trans,GFI_Mach_Nac_equip,
             GFI_Mach_Imp,GFI_Mach_Imp_Trans,GFI_Mach_Imp_equip,
             GFI_Cons_Res,GFI_Cons_NonRes, ncol=3)


#construcción

    
    
        
#--------------------  Plot MoM --------------------
    
    #Plot charts
    
    IFB_Index_MoM <- melt(t(window(IFB_MoM,start="2020-01-01")),"date")
    colnames(IFB_Index_MoM)<-c("description","date","value")
    IFB_Index_MoM$date<-as.Date(IFB_Index_MoM$date)
    
    names<-levels(IFB_Index_MoM$description)
    
    
    Gross_fixed_investment<-plot_index(data_plot=IFB_Index_MoM,names[1],title="Inverisión Fija Bruta",color_plot=Orange[[1]],y_option=3)
    GFI_Mach_Tot<-  plot_index(data_plot=IFB_Index_MoM,names[2],title="Maquinaria y equipo; Total",color_plot=Turq[[1]],y_option=3)
    GFI_Mach_Nac<-plot_index(data_plot=IFB_Index_MoM,names[3],title="Maquinaria y equipo; Nacional",color_plot=Turq[[1]],y_option=3)
    GFI_Mach_Nac_Trans<-plot_index(data_plot=IFB_Index_MoM,names[4],title="Nacional; Equipo de transporte",color_plot=Turq[[1]],y_option=3)
    GFI_Mach_Nac_equip<-plot_index(data_plot=IFB_Index_MoM,names[5],title="Nacional; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=3)
    GFI_Mach_Imp<-plot_index(data_plot=IFB_Index_MoM,names[6],title="Maquinaria y equipo; Importado",color_plot=Turq[[1]],y_option=3)
    GFI_Mach_Imp_Trans<-plot_index(data_plot=IFB_Index_MoM,names[7],title="Importado; Equipo de transporte",color_plot=Turq[[1]],y_option=3)
    GFI_Mach_Imp_equip<-plot_index(data_plot=IFB_Index_MoM,names[8],title="Importado; Maquinaria, equipo y otros bienes",color_plot=Turq[[1]],y_option=3)
    GFI_Cons_Tot<-plot_index(data_plot=IFB_Index_MoM,names[9],title="Construcción; Total",color_plot=Teal[[1]],y_option=3)
    GFI_Cons_Res<-plot_index(data_plot=IFB_Index_MoM,names[10],title="Construcción; Residencial",color_plot=Teal[[1]],y_option=3)
    GFI_Cons_NonRes<- plot_index(data_plot=IFB_Index_MoM,names[11],title="Construcción; No residencial",color_plot=Teal[[1]],y_option=3)
    
    grid.arrange(Gross_fixed_investment, GFI_Mach_Tot,GFI_Cons_Tot,
                 GFI_Mach_Nac,GFI_Mach_Nac_Trans,GFI_Mach_Nac_equip,
                 GFI_Mach_Imp,GFI_Mach_Imp_Trans,GFI_Mach_Imp_equip,
                 GFI_Cons_Res,GFI_Cons_NonRes, ncol=3)
    
    grid.arrange(Gross_fixed_investment, GFI_Mach_Tot,GFI_Cons_Tot, ncol=1)  
    
  
