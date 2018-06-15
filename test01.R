library(dplyr)

id_test1 = commonIDdf %>% group_by(axioma_id) %>% 
  dplyr::summarise(facet_id_num = n())

id_test1[which(id_test1$facet_id_num>1),]

id_test2 = commonIDdf %>% group_by(facet_id_num) %>% 
  dplyr::summarise(axioma_id_num = n())

id_test2[which(id_test2$axioma_id_num>1),]



id_check = merge(data.frame(facet_id = unique(portwts$facet_id),stringsAsFactors=F),
                 maptable, by="facet_id", all=T)
id_check = id_check[which(!is.na(id_check$axioma_id)),]



agg_check = df_agg %>% dplyr::group_by(date) %>% 
  dplyr::summarise(totWT_ben=sum(benchwts,na.rm=T), 
                   totWT_port = sum(portwt, na.rm=T))





calculating returns..?
d3 = d2 %>% dplyr::mutate(ret_prct = c(NA,Price[-1]/Price[-n()]-1),
                       ret_prct=ifelse(date_diff<=33&date_diff>=26, ret_prct,NA),
                       ret_log=c(NA, log(Price[-1]/Price[-n()])),
                       ret_log=ifelse(date_diff<=33&date_diff>=26, ret_log,NA)) %>%
  merge(group,by="Factset_ID", all.x = T, all.y = F) %>%
  dplyr::group_by(group,Date) %>%
  dplyr::mutate(grouptotalwt =sum(wt) %>%
                  dplyr::arrange(group,Date)
  
rm(list = ls())
setwd("C:/Attribution1/Data")
{
  install.packages("dplyr")
  install.packages("pa")
  install.packages("lubridate")
  install.packages("dplyr")
  install.packages("ggplot")
  install.packages("ggplot2")
  install.packages("tidyr")
  install.packages("openxlsx")
  install.packages("data.table")
  install.packages("reshape")
  install.packages("tictoc")
  install.packages("openxlsx")
  install.packages("RODBC")
}

{
  library(lubridate)
  library(pa)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(data.table)
  library(reshape)
  library(tictoc)
  library(openxlsx)
  library(RODBC)
}


# read GDE holdings data
readPortWt = function(pathi,letterDig){
  files = list.files(pathi)
  mons = as.numeric(substr(files,letterDig+1,nchar(files)-4))
  mons = as.Date(as.character(mons),format = "%Y%m%d")
  
  diList = list()
  for (i in 1:length(files)){
    # di = read.table()
    di = read.csv(paste0(pathi,'/',files[i]),sep = ",")
    di = data.frame(Date = mons[i], Axioma_ID = di[,1], Port_Weight = di[,2], stringsAsFactors = F)
    diList[[i]] = di
  }
  return(diList)
}

pathPortWt = paste0(getwd(),"/GDE")
portWt = readPortWt(pathPortWt,3)

# read index data and stock performance data
pathBench = getwd()
dfBench = read.csv(paste0(pathBench,'/benchmark.csv'),sep="|")
dfBench$Factset_ID=substr(dfBench$FACTSET_PERM_ID,1,8)
dfBench$Country=substr(dfBench$FACTSET_PERM_ID,10,11)
dfBench$Date=as.Date(dfBench$BUSINESS_MONTH_END)
dfBench = dfBench[c("Date","Factset_ID","COMPANYNAME","Country","PRICE","INDEX_WEIGHT")]
colnames(dfBench) = c("Date","Factset_ID","Company_Name","Country","Price","Index_Weight")

# make a more robust function
# gregexprTmp = function(x,patt="-"){
#   if(length(x)!=1){
#     warning("only take one string!");stop()
#   }else{
#     return(as.numeric(gregexpr(patt,x)[[1]]))
#   }
# }

# read mapping info and merge with dfBench
dfMap = openxlsx::read.xlsx("Axioma ID V2.xlsx",startRow=7,colNames=T)
dfMap = dfMap[c(1, 3, 8)]
colnames(dfMap)=c("Axioma_ID","Factset_ID","GICS_Sector")

# check number of unique ids
length(unique(dfMap$Axioma_ID)) # 8589
length(unique(dfMap$Factset_ID)) # 8591
length(unique(dfBench$Factset_ID)) # 1738
PortWts=as.data.frame(data.table::rbindlist(portWt))
length(unique(PortWts$Axioma_ID)) # 531

# check if the two IDs are uniquely mapped
id_check=merge(data.frame(Factset_ID=unique(PortWts$Axioma_ID),stringsAsFactors = F),dfMap,by = "Factset_ID",all=T)
id_check=id_check[which(!is.na(id_check$Axioma_ID)),]
dim(id_check)
length(unique(PortWts$Axioma_ID))

# calc return in dfBench
# check if there is gap in months
d2 = dfBench %>% dplyr::arrange(Factset_ID, Date) %>% mutate(date_diff = c(NA,Date[-1] - Date[-n()]))
summary(d2$date_diff)
gap = data.frame(Factset_ID = unique(d2$Factset_ID),group =Factset_ID , stringsAsFactors = F)

# d3=na.omit(d2)
# d2[!complete.cases(d2),]

d3 = d2 %>% dplyr::mutate(ret_prct = c(NA,Price[-1]/Price[-n()]-1),
                       ret_prct=ifelse(date_diff<=33&date_diff>=26, ret_prct,NA),
                       ret_log=c(NA, log(Price[-1]/Price[-n()])),
                       ret_log=ifelse(date_diff<=33&date_diff>=26, ret_log,NA)) %>%
  merge(group,by="Factset_ID", all.x = T, all.y = F) %>%
  dplyr::group_by(group,Date) %>%
  dplyr::mutate(grouptotalwt =sum(wt)) %>%
                  dplyr::arrange(Factset_ID,Date)

# merge
df = merge(merge(dfMap, dfBench, by = "Factset_ID", all = T), PortWts, by = "Axioma_ID", all = T)



# dfMap$DATe=as.Date(d$date,origin="1899-12-30")
             

# save all
save(list = c("dfBench","portWt"),file = paste0(getwd(),"/data_gen.rdata"))





# Brinson
p1 = brinson(x = dfBench, date.var = "Date",
             cat.var = "Sector",bench.weight="Index_Weight",portfolio.weight = "Port_Weight", ret.var="ret_pct")































