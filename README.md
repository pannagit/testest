# testest

rm(list=ls()); load("df.rdata")
library(reshape2); library(dplyr); library(xts); library(data.table)

load("df.rdata"); d = df # %>% ungroup() %>% dplyr::filter(date==date[1])
d = d %>% group_by(date) %>% mutate(sector=sample(1:10, n(), replace=T))
colnames(d)[c(6,4)] = c("fx","ret_loc")
d$ret_USD = (1+d$ret_loc)*(1+d$fx) - 1

date="date"; country="country"; sector="sector";
benchwt="benchwt"; portwt="portwt";
ret_loc="ret_loc"; fx="fx"; ret_USD = "ret_USD"
SmoothAlgo="Fudge"; # ret = "ret_loc"

BrinsonSgl = function(d, date="date", sector="sector", ret="ret",
                      benchwt="benchwt", portwt="portwt") {

  # ------- rename col and check input ------- 
  
  for (x in c("date","sector","benchwt","portwt","ret")) {
    idsi = which(colnames(d)==get(x))
    if (length(idsi)==0) {
      warning(paste0(get(x), " not found in the data")); stop()
    } else {colnames(d)[idsi] <- x }
  }
  
  if (length(unique(d$date))>1) {
    warning('Only support single day'); stop()
  }
  
  # ------- calculation ------- 
  
 tabBrinson = d %>% dplyr::arrange(sector) %>% dplyr::group_by(sector) %>% 
    dplyr::summarise(Port.Weight = sum(portwt),
                     Bench.Weight = sum(benchwt),
                     Port.Ret = sum(portwt*ret)/Port.Weight,
                     Port.Ret = ifelse(is.nan(Port.Ret),0,Port.Ret),
                     Bench.Ret = sum(benchwt*ret)/Bench.Weight,
                     Bench.Ret = ifelse(is.nan(Bench.Ret),0,Bench.Ret)) %>% 
    mutate(Allocation.Effect = Bench.Ret*(Port.Weight-Bench.Weight),
           StkSelect.Effect = (Port.Ret-Bench.Ret)*Bench.Weight,
           Interatction = Port.Ret*Port.Weight + Bench.Weight*Bench.Ret -
                          Port.Ret*Bench.Weight - Bench.Ret*Port.Weight,
           Active.Ret = Port.Weight*Port.Ret - Bench.Weight*Bench.Ret)
    # 
    # colSums(tabBrinson[,6:9])
    # sum((d$portwt - d$benchwt) * d$ret)

 tabBrinson
    
  
}

GlobalAttribution <- function(d, date="date", country="country", sector="sector",
                              benchwt="benchwt", portwt="portwt",
                              ret_loc="ret_loc", ret_USD="ret_USD", fx="fx") {

  # ------- rename col and check input ------- 

  for (x in c("date","country","sector","benchwt","portwt","ret_loc","ret_USD","fx")) {
    idsi = which(colnames(d)==get(x))
    if (length(idsi)==0) {
      warning(paste0(get(x), " not found in the data")); stop()
    } else {colnames(d)[idsi] <- x }
  }
  
  if (length(unique(d$date))>1) {
    warning('Only support single day'); stop()
  }

  if (abs(sum(d$portwt)-1) > 0.000001) {
    warning('Portfolwio weights do not add up to 1'); stop()
  }

  if (abs(sum(d$benchwt)-1) > 0.000001) {
    warning('Benchmark weights do not add up to 1'); stop()
  }

  fx_chk = d %>% group_by(country) %>%
    summarise(val=sum(abs((1+ret_USD)/(1+ret_loc)-mean((1+ret_USD)/(1+ret_loc)))))
  if (sum(fx_chk$val > 0.000001)>0 ) {
    warning(paste0("FX rate not the same within country for on ", d$date[1]," : ",
                   paste0(fx_chk$country[which(fx_chk$val > 0.000001)], collapse=", ")))
    stop()
  }


  # ------- calculation -------

  d2 = d %>% dplyr::arrange(country) %>%
                dplyr::group_by(country) %>% 
    dplyr::mutate(wpRescale = portwt/sum(portwt),
           wbRescale = benchwt/sum(benchwt))
  
  dcalc = d2 %>% 
    dplyr::summarise( wpc = sum(portwt),
                      wbc = sum(benchwt),
                      rplc = sum(portwt*ret_loc)/wpc,
                      rblc = sum(benchwt*ret_loc)/wbc,
                      rpuc = sum(portwt*ret_USD)/wpc,
                      rbuc = sum(benchwt*ret_USD)/wbc,
                      fx.var = fx[1]) %>%
    ungroup() %>%
    mutate(rblg = sum(rblc*wbc),
           rplg = sum(rplc*wpc),
           rbug = sum(rbuc*wbc),
           rpug = sum(rpuc*wpc),
           eff_1_country = (wpc-wbc)*(rblc-rblg),
           eff_2_Local_Port = wpc*(rplc-rblc),
           eff_3_FX_Hedgable = (wpc-wbc)*fx.var,
           eff_4_Local = fx.var*(rplc*wpc - rblc*wbc))

  # ------- sector contribution -------
  
  dList = split(d2[,c("date","country","ret_loc","wpRescale","wbRescale","sector")] %>% 
                  mutate(cntry2=country), f=d$country)
  
  SectorTab = as.data.frame(data.table::rbindlist(lapply(dList, function(x) { 
            data.frame(country=x$cntry2[[1]], 
                       BrinsonSgl(x, ret="ret_loc", benchwt="wbRescale", portwt="wpRescale")) }))) %>% 
    merge(dcalc[,c("country","wpc")], by="country") %>% 
    mutate(Allocation.Effect=Allocation.Effect*wpc, 
           StkSelect.Effect=StkSelect.Effect*wpc,
           Interatction=Interatction*wpc,
           Active.Ret=Active.Ret*wpc, 
           date = d$date[1]) %>% 
    select(-wpc)
         
  # ------- format output -------
  
    effSet = c(paste0("eff_",c("1_country","2_Local_Port","3_FX_Hedgable","4_Local")))
    CountryTab = dcalc[,setdiff(colnames(dcalc),c("rblg","rplg","rbug","rpug"))]
    total = data.frame(country="TOTAL", t(colSums(CountryTab[,-1])))
    colnames(total) = colnames(CountryTab)
    CountryTab = rbind(data.frame(CountryTab), total)
    CountryTab$TOTAL = rowSums(CountryTab[,effSet])
    CountryTab$date = d$date[1]

    retTab = data.frame(date=d$date[1],
                        port_ret_USD=sum(d$portwt*d$ret_USD),
                        ben_ret_USD=sum(d$benchwt*d$ret_USD),
                        active_return=sum(d$portwt*d$ret_USD-d$benchwt*d$ret_USD))
              
    if (abs(retTab$active_return -
             CountryTab$TOTAL[which(CountryTab$country=="TOTAL")]) > 0.000001) {
      warning("attribution does not add up to active return")
    }

    return(list(retTab=retTab, CountryTab=CountryTab, SectorTab=SectorTab))

}

# start.date=NULL
# end.date=NULL
# SmoothAlgo="Fudge"
# date.var="date"
# ctry.var="country"
# cat.var="sector"
# bench.weight="benchwt"
# portfolio.weight="portwt"
# ret.var="ret"
# fx.var="fx"
# d <- data.frame(d)

MultiGlobalAttribution <- function(d, start.date=NULL, end.date=NULL, SmoothAlgo="Fudge",
                                   tic = "tic",
                                   date="date", country="country", sector="sector",
                                   benchwt="benchwt", portwt="portwt",
                                   ret_loc="ret_loc", ret_USD="ret_USD", fx="fx") {

    
    for (x in c("date","country","sector","tic","benchwt","portwt","ret_loc","ret_USD","fx")) {
      idsi = which(colnames(d)==get(x))
      if (length(idsi)==0) {
        warning(paste0(get(x), " not found in the data")); stop()
      } else {colnames(d)[idsi] <- x }
    }
    
    if (!(SmoothAlgo %in% c("Fudge", "Carino", "Menchero","GARP","Frongello","Davies&Laker"))){
      warning("Invalid SmoothAlgo input. "); stop()
    }
    
    dList = split(d, f=d$date)
    MGAList = lapply(dList, GlobalAttribution)
    
    d_ar = d %>% ungroup() %>% transmute(date, country, sector, tic, 
                                         act_ret=(portwt-benchwt)*ret_USD,
                                         act_bet = portwt-benchwt)
    
    MGA = as.data.frame(data.table::rbindlist(lapply(MGAList, 
                                                         function(x) {return(x[[2]])})))

    MGA_TOTAL = MGA %>% filter(country=="TOTAL")
    
    MGA_sector = as.data.frame(data.table::rbindlist(lapply(MGAList, 
                                                            function(x) {return(x[[3]])})))
    
    MGA_actRet =as.data.frame(data.table::rbindlist(lapply(MGAList, 
                                                             function(x) {return(x[[1]])})))
 
       
}
    
# 
# a = GlobalAttribution(d)
# d1 = a$retTab
# d2 = a$CountryTab
# d3 = a$SectorTab
