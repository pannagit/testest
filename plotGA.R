rm(list=ls()); 
library(ggplot2); library(dplyr)
library(scales); library(reshape2)

# ----------- functions ----------

defineTags = function(setTag) {
  if (setTag=="set1") {
    return(list(ylabs="Weights in %",
                titles="Portfolio, Benchmark and Active Weights by Country on "))
  } else if (setTag=="set2") {
    return(list(ylabs="Returns in %",
                titles="Portfolio, Benchmark and Active Returns by Country on "))
  } else if (setTag=="set3") {
    return(list(ylabs="Attributed Effects in %",
                titles="Decompose Active Return on "))
  }
}

sglBarGen = function(dfx, fillvar="variable",textSize=3, 
                     xVar="country", prct=T, suppLegend=F) {
  dfx = data.frame(dfx)
  if (fillvar != xVar) {
    dfx$fillvar = dfx[,fillvar];
    dfx$xVar = dfx[,xVar]
    p = ggplot(dfx, aes(x=xVar, y=value, fill=fillvar)) 
  } else {
    dfx$xVar = dfx[,xVar]
    p = ggplot(dfx, aes(x=xVar, y=value, fill=xVar)) 
  }
  
  p = p +
    geom_bar(stat="identity", position=position_dodge()) +
    xlab(xVar)+
    guides(fill=guide_legend(title="")) +
    geom_hline(aes(yintercept=0))
  
  if (prct) { p = p + scale_y_continuous(labels=scales::percent) +
    geom_text(size=textSize, position = position_dodge(width = 1),
              aes(x=xVar, y=value, label=sprintf("%1.2f%%", 100*value))) 
  } else {
    p = p + geom_text(size=textSize, position = position_dodge(width = 1),
                      aes(x=xVar, y=value, label=sprintf("%#.2f", value)))
  }
  
  if (suppLegend) {
    p = p + theme(legend.position="none")
  }
  
  return(p)
}

plotGA_byCountry = function(sglDf, sets, textSize=3) {
  
  ggList = list()
  
  for (i in 1:length(sets)) {
    
    dfx = sglDf %>% filter(variable %in% sets[[i]], country!="TOTAL")
    texts = defineTags(names(sets)[i])
    
    if (i==3) { dfx$value = dfx$value / sum(dfx$value) } 
    
    if (i==2) {
      
      dfx$port = "Portfolio"
      dfx$port[which(dfx$variable %in% c("BENCH_RET_USD","BENCH_RET_LOC"))] = "Benchmark"
      dfx$port[which(dfx$variable %in% c("ACT_RET_USD","ACT_RET_LOC"))] = "Active"
      dfx$CURCY = "LOCAL"
      dfx$CURCY[which(dfx$variable %in% sets[[i]][4:6])] = "USD"
      dfx$port = factor(dfx$port, levels=c("Portfolio","Benchmark","Active"))
      
      ggList = append(ggList,
                      list(sglBarGen(dfx,textSize=textSize,fillvar="port") +
                             facet_wrap(~CURCY) +
                             ylab(texts$ylabs) +
                             ggtitle(paste0(texts$titles,dfx$date[1]))))
      ggList = append(ggList,
                      list(sglBarGen(dfx,textSize=textSize,fillvar="CURCY") +
                             facet_wrap(~port) +
                             ylab(texts$ylabs) +
                             ggtitle(paste0(texts$titles,dfx$date[1])))
      )
      
    } else {
      ggList = append(ggList,list(sglBarGen(dfx,textSize=textSize) +
                                    ylab(texts$ylabs) +
                                    ggtitle(paste0(texts$titles,dfx$date[1]))))
    }; rm(dfx)
  }
  return(ggList)
}

plotGA_Total = function(sglDf, textSize=3) {
  
  dfx = sglDf %>% filter(country=="TOTAL", variable!="FX_RET")
  dfxb = dfx[grep("EFF",dfx$variable),] %>% filter(variable!="TOTAL_EFF") %>% 
    mutate(value=value/sum(value))
  dfx = dfx[-grep("EFF",dfx$variable),]
  dfx$port = "Portfolio"
  dfx$port[grep("BENCH",dfx$variable)] = "Benchmark"
  dfx$port[grep("ACT",dfx$variable)] = "Active"
  dfx$port = factor(dfx$port, levels=c("Portfolio","Benchmark","Active"))
  dfx$cat = "Weights"
  dfx$cat[grep("LOC",dfx$variable)] = "Local Return"
  dfx$cat[grep("USD",dfx$variable)] = "USD Return"
  dfx$cat = factor(dfx$cat, levels=c("Weights","Local Return","USD Return"))
  
  return(list(
    sglBarGen(dfx, fillvar="port", textSize=textSize, xVar="port",suppLegend=T) +
      facet_wrap(~cat, scale="free_y") + xlab("") +
      ylab("Weights / Returns in %") + 
      ggtitle(paste0("Portfolio, Benchmark and Active Weights and Returns on ",dfx$date[1])),
    
    sglBarGen(dfxb, fillvar="variable", textSize=textSize, xVar="variable",suppLegend=T) +
      ylab("Attributed Effects in %") + 
      ggtitle(paste0("Decompose Active Return on ",dfx$date[1]))
    
  )
  )
  
  
  
}

plotGA_Wrap = function(sglDf, sets, textSize=3) {
  
  return(c(plotGA_byCountry(sglDf, sets, textSize),
           plotGA_Total(sglDf, textSize)))
  
}


# ----------- fake data ----------

MGA = data.frame(date=sort(rep(as.Date(42001:42200, origin="1899-12-30"),1,200*4)),
                 country=rep(c("ABC","DEF","GHI","JKL"),1,200*4))

colSet = c("PORT_WT","BENCH_WT","ACT_BET","PORT_RET_LOC","BENCH_RET_LOC",
           "ACT_RET_LOC","FX_RET","PORT_RET_USD","BENCH_RET_USD","ACT_RET_USD",
           "CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF")

for (i in 3:16) { MGA[,i] = runif(nrow(MGA)) }
colnames(MGA)[-c(1,2)] = colSet

MGA = dcast(melt(MGA, id.vars=c("date","country")) %>% 
        filter(variable == "PORT_WT" | variable == "BENCH_WT") %>% 
        group_by(date, variable) %>% mutate(value=value/sum(value)),
        date+country~variable, value.var="value") %>%
     merge(MGA[,c("date","country",colSet[-c(1,2)])], by=c("date","country"))
MGA$ACT_BET = MGA$PORT_WT - MGA$BENCH_WT
MGA$ACT_RET_USD = MGA$PORT_RET_USD - MGA$BENCH_RET_USD

MGA$TOTAL_EFF = rowSums(MGA[,c("CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF")])

MGA = rbind((dcast(melt(MGA, id.vars=c("date","country")) %>% 
          group_by(date,variable) %>% summarise(TOTAL=sum(value)),
          date~variable, value.var="TOTAL") %>% mutate(country="TOTAL"))[,colnames(MGA)],MGA) %>% 
  arrange(date, country)

############################################
############################################
# read the comments below and come back
# here to rename your MGA. 
# the following is an example.
# my MGA[,3:4] is port_wt and ben_wt
# if I rename them to wpc and wbc
# when you call funtion plotGA(...)
# you need to write sth like this
# sets = list(c("wpc","wbc","ACT_BET"), ... )
colnames(MGA)[c(3,4)] = c("wpc","wbc")
# !!!!!!!!!! the basic idea here is that !!!!!!!!!! 
# we melt MGA, and make MGA's colname the x-axis label
# therefore if you wanna change the x-axis label
# the easiest way is to rename MGA's columns
############################################
############################################

MGAL = melt(MGA, id.vars=c("date","country"))

# ------- Single period plot -------

sglDf = MGAL %>% filter(date==date[1])

############################################
############################################
# !!!!!!!!!! important !!!!!!!!!!!!!!!!
# make sure country is lower case
# make sure date is also lower case
# if you wanna change to upper case
# just find (w/ Match Case) and Replace JUST in this script
# !!!!!!!!!! important !!!!!!!!!!!!!!!!
# try unique(sglDf$country) using my fake data
# notice that sglDf or MGA in my fake data contains country = "TOTAL"
# !!!!!!! make sure that "TOTAL" is upper case
# if you check my functions, you may find that "TOTAL" is hard coded.
# my MGA = rbind(your MGA, your MGA_TOTAL)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sglDf is a subset of your MGA, including country = "TOTAL"
# It is attribution to country level on one day.
# e.g. sgldf = MGA %>% filter(date==as.Date("2017-12-31"))
# sgldf = melt(sgldf, id.vars=c("date", "country"))
# !!!!!!!!!!!!!!Notice, you may need to create a column in MGA
# such that MGA$ACT_BET = MGA$wpc - MGA$wbc
# !!!!!!!!!!!!!!Notice, you may need to create a column in MGA
# that MGA$ACT_RET_LOC = MGA$rplc - MGA$rblc
# !!!!!!!!!!!!!!Notice, you may need to create a column in MGA
# that MGA$ACT_RET_USD = MGA$rpuc - MGA$rbuc
############################################
############################################

textSize = 3
# my facke name colnames
sets = list(set1 = c("wpc","wbc","ACT_BET"),
            set2 = c("PORT_RET_LOC","BENCH_RET_LOC","ACT_RET_LOC",
                     "PORT_RET_USD","BENCH_RET_USD","ACT_RET_USD"),
            set3 = c("CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF"))


##########################################
##########################################
# plotGA_Wrap(sglDf, sets, textSize) creats a list of plots for a single day
# textSize (try textSize=3) is the font size of numbers shown in bar charts
# Now the key is the 2nd parameter sets
# sets is a list of MGA_TOTAL colnames.
# colnames from yours
# sets = list(set1 = c("wpc","wc","ACT_BET"),
#             set2 = c("rplc","rblc","ACT_RET_LOC",
#                      "rpuc","rbuc","ACT_RET_USD"),
#             set3 = c("eff_1_country","eff_2_Local_Port","eff_3_FX_Hedgable","eff_4_FX_NonHedgable"))
# !!!!!!!!!!!! Important !!!!!!!!!!!!!!
# to make sure the code work
# you'd to make sure that 
# (1) sets[[1]] = c(MGA's portwt name, MGA's benchwt name, MGA's active bet name)
# (2) sets[[2]] = c(MGA's ret port loc name, ret bench loc name, act return loc name,
#                    ret port USD name, ret bench USD name, active ret USD Name)
# (3) sets[[3]] = c(MGA's 4 effects names)
##########################################
##########################################

plots_Single = plotGA_Wrap(sglDf, sets, textSize=3)
plots_Single[[1]]
plots_Single[[2]]
plots_Single[[3]]
plots_Single[[4]]
plots_Single[[5]]
plots_Single[[6]]



# ---- fake data 2 

MGA_agg = data.frame(Eff=c("CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF"),
                       ARCntr=runif(4),TEContr=runif(4),
                       IRCntr=runif(4))
MGA_agg = melt(MGA_agg, id.vars="Eff")
MGA_agg$variable = factor(MGA_agg$variable, levels=c("ARCntr","TEContr","IRCntr"))

start.Date = "2018-01-01"
end.Date= "2018-06-30"

##########################################
##########################################
# this part corresponds to your MGA_TE
# your MGA_TE only contains TrkErr attribution over a time period
# in my case MGA_agg should contain Mean Active Return, Trk Err, Info Ratio attributions
# you may need to construct this kind of data table from your attr output tables
# print(MGA_agg) to find what it looks like.
##########################################
##########################################

plots_mga_agg = list()

# country = TOTAL
# barchart AR TE IR Contribution

plots_mga_agg[[1]] = sglBarGen(MGA_agg, xVar="Eff", 
                           fillvar="Eff", textSize=textSize, prct=F, suppLegend=T) +
              facet_wrap(~variable, scales="free_y") +
              ylab("Active Ret / Trk Err / Info Ratio") +
              ggtitle(paste0("Active Return, Tracking Error and Info Ratio Attribution ",
                             start.Date," to ", end.Date)) 

plots_mga_agg[[2]] = sglBarGen(MGA_agg %>% group_by(variable) %>% mutate(value=value/sum(value)) , 
                            xVar="Eff", fillvar="Eff", textSize=textSize, prct=T, suppLegend=T) +
              facet_wrap(~variable) +
              ylab("% Contribution to Active Ret / Trk Err / Info Ratio") +
              ggtitle(paste0("Active Return, Tracking Error and Info Ratio Attribution (%) ",
                             start.Date," to ", end.Date)) 

plots_mga_agg[[1]]
plots_mga_agg[[2]]


###################################
###################################
# Are we missing such info that over a time period,
# what effects are to Mean AR, Trk Err, Info Ratio on country level?
###################################
###################################


###################################
###################################
# If you want plot time-series, here is an example

# remove TOTAL
plot_dsub = MGA %>% filter(country!="TOTAL")
# melt using reshape2
plot_dsub = reshape2::melt(plot_dsub, id.vars=c("date","country"))

# subset plot_dsub to plot effects only
plot_dsub_effs = plot_dsub %>% filter(variable %in% sets[[3]])

ggplot(data=plot_dsub_effs, aes(x=date, y=value, color=country)) +
  facet_wrap(~variable) +
  scale_x_date(expand=c(0,0)) +
  geom_line()

ggplot(data=plot_dsub_effs, aes(x=date, y=value, color=variable)) +
  facet_wrap(~country, ncol=1) +
  scale_x_date(expand=c(0,0)) +
  geom_line()

ggplot(data=plot_dsub_effs, aes(x=date, y=value, fill=country)) +
  facet_wrap(~variable) +
  scale_x_date(expand=c(0,0)) +
  geom_area(stat = "bin")