rm(list=ls())
library(dplyr); library(reshape2)
library(ggplot2); library(ggforce); library(cowplot)
 

cntry = data.frame(Country = c("AT", "AU", "BE", "CA", "CH", "DE", "DK", "ES", 
                    "FI", "FR", "GB", "HK", "IE", "IL", "IT", "JP", 
                    "NL", "NO", "NZ", "PT", "SE", "SG", "US"),
                  CountryName = c("Austria", "Australia", "Belgium", "Canada", "Switzerland", 
                    "Germany", "Denmark", "Spain", "Finland", "France", "United Kingdom", 
                    "Hong Kong, China", "Ireland", "Israel", "Italy", "Japan", 
                    "Netherlands", "Norway", "New Zealand", "Portugal", 
                    "Sweden", "Singapore", "United States"),
                  Region = c("Europe Part-1","Asia-Pacific","Europe Part-1","North America & Israel",
                             "Europe Part-2","Europe Part-1","Europe Part-2","Europe Part-2","Europe Part-2",
                             "Europe Part-1","Europe Part-1","Asia-Pacific","Europe Part-1","North America & Israel",
                             "Europe Part-1","Asia-Pacific","Europe Part-2","Europe Part-2",
                             "Asia-Pacific","Europe Part-2","Europe Part-2","Asia-Pacific","North America & Israel"))
plotdf = data.frame(Country = c("AT", "AU", "BE", "CA", "CH", "DE", "DK", "ES", 
                             "FI", "FR", "GB", "HK", "IE", "IL", "IT", "JP", 
                             "NL", "NO", "NZ", "PT", "SE", "SG", "US"), 
                    Benchmark=runif(23), Portfolio=runif(23))
plotdf[,2:3] = sapply(plotdf[,2:3], function(x) x/sum(x))
regions = c("North America & Israel", "Asia-Pacific","Europe Part-1","Europe Part-2")
cols = c("#00BA38","#effcf3","#ff3b2d","#ffcdc9",
         "#5192ff","#b7d2ff","#FFA500","#ffe6ba")
startDate = "2018-01-01"; endDate = "2018-06-30"

plotPie = function(plotdf, cntry, regions, cols, startDate, endDate) 
{
  plotdf2 = melt(merge(plotdf, cntry,  by="Country", 
                      all.x=T, all.y=F), 
                 id.vars=c("Country","CountryName","Region"))
  colnames(plotdf2)[1:2] = c("CountryCode","Country")
  
  if ((length(setdiff(unique(plotdf2$Region), regions))!=0) | 
      (length(setdiff(regions, unique(plotdf2$Region)))!=0)) {
    warning(""); stop();
  }
  
  plist = list()
  for (i in 1:length(regions)) {
    di2 = plotdf2[which(plotdf2$Region!=regions[i]),] %>% 
      dplyr::group_by(Region,variable) %>% 
      summarise(value=sum(value)) %>% mutate(Country=Region,CountryCode=Region)
    di = rbind(plotdf2[which(plotdf2$Region==regions[i]),], data.frame(di2))
    di = di %>% dplyr::mutate(Region=factor(Region, levels=regions)) %>% 
      dplyr::group_by(variable) %>% 
      dplyr::arrange(variable, Region, Country) %>% 
      dplyr::mutate(end_angle =2*pi*cumsum(value),
                    start_angle = lag(end_angle, default = 0),
                    mid_angle = (end_angle+start_angle)/2,
                    lab = paste0(sprintf("%1.2f%%",value))) %>% 
      arrange(Region, Country)
    ColSet = c(cols[1:(length(unique(di$Country))-3)], 
               "#f2f2f2","#737373","#bfbfbf")
    
    plist[[i]] = ggplot(di) + 
      geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                       start = start_angle, end = end_angle, fill = Country)) +
      geom_text(aes(x = .9*sin(mid_angle), y = .9*cos(mid_angle), 
                    label = lab),
                hjust = .5, vjust = .5, size=3.5) +
      scale_fill_manual(values = ColSet) + 
      coord_fixed() +
      scale_x_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
      scale_y_continuous(limits = c(-1, 1), name = "", breaks = NULL, labels = NULL) +
      facet_grid(~variable) +
      ggtitle(paste0("Average Weights : ", regions[i],"  ",
                     startDate," to ",endDate)) +
      theme(plot.title = element_text(hjust = 0))
      
    print(plist[[i]])
  }
  
  graphics.off()

  plot_grid(plist[[1]],plist[[2]], plist[[3]], plist[[4]], ncol=2,align="v")

}

p = plotPie(plotdf, cntry, regions, cols, startDate, endDate) 
ggsave(plot=p, filename="piechart.png",width=15, height=8)


