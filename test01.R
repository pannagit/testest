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
  
