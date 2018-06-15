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