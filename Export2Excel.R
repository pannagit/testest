
library(openxlsx)
Sys.setenv(R_ZIPCMD= "g:\\zip.exe")


wb <- createWorkbook()          
addWorksheet(wb, "Sheet1") 
writeData(wb, "Sheet1", data.frame(getOut2), startCol=1, startRow=1, colNames=TRUE)
# addWorksheet(wb, "Sheet2") 
# writeData(wb, "Sheet2", xout2, startCol=1, startRow=1, colNames=TRUE)

Time <- Sys.time()
Time <- gsub(":","",Time)
Time <- gsub(" ","",Time) 
Time <- gsub("-","",Time)
Time <- paste0(substr(Time,1,8), ".v", substr(Time,9,14))
xlsname <- paste0(getwd(),"/ViewData.",Time,".xlsx")
saveWorkbook(wb, xlsname)
shell.exec(xlsname)
