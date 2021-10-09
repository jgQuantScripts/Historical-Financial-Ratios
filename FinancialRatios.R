source("FIN_FUN.R")

ticker = "GOOGL"

# get Income Statements
IS = getAllIS(ticker=ticker)
# get Cash Flow Statements
CF = getAllCF(ticker=ticker)
# get Balance Sheet
BL = getAllBL(ticker=ticker)

# getting Ratios
RATIOS = getFinRatios(IS=IS,CF=CF,BL=BL)

# transpose table
BLt = as.data.frame(t(BL))
CFt = as.data.frame(t(CF))
ISt = as.data.frame(t(IS))
# write table as csv
write.table(BLt,paste0("~/Desktop/",ticker,"_BLT.csv"),sep = ",")
write.table(CFt,paste0("~/Desktop/",ticker,"_CFT.csv"),sep = ",")
write.table(ISt,paste0("~/Desktop/",ticker,"_IST.csv"),sep = ",")