require("httr");require("rvest");require("xml2");require("quantmod");require("stringr")
# ************************************************************************************************
#                               Balance Sheet
# ************************************************************************************************
# this function will get you the latest quarterly BL (5 quarters)
getLatestBL = function(ticker)
{
  url = paste0("https://www.barchart.com/stocks/quotes/",ticker,
               "/balance-sheet/quarterly")
  pg = read_html(url)
  
  df = pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
  VAL = pg %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                       "div/div[2]/div/div/div/div[3]/div[1]",
                                       "/span/span")) %>% html_text()
  
  if(VAL == "thousands"){VAL = "000"}
  # change 1st value of df - for colname change
  df[1,1] <- "Description"
  colnames(df) <- df[1,]
  # subset df to remove colnames
  df = df[2:nrow(df),]
  # TOTAL appears 5 different times
  df$Description[str_detect(df$Description,"TOTAL")] <- c("Total Current Assets",
                                                          "Total Non-Current Assets",
                                                          "Total Current Liabilities",
                                                          "Total Non-Current Liabilities",
                                                          "Total Shareholder's Equity")
  # get rid of special characters
  df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
  df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
  df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
  df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
  df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
  
  # add VAL to each numeric variable in BL statement otherwise NA
  for(ii in 2:ncol(df)){
    df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                  df[,ii], 
                                                  as.numeric(paste0(df[,ii],VAL))
    )
    )
    )
  }
  # remove ALL empty rows
  df = df[!(rowSums(is.na(df)) == 5),]
  # remove duplicated rows
  df <- unique(df) %>% as.data.frame()
  # make sure it is data.frame
  df <- data.frame(df[,2:6],row.names = df$Description)
  # return data.frame
  df
}
# this function will get you 9 pages of quarterly BL (45 quarters)
getRestBL = function(ticker)
{
  urls = paste0("https://www.barchart.com/stocks/quotes/",ticker,
                "/balance-sheet/quarterly?reportPage=",paste(2:10))
  
  pg = lapply(as.list(1:length(urls)), function(ii){
    # 3 second script sleep
    Sys.sleep(3)
    pg = try(read_html(urls[ii]),silent = TRUE)
    if(!inherits(pg,'try-error'))
      pg
  })
  # remove empty lists - removes pages without content
  pg = pg[lapply(as.list(1:length(pg)), function(ii) pg[[ii]] %>% html_nodes("table") %>% length) > 0]
  # extract tables
  df = lapply(as.list(1:length(pg)), function(ii){
    
    df = pg[ii][[1]] %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    VAL = pg[ii][[1]] %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                                  "div/div[2]/div/div/div/div[3]/div[1]",
                                                  "/span/span")) %>% html_text()
    
    if(VAL == "thousands"){VAL = "000"}
    # change 1st value of df - for colname change
    df[1,1] <- "Description"
    colnames(df) <- df[1,]
    # subset df to remove colnames
    df = df[2:nrow(df),]
    # TOTAL appears 5 different times
    df$Description[str_detect(df$Description,"TOTAL")] <- c("Total Current Assets",
                                                            "Total Non-Current Assets",
                                                            "Total Current Liabilities",
                                                            "Total Non-Current Liabilities",
                                                            "Total Shareholder's Equity")
    # get rid of special characters
    df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
    df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
    df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
    df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
    df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
    
    # add VAL to each numeric variable in CF statement otherwise NA
    for(ii in 2:ncol(df)){
      df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                    df[,ii], 
                                                    as.numeric(paste0(df[,ii],VAL))
      )
      )
      )
    }
    # remove ALL empty rows
    df = df[!(rowSums(is.na(df)) == 5),]
    # remove duplicated rows
    df <- unique(df) %>% as.data.frame()
    # make sure it is data.frame
    df <- data.frame(df[,2:6],row.names = df$Description)
    # return data.frame
    df
  })
  # merge by row.names
  df2 = merge(df[[1]],df[[2]],by="row.names", all=TRUE)
  df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  # merge by row names and convert to data.frame
  for(ii in 3:length(df)){
    df2 = merge(df2,df[[ii]],by="row.names", all=TRUE) %>% suppressWarnings()
    df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  }
  # return df2
  df2
}
# add BL ratios
getBLRatios = function(BL,ticker)
{
  # https://www.oldschoolvalue.com/stock-valuation/cash-flow-ratios/
  # ***************************************************************************************
  #                                     add STK PRC
  # ***************************************************************************************
  # get data convert to quarterly and extract closing prices
  tmp = Cl(to.quarterly(getSymbols(ticker, auto.assign = FALSE, from="2000-01-01")))
  # convert BL - names to Quarterly timestamps
  Qdates = as.yearqtr(names(BL)[1:length(BL)], format="X%m.%Y")
  # order is decreasing
  Qdates = Qdates[order(Qdates,decreasing = TRUE)]
  # convert stk prices to as.data.frame
  stk = as.data.frame(t(tmp[Qdates]), row.names = NULL)
  # run through each column to extract stk price at the end of each Quarter
  toR <- as.data.frame(NA)
  for(ii in 1:ncol(BL))
  {
    # converts to yearqtr for each column
    QQ = as.yearqtr(names(BL)[ii], format="X%m.%Y")
    # subset desired Quarter price
    prc = try(stk[,paste(QQ)],silent = TRUE)
    if(inherits(prc,'try-error'))
    {
      # add NA if no stk Price is available
      toR = cbind(toR,NA)
    }else{
      # otherwise add stock price
      toR = cbind(toR,prc)  
    }
  }
  # data frame// format rows
  toR <- data.frame(toR[,2:ncol(toR)], row.names = "Stock Price")
  colnames(toR) <- names(BL)
  # ***************************************************************************************
  #                                       Current Ratio
  # ***************************************************************************************
  currentR = round(as.numeric(BL["Total Current Assets",])/
                     as.numeric(BL["Total Current Liabilities",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  currentR <- data.frame(currentR, row.names = "Current Ratio")
  colnames(currentR) <- names(BL)
  # ***************************************************************************************
  #                                        Quick Ratio
  # ***************************************************************************************
  quickR = round((as.numeric(BL["Total Current Assets",])-as.numeric(BL["Inventories",]))/
                   as.numeric(BL["Total Current Liabilities",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  quickR <- data.frame(quickR, row.names = "Quick Ratio")
  colnames(quickR) <- names(BL)
  # ***************************************************************************************
  #                                       Total Debt 2 Equity Ratio
  # ***************************************************************************************
  debt2Eqt = round((as.numeric(BL["Total Liabilities",])/
                      as.numeric(BL["Total Shareholder's Equity",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  debt2Eqt <- data.frame(debt2Eqt, row.names = "Total Debt-to-Equity Ratio")
  colnames(debt2Eqt) <- names(BL)
  # ***************************************************************************************
  #                                       Long-Term Debt 2 Equity Ratio
  # ***************************************************************************************
  Ltdebt2Eqt = round((as.numeric(BL["Total Non-Current Liabilities",])/
                        as.numeric(BL["Total Shareholder's Equity",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  Ltdebt2Eqt <- data.frame(Ltdebt2Eqt, row.names = "Long-Term Debt-to-Equity Ratio")
  colnames(Ltdebt2Eqt) <- names(BL)
  # ***************************************************************************************
  #                                       Short-Term Debt 2 Equity Ratio
  # ***************************************************************************************
  Stdebt2Eqt = round((as.numeric(BL["Total Current Liabilities",])/
                        as.numeric(BL["Total Shareholder's Equity",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  Stdebt2Eqt <- data.frame(Stdebt2Eqt, row.names = "Short-Term Debt-to-Equity Ratio")
  colnames(Stdebt2Eqt) <- names(BL)
  # ***************************************************************************************
  #                                        Working Capital
  # ***************************************************************************************
  workingK = round((as.numeric(BL["Total Current Assets",])-
                      as.numeric(BL["Total Current Liabilities",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  workingK <- data.frame(workingK, row.names = "Working Capital")
  colnames(workingK) <- names(BL)
  # ***************************************************************************************
  #                                        Working Capital/Share
  # ***************************************************************************************
  workKPerShr = round((as.numeric(BL["Total Current Assets",])-
                         as.numeric(BL["Total Current Liabilities",]))/
                        as.numeric(BL["Shares Outstanding, K",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  workKPerShr <- data.frame(workKPerShr, row.names = "Working Capital Per Share")
  colnames(workKPerShr) <- names(BL)
  # ***************************************************************************************
  #                                     Rowbind -> Output
  # ***************************************************************************************
  ALL = rbind(toR,currentR,quickR,Stdebt2Eqt,Ltdebt2Eqt,debt2Eqt,workingK,workKPerShr)
  
  ALL
}
# gets All Statements + Ratios
getAllBL = function(ticker)
{
  # get First Page
  pg1   = getLatestBL(ticker)
  # get ALL Pages
  pg210 = getRestBL(ticker)
  
  # combine tables & merge by row.names
  BL = merge(pg1,pg210,by="row.names", all=TRUE)
  BL = data.frame(BL[,2:ncol(BL)], row.names = BL[,1])
  # get Ratios
  RATIOS = getBLRatios(BL=BL,ticker=ticker)
  # rowbind with BL
  BL = rbind(BL,RATIOS)
  
  BL
}
# ************************************************************************************************
#                               Cash Flow Statement
# ************************************************************************************************
# this function will get you the latest quarterly CF (5 quarters)
getLatestCF = function(ticker)
{
  url = paste0("https://www.barchart.com/stocks/quotes/",ticker,
               "/cash-flow/quarterly")
  pg = read_html(url)
  
  df = pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
  VAL = pg %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                       "div/div[2]/div/div/div/div[3]/div[1]",
                                       "/span/span")) %>% html_text()
  
  if(VAL == "thousands"){VAL = "000"}
  # change 1st value of df - for colname change
  df[1,1] <- "Description"
  colnames(df) <- df[1,]
  # subset df to remove colnames
  df = df[2:nrow(df),]
  
  # get rid of special characters
  df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
  df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
  df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
  df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
  df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
  
  # add VAL to each numeric variable in CF statement otherwise NA
  for(ii in 2:ncol(df)){
    df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                  df[,ii], 
                                                  as.numeric(paste0(df[,ii],VAL))
    )
    )
    )
  }
  # remove ALL empty rows
  df = df[!(rowSums(is.na(df)) == 5),]
  # change duplicate row name
  df$Description[nrow(df)-2] <- "Operating CF"
  # make sure it is data.frame
  df <- data.frame(df[,2:6],row.names = df$Description)
  # return data.frame
  df
}
# this function will get you 9 pages of quarterly CF (45 quarters)
getRestCF = function(ticker)
{
  urls = paste0("https://www.barchart.com/stocks/quotes/",ticker,
                "/cash-flow/quarterly?reportPage=",paste(2:10))
  
  pg = lapply(as.list(1:length(urls)), function(ii){
    # 3 second script sleep
    Sys.sleep(3)
    pg = try(read_html(urls[ii]),silent = TRUE)
    if(!inherits(pg,'try-error'))
      pg
  })
  # remove empty lists - removes pages without content
  pg = pg[lapply(as.list(1:length(pg)), function(ii) pg[[ii]] %>% html_nodes("table") %>% length) > 0]
  # extract tables
  df = lapply(as.list(1:length(pg)), function(ii){
    
    df = pg[ii][[1]] %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    VAL = pg[ii][[1]] %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                                  "div/div[2]/div/div/div/div[3]/div[1]",
                                                  "/span/span")) %>% html_text()
    
    if(VAL == "thousands"){VAL = "000"}
    # change 1st value of df - for colname change
    df[1,1] <- "Description"
    colnames(df) <- df[1,]
    # subset df to remove colnames
    df = df[2:nrow(df),]
    
    # get rid of special characters
    df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
    df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
    df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
    df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
    df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
    
    # add VAL to each numeric variable in CF statement otherwise NA
    for(ii in 2:ncol(df)){
      df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                    df[,ii], 
                                                    as.numeric(paste0(df[,ii],VAL))
      )
      )
      )
    }
    # remove ALL empty rows
    df = df[!(rowSums(is.na(df)) == 5),]
    # change duplicate row name
    df$Description[nrow(df)-2] <- "Operating CF"
    # make sure it is data.frame
    df <- data.frame(df[,2:6],row.names = df$Description)
    # return data.frame
    df
  })
  # merge by row.names
  df2 = merge(df[[1]],df[[2]],by="row.names", all=TRUE)
  df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  # merge by row names and convert to data.frame
  for(ii in 3:length(df)){
    df2 = merge(df2,df[[ii]],by="row.names", all=TRUE) %>% suppressWarnings()
    df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  }
  # return df2
  df2
}
# add CF ratios
getCFRatios = function(CF,ticker)
{
  # https://www.oldschoolvalue.com/stock-valuation/cash-flow-ratios/
  # ***************************************************************************************
  #                                     add STK PRC
  # ***************************************************************************************
  # get data convert to quarterly and extract closing prices
  tmp = Cl(to.quarterly(getSymbols(ticker, auto.assign = FALSE, from="2000-01-01")))
  # convert CF - names to Quarterly timestamps
  Qdates = as.yearqtr(names(CF)[1:length(CF)], format="X%m.%Y")
  # order is decreasing
  Qdates = Qdates[order(Qdates,decreasing = TRUE)]
  # convert stk prices to as.data.frame
  stk = as.data.frame(t(tmp[Qdates]), row.names = NULL)
  # run through each column to extract stk price at the end of each Quarter
  toR <- as.data.frame(NA)
  for(ii in 1:ncol(CF))
  {
    # converts to yearqtr for each column
    QQ = as.yearqtr(names(CF)[ii], format="X%m.%Y")
    # subset desired Quarter price
    prc = try(stk[,paste(QQ)],silent = TRUE)
    if(inherits(prc,'try-error'))
    {
      # add NA if no stk Price is available
      toR = cbind(toR,NA)
    }else{
      # otherwise add stock price
      toR = cbind(toR,prc)  
    }
  }
  # data frame// format rows
  toR <- data.frame(toR[,2:ncol(toR)], row.names = "Stock Price")
  colnames(toR) <- names(CF)
  # ***************************************************************************************
  #  Operating Cash Flow / Total Cash Flow: How much of the cash comes from Operations
  # ***************************************************************************************
  op2TotalCF = round(as.numeric(CF["Operating Cash Flow",])/
                       (as.numeric(CF["Operating Cash Flow",])+
                          as.numeric(CF["Financing Cash Flow",])+
                          as.numeric(CF["Investing Cash Flow",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  op2TotalCF <- data.frame(op2TotalCF, row.names = "Cash Generating Ratio")
  colnames(op2TotalCF) <- names(CF)
  # ***************************************************************************************
  #  External Financing Ratio (CFF / CFO): How much does the company depend on Financing
  # ***************************************************************************************
  EFR = round(as.numeric(CF["Financing Cash Flow",])/
                as.numeric(CF["Operating Cash Flow",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  EFR <- data.frame(EFR, row.names = "External Financing Ratio")
  colnames(EFR) <- names(CF)
  # ***************************************************************************************
  #                                     Rowbind -> Output
  # ***************************************************************************************
  ALL = rbind(toR,op2TotalCF,EFR)
  
  ALL
}
# gets All Statements + Ratios
getAllCF = function(ticker)
{
  # get First Page
  pg1   = getLatestCF(ticker)
  # get ALL Pages
  pg210 = getRestCF(ticker)
  
  # combine tables & merge by row.names
  CF = merge(pg1,pg210,by="row.names", all=TRUE)
  CF = data.frame(CF[,2:ncol(CF)], row.names = CF[,1])
  # get Ratios
  RATIOS = getCFRatios(CF=CF,ticker=ticker)
  # rowbind with CF
  CF = rbind(CF,RATIOS)
  
  CF
}
# ************************************************************************************************
#                               Income Statement
# ************************************************************************************************
# this function will get you the latest quarterly IS (5 quarters)
getLatestIS = function(ticker)
{
  url = paste0("https://www.barchart.com/stocks/quotes/",ticker,
               "/income-statement/quarterly")
  pg = read_html(url)
  
  df = pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
  VAL = pg %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                       "div/div[2]/div/div/div/div[3]/div[1]",
                                       "/span/span")) %>% html_text()
  
  if(VAL == "thousands"){VAL = "000"}
  # change 1st value of df - for colname change
  df[1,1] <- "Description"
  colnames(df) <- df[1,]
  # subset df to remove colnames
  df = df[2:nrow(df),]
  
  # get rid of special characters
  df[,2] <- gsub("\\$","",gsub("\\,","",df[,2]))
  df[,3] <- gsub("\\$","",gsub("\\,","",df[,3]))
  df[,4] <- gsub("\\$","",gsub("\\,","",df[,4]))
  df[,5] <- gsub("\\$","",gsub("\\,","",df[,5]))
  df[,6] <- gsub("\\$","",gsub("\\,","",df[,6]))
  
  # select rows only
  ROWS = c("Sales","Cost of Goods","Gross Profit","Operating Expenses",                   
           "Operating Income","Interest Expense","Other Income","Pre-tax Income",
           "Income Tax","Net Income","EPS Basic Total Ops",
           "EPS Diluted Total Ops","Ebitda")
  
  df = lapply(as.list(1:length(ROWS)), function(ii){
    tmp = subset(df, df$Description == ROWS[ii])
    tmp[1,2:ncol(tmp)] = paste0(tmp[1,2:ncol(tmp)],VAL) 
    tmp
  })
  # remove any empty cases
  df = df[lapply(df, length)>0]
  # rbind list
  df = do.call(rbind,df)
  # remove NA row
  INT = which(is.na(df[,1]))
  if(length(INT)!=0){df= df[-INT,]}
  
  # make sure it is data.frame
  df <- as.data.frame(df)
  # convert to numeric
  df[,2:ncol(df)]<- round(sapply(df[,2:ncol(df)], as.numeric),2)
  
  # make sure it is data.frame
  df = as.data.frame(df)
  
  # return data.frame
  df
}
# this function will get you 9 pages of quarterly IS (45 quarters)
getRestIS = function(ticker)
{
  urls = paste0("https://www.barchart.com/stocks/quotes/",ticker,
                "/income-statement/quarterly?reportPage=",paste(2:10))
  
  pg = lapply(as.list(1:length(urls)), function(ii){
    # 3 second script sleep
    Sys.sleep(3)
    pg = try(read_html(urls[ii]),silent = TRUE)
    if(!inherits(pg,'try-error'))
      pg
  })
  # remove empty lists - removes pages without content
  pg = pg[lapply(as.list(1:length(pg)), function(ii) pg[[ii]] %>% html_nodes("table") %>% length) > 0]
  # extract tables
  df = lapply(as.list(1:length(pg)), function(ii){
    
    df = pg[ii][[1]] %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    VAL = pg[ii][[1]] %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                                  "div/div[2]/div/div/div/div[3]/div[1]",
                                                  "/span/span")) %>% html_text()
    
    if(VAL == "thousands"){VAL = "000"}
    # change 1st value of df - for colname change
    df[1,1] <- "Description"
    colnames(df) <- df[1,]
    # subset df to remove colnames
    df = df[2:nrow(df),]
    
    # get rid of special characters
    df[,2] <- gsub("\\$","",gsub("\\,","",df[,2]))
    df[,3] <- gsub("\\$","",gsub("\\,","",df[,3]))
    df[,4] <- gsub("\\$","",gsub("\\,","",df[,4]))
    df[,5] <- gsub("\\$","",gsub("\\,","",df[,5]))
    df[,6] <- gsub("\\$","",gsub("\\,","",df[,6]))
    
    
    # select rows only
    ROWS = c("Sales","Cost of Goods","Gross Profit","Operating Expenses",                   
             "Operating Income","Interest Expense","Other Income","Pre-tax Income",
             "Income Tax","Net Income","EPS Basic Total Ops",
             "EPS Diluted Total Ops","Ebitda")
    # subset select rows
    df = lapply(as.list(1:length(ROWS)), function(ii){
      tmp = subset(df, df$Description == ROWS[ii])
      tmp[1,2:ncol(tmp)] = paste0(tmp[1,2:ncol(tmp)],VAL) 
      tmp
    })
    # remove any empty cases
    df = df[lapply(df, length)>0]
    # rbind list
    df = do.call(rbind,df)
    # remove NA row
    INT = which(is.na(df[,1]))
    if(length(INT)!=0){df= df[-INT,]}
    
    # make sure it is data.frame
    df <- as.data.frame(df)
    # convert to numeric
    df[,2:ncol(df)]<- round(sapply(df[,2:ncol(df)], as.numeric),2) %>% suppressWarnings()
    # make sure it is data.frame
    df = as.data.frame(df)
    # return data.frame
    df[,2:ncol(df)]
  })
  # combine tables
  df = do.call(cbind,df)
  # return table
  df
}
# add IS ratios
getISRatios = function(IS,ticker)
{
  # ***************************************************************************************
  #                                     Gross Margin
  # ***************************************************************************************
  GM = round(as.numeric(subset(IS,IS$Description == "Gross Profit"))/
               as.numeric(subset(IS,IS$Description == "Sales")),digits = 4) %>% suppressWarnings()
  # convert to data.frame
  GM = as.data.frame(t(GM))
  # change row description + add colnames
  GM[1,1] = "Gross Margin"; colnames(GM) = names(IS)
  # ***************************************************************************************
  #                                     Profit Margin
  # ***************************************************************************************
  PM = round(as.numeric(subset(IS,IS$Description == "Net Income"))/
               as.numeric(subset(IS,IS$Description == "Sales")),digits = 4) %>% suppressWarnings()
  # convert to data.frame
  PM = as.data.frame(t(PM))
  # change row description + add colnames
  PM[1,1] = "Profit Margin"; colnames(PM) = names(IS)
  # ***************************************************************************************
  #                                     add STK PRC
  # ***************************************************************************************
  # get data convert to quarterly and extract closing prices
  tmp = Cl(to.quarterly(getSymbols(ticker, auto.assign = FALSE, from="2000-01-01")))
  # convert IS - names to Quarterly timestamps
  Qdates = as.yearqtr(names(IS)[2:length(IS)], format="%m-%Y")
  # order is decreasing
  Qdates = Qdates[order(Qdates,decreasing = TRUE)]
  # convert stk prices to as.data.frame
  stk = as.data.frame(t(tmp[Qdates]), row.names = NULL)
  # run through each column to extract stk price at the end of each Quarter
  toR <- as.data.frame(NA)
  for(ii in 2:ncol(IS))
  {
    # converts to yearqtr for each column
    QQ = as.yearqtr(names(IS)[ii], format="%m-%Y")
    # subset desired Quarter price
    prc = try(stk[,paste(QQ)],silent = TRUE)
    if(inherits(prc,'try-error'))
    {
      # add NA if no stk Price is available
      toR = cbind(toR,NA)
    }else{
      # otherwise add stock price
      toR = cbind(toR,prc)  
    }
  }
  # add description
  toR[1,1] = "Stock Price"
  # format colnames
  colnames(toR) = names(IS)
  # ***************************************************************************************
  #                                     add P/E Ratio (wont consider negative earnings)
  # ***************************************************************************************
  AnnualEPS = suppressWarnings(as.numeric(subset(IS,IS$Description == "EPS Basic Total Ops"))*4)
  PE = round(as.numeric(toR)/AnnualEPS,
             digits = 4) %>% suppressWarnings()
  # convert to data.frame
  PE = as.data.frame(t(PE))
  # change row description + add colnames
  PE[1,1] = "PE Ratio"; colnames(PE) = names(IS)
  # ***************************************************************************************
  #                                     Rowbind -> Output
  # ***************************************************************************************
  ALL = rbind(GM,PM,toR,PE)
  
  ALL
}
# gets All Statements + Ratios
getAllIS = function(ticker)
{
  # get First Page
  pg1   = getLatestIS(ticker)
  # get ALL Pages
  pg210 = getRestIS(ticker)
  # combine tables
  IS = cbind(pg1, pg210)
  # get Ratios
  RATIOS = getISRatios(IS=IS,ticker=ticker)
  # rowbind with IS
  IS = rbind(IS,RATIOS)
  
  IS
}
# ************************************************************************************************
#                                 Ratios
# ************************************************************************************************
# https://corporatefinanceinstitute.com/resources/knowledge/finance/financial-ratios/
#  * TURNOVER Ratios use given stats & not Averaged

getFinRatios = function(IS,CF,BL)
{
  IS = data.frame(IS[,2:ncol(IS)], row.names = IS$Description)
  
  # ************************************************************************************************
  #                                             Liquidity Ratios
  # ************************************************************************************************
  # Quick Ratio
  quickR    = BL["Quick Ratio",]
  # Current Ratio
  currentR  = BL["Current Ratio",]
  # Cash Ratio
  cashR     = round(as.numeric(BL["Cash & Cash Equivalents",])/
              as.numeric(BL["Total Current Liabilities",]),4) %>% t %>% as.data.frame()
  cashR <- data.frame(cashR, row.names = "Cash Ratio")
  colnames(cashR) <- names(BL)
  # Operating Cash Flow Ratio
  opCF = round(as.numeric(CF["Operating CF",])/
                 as.numeric(BL["Total Current Liabilities",]),4) %>% t %>% as.data.frame()
  opCF <- data.frame(opCF, row.names = "Operating CF Ratio")
  colnames(opCF) <- names(BL)
  # Total Cash Per Share
  cashPShr = round(as.numeric(BL["Cash & Cash Equivalents",])/
                 as.numeric(BL["Shares Outstanding, K",]),4) %>% t %>% as.data.frame()
  cashPShr <- data.frame(cashPShr, row.names = "Cash Per Share")
  colnames(cashPShr) <- names(BL)
  # combine all ratios
  liquidityR = rbind(quickR,currentR,cashR,opCF,cashPShr)
  # ************************************************************************************************
  #                                             Leverage Ratios
  # ************************************************************************************************
  # Short Term Debt-to-Equity
  STD2EQT = BL["Short-Term Debt-to-Equity Ratio",]
  # Long Term Debt-to-Equity
  LTD2EQT = BL["Long-Term Debt-to-Equity Ratio",]
  # Debt-to-Equity
  D2EQT = BL["Total Debt-to-Equity Ratio",]
  # Debt Ratio
  debtR = round(as.numeric(BL["Total Liabilities",])/
                 as.numeric(BL["Total Assets",]),4) %>% t %>% as.data.frame()
  debtR <- data.frame(debtR, row.names = "Debt Ratio")
  colnames(debtR) <- names(BL)
  # all ratios
  leverR = rbind(STD2EQT,LTD2EQT,D2EQT,debtR)
  # ************************************************************************************************
  #                                             Efficiency Ratios
  # ************************************************************************************************
  # Asset Turnover*
  assetTO = round(as.numeric(IS["Sales",])/
                  as.numeric(BL["Total Assets",]),4) %>% t %>% as.data.frame()
  assetTO <- data.frame(assetTO, row.names = "Asset Turnover Ratio")
  colnames(assetTO) <- names(BL)
  # Inventory Turnover*
  inTO = round(as.numeric(IS["Sales",])/
                    as.numeric(BL["Inventories",]),4) %>% t %>% as.data.frame()
  inTO <- data.frame(inTO, row.names = "Inventory Turnover Ratio")
  colnames(inTO) <- names(BL)
  # AR Turnover*
  arTO = round(as.numeric(IS["Cost of Goods",])/
                 as.numeric(BL["Receivables",]),4) %>% t %>% as.data.frame()
  arTO <- data.frame(arTO, row.names = "AR Turnover Ratio")
  colnames(arTO) <- names(BL)
  # Days sales in inventory ratio*
  dSir = round(365/inTO,4)  %>% as.data.frame()
  dSir <- data.frame(dSir, row.names = "Day Sales in Inventory")
  colnames(dSir) <- names(BL)
  # Days Sales Outstanding (DSO)*
  # *Using total Sales (as credit sales not in Financials)
  dSO = round((as.numeric(CF["Accounts receivable",])/
                 as.numeric(IS["Sales",]))*365,4) %>% t %>% as.data.frame()
  dSO <- data.frame(dSO, row.names = "Day Sales Outstanding")
  colnames(dSO) <- names(BL)
  # Days Payable Outstanding (DPO)*
  # *Using total Sales (as credit sales not in Financials)
  dPO = round((as.numeric(CF["Accounts payable and accrued liabilities",])/
                 as.numeric(IS["Cost of Goods",]))*365,4) %>% t %>% as.data.frame()
  dPO <- data.frame(dPO, row.names = "Days Payable Outstanding")
  colnames(dPO) <- names(BL)
  # Estimated Cash Conversion Cycle
  CCC = as.numeric(dSir)+as.numeric(dSO) - as.numeric(dPO) %>% t %>% as.data.frame()
  CCC <- data.frame(CCC, row.names = "Cash Conversion Cycle")
  colnames(CCC) <- names(BL)
  # all ratios
  eff = rbind(assetTO,inTO,arTO,dSir,dSO,dPO,CCC)
  # ************************************************************************************************
  #                                             Profitability Ratios
  # ************************************************************************************************
  # Gross Margin
  GM = IS["Gross Margin",]
  # Profit MArgin
  PM = IS["Profit Margin",]
  # Return on Assets
  ROA = round(as.numeric(IS["Net Income",])/
                as.numeric(BL["Total Assets",]),4) %>% t %>% as.data.frame()
  ROA <- data.frame(ROA, row.names = "ROA")
  colnames(ROA) <- names(BL)
  # Return on Equity
  ROE = round(as.numeric(IS["Net Income",])/
                as.numeric(BL["Total Shareholder's Equity",]),4) %>% t %>% as.data.frame()
  ROE <- data.frame(ROE, row.names = "ROE")
  colnames(ROE) <- names(BL)
  # all ratios
  proff = rbind(GM,PM,ROA,ROE)
  # ************************************************************************************************
  #                                             Market Value Ratios
  # ************************************************************************************************
  # Book Value Per Share* NO ADJUSTMENT FOR PREFERRED QUITY
  BV = round(as.numeric(BL["Total Shareholder's Equity",])/
                as.numeric(BL["Shares Outstanding, K",]),4) %>% t %>% as.data.frame()
  BV <- data.frame(BV, row.names = "Book Value Per Share")
  colnames(BV) <- names(BL)
  # Basic/Diluted EPS
  bEPS = IS["EPS Basic Total Ops",]
  dEPS = IS["EPS Diluted Total Ops",]
  # P/E Ratio
  PE = IS["PE Ratio",]
  # Payout Ratio
  # check if the company paid out dividends:
  DIV = abs(CF["Dividend Paid",])
  DIV[is.na(DIV)] <- 0
  
  POR = round(as.numeric(DIV)/as.numeric(CF["Net Income",]),4) %>% t %>% as.data.frame()
  POR <- data.frame(POR, row.names = "Payout Ratio")
  colnames(POR) <- names(BL)
  # Stock Price
  PRC = IS["Stock Price",]
  
  MVR = rbind(BV,bEPS,dEPS,PE,POR,PRC)
  # ************************************************************************************************
  #                                            Combine Ratios
  # ************************************************************************************************
  # combine all ratios
  ALL = rbind(liquidityR,leverR,eff,proff,MVR)
}





