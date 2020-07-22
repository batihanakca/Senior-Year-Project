keson <- function(qType, tabName, fromMonth, fromYear, toMonth, toYear, conn){
  
  con <- conn
  delByDateDf1 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf2 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf3 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf4 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf5 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf6 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf7 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  delByDateDf8 = data.frame(Code = character(), Type = character(), CumDate = character(),WeekNo = double(), CoefType = character(),MonthlyDate = character(),Value = double(),Date = character())
  
  for (year in fromYear:toYear) {
    if(fromYear != toYear)
    {   
      if (year == fromYear)
      {
        for (mon1 in fromMonth:12) {
          if (mon1 > 9)
          {
            delByDateDf1 <- rbind(delByDateDf1,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'",as.character(mon1),as.character(year),"'")))
          }                
          else
          {
            delByDateDf2 <- rbind(delByDateDf2,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'0",as.character(mon1),as.character(year),"'")))
          }
          
        }
      }
      else if(year == toYear)
      {
        for (mon2 in 1:toMonth) {
          
          if (mon2 > 9)  
          {
            delByDateDf3 <- rbind(delByDateDf3,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'",as.character(mon2),as.character(year),"'")))
          }                
          else
          {
            delByDateDf4 <- rbind(delByDateDf4,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'0",as.character(mon2),as.character(year),"'")))
          } 
        }
        
      }
      else
      {
        for (mon3 in 1:12) {
          if (mon3 > 9)
          {
            delByDateDf5 <- rbind(delByDateDf5,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'",as.character(mon3),as.character(year),"'")))
          }                
          else
          {
            delByDateDf6 <- rbind(delByDateDf6,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'0",as.character(mon3),as.character(year),"'")))
          } 
        }
      }
    }
    
    else
    {
      for (mon4 in fromMonth:toMonth) 
      {
        if (mon4 > 9)
        {
          delByDateDf7 <- rbind(delByDateDf7,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'",as.character(mon4),as.character(year),"'")))
        }                
        else
        {
          delByDateDf8 <- rbind(delByDateDf8,dbGetQuery(con,paste0(qType," * FROM ", tabName," WHERE MonthlyCode = ","'0",as.character(mon4),as.character(year),"'")))
        } 
        
      }
    }
  }
  return(rbind(delByDateDf2,delByDateDf1,delByDateDf4,delByDateDf3,delByDateDf6,delByDateDf5,delByDateDf8,delByDateDf7))
}