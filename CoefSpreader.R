CoefSpreader <- function(Ctype)
{
  if(max(Ctype$WeekNo) == 4)
  {
    Ctype <- Ctype %>% 
      spread(WeekNo,Value, fill = 0) 
    
    names(Ctype)[6] = "Week1"
    names(Ctype)[7] = "Week2" 
    names(Ctype)[8] = "Week3" 
    names(Ctype)[9] = "Week4" 
    
    Ctype <- Ctype %>% 
      group_by(Code,MonthlyCode) %>% 
      mutate(Value = Week1 + Week2 + Week3 + Week4) %>% 
      summarise(Week1 = sum(Week1), Week2 = sum(Week2), Week3 = sum(Week3), Week4 = sum(Week4), Sum = sum(Value))
  }
  else if(max(Ctype$WeekNo) == 5)
  {
    Ctype <- Ctype %>% 
      spread(WeekNo,Value, fill = 0) 
    
    names(Ctype)[6] = "Week1"
    names(Ctype)[7] = "Week2" 
    names(Ctype)[8] = "Week3" 
    names(Ctype)[9] = "Week4" 
    names(Ctype)[10] = "Week5" 
    
    Ctype <- Ctype %>% 
      group_by(Code,MonthlyCode) %>% 
      mutate(Value = Week1 + Week2 + Week3 + Week4 + Week5) %>% 
      summarise(Week1 = sum(Week1), Week2 = sum(Week2), Week3 = sum(Week3), Week4 = sum(Week4), Week5 = sum(Week5),Sum = sum(Value))
    
  }

  return(Ctype)
}