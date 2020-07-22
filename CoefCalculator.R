CoefCalculator <- function(CType, WeekNo, CName){
  
  CoefTable <- data.frame(CoefType = as.character(), firstWeek = as.numeric(), secondWeek = as.numeric(), thirdWeek = as.numeric(), fourthWeek = as.numeric(),fifthWeek = as.numeric())
  Coef <- c()
  
  Coef[1] <-  CName
  Coef[2] <-  round(crossprod(CType$Week1, CType$Sum) / crossprod(CType$Sum, CType$Sum),5)
  Coef[3] <-  round(crossprod(CType$Week2, CType$Sum) / crossprod(CType$Sum, CType$Sum),5)
  Coef[4] <-  round(crossprod(CType$Week3, CType$Sum) / crossprod(CType$Sum, CType$Sum),5)
  Coef[5] <-  round(crossprod(CType$Week4, CType$Sum) / crossprod(CType$Sum, CType$Sum),5)
  
  if(WeekNo == 5)
  {
    Coef[6] <-  round(crossprod(CType$Week5, CType$Sum) / crossprod(CType$Sum, CType$Sum),5)
  }
  else
  {
    Coef[6] <-  -1
  }
  
  CoefTable <- rbind(CoefTable,Coef)
  return(Coef)
}