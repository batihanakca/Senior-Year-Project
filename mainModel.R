library(Rglpk)
library(phonTools)
library(pracma)

isalmost <- function(a,b,tol) {
  k <- all.equal(a,b,tolerance = tol)
  
  if (is.character(k)) {
    k <- FALSE
  }
  return(k)
  
}


solveSeyhanModel <- function(Code,FMOQQ,VMOQQ,Hold,Forecast1,Forecast2,Error1,Error2,Inv,POrder,Limit,SL,SI) {
  
  overall_uplim <- 0.999
  N <- nrow(Code)
  SumLimit <- zeros(N,1)
  
  SKU_Class <- setClass("SKU", slots = c(code="numeric",
                                         FixedMOQ="numeric",
                                         VariableMOQ="numeric",
                                         Mean1="numeric",
                                         Mean2="numeric",
                                         Sigma1="numeric",
                                         Sigma2="numeric",
                                         Inventory0 ="numeric",
                                         PreOrder="numeric",
                                         BigM="numeric",
                                         Expected2="numeric",
                                         CritBound="numeric",
                                         Holding="numeric",
                                         ParamY="numeric",
                                         ParamZ="numeric",
                                         SolX="numeric",
                                         LowerVar="numeric",
                                         UpperVar="numeric"
  ))
  
  SKU <- SKU_Class(code=as.numeric(Code[1,]),
                   FixedMOQ=as.numeric(FMOQQ[1,]),
                   VariableMOQ=as.numeric(VMOQQ[1,]),
                   Mean1=as.numeric(Forecast1[1,]),
                   Mean2=as.numeric(Forecast2[1,]),
                   Sigma1=as.numeric(Error1[1,]),
                   Sigma2=as.numeric(Error2[1,]),
                   Inventory0 =as.numeric(Inv[1,]),
                   PreOrder=as.numeric(POrder[1,]),
                   BigM=as.numeric(2*(Limit[1,])),
                   Expected2=as.numeric(Forecast2[1,]),
                   CritBound=as.numeric(SI[1,]),
                   Holding=as.numeric(Hold[1,]),
                   ParamY=as.numeric(zeros(as.numeric(2*Limit[1,]),1)),
                   ParamZ=as.numeric(zeros(as.numeric(2*Limit[1,]),1)),
                   SolX=as.numeric(zeros(as.numeric(2*Limit[1,]),1)),
                   LowerVar=as.numeric(1),
                   UpperVar=as.numeric(2*Limit[1,]))
  
  
  for (ia in 2:N) {
    
    instance <- SKU_Class(code=as.numeric(Code[ia,]),
                          FixedMOQ=as.numeric(FMOQQ[ia,]),
                          VariableMOQ=as.numeric(VMOQQ[ia,]),
                          Mean1=as.numeric(Forecast1[ia,]),
                          Mean2=as.numeric(Forecast2[ia,]),
                          Sigma1=as.numeric(Error1[ia,]),
                          Sigma2=as.numeric(Error2[ia,]),
                          Inventory0 =as.numeric(Inv[ia,]),
                          PreOrder=as.numeric(POrder[ia,]),
                          BigM=as.numeric(2*Limit[ia,]),
                          Expected2=as.numeric(Forecast2[ia,]),
                          CritBound=as.numeric(SI[ia,]),
                          Holding=as.numeric(Hold[ia,]),
                          ParamY=as.numeric(zeros(as.numeric(2*Limit[ia,]),1)),
                          ParamZ=as.numeric(zeros(as.numeric(2*Limit[ia,]),1)),
                          SolX=as.numeric(zeros(as.numeric(2*Limit[ia,]),1)),
                          LowerVar=as.numeric(1),
                          UpperVar=as.numeric(2*Limit[ia,]))
    
    
    SKU <- c(SKU,instance)
    
  }

  seyhan <- c() 
    
  for (ib in 1:N) {
  #print(ib)
   #  if(   (ib==80 ) ){Y=0
   #    Z=0}
   #     else{
    Y <- c()
    Z <- c()
   
    funY1 <-  function(x,y) (SKU[[ib]]@Inventory0 +SKU[[ib]]@PreOrder-x)*((2*pi*(SKU[[ib]]@Sigma2)*(SKU[[ib]]@Sigma1))^-1)*exp(-((x-SKU[[ib]]@Mean1)^2/(2*(SKU[[ib]]@Sigma1^2))+(y-SKU[[ib]]@Mean2)^2/(2*(SKU[[ib]]@Sigma2^2))))
    funY2 <-  function(x,y) (y)*((2*pi*(SKU[[ib]]@Sigma2)*(SKU[[ib]]@Sigma1))^-1)*exp(-((x-SKU[[ib]]@Mean1)^2/(2*(SKU[[ib]]@Sigma1^2))+(y-SKU[[ib]]@Mean2)^2/(2*(SKU[[ib]]@Sigma2^2))))
    Yymin1 <- function(x)   (SKU[[ib]]@Inventory0 +SKU[[ib]]@PreOrder-x)
    Yymax2 <- function(x)   (SKU[[ib]]@Inventory0 +SKU[[ib]]@PreOrder-x)
    funZ <-   function(x,y) (SKU[[ib]]@Inventory0+SKU[[ib]]@PreOrder-x-y)*((2*pi*(SKU[[ib]]@Sigma2)*(SKU[[ib]]@Sigma1))^-1)*exp(-((x-SKU[[ib]]@Mean1)^2/(2*(SKU[[ib]]@Sigma1^2))+(y-SKU[[ib]]@Mean2)^2/(2*(SKU[[ib]]@Sigma2^2))))
    Zymax <-  function(x)   (SKU[[ib]]@Inventory0 +SKU[[ib]]@PreOrder-x)
    
    qY1=try_default(integral2(funY1,SKU[[ib]]@Mean1-3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean1+3*SKU[[ib]]@Sigma1,Yymin1,SKU[[ib]]@Mean2+3*SKU[[ib]]@Sigma2,reltol = 1e-6, abstol = 1e-12, singular = TRUE)$Q,default=-1)
    qY2=try_default(integral2(funY2,SKU[[ib]]@Mean1-3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean1+3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean2-3*SKU[[ib]]@Sigma2,Yymax2,reltol = 1e-6, abstol = 1e-12, singular = TRUE)$Q,default=-1)
    qZ=try_default(integral2(funZ,SKU[[ib]]@Mean1-3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean1+3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean2-3*SKU[[ib]]@Sigma2,Zymax,reltol = 1e-6, abstol = 1e-12, singular = TRUE)$Q,default=10000000000)
    Y=c(Y,  (as.numeric(qY1)+    as.numeric(qY2)))
    Z=c(Z,    (as.numeric(qZ) )         )  
       # }
    for (jb in 2:SKU[[ib]]@BigM) {
     # print(jb)
     # if(   (ib==84&jb==43)  ){yy=0
     # zz=0}
     # else{
      funY1   <-  function(x,y) (SKU[[ib]]@Inventory0 + SKU[[ib]]@PreOrder + SKU[[ib]]@FixedMOQ + (jb-2)*SKU[[ib]]@VariableMOQ-x)*((2*pi*(SKU[[ib]]@Sigma2)*(SKU[[ib]]@Sigma1))^-1)*exp(-((x-SKU[[ib]]@Mean1)^2/(2*(SKU[[ib]]@Sigma1^2))+(y-SKU[[ib]]@Mean2)^2/(2*(SKU[[ib]]@Sigma2^2))))
      funY2   <-  function(x,y) (y)*((2*pi*(SKU[[ib]]@Sigma2)*(SKU[[ib]]@Sigma1))^-1)*exp(-((x-SKU[[ib]]@Mean1)^2/(2*(SKU[[ib]]@Sigma1^2))+(y-SKU[[ib]]@Mean2)^2/(2*(SKU[[ib]]@Sigma2^2))))
      Yymin1  <-  function(x)   (SKU[[ib]]@Inventory0 + SKU[[ib]]@PreOrder + SKU[[ib]]@FixedMOQ + (jb-2)*SKU[[ib]]@VariableMOQ-x)
      Yymax2  <-  function(x)   (SKU[[ib]]@Inventory0 + SKU[[ib]]@PreOrder + SKU[[ib]]@FixedMOQ + (jb-2)*SKU[[ib]]@VariableMOQ-x)
      funZ    <-  function(x,y) (SKU[[ib]]@Inventory0 + SKU[[ib]]@PreOrder + SKU[[ib]]@FixedMOQ + (jb-2)*SKU[[ib]]@VariableMOQ-x-y)*((2*pi*(SKU[[ib]]@Sigma2)*(SKU[[ib]]@Sigma1))^-1)*exp(-((x-SKU[[ib]]@Mean1)^2/(2*(SKU[[ib]]@Sigma1^2))+(y-SKU[[ib]]@Mean2)^2/(2*(SKU[[ib]]@Sigma2^2))))
      Zymax   <-  function(x)   (SKU[[ib]]@Inventory0 + SKU[[ib]]@PreOrder + SKU[[ib]]@FixedMOQ + (jb-2)*SKU[[ib]]@VariableMOQ-x)
      
      qY1=try_default(integral2(funY1,SKU[[ib]]@Mean1-3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean1+3*SKU[[ib]]@Sigma1,Yymin1,SKU[[ib]]@Mean2+3*SKU[[ib]]@Sigma2,reltol = 1e-6, abstol = 1e-12, singular = TRUE)$Q ,default=-1)
      qY2=try_default(integral2(funY2,SKU[[ib]]@Mean1-3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean1+3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean2-3*SKU[[ib]]@Sigma2,Yymax2,reltol = 1e-6, abstol = 1e-12)$Q ,default=-1)
      qZ=try_default(integral2(funZ,SKU[[ib]]@Mean1-3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean1+3*SKU[[ib]]@Sigma1,SKU[[ib]]@Mean2-3*SKU[[ib]]@Sigma2,Zymax,reltol = 1e-6, abstol = 1e-12, singular = TRUE)$Q ,default=1000000000)
      yy=(as.numeric(qY1 ) +    as.numeric(qY2))
      zz=(as.numeric(qZ) )
       # }
       Y=c(Y, yy )
    
      Z=c(Z, zz   )
      
      
      
      
      
      }
    
    SKU[[ib]]@ParamY <- as.numeric(Y)
    SKU[[ib]]@ParamZ <- as.numeric(Z)
  }
  
  # print(SKU[[3]]@ParamY)
  # print(SKU[[148]]@ParamY)
  
  for (iz in 1:N) {

    lower_check <- TRUE
    upper_check <- FALSE

      for (jz in 1:SKU[[iz]]@BigM) {
        
        if (lower_check == TRUE) {
          if ((SKU[[iz]]@ParamY[jz]/SKU[[iz]]@Expected2)>SKU[[iz]]@CritBound){
            SKU[[iz]]@LowerVar <- jz
            lower_check <- FALSE
            upper_check <- TRUE
          }
        }
        
        if (upper_check){
          if ((SKU[[iz]]@ParamY[jz]/SKU[[iz]]@Expected2)>overall_uplim){
            SKU[[iz]]@UpperVar <- jz
            upper_check <- FALSE
          }
        }
      }
    

    SKU[[iz]]@ParamY <- SKU[[iz]]@ParamY[SKU[[iz]]@LowerVar:SKU[[iz]]@UpperVar]
    SKU[[iz]]@ParamZ <- SKU[[iz]]@ParamZ[SKU[[iz]]@LowerVar:SKU[[iz]]@UpperVar]
    
    if (iz == 1) {
      SumLimit[iz] <- SKU[[iz]]@UpperVar - SKU[[iz]]@LowerVar + 1
    } else if (iz!=1) {
      SumLimit[iz]  <- SumLimit[iz-1] + SKU[[iz]]@UpperVar - SKU[[iz]]@LowerVar + 1
    }
    #print(paste(as.character(SKU[[iz]]@UpperVar),as.character(SKU[[iz]]@LowerVar)))
  }
  
  ff  <- zeros(1,SumLimit[N])
  A   <- zeros((N+1),SumLimit[N])
  lb  <- zeros(1,SumLimit[N])
  ub  <- zeros(1,SumLimit[N]) + 1
  b   <- zeros(N+1,1)
  
  #print(SKU[[23]]@UpperVar)
  #print(SKU[[23]]@LowerVar)
  
  for (ic in 1:N) {
    for (jc in 1:(SKU[[ic]]@UpperVar - SKU[[ic]]@LowerVar + 1)) {
      if (ic == 1) {
        
        ff[1,jc] <- SKU[[ic]]@Holding*SKU[[ic]]@ParamZ[jc]
        A[ic,jc] <- 1
        A[N+1,jc] <- SKU[[ic]]@ParamY[jc]
        
      } else if (ic != 1) {
        #print(ic)
        #print(SumLimit[ic-1,1])
        #print(jc)
        ff[1,SumLimit[ic-1,1] + jc] <- SKU[[ic]]@Holding*SKU[[ic]]@ParamZ[jc]
        A[ic, SumLimit[ic-1,1] + jc] <- 1
        A[N+1,SumLimit[ic-1,1] + jc] <- SKU[[ic]]@ParamY[jc]
      }
    }
    
    b[ic,1] <- 1
    b[N+1,1] <- b[N+1,1]+SKU[[ic]]@Expected2
    
  }
  
  b[N+1,1] <- as.matrix(SL*b[N+1,1])
  
  dir <- c(rep("==",N),">=")
  ##bounds <- list(upper = list(ind = c (1:sum(Limit), val = t(lb))))
  ##types <- c(rep("B",SumLimit[N]))
  
  print(nrow(ff))
  print("UNUTMAYIN KÝ, BANA... HÝÇBÝR ÞEY... OLMAZ!!!")
  print(length(ff))
  solveModel <- Rglpk_solve_LP(obj = t(ff), mat =  A,dir =  dir, rhs = b, types = "B", control = list("verbose" =
                                                                                                       TRUE, "canonicalize_status" = FALSE) )
  ## print(solveModel)
  Total_Cost <- solveModel[[1]]
  
  for (id in 1:N) {
    X <- c()  
    for (jd in 1:(SKU[[id]]@UpperVar - SKU[[id]]@LowerVar + 1)) {
      
      if (id == 1) {
        X <- c(X,solveModel[[2]][jd])
      } else if (id != 1) {
        X <- c(X,solveModel[[2]][SumLimit[id-1,1] + jd])
      }
      
    }
    SKU[[id]]@SolX <- as.numeric(X) 
  }
    
  SumOServ <- 0
  SumExp <- 0
  SKU_Service <- zeros(N,1)
  Order <- zeros(N,1)
  
  for (ie in 1:N) {
    
    SKU_Service[ie,1] <- sum(SKU[[ie]]@ParamY*SKU[[ie]]@SolX)/SKU[[ie]]@Expected2
    SumOServ <- SumOServ + sum(SKU[[ie]]@ParamY*SKU[[ie]]@SolX)
    SumExp <- SumExp + SKU[[ie]]@Expected2
    
    for (je in 1:(SKU[[ie]]@UpperVar - SKU[[ie]]@LowerVar + 1)) {
      
      if (isalmost(SKU[[ie]]@SolX[je],1,exp(-10))) {
        if (je == 1 && SKU[[ie]]@LowerVar == 1) {
          Order[ie] <- 0
        } else {
          Order[ie] <- SKU[[ie]]@FixedMOQ + (SKU[[ie]]@LowerVar + je - 1 - 2)*SKU[[ie]]@VariableMOQ
        }
      }
    }
  }
  
  SKU_Service <- as.matrix(cbind(Code, SKU_Service))
  Order <- as.matrix(cbind(Code,Order))
  Overall_Service <- SumOServ/SumExp
  
  result <- list(Total_Cost = Total_Cost, Overall_Service = Overall_Service, SKU_Service = SKU_Service, Order = Order,SKU=SKU)
  return(result)
}
