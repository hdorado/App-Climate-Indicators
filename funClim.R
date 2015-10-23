
#GRÁFICAR DATOS DE CLIMA

ggplotClima <- function(baseClimatica)
{  
  
  
  longBaseClimatica <- reshape(baseClimatica,idvar="Indicador",ids=row.names(baseClimatica),
                               times=names(baseClimatica)[-1],timevar="Variable",
                               varying=list(names(baseClimatica)[-1]),direction="long") 
  names(longBaseClimatica) <- c( "Date","Variable","Valor","Indicador")
  longBaseClimatica$Variable <- factor(longBaseClimatica$Variable,levels = c("TMAX","TMIN","RAIN","RHUM","ESOL"))
  
  a <- ggplot(longBaseClimatica,aes(y=Valor,x=Date))+geom_line(colour="blue")+facet_grid(Variable~.,scale = "free")+
    theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid = element_blank())+ylab("     wam2                  %                mm                  C                  C           ")
  
  print(a)
}

#COMPLETAR FECHAS

dateEvent <-  function(cropEvent)
{
  daysAfter <-  switch(as.character(cropEvent$Variety),F733 = c(43,84),LAGUNAS = c(44,85),F60=c(45,87),F473=c(43,85),MOCARI=c(43,86),c(44,86))  
  cropEvent$Ini_Panicle_Date  <- as.Date(cropEvent$Sowing_Date+daysAfter[1], origin="1899-12-30")
  cropEvent$Heading_Date      <- as.Date(cropEvent$Sowing_Date+daysAfter[2], origin="1899-12-30")
  
  return(cropEvent)
}

#INDICADORES CLIMÁTICOS

climetIndicators <- function(cropEvent,baseClima)
{
  stage <- c("","VEG","REP","RIP")
  
  IndSOW <- which(baseClima$DATE==cropEvent$Sowing_Date)
  IndHAR <- which(baseClima$DATE==cropEvent$Harvest_Date)
  IndVEG <- which(baseClima$DATE==cropEvent$Ini_Panicle_Date)  
  IndREP <- which(baseClima$DATE==cropEvent$Heading_Date)
  
  climIndStage <- list(0)  
  
  for(i in 1:4)
  {
    lapse <- switch(stage[i],VEG = c(IndSOW,IndVEG-1) ,REP = c(IndVEG,IndREP-1),RIP = c(IndREP,IndHAR), c(IndSOW,IndHAR) )
    umTmx <- switch(stage[i],VEG = 35,REP = 37, RIP = 31 ,34)
    
    if(stage[i]=="" )
    {  
      Temp_Max          <- max(baseClima$TMAX[lapse[1]:lapse[2]])
      Temp_Min          <- min(baseClima$TMIN[lapse[1]:lapse[2]])
      Temp_Max_Avg      <- mean(baseClima$TMAX[lapse[1]:lapse[2]])
      Temp_Min_Avg      <- mean(baseClima$TMIN[lapse[1]:lapse[2]])
      Temp_Avg          <- mean(baseClima$Temp_Avg[lapse[1]:lapse[2]])
      Diurnal_Range_Avg <- mean(baseClima$Diurnal_Range[lapse[1]:lapse[2]])
      Sol_Ener_Accu     <- sum(baseClima$ESOL[lapse[1]:lapse[2]])
      Temp_Max_34_Freq  <- sum(baseClima$TMAX[lapse[1]:lapse[2]] >= umTmx)/(lapse[2]-lapse[1]+1)
      Rel_Hum_Avg       <- mean(baseClima$RHUM[lapse[1]:lapse[2]])
      Rain_Accu         <- sum(baseClima$RAIN[lapse[1]:lapse[2]])
      Rain_10_Freq      <- sum(baseClima$RAIN[lapse[1]:lapse[2]]>=10)/(lapse[2]-lapse[1]+1)
      
      namVars <- c("Temp_Max","Temp_Min","Temp_Max_Avg","Temp_Min_Avg","Temp_Avg","Diurnal_Range_Avg","Sol_Ener_Accu","Temp_Max_34_Freq","Rel_Hum_Avg","Rain_Accu","Rain_10_Freq")
      climOutputs <- data.frame(Temp_Max,Temp_Min,Temp_Max_Avg,Temp_Min_Avg,Temp_Avg,Diurnal_Range_Avg,Sol_Ener_Accu,Temp_Max_34_Freq,Rel_Hum_Avg,Rain_Accu,Rain_10_Freq)
      
    }else{
      
      Temp_Max_Avg      <- mean(baseClima$TMAX[lapse[1]:lapse[2]])
      Temp_Min_Avg      <- mean(baseClima$TMIN[lapse[1]:lapse[2]])
      Temp_Avg          <- mean(baseClima$Temp_Avg[lapse[1]:lapse[2]])
      Diurnal_Range_Avg <- mean(baseClima$Diurnal_Range[lapse[1]:lapse[2]])
      Sol_Ener_Accu     <- sum(baseClima$ESOL[lapse[1]:lapse[2]])
      Temp_Max_XX_Freq  <- sum(baseClima$TMAX[lapse[1]:lapse[2]] >= umTmx)/(lapse[2]-lapse[1]+1)
      Rel_Hum_Avg       <- mean(baseClima$RHUM[lapse[1]:lapse[2]])
      Rain_Accu         <- sum(baseClima$RAIN[lapse[1]:lapse[2]])
      Rain_10_Freq      <- sum(baseClima$RAIN[lapse[1]:lapse[2]]>=10)/(lapse[2]-lapse[1]+1)
      
      namVars <- paste(c("Temp_Max_Avg","Temp_Min_Avg","Temp_Avg","Diurnal_Range_Avg","Sol_Ener_Accu",paste0("Temp_Max_",umTmx,"_Freq"),"Rel_Hum_Avg","Rain_Accu","Rain_10_Freq"),stage[i],sep="_")
      climOutputs <- data.frame(Temp_Max_Avg,Temp_Min_Avg,Temp_Avg,Diurnal_Range_Avg,Sol_Ener_Accu,Temp_Max_XX_Freq,Rel_Hum_Avg,Rain_Accu,Rain_10_Freq)
    }
    names(climOutputs) <- namVars
    climIndStage[[i]]  <- climOutputs
  }
  climIndStageF <- append(list(cropEvent),climIndStage)
  climIndRow    <- do.call(cbind,climIndStageF)
  return(climIndRow)
}  

#INDICADORES BASE

indicatorsComputing <- function(baseClima,lotes,TempBase){
  

    
    baseClima$Temp_Avg                        <- (baseClima$TMAX+baseClima$TMIN)/2
    baseClima$Diurnal_Range                   <-  baseClima$TMAX-baseClima$TMIN
    baseClima$Temp_Eff                        <-  baseClima$Temp_Avg-TempBase
    baseClima$Temp_Eff[baseClima$Temp_Avg <= baseClima$TempBase]  <-  0
    baseClima$Temp_Eff[which(baseClima$Temp_Avg >  28)] <-  51-(1.214*baseClima$Temp_Avg)[which(baseClima$Temp_Avg >  28)]
    

  dateLotes <- do.call(rbind,lapply(1:nrow(lotes),function(x){dateEvent(lotes[x,])}))
  
  ## Calcular indicadores
  
  
  lotesFinal <- do.call(rbind,lapply(1:nrow(dateLotes),function(x){climetIndicators(dateLotes[x,],baseClima)}))
  
  lotesFinal = lotesFinal[which(!is.na(lotesFinal$Temp_Avg)),]
  
  
  return(lotesFinal)
  
}
