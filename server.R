
#load(functionCode)
library(ggplot2)
library(tools)
source("funClim.R")



shinyServer(function(input,output){

  
  #Lectura de la base de clima
  baseClima <- reactive({

    valBaseClima <- input$baseClima

    
    baseFinal <- if (is.null(valBaseClima)){NULL}else{
      dPvalBaseClima <- valBaseClima$name    
      path <- substring(dPvalBaseClima,nchar(dPvalBaseClima)-2,nchar(dPvalBaseClima))
      
      while(is.null(valBaseClima)|path!="csv"){
        validate(need(!is.null(valBaseClima)& path=="csv" , "No se han cargado datos de clima en formato csv"))
      }   
      
      read.csv(valBaseClima$datapath)} 
     
     
     return(baseFinal)
  })
    
  #Lectura de la base de Lotes
  
  baseLotes <- reactive({
    
    valBaseLotes <- input$lotes
    
    
    baseFinal <- if (is.null(valBaseLotes)){NULL}else{
      dPvalBaseLotes <- valBaseLotes$name    
      path <- substring(dPvalBaseLotes,nchar(dPvalBaseLotes)-2,nchar(dPvalBaseLotes))
      
      while(is.null(valBaseLotes)|path!="csv"){
        validate(need(!is.null(valBaseLotes)& path=="csv" , "No se han cargado datos de lotes en formato csv"))
      }   
      
      read.csv(valBaseLotes$datapath)} 
   
    return(baseFinal)
  })
  
  baseFechaActual <- reactive({
    input$darFormatoFecha
    
      isolate({ 
  
      if(input$darFormatoFecha[1]==0){return(NULL)}
      else{
        withProgress(message = 'Cargando...', detail = "", value = 1, {
       
        formDate <- switch(input$formatoFecha,                     
                          "MM/DD/YYYY" = "%m/%d/%Y",
                          "DD/MM/YYYY" = "%d/%m/%Y",
                          "MM/DD/YY"   = "%m/%d/%y",
                          "DD/MM/YY"   = "%d/%m/%y",
                          "YYYY-MM-DD" = "%Y-%m-%d",
                          "%m/%d/%Y")
        
        nBaseClima  <- baseClima()
      
        nBaseClima$DATE <- as.Date(nBaseClima$DATE,formDate)
  
        nBaseLotes              <- baseLotes() 
        nBaseLotes$Sowing_Date  <- as.Date(nBaseLotes$Sowing_Date,formDate) 
        nBaseLotes$Harvest_Date <- as.Date(nBaseLotes$Harvest_Date,formDate) 
    
        outRange <- nBaseLotes$Harvest_Date>max(nBaseClima$DATE)|nBaseLotes$Sowing_Date<min(nBaseClima$DATE)
        
        if(sum(outRange)>0)
        {
          mesn <- "Hay lotes que estan por fuera del rango de clima, revisar antes:"
          
        }else{mesn <- "Todos los lotes están dentro del Rango de Clima"}
        
      return(list(nBaseClima,nBaseLotes,input$temBase,mesn))
        })
      }
    })
  })
  
  climateIndicators <- reactive({
    
    if(input$generarIndicador[1]==0){return(NULL)}
    
    else{
    
    isolate({
      withProgress(message = 'Cargando...', detail = "", value = 1, {
      cIE <- indicatorsComputing(baseFechaActual()[[1]],baseFechaActual()[[2]], baseFechaActual()[[3]])
      return(cIE)
      })
    })
    }
  })
  
  
  
#   dirSav <- reactive({
#     
#     
#     
#     if(input$dirSave[1]==0){return(NULL)}
#     
#     else{
#     
#     isolate({
#       
#       dirS <- choose.dir("")
#       write.csv(climateIndicators(),paste0(dirS,"\\EventosConIndicadoresClimaticos.csv"),row.names=F)
#       return(paste("Los idicadores han sido almacenados en:",dirS,"\\EventosConIndicadoresClimaticos.csv"))
#     })
#     }
#     })
  
  


  
  
  #Outputs---------
    
     output$summaryClimate <- renderPrint({

       if(is.null(baseFechaActual()))
         return(cat("No se ha actualizado el formato de la fecha"))
         #input$formatoFecha
        else{
         mesn <- baseFechaActual()[[4]] 
         a <- summary(baseFechaActual()[[1]]$DATE)
         b <- summary(baseFechaActual()[[2]]$Sowing_Date)
         c <- summary(baseFechaActual()[[2]]$Harvest_Date)
         
         myDateSum <- list(a,b,c)
         names(myDateSum) <-c("Station","Sowing","Harvest")
         
         cat(paste(mesn,"\n"));
         
         myDateSum 
         
       
        }
       
     })
  
    output$datosClimaticos <- renderTable({     
      if(is.null(baseClima()))
        return(NULL)
      baseClima()[1:5,]
    })
  
    output$datosLotes<- renderTable({     
      if(is.null(baseLotes()))
        return(NULL)
      baseLotes()[1:5,][,1:5]
  })
  
  output$plotClima <- renderPlot(  
    if(is.null(baseFechaActual()))
      return("NULL")    
    else{ggplotClima(baseFechaActual()[[1]])}
    
  )
  
  output$summaryClimateIndicators <- renderPrint({
    if(is.null(climateIndicators()))
      return(cat("No se han generado indicadores de clima"))
    else{
      
      cIE   <- climateIndicators()
      nCciE <- ncol(cIE)
      
      summary(cIE[(nCciE-38):nCciE])
     
    }
    
  })
  
#   output$avisoFinal <- renderText({
# 
#       
#       dirSav()
#    
#     
#   })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("EventosConIndicadoresClimaticos", '.csv', sep='') },
    content = function(file) {
      write.csv(climateIndicators(), file)
    }
  )
})


