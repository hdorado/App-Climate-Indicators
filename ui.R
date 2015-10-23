
shinyUI(
 fluidPage(
  
  fluidRow(
    
   column(10,
    titlePanel(strong("Generador de indicadores climáticos por etapa del cultivo, para arroz:")),
    br(),
   
    p("Esta aplicación permite al usuario generar en una sola base de datos
      indicadores climáticos en las etapas vegetativa, reproducción y llenado 
      de grano en eventos de cosecha de arroz, es fundamental tener en cuenta 
      cúal es la estructura de las bases de datos antes de comenzar el proceso,
      los encabezados deben coindir, para mayor información consulte estos archivos de ejemplo: ",a("datos climáticos",href="ftp://ftp.ciat.cgiar.org/DAPA/projects/BIGDATA_AEPS/TOY_SETS/estacionClimatica.csv"),"y",a("datos de cosecha",href="ftp://ftp.ciat.cgiar.org/DAPA/projects/BIGDATA_AEPS/TOY_SETS/eventosDeCosecha.csv")),

    h4(strong("Cargar bases de datos")),
    
    h6("Tenga en cuenta que los encabezados deben corresponder con las siguientes careacteríticas:",br(),br(),br(),"  a. Para
              los datos climáticos",em("DATE (Fecha)"),em("ESOL (Energía solar acumulada)"), em("RAIN (Precipitación)"), "y", em("RHUM (Humedad Relativa)"),
       br(),br(),"b. Los datos de cosecha:",em("Fecha de siembra (Sowing_Date)"),em("Fecha de cosecha(Harvest_Date)")," y ",em("Variedad (Variety)")),
    
    br(),br(),
    
    fluidRow(
      
      
    column(6,
     
    
     fileInput("baseClima",label = div("1. Seleccione el archivo con los datos climáticos:"),accept = c('text/csv',                                                                                                 
                                                                                                      'text/comma-separated-values',
                                                                                                      'text/tab-separated-values',
                                                                                                      'text/plain',
                                                                                                      '.csv',
                                                                                                      '.tsv')),
    
     tableOutput("datosClimaticos"),
     

     
     numericInput("temBase",label = "3. Seleccione la temperatura base eficaz",value = 11)
    ),
    
    column(6,
    

     fileInput("lotes",label = "2. Seleccione el archivo con los eventos de cosecha",accept = c('text/csv',
                                                                                             'text/comma-separated-values',
                                                                                             'text/tab-separated-values',
                                                                                             'text/plain',
                                                                                             '.csv',
                                                                                             '.tsv')),
     tableOutput("datosLotes"),
     
     selectInput("formatoFecha",label="4. Seleccione el formato de fecha utillizado",choices = c("MM/DD/YYYY","DD/MM/YYYY","MM/DD/YY","DD/MM/YY","YYYY-MM-DD"))
    )
    ),
    
  

    helpText(em("Nota: Estandarizar el mismo formato en todos los campos con fecha los cuales serían: fecha de siembra, fecha de cosecha y fecha en la base climática")),
    
    br(),
     p(strong("5. Una vez seleccionado el formato, dé click para ajustar a los datos"),align="center"),
 
    
    #actionButton("darFormatoFecha",label="Dar formato a fecha"),
    
    div(tags$button( "Dar formato a fechas", id="darFormatoFecha", type="button", class="btn action-button", onclick="self.close()",style="border: 1px solid rgb(128, 128, 128); background-color:white"),align = "center") ,
    
    
    br(),
   
    verbatimTextOutput("summaryClimate"),
  
    br(),
    
    plotOutput("plotClima"),
  
    strong("6. Generación de indicadores climáticos, por etapa del cultivo"),
    br(),
    br(),
    actionButton("generarIndicador",label="Comenzar"),
    textOutput(""),
  
    br(),
  
    verbatimTextOutput("summaryClimateIndicators"),
  
    strong("7. Almacenar los indicadores climáticos una vez hayan sido generados:"),
    br(),
    br(),
    #actionButton("dirSave",label="Seleccionar carpeta"),
   
    downloadButton('downloadData', 'Descargar'),
  
    br(),br(),br()

    #textOutput("avisoFinal"),
  
    
  
   )  
  )
 )
)
