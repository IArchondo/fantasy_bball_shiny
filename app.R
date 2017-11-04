a.paclist <- list("fmsb","ggplot2","scales","rvest","dplyr","XML","RCurl","lubridate","stringr","plyr")
lapply(a.paclist,require,character.only=T)

f.ftourl <- function(nombre,temporada){
  if (temporada == "2017-18"){temp <- "2018"} else {temp <- "2017"} 
  
  nombre <- gsub(pattern="\\.","",x=nombre)
  a.split <- strsplit(nombre," ")
  s.inicial <- tolower(substr(a.split[[1]][2],1,1))
  s.urlnom <- paste(substr(a.split[[1]][2],1,5),substr(a.split[[1]][1],1,2),"01",sep="") %>% tolower()
  s.url <- paste("https://www.basketball-reference.com/players",s.inicial,s.urlnom,"gamelog/",temp,"/#pgl_basic::none",sep="/")
  dfGL <- readHTMLTable(getURL(s.url))$pgl_basic
  print(s.url)
  dfGL=subset(dfGL,dfGL$Rk!="Rk")
  
  dfGL <- dfGL[,-c(1,4,6,8)]
  dfGL <- dfGL[,1:25]
  
  dfGL <- subset(dfGL,dfGL$BLK!="Did Not Dress")
  dfGL <- subset(dfGL,dfGL$BLK!="Not With Team")
  dfGL <- subset(dfGL,dfGL$BLK!="Inactive")
  dfGL <- subset(dfGL,dfGL$BLK!="Did Not Play")
  
  dfGL$Date <- as.Date(as.character(dfGL$Date),format="%Y-%m-%d")
  
  for (i in 7:25){
    dfGL[,i] <- as.numeric(as.character(dfGL[,i]))
  }
  
  dfGL$MP <- as.character(dfGL$MP)
  split <- strsplit(dfGL$MP,":")
  
  for (i in 1:nrow(dfGL)){
    dfGL[i,6] <- as.numeric(round(as.numeric(split[[i]][1])+as.numeric(split[[i]][2])/60,1))
  }
  
  dfGL$MP <- as.numeric(dfGL$MP)
  
  colnames(dfGL) <- gsub(pattern="3","T3",x=colnames(dfGL))
  dfGL$T2P <- dfGL$FG-dfGL$T3P
  dfGL$T2PA <- dfGL$FGA-dfGL$T3PA
  
  dfGL[,9] <- round(dfGL[,9],2)
  dfGL[,12] <- round(dfGL[,9],2)
  dfGL[,15] <- round(dfGL[,9],2)
  
  dfGL$FP=dfGL$AST*2+dfGL$ORB*1.5+dfGL$DRB+dfGL$BLK*2+dfGL$STL*2+dfGL$FT*1.5+
    dfGL$T2P*2.5+dfGL$T3P*3.5-dfGL$FTA*0.5-dfGL$T2PA*0.5-dfGL$T3PA*0.5-
    dfGL$TOV*2
  
  dfGL$FPPM <- round(dfGL$FP/dfGL$MP,2)
  
  
  df <- dfGL[0,6:29]
  
  
  for (i in 1:nrow(dfGL)){
    df[i,] <- colSums(dfGL[1:i,6:29])
    
  }
  
  df <- cbind(dfGL[,1:5],df)
  
  df$FP=df$AST*2+df$ORB*1.5+df$DRB+df$BLK*2+df$STL*2+df$FT*1.5+
    df$T2P*2.5+df$T3P*3.5-df$FTA*0.5-df$T2PA*0.5-df$T3PA*0.5-
    df$TOV*2
  df$FPPM <- df$FP/df$MP
  
  dfGL$FPPMmov <- round(df$FPPM,1)
  
  gg <- ggplot(dfGL,aes(x=Date,y=FPPM))+
    geom_line(size=0.6,color="#097EC3")+
    geom_smooth()+
    geom_line(aes(x=dfGL$Date,y=dfGL$FPPMmov),color="red")+
    geom_point()+
    geom_text(aes(label=ifelse(dfGL$FPPM>=quantile(dfGL$FPPM,0.95),as.character(dfGL$Opp),"")),hjust=-0.3)+
    scale_x_date(date_breaks = "1 week",date_labels="%d-%m")+
    theme(axis.text.y= element_text(face="bold",size=12),axis.text.x=element_text(angle=90, hjust=1),
          axis.title.x = element_blank())
  
  return(list(gg,dfGL))
}

limpiar <- function(df.tot) {
  df.tot$Rk <- NULL
  df.tot$X <- NULL
  df.tot$eFG. <- NULL
  df.tot$PF <- NULL
  df.tot$FPM <- NULL 
  df.tot[,"FG."]=df.tot[,"FG"]/df.tot[,"FGA"]
  df.tot$T3P.=df.tot$T3P/df.tot$T3PA
  df.tot$T2P.=df.tot$T2P/df.tot$T2PA
  df.tot$eFG.=(df.tot$FG+0.5*df.tot$T3P)/df.tot$FGA
  df.tot$FT.=df.tot$FT/df.tot$FTA
  df.tot$FP=df.tot$AST*2+df.tot$ORB*1.5+df.tot$DRB+df.tot$BLK*2+df.tot$STL*2+df.tot$FT*1.5+
    df.tot$T2P*2.5+df.tot$T3P*3.5-df.tot$FTA*0.5-df.tot$T2PA*0.5-df.tot$T3PA*0.5-
    df.tot$TOV*2
  df.tot$MPPG <- df.tot$MP/df.tot$G
  df.tot$FPPG <- df.tot$FP/df.tot$G
  df.tot$FPPM <- df.tot$FP/df.tot$MP
  return(df.tot)
}

##------------------------FUNCION que devuelve las urls de todos los box scores de un día-----------------------
f.geturlsbox <- function(i.month,i.day,i.year) {
  
  url <- paste("https://www.basketball-reference.com/boxscores/?month=",i.month,"&day=",i.day,"&year=",i.year,sep="")
  print(url)
  # url <- s.url
  html <- paste(readLines(url), collapse="\n")
  library(stringr)
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  
  a <- matched[[1]]
  
  ind <- grep("boxscores/2",a)
  
  a2 <- a[ind]
  
  indhref <- grep("<a",a2)
  
  a3 <- a2[-indhref]
  
  a.urls <- paste("https://www.basketball-reference.com",a3,sep="")
  return(a.urls)
}
##------------------------FUNCION------------------------------------------------------------------------------------

##------------------------FUNCION crea una lista con todos los boxscores de un array de urls--------------
f.listdfs <- function(a.urls){
  l.dfs_tot <- list()
  for (i in 1:length(a.urls)) {
    s.url <- a.urls[i]
    l.dfs <- readHTMLTable(getURL(s.url))
    l.dfs <- l.dfs[c(1,3)]
    l.dfs_tot <- c(l.dfs_tot,l.dfs)
    print(paste(i,"box score agregado",sep=" "))
  }
  return(l.dfs_tot)
}
##----------------------------------------------------------------------------------------------------------
##------------------------FUNCION que limpia los boxscores de una lista de boxscores----------------------------
f.limpbs <- function(l.bs){
  dfbs2.1 <- as.data.frame(l.bs)
  dfbs2.1 <- dfbs2.1[,1:20]
  colnames(dfbs2.1) <- c("Player","MP","FG","FGA","FG.","3P","3PA","3P.","FT","FTA","FT.","ORB","DRB","TRB","AST","STL",
                         "BLK","TOV","PF","PTS")
  dfbs2.1 <- subset(dfbs2.1,dfbs2.1$MP!="Did Not Play")
  dfbs2.1 <- subset(dfbs2.1,dfbs2.1$MP!="Did Not Dress")
  dfbs2.1 <- subset(dfbs2.1,dfbs2.1$MP!="Not With Team")
  dfbs2.1 <- subset(dfbs2.1,dfbs2.1$Player!="Reserves")
  
  
  
  for (i in 3:20){
    dfbs2.1[,i] <- as.numeric(as.character(dfbs2.1[,i]))
  }
  
  dfbs2.1$MP <- as.character(dfbs2.1$MP)
  split <- strsplit(dfbs2.1$MP,":")
  
  for (i in 1:nrow(dfbs2.1)){
    dfbs2.1[i,2] <- as.numeric(round(as.numeric(split[[i]][1])+as.numeric(split[[i]][2])/60,1))
  }
  
  dfbs2.1$MP <- as.numeric(dfbs2.1$MP)
  
  colnames(dfbs2.1) <- gsub(pattern="3","T3",x=colnames(dfbs2.1))
  
  dfbs2.1$T2P <- dfbs2.1$FG-dfbs2.1$T3P
  dfbs2.1$T2PA <- dfbs2.1$FGA-dfbs2.1$T3PA
  
  dfbs2.1$FP=dfbs2.1$AST*2+dfbs2.1$ORB*1.5+dfbs2.1$DRB+dfbs2.1$BLK*2+dfbs2.1$STL*2+dfbs2.1$FT*1.5+
    dfbs2.1$T2P*2.5+dfbs2.1$T3P*3.5-dfbs2.1$FTA*0.5-dfbs2.1$T2PA*0.5-dfbs2.1$T3PA*0.5-
    dfbs2.1$TOV*2
  
  dfbs2.1$FPPM <- dfbs2.1$FP/dfbs2.1$MP
  
  for (i in c(5,8,11,24)){
    dfbs2.1[,i] <- round(dfbs2.1[,i],2)
  }
  
  return(dfbs2.1)
}
##------------------------FUNCION----------------------------------------------------------------------------------
##------------------------FUNCION CON TODO------------------------------------------------------------------
f.urltobs <- function(a.month,a.day,a.year){
  withProgress(message = 'Descargando datos', value = 0,{
    incProgress(0.0,detail="Estableciendo partidos")
  a.urls <- f.geturlsbox(a.month,a.day,a.year)
  incProgress(0.25,detail="Descargando partidos")
  l.dfs <- f.listdfs(a.urls)
  incProgress(0.25,detail="Limpiando partidos")
  l.limp <- lapply(l.dfs,f.limpbs)
  incProgress(0.25,detail="Combinando partidos")
  df.unid <- rbind.fill(l.limp)
  df.unid <- df.unid[rev(order(df.unid$FPPM)),]
  df.unid <- subset(df.unid,df.unid$MP>4)
  return(df.unid)})
}



##----------------------------------------------------------------------------------------------------------



df.Tablatot_16 <- limpiar(read.csv2("Totalesact.csv"))

df.TablatotPM_16 <- cbind(df.Tablatot_16[,1:7],(df.Tablatot_16[,8:28]/df.Tablatot_16$MP))

df.Tablatot.pie_16 <- df.Tablatot_16[,c("Player","T3P","T2P","FT","ORB","DRB","AST","STL","BLK")]


###--------------------------------------------------------------------------------------------------------
###--------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel((div(img(src="logolet2.png")))),
  mainPanel(
    tabsetPanel(id="tabs1",
                tabPanel("Hoy",
                         fluidRow(column(12,br())),
                         fluidRow(column(5,h3("Destacados del día"))),
                         fluidRow(column(1,selectInput("i.selectinput.dia",h4("Día"),choices=as.character(c(1:31)),selected="1",width="70px")),
                                  column(1,selectInput("i.selectinput.mes",h4("Mes"),choices=as.character(c(1:12)),selected="1",width="70px")),
                                  column(2,selectInput("i.selectinput.ano",h4("Año"),choices=as.character(c(2016,2017)),selected="2017",width="90px")),
                                  column(1,br(),br(),actionButton("i.actionbutton.hoy"," Anoche",icon("calendar-check-o"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                  column(2,br(),br(),actionButton("i.actionbutton.descargarhoy"," Bajar Datos",icon("download"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                  column(2,selectInput("i.selectinput.ord_destacados",h4("Ordenar por"),choices=c("FPPM","Desv"),selected="FPPM"))),
                         fluidRow(column(12,h4("Generales"))),
                         fluidRow(column(12,tableOutput("o.tableoutput.destacados"))),
                         fluidRow(column(12,h4("Guards"))),
                         fluidRow(column(12,tableOutput("o.tableoutput.destacadosG"))),
                         fluidRow(column(12,h4("Forwards"))),
                         fluidRow(column(12,tableOutput("o.tableoutput.destacadosF"))),
                         fluidRow(column(12,h4("Centers"))),
                         fluidRow(column(12,tableOutput("o.tableoutput.destacadosC")))
                         ),
                tabPanel("Buscar jugador",
                         fluidRow(column(12,br())),
                         fluidRow(column(5,h3("Buscar jugador"))),
                         fluidRow(column(3,textInput("I.textinput.nombrejugador",h4("Nombre jugador:",value=""))),
                                  column(2,br(),br(),actionButton("i.actionbutton.bajardatos"," Bajar Day2Day",icon("download"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                  column(2,br(),selectInput("i.selectinput.tabgentemporada",h4(""),choices=c("2016-17","2017-18"),selected="2017-18",width="110px"))
                                  ),
                         fluidRow(column(12,tableOutput("O.tableoutput.jug_tabgen"))),
                         fluidRow(column(4,plotOutput("o.plotoutput.radar")),
                                  column(4,br(),br(),plotOutput("o.plotoutput.pie")),column(4,
                                  fluidRow(br()),
                                  fluidRow(br()),
                                  fluidRow(
                                  column(4,textOutput("o.textoutput.basicstatFPPM"),style="color:deepskyblue;font-size:110px;font-family:'Bauhaus 93'")),
                                  fluidRow(
                                    column(4,textOutput("o.textoutput.basicstatFPPG"),style="color:deepskyblue4;font-size:110px;font-family:'Bauhaus 93'")))),
                         
                         fluidRow(column(10,plotOutput("o.plotoutput.linea")),column(1,plotOutput("o.plotoutput.boxplotFPPM")),
                                  column(1,textOutput("o.textoutput.sd"))),
                         fluidRow(column(6,div(style='height:500px;width:1700px; overflow-y: scroll', dataTableOutput("o.tableoutput.day2day"))))
                         # fluidRow(column(12,br())),
                         
                         ),
                tabPanel("Tabla general",
                         fluidRow(column(12,br())),
                         fluidRow(column(12,h3(paste("Tabla completa",Sys.Date(),sep=": "),style="color:#006EC1"))),
                         fluidRow(column(3,selectInput("I.select.VarTabTot",h4("Ordenar por"),choices=list("FPPM",
                                                                                                          "FPPG","MPPG","AST","STL","BLK","TOV"),
                                                                               selected="FPPM")),
                                  column(3,selectInput("I.select.AscDesc",h4("Por orden"),choices=list("Descendente"=1,"Ascendente"=2),
                                                       selected=1)),
                                  column(3,checkboxGroupInput(inline=T,"I.checkgroup.Posiciones",h4("Posiciones"),
                                                              choices=list("Todos"=1,
                                                                           "G"=2,
                                                                           "F"=3,
                                                                           "C"=4), selected=1)),
                                  column(3,sliderInput("I.slider.MP",h4("Min minutos jugados"),min=0,max=1,value=0.05,step=0.05))),
                         fluidRow(column(12,br(),div(style='height:620px;width:1800px; overflow-y: scroll', tableOutput("do.TablaCompleta"))))),
                tabPanel("Lideres")
                
                
                )))

###-----------------------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------------------------
###------------------------------------------------------------------------------------------------------------------------

server <- function(input,output,session) {
  
  
  theURL=getURL("https://www.basketball-reference.com/leagues/NBA_2018_totals.html",.opts=list(ssl.verifypeer = FALSE))
  
  colclas=c("numeric","character","character","numeric","factor","integer","integer",
            "integer","integer","integer","numeric","integer","integer","numeric","integer",
            "integer","numeric","numeric","integer","integer","numeric","integer","integer",
            "integer","integer","integer","integer","integer","integer","integer")
  
  
  df.tot=data.frame(readHTMLTable(theURL,colClasses=colclas))
  names(df.tot)=gsub(x=names(df.tot),pattern="totals_stats.",replacement="")
  
  df.tot=subset(df.tot,df.tot$Rk!="Rk")
  df.tot=subset(df.tot,df.tot$Tm!="TOT")
  
  df.tot=df.tot %>% group_by(Rk,Player,Age) %>% summarise_all(
    funs(if(is.integer(.)) {sum(.)} else {paste(as.character(.),collapse=", ")} ))
  
  colnames(df.tot)= ifelse(grepl("3|2",colnames(df.tot)),paste("T",colnames(df.tot),sep=""),
                           colnames(df.tot))
  
  df.tot <- limpiar(df.tot)
  
  for (i in c(10,13,16,19,28,30,31,32)){
    df.tot[,i] <- round(df.tot[,i],2)
  }
  
  df.tot <- df.tot[,c(1:27,29,28,30:32)]
  
  df.tot$Pos <- as.factor(sapply(strsplit(df.tot$Pos,split=", ") , "[[", 1))
  df.tot$Tm <- as.factor(df.tot$Tm)
  df.tot$MP <- as.integer(df.tot$MP)
  df.tot$Age <- as.integer(df.tot$Age)
  
  df.Tablatot <- as.data.frame(df.tot)
  
  df.TablatotPM <- cbind(df.Tablatot[,1:7],(df.Tablatot[,8:28]/df.Tablatot$MP))
  
  df.Tablatot.pie <- df.Tablatot[,c("Player","T3P","T2P","FT","ORB","DRB","AST","STL","BLK")]
  
  
##--------------------------------BOTONES----------------------------------------------------------------------------  
  #Botón que triggerea(?) la descarga de datos day2day
  l.tabladay2day <- eventReactive(input$i.actionbutton.bajardatos,{
    data <- f.ftourl(input$I.textinput.nombrejugador,input$i.selectinput.tabgentemporada)
    return(data)
  })
  
  #Botón que trigerea la descarga de datos de anoche
  l.datosdia <- eventReactive(input$i.actionbutton.descargarhoy,{
    data <- f.urltobs(input$i.selectinput.mes,input$i.selectinput.dia,input$i.selectinput.ano)
    df.merge <- df.Tablatot[,c(1,3,32,4)]
    colnames(df.merge) <- c("Player","Pos","FPPM.temp","Tm")
    data <- merge(data,df.merge,by.x="Player",by.y="Player",sort=F)
    data$Desv <- data$FPPM-data$FPPM.temp
    data <- data[,c(1,27,25,2:22,28,23,24)]
    data$MP <- as.integer(round(data$MP,0))
    
    for (i in c(5,6,8,9,11,12,14:24)){
      data[,i] <- as.integer(data[,i])
    }
    
    data <- data[,c(1:7,8:22,25,26,27)]
    
    return(data)
  })
  
  #Botón que vuelve al día de hoy
  observeEvent(input$i.actionbutton.hoy,{
    hoy <- as.character(Sys.Date()-1)
    hoy_sp <- strsplit(hoy,"-")
    updateSelectInput(session,"i.selectinput.dia",selected=hoy_sp[[1]][3])
    updateSelectInput(session,"i.selectinput.mes",selected=hoy_sp[[1]][2])
    updateSelectInput(session,"i.selectinput.ano",selected=hoy_sp[[1]][1])
    
  }
  )
  
  ##------------------------TABLA COMPLETA-------------------------------------------------------------------------------
  output$do.TablaCompleta=renderTable({
    df.Tablatot_f <- subset(df.Tablatot,df.Tablatot$MP>=quantile(df.Tablatot$MP,input$I.slider.MP))
    yeah <<- input$I.checkgroup.Posiciones
     if(1 %in% input$I.checkgroup.Posiciones){
       df.Tablatot_f2 <- df.Tablatot_f
     }
    else{ 
      if(input$I.checkgroup.Posiciones==2){
       df.Tablatot_f2 <- df.Tablatot_f[grep("G",df.Tablatot_f$Pos),]}
    else{ 
      if (input$I.checkgroup.Posiciones==3){
        df.Tablatot_f2 <- df.Tablatot_f[grep("F",df.Tablatot_f$Pos),]}
      else{ 
        if (input$I.checkgroup.Posiciones==4){
          df.Tablatot_f2 <- df.Tablatot_f[grep("C",df.Tablatot_f$Pos),]}  
       }}}

    loc <- which(colnames(df.Tablatot_f2)==input$I.select.VarTabTot)
    
    
    
    if(input$I.select.AscDesc==1) {
    df.Tablatot_f2[rev(order(c(df.Tablatot_f2[,loc]))),]} else {
      df.Tablatot_f2[order(c(df.Tablatot_f2[,loc])),]}
    
    
  }, na="-",hover=T,striped=T)
  
  ##-----------------------------ANOCHE------------------------------------------------------------------------
       
  #destacados
  output$o.tableoutput.destacados <- renderTable({
    data <- l.datosdia()
    loc <- which(colnames(data)==input$i.selectinput.ord_destacados)
    data <- data[rev(order(c(data[,loc]))),]
    data[1:10,]
  })
  
  #destacadosG
  output$o.tableoutput.destacadosG <- renderTable({
    data <- l.datosdia()
    data <- data[grep("G",data$Pos),]
    loc <- which(colnames(data)==input$i.selectinput.ord_destacados)
    data <- data[rev(order(c(data[,loc]))),]
    data[1:5,]
  })
  
  #destacadosF
  output$o.tableoutput.destacadosF <- renderTable({
    data <- l.datosdia()
    data <- data[grep("F",data$Pos),]
    loc <- which(colnames(data)==input$i.selectinput.ord_destacados)
    data <- data[rev(order(c(data[,loc]))),]
    data[1:5,]
    
  })
  
  #destacadosC
  output$o.tableoutput.destacadosC <- renderTable({
    data <- l.datosdia()
    data <- data[grep("C",data$Pos),]
    loc <- which(colnames(data)==input$i.selectinput.ord_destacados)
    data <- data[rev(order(c(data[,loc]))),]
    data[1:5,]
    
  })
  
  
  ##----------------------------BUSCAR JUGADOR-------------------------------------------------------------------
  #Datos generales
  output$O.tableoutput.jug_tabgen=renderTable({
    # df.datgen <- ifelse(input$i.selectinput.tabgentemporada=="2017-18",df.Tablatot,df.Tablatot_16)
    if (input$i.selectinput.tabgentemporada=="2017-18"){
      df.datgen <- df.Tablatot
    } else{df.datgen <- df.Tablatot_16}
    
    if(input$I.textinput.nombrejugador==""){} else{
    subset(df.datgen,df.datgen$Player==input$I.textinput.nombrejugador)}
    
  })
  

  
  
          #RADAR----------------
  output$o.plotoutput.radar <- renderPlot({
    if (input$i.selectinput.tabgentemporada=="2017-18"){
      df.datrad <- df.TablatotPM
      df.datgen <- df.Tablatot
    } else{
    df.datrad <- df.TablatotPM_16
    df.datgen <- df.Tablatot_16
    }
    
    if(input$I.textinput.nombrejugador==""){} else{
      df.TablatotPM_red <- subset(df.datrad,df.datrad$MP>=quantile(df.datgen$MP,0.05))
      df.jug <- subset(df.datgen,df.datgen$Player==input$I.textinput.nombrejugador)
      df.jug2 <- df.jug[,8:28]/df.jug$MP
      df.jug2[2,] <- apply(df.TablatotPM_red[,8:28],2,max)
      df.jug2[3,]<- apply(df.TablatotPM_red[,8:28],2,min)
      df.jug2 <- df.jug2[c(2,3,1),]
      df.jug3 <- df.jug2[,-c(3,6,9,12)]

      # radarchart(df.jug3,pfcol = rgb(0/255,43/255,92/255,0.5),
      #            pcol=rgb(180/255,151/255,90/255,1),plwd=2)          0,191,255
      
      radarchart(df.jug3,pfcol = rgb(0/255,104/255,139/255,0.5),
                 pcol=rgb(0/255,191/255,255/255,0.5),plwd=2)
    }
  },width=400,height=400)
  
  
          #PIE----------------------------
  output$o.plotoutput.pie <- renderPlot({
    
    
    if (input$i.selectinput.tabgentemporada=="2017-18"){
      df.datpie <- df.Tablatot.pie
    } else{df.datpie <- df.Tablatot.pie_16}
    
    if(input$I.textinput.nombrejugador==""){} else {
        df.Tablatot.CP <- subset(df.datpie,df.datpie$Player==input$I.textinput.nombrejugador)
        
        t <- as.data.frame(t(df.Tablatot.CP))
        t <- as.data.frame(t[2:9,])
        
        t[,1] <- as.numeric(as.character(t[,1]))
        t[,2] <- rownames(t)
        t[,3] <- c(3,2,1,1.5,1,2,2,2)
        t[,4] <- t[,1]*t[,3]
        t[,5] <- round((t[,4]/sum(t[,4])),4)*100
        t[,6] <- as.factor(c(1,1,1,2,2,1,2,2))
        t[,7] <- as.factor(c(4,3,2,4,3,1,2,1))
        
        ggplot(t,aes(x=t[,6],y=t[,7],fill=t[,5]))+
          geom_tile()+
          geom_text(size=6,aes(fontface="bold",label=paste(t[,2],": ",t[,5],"%",sep="")),color="white")+
          scale_fill_continuous(low="deepskyblue",high="deepskyblue4",limits=c(0,50))+
          theme(axis.ticks = element_blank(),axis.title = element_blank(),axis.text = element_blank(),
                panel.background = element_rect(fill="white",color="white"),legend.title = element_blank(),
                legend.position = "none")
        }
      },width=325,height=325)
  
  
  
     #BASICSTATS
  output$o.textoutput.basicstatFPPM <- renderText({
    if (input$i.selectinput.tabgentemporada=="2017-18"){
      df.datgen <- df.Tablatot
    } else{df.datgen <- df.Tablatot_16}
    
    if(input$I.textinput.nombrejugador==""){} else{
      round(subset(df.datgen,df.datgen$Player==input$I.textinput.nombrejugador)[,32] ,2)
      }
  })
  
  output$o.textoutput.basicstatFPPG <- renderText({
    if (input$i.selectinput.tabgentemporada=="2017-18"){
      df.datgen <- df.Tablatot
    } else{df.datgen <- df.Tablatot_16}
    
    if(input$I.textinput.nombrejugador==""){} else{
      round(subset(df.datgen,df.datgen$Player==input$I.textinput.nombrejugador)[,31] ,2)
      
    }
  })
  
  ##-----------------#Linea
  output$o.plotoutput.linea <- renderPlot({
    if(input$I.textinput.nombrejugador==""){} else{
      
      
      # f.ftourl(input$I.textinput.nombrejugador)
      l.tabladay2day()[[1]]
      
    }
  },width=1000,height=250)
  
  output$o.plotoutput.boxplotFPPM <- renderPlot({
    if(input$I.textinput.nombrejugador==""){} else{
      
      df.FPPM <- l.tabladay2day()[[2]]
      ggplot(data=df.FPPM,aes(x=1,y=df.FPPM$FPPM))+
        geom_boxplot()+
        geom_label(aes(y=mean(df.FPPM$FPPM),label=paste("sd:",round(sd(df.FPPM$FPPM),2),sep=" "),color="red"))+
        theme(axis.text.y= element_text(face="bold",size=12),axis.title.y = element_blank(),
              axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),legend.position = "none")
      
    }
  },width=200,height=250)
  
  
  
  output$o.tableoutput.day2day <- renderDataTable({
    df_x <- l.tabladay2day()[[2]][,-c(3,23,25)]
    df_x[,2] <- format(df_x[,2],format="%d-%m")
    df_x
  })
  
  
}

shinyApp(ui = ui, server = server)