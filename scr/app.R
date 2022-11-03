#
###############################
####  RCCC-WBM synthesis   ####
###############################
library(ggplot2)
library(shinythemes)
library(shiny)
library(formattable)
library(DT)
SIMULATE =function(year,month,P,Er,Discharge,Tem,Ks,Kg,Ksn,Smax,S0,SN0,A,unit,Null=-99){
    n = length(year)   #length of data sequence
    Pr = NULL  #rainfall /mm
    Psn = NULL  #snow-melt /mm
    # for (i in 1:n) {
    #   if (Tem[i] <= 3 & Tem[i] >= -3) {
    #     Psn[i] = (3-Tem[i])/6*P[i] 
    #     Pr[i] = P[i]-Psn[i]        
    #   } else if( Tem[i] < -3) {
    #     Psn[i] = P[i]
    #     Pr[i] = 0
    #   }
    #   else {
    #     Psn[i] = 0
    #     Pr[i] = P[i]
    #   }
    # }
    Qs = NULL   #surface flow
    Qg = NULL   #groundwater discharge
    Qsn = NULL  #snow-melt driven runoff
    Q = NULL    #total runoff
    S = NULL    #soil moisture
    
    SN = NULL   #snow accumulation
    E = NULL    #actual evaporation
    
    for (i in 1:n) {
        if (i == 1) {
            S_p = S0   #S_p antecedent soil moisture
            SN_p = SN0  #SN_p antecedent snow accumulation
            #Qsn_p = 0.5
        }
        else {
            S_p = S[i-1]
            SN_p = SN[i-1]
            #Qsn_p = Qsn[i-1]
        }
        
        if (Tem[i] <= -3) {
            Pr[i] = 0
            Psn[i] = P[i]
            Qs[i] = 0
            Qsn[i] = 0
            Qg[i] = Kg*S_p
            
            E[i] = 0.2*Psn[i] #20% precipitated snow for Snow evaporation
            SN[i] = SN_p + 0.8*Psn[i] #80% snow accumulated
            S[i] = S_p - Qg[i]
            #*****soil moisture correction*****
            if (S[i] <= 0.08*Smax) {
                Qg[i] = Qg[i] + (S_p - 0.08*Smax)*Qg[i]/(Qg[i]+Er[i]+0.001)
                E[i] = E[i] + (S_p - 0.08*Smax)*(Er[i]+0.001)/(Qg[i]+Er[i]+0.001)
                S[i] = 0.08*Smax
            }
            if (S[i] >= Smax) {
                Qg[i] = Qg[i] + S_p - Smax
                S[i] = Smax
            }
            
        } else if (Tem[i] > -3 & Tem[i] <= 3) {
            Pr[i] = P[i]*(Tem[i]+3)/6
            Psn[i] = P[i]*(3-Tem[i])/6
            SN[i] = SN_p+Psn[i]
            Qsn[i] = Ksn*SN[i]*exp((Tem[i]-3)/6)
            SN[i] = SN[i] - Qsn[i]
            
            #*******snow storage correction****
            if (SN[i] < 0 ) {
                Qsn[i] = SN_p+Psn[i]
                SN[i] = 0
            }
            Qg[i] = Kg*S_p
            Qs[i] = Ks*Pr[i]*S_p/Smax
            
            S[i] = S_p-Qg[i]+Pr[i]-Qs[i]
            E[i] = Er[i]*S[i]/Smax
            S[i] = S[i]-E[i]
            total = Qs[i] + Qg[i] + Er[i]+0.01
            #*****soil moisture correction*****
            if (S[i] > Smax) {
                Qs[i] = Qs[i] + (S[i] - Smax)*Qs[i]/total
                Qg[i] = Qg[i] + (S[i] - Smax)*Qg[i]/total
                E[i] = E[i] + (S[i] - Smax)*(E[i]+0.01)/total
                S[i] = Smax
            }
            if (S[i] <= 0.08*Smax) {
                Qs[i] = Qs[i] + (S[i] - 0.08*Smax)*Qs[i]/total
                Qg[i] = Qg[i] + (S[i] - 0.08*Smax)*Qg[i]/total
                E[i] = E[i] + (S[i] - 0.08*Smax)*(E[i]+0.01)/total
                S[i] = 0.08*Smax
            }
            
        } else { #Tem[i] > 3
            Pr[i] = P[i]
            Psn[i] = 0
            if (SN_p > 0) {
                Qsn[i] = 0.5*Ksn*SN_p
                Pr[i] = Pr[i] + SN_p - Qsn[i]
                SN[i] = 0
                
            } else {
                Qsn[i] = 0
                SN[i] = 0
                Pr[i] = Pr[i] + SN_p
            }
            Qs[i] = Ks*Pr[i]*S_p/Smax
            Qg[i] = Kg*S_p
            E[i] = Er[i]*S_p/Smax
            S[i] = S_p+Pr[i]-Qs[i]-Qg[i]-E[i]
            total = Qs[i] + Qg[i] +Er[i]+0.01
            #*****soil moisture correction*****
            if (S[i] > Smax) {
                Qs[i] = Qs[i] + (S[i] - Smax)*Qs[i]/total
                Qg[i] = Qg[i] + (S[i] - Smax)*Qg[i]/total
                E[i] = E[i] + (S[i] - Smax)*(E[i]+0.01)/total
                S[i] = Smax
            }
            if (S[i] <= 0.08*Smax) {
                Qs[i] = Qs[i] + (S[i] - 0.08*Smax)*Qs[i]/total
                Qg[i] = Qg[i] + (S[i] - 0.08*Smax)*Qg[i]/total
                E[i] = E[i] + (S[i] - 0.08*Smax)*(E[i]+0.01)/total
                S[i] = 0.08*Smax
            }
        }
        
        Q[i] = Qs[i] + Qg[i] + Qsn[i]  #adjusted total runoff
    }
    R_simulated = Q   #unit:mm
    Discharge[Discharge == Null] = NA
    if (!unit) {
        R_recorded = Discharge*3600*24/A/(10^6)*1000   #unit:mm
    } else {
        R_recorded = Discharge*30*24*3.6/A
    }
    R_recorded[is.na(Discharge)] = R_simulated[is.na(Discharge)]
    SResults = data.frame(year,month,round(Pr,digits = 1),round(Psn,digits = 1),round(E,digits =1),round(R_simulated,digits = 2),round(R_recorded,digits =2),round(S,digits = 1),round(SN,digits = 1))
    Simulation_Result = `colnames<-`(SResults,c("year","month","Pr","Psnow","artual E","R_simulated","R_recorded","Soilmoisture","Accumulatedsnow"))
    return(Simulation_Result)
}

ui <- navbarPage("RCCC-WBM",
                 
                 # ---------------Introduction------------------
                 tabPanel("RCCC-WBM Introduction",
                          shinythemes::themeSelector(),
                          fluidRow(column(12,
                                          wellPanel(
                                              h3("RCCC-WBM Description",align = "center")       
                                          ))),
                          fluidRow(column(12,
                                          wellPanel(
                                              p("The RCCC-WBM is a simplified hydrological model, developed by the Research Center for Climate Change , Ministry of Water Resources of China. The model estimates daily or monthly stream flow from precipitation, temperature, and pan evaporation. Its advantages include a simpler structure, fewer parameters and more flexibility. The model has been successfully applied to over 200 typical catchments in China."),
                                              br(),
                                              p("Developers:Quoqing Wang (gqwang@nhri.cn)",align = "right"),
                                              p("Xiaoxiang Guan (xxguan@hhu.edu.cn)",align = "right")
                                              
                                          ))),
                          p(em("Address:Nanjing Hydraulic Research Institute,223,Guangzhou Road,NanJing210029"),align = "center")
                 ),
                 tabPanel("Basin information",
                          textInput("Basinname","Basin name | ID:",
                                    value = ""),
                          numericInput("A",
                                       "Basin area (A)/square kilometer:",
                                       step = 50,
                                       value = 10315)),
                 #----------Data Input------------------
                 tabPanel("Data input",
                          fluidRow(
                              column(3,
                                     wellPanel(
                                         radioButtons("Sequencetype", "Sequence type",
                                                      choices = c(
                                                          monthly = "monthly",
                                                          daily = "daily"),
                                                      selected = "monthly"),
                                         selectInput("Null", 
                                                     "Null value:",
                                                     choices = c(-99.9,-99,NA)
                                                     ,selected = -99),
                                         tags$hr(),
                                         checkboxInput("header", "Header", FALSE),
                                         checkboxInput("unit", "Unit of monthly Q:m3/s", TRUE),
                                         radioButtons("sep", "Separator",
                                                      choices = c(Comma = ",",
                                                                  Semicolon = ";",
                                                                  Tab = "\t"),
                                                      selected = ","),
                                         radioButtons("quote", "Quote",
                                                      choices = c(None = "",
                                                                  "Double Quote" = '"',
                                                                  "Single Quote" = "'"),
                                                      selected = "")
                                         
                                         # radioButtons("disp", "Display",
                                         #              choices = c(Head = "head",
                                         #                          All = "all"),
                                         #              selected = "head")
                                         
                                     )),
                              column(9, wellPanel(
                                  fluidRow(
                                      column(6,wellPanel(
                                          fileInput("Dataset", "Choose dataset File",
                                                    multiple = TRUE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv"))
                                      )),
                                      column(6, 
                                             wellPanel(h5("Data sequence range:",align = "left"),
                                                       tableOutput("range"))
                                             )
                                  ),
                                  
                                  fluidRow(
                                      column(12,wellPanel(
                                          DT::dataTableOutput("dataset_table")
                                      ))
                                  )
                              ))
                          )
                          
                 )
                 
                 
                 ,
                 #-------------Simulation Panel-----------
                 tabPanel("Simulation",
                          fluidRow(
                              column(3,wellPanel(
                                  
                                  sliderInput("period", "Select baseline period", min = 1940, 
                                              max = 2020, value = c(1955, 1965),step = 1),
                                  numericInput("SN0",
                                               "Preliminary snow (SN0)/mm:",
                                               min = 0,
                                               value = 2,
                                               step = 1),
                                  numericInput("S0",
                                               "Preliminary soil moisture (S0)/mm:",
                                               min = 0,
                                               step = 2,
                                               value = 38),
                                  hr(),
                                  numericInput("Smax",
                                               "Maximum soil moisture storage (Smax)/mm:",
                                               value = 136,step = 0.5),
                                  numericInput("Ks",
                                               "Surface flow coefficient (KS):",
                                               min = 0,
                                               max = 1,
                                               step = 0.001,
                                               value = 0.4),
                                  numericInput("Kg",
                                               "Groundwater flow lag coefficient (KG):",
                                               min = 0,
                                               max = 1,
                                               step = 0.001,
                                               value = 0.12),
                                  numericInput("Ksn",
                                               "Snow-melt driven runoff coefficient (Ksn):",
                                               min = 0,
                                               max = 3,
                                               step = 0.0001,
                                               value = 0.04),
                                  h3("Results",align = "center"),
                                  selectInput("dataset", 
                                              "Choose a dataset:",
                                              choices = c("Simulation Result", "Simulation accuracy","Quarterly accuracy", "Parameter set")),
                                  
                                  downloadButton("downloadData","Download simulation results")
                              )),
                              column(9,
                                     wellPanel(
                                         fluidRow(
                                             column(7,wellPanel(h4("Comparison of simulated and recorded monthly discharge"),
                                                                plotOutput("distPlot",height = "300px"),
                                                                downloadButton("simulation_down","Download image"))),
                                             column(5,wellPanel(h4("Time series of SumKi"),
                                                                plotOutput("SumKi",height = "300px"),
                                                                downloadButton("Sumki_down","Download image")))
                                         ),
                                         fluidRow(
                                             column(6,wellPanel(
                                                 h3("Water Balance Validation"),
                                                 tableOutput("WBE")
                                             )),
                                             column(6,wellPanel(
                                                 h3("Simulation accuracy"),
                                                 tableOutput("SR")
                                             ))
                                         ),
                                         fluidRow(
                                             column(6,wellPanel(
                                                 h3("Parameters"),
                                                 tableOutput("Pa")
                                             )),
                                             column(6,wellPanel(
                                                 h3("Quarterly Accuracy"),
                                                 tableOutput("QA")
                                             ))
                                         )
                                     ))
                              
                          )
                          # fluidRow(
                          #     column(3,wellPanel(
                          #         h3("Results",align = "center"),
                          #         selectInput("dataset", 
                          #                     "Choose a dataset:",
                          #                     choices = c("Simulation Result", "Simulation accuracy","Quarterly accuracy", "Parameter set")),
                          #         
                          #         downloadButton("downloadData","Download simulation results")
                          #     )),
                          #     column(9,wellPanel(
                          #         
                          #     ))
                          # )
                          
                          
                 ),
                 #-----Quantitatively attribution-----
                 tabPanel("Quantitative attribution",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      sliderInput("period2", "Select change period", min = 1940, 
                                                  max = 2020, value = c(1970, 1990),step = 1),
                                      downloadButton("Relationship_down","Download this image"),
                                      hr(),
                                      downloadButton("analysis_down","Download simulation results in change period")
                                      
                                  ),
                                  mainPanel(h4("Relationship between annual precipitation and runoff and air temperature:"),
                                            plotOutput("Relationship")
                                  )
                              )
                          ),
                          
                          tableOutput("quantitativeimpacts")
                          
                 ),
                 #--------Sensitivity Analysis---------------
                 tabPanel("Sensitivity Analysis",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(h3("Set Scenario"),
                                               #sliderInput("period_scenario", "Set period for this scenario:", min = 1940, 
                                               #               max = 2020, value = c(1955, 1975),step = 1),
                                               #  h5(em("Pay attention to the entire data series range!")),
                                               # hr(),
                                               sliderInput("Tin","Increment of Temperature(Celsius):",
                                                           min = -3, max = 3, value = 0, step = 0.05, animate = T),
                                               sliderInput("Pin","Increment of Precipitation(%):",
                                                           min = -40, max = 40, value = 0, step = 5, animate = T),
                                               h4("Relationship between Temperature and Evapotranspiration"),
                                               plotOutput("E_Tem"),
                                               downloadButton("E_Tem_down","Download this image")
                                  ),
                                  mainPanel(h3("Result",align = "center"),
                                            h4("Response of mean annual runoff"),
                                            tableOutput("scenario1"),
                                            h4("Response of mean quarterly runoff"),
                                            tableOutput("scenario2"),
                                            hr(),
                                            h4("Response of mean monthly runoff"),
                                            plotOutput("mongthlyrunoffplot")
                                  )
                              )
                          )
                          
                 ),
                 #-------Sensivitiy analysis of parameters------------------
                 tabPanel("Parameter analysis",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      h3("Choose Scenario"),
                                      sliderInput("period_scenario2", "Set period for this scenario:", min = 1940, 
                                                  max = 2020, value = c(1955, 1965),step = 1),
                                      downloadButton("Parameter_analysis_dataset","Download result dataset"),
                                      hr(),
                                      downloadButton("Parameter_analysis_down","Download the image")
                                  ),
                                  mainPanel(
                                      fluidRow(
                                          column(6,
                                                 h4("NSE: Nash coeffcients of simulation"),
                                                 plotOutput("Parameter_analysis_plot1",height = "300px")
                                          ),
                                          column(6,
                                                 h4("Bias between recorded and simulated runoff:"),
                                                 plotOutput("Parameter_analysis_plot2",height = "300px")
                                          )
                                      )
                                      
                                  )
                              )
                          )
                 )
                 
)

#------Define server logic required to draw a histogram--------------------
server <- function(input, output) {
    #--------------data deplay------------------
    Dataset <- reactive({
        req(input$Dataset)
        df <- read.csv(input$Dataset$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        df
    })
    
    output$range <- renderTable({ 
        req(input$Dataset)
        df1 = Dataset()
        len = length(df1[,1])
        
        qi = paste(df1[1, 1],"-",df1[1, 2],sep = "")
        zhi = paste(df1[len, 1],"-",df1[len, 2],sep = "")
        `colnames<-`(data.frame(qi,zhi), c("From","To"))
    })
    
    
    output$dataset_table <- DT::renderDataTable({   #upload data
        req(input$Dataset)
        df1 = Dataset()
        if (input$Sequencetype == "monthly") {
            df1 = `colnames<-`(df1,c("y","m","P", "E", "Tem", "Qobs"))
        } else {
            df1 = `colnames<-`(df1,c("y","m","d","P", "E", "Tem", "Qobs"))
        }
        #df1 = `colnames<-`(df1,c("y","m","P", "E", "Tem", "Qobs"))
        DT::datatable(df1)
        
    })
    
    #--------------monthly dataset raw--------------------------
    datasetraw <- reactive({
        req(input$Dataset)
        dat = Dataset()
        
        if (input$Sequencetype == "monthly") {
            year = dat[,1]
            month = dat[,2]
            P <- dat[,3]
            Er = dat[,4]
            Discharge = dat[,6]
            Tem = dat[,5]
        } else {
            #***********daily data rearrange****************
            pre = aggregate(P~y+m, data = dat, FUN = sum)
            pre = pre[order(pre$y, pre$m),]
            year = pre[,1]
            month = pre[,2]
            P <- pre[,3]
            
            EE = aggregate(E ~y+m, data = dat, FUN = sum)
            EE = EE[order(EE$y, EE$m),]
            E = EE[,3]
            
            TT = aggregate(Tem~y+m, data = dat, FUN = mean)
            TT = TT[order(TT$y, TT$m),]
            Tem = TT[,3]
            QQ = aggregate(Qobs ~ y+m, data = dat, FUN = mean)
            QQ = QQ[order(QQ$y, QQ$m),]
            Discharge = QQ[,3]
        }
        
        dataset = data.frame(year, month, 
                             P, Er, Tem, Discharge)
        `colnames<-`(dataset,
                     c("year","month","P","Er","Tem","Discharge"))
    })
    data <- reactive({
        req(input$Dataset)
        dat = datasetraw()
        #----runoff generation calculation------
        Ks = input$Ks
        Kg = input$Kg
        Ksn = input$Ksn
        Smax = input$Smax
        S0 = input$S0
        SN0 = input$SN0
        A = input$A
        unit = input$unit
        Null = input$Null
        
        a = input$period[1]
        b = input$period[2]
        logic = dat[,1] >= a & dat[,1] <=b
        year = dat[logic,1]
        month = dat[logic,2]
        P = dat[logic,3]
        Er = dat[logic,4]
        Tem = dat[logic,5]
        Discharge = dat[logic,6]
        
        SIMULATE(year, month, P, Er, Discharge, Tem,
                 Ks, Kg, Ksn, Smax, S0, SN0, A, unit, Null)
        
    })  #simulation for baseline period
    
    output$distPlot <- renderPlot({
        req(input$Dataset)
        data = data()    #attach reactive data #simulation for baseline period
        plot(data$R_simulated,type = "l",lty = 2,lwd = 2
             ,tck = 0.01,xlab = paste(input$period[1],"-",input$period[2])
             ,ylim = c(0,max(data$R_recorded)+5),ylab = "Discharge(mm)",col = "blue")
        points(data$R_recorded,type = "l",lty = 1,col = "black")
        legend("topleft",c("simulated","recorded"),lty = c(2,1),col = c("blue","black"),bty="n")
    })
    output$SumKi <- renderPlot({
        req(input$Dataset)
        dataset = datasetraw()
        Ks = input$Ks
        Kg = input$Kg
        Ksn = input$Ksn
        Smax = input$Smax
        S0 = input$S0
        SN0 = input$SN0
        A = input$A
        unit = input$unit
        Null = input$Null
        result = SIMULATE(dataset$year,dataset$month,dataset$P,
                          dataset$Er,dataset$Discharge,dataset$Tem,
                          Ks,Kg,Ksn,Smax,S0,SN0,A,unit,Null)
        
        qi = dataset[1,1]
        zhi = dataset[,1][length(dataset[,1])]
        t_year = (qi:zhi)
        nyear = length(t_year)
        Qsim = NULL
        Qre = NULL
        for (i in 1:nyear){
            Qsim[i] = sum(result$R_simulated[result[,1] == t_year[i]],na.rm = T)
            Qre[i] = sum(result$R_recorded[result[,1] == t_year[i]],na.rm = T)
        }
        sig = function(x,y){
            if(x > y) {return(1)}
            else if (x == y) {return(0)}
            else {return(-1)}
        }
        K = NULL
        for (i in 1:nyear){
            K[i] = sig(Qsim[i],Qre[i])
        }
        
        SumKi = NULL
        SumKi[1] = K[1]
        for (i in 2:nyear) {
            SumKi[i] = SumKi[i-1]+K[i]
        }
        graph = data.frame(t_year,SumKi)
        graph = `colnames<-`(graph,c("year","SumKi"))
        ggplot(data = graph,aes(x= year,y = SumKi))+
            geom_point()+ylab("SumKm")
    })
    output$Pa <- renderTable({  
        req(input$Dataset)
        Pa = data.frame(c("Ks","Kg","Ksn","Smax"),c(input$Ks,input$Kg,input$Ksn,input$Smax),
                        c("Surface flow coefficient","Groundwater flow lag coefficient",
                          "Snow-melt driven runoff coefficient","Maximum soil moisture storage(mm)"))
        `colnames<-`(Pa,c("Parameters","Value","Explanation"))
        
    }) #parameters
    output$QA <- renderTable({
        req(input$Dataset)
        data = data() #simulation results for baseline period
        name = c("Quater","Mean annual simulated runoff(mm)","Mean annual recorded runoff(mm)","Relative error(%)")
        si = NULL
        re = NULL
        ny = data[,1][length(data[,1])] - data[,1][1] + 1
        si[1] = sum(data[,6][data[,2] == 12|data[,2] == 1|data[,2] == 2],na.rm = T)/ny
        re[1] = sum(data[,7][data[,2] == 12|data[,2] == 1|data[,2] == 2],na.rm = T)/ny
        si[2] = sum(data[,6][data[,2] == 3|data[,2] == 4|data[,2] == 5],na.rm = T)/ny
        re[2] = sum(data[,7][data[,2] == 3|data[,2] == 4|data[,2] == 5],na.rm = T)/ny
        si[3] = sum(data[,6][data[,2] == 6|data[,2] == 7|data[,2] == 8],na.rm = T)/ny
        re[3] = sum(data[,7][data[,2] == 6|data[,2] == 7|data[,2] == 8],na.rm = T)/ny
        si[4] = sum(data[,6][data[,2] == 9|data[,2] == 10|data[,2] == 11],na.rm = T)/ny
        re[4] = sum(data[,7][data[,2] == 9|data[,2] == 10|data[,2] == 11],na.rm = T)/ny
        Re = round((si - re) / re * 100, digits = 2)
        si = round(si, digits = 1)
        re = round(re, digits = 1)
        QA = data.frame(c("12~2", "3~5", "6~8", "9~11"), si, re, Re)
        `colnames<-`(QA, name)
    })
    output$SR <- renderTable({
        req(input$Dataset)
        data = data()   #attach reactive data #simulation results for baseline period
        
        R2 = round(1 - sum((data$R_recorded - data$R_simulated)^2) / sum((data$R_recorded - mean(data$R_recorded))^2), digits = 3)
        Re = round( sum(data$R_simulated - data$R_recorded) / sum(data$R_recorded)*100, digits = 3)
        
        Result = data.frame(c("NSE","Bias"),c(R2, Re),c("Nash-Sutcliffe efficiency criterion","Relative error of volumetric fit_%"))
        `colnames<-`(Result,c("Metrics","Value","Description"))
    })  #simulation evaluation metrics
    
    output$WBE <- renderTable({
        req(input$Dataset)
        data = data()   #attach reactive data  #simulation results for baseline period
        Pt = sum(na.rm = T, data[,3]) + sum(na.rm = T, data[,4])
        Et = sum(na.rm = T, data[,5])
        Rst = sum(na.rm = T, data[,6])
        Rrt = sum(na.rm = T, data[,7])
        PEt = Pt-Et
        delta = data[,8][length(data[,8])] - data[,8][1] + data[,9][length(data[,9])] - data[,9][1]
        Re = (Rst+delta- PEt)/PEt*100
        WBE = data.frame(c(PEt),c(Rst),c(Rrt),c(Re))
        `colnames<-`(WBE,c("Total PE(mm)","Total simulated runoff(mm)","Total recorded runoff(mm)","Relative error(%)"))
    })
    
    datasetOutput <- reactive({
        req(input$Dataset)
        data = data()  #simulation results for baseline period
        #--------------------------Simulation_accuracy---------------------
        R2 = 1-sum((data$R_recorded-data$R_simulated)^2)/sum((data$R_recorded-mean(data$R_recorded))^2)
        Re = sum(data$R_simulated-data$R_recorded)/sum(data$R_recorded)*100
        ResultAccu = data.frame(c("NSE","Bias"),c(R2,Re),c("Nash-Sutcliffe efficiency criterion","Relative error of volumetric fit/%"))
        Simulation_accuracy = `colnames<-`(ResultAccu,c("Criterias","Value","Explanation"))
        
        Simulation_Result = data
        #--------------------------Parameter set--------------------
        Parameter_set = data.frame(c("Ks","Kg","Ksn","Smax"),c(input$Ks,input$Kg,input$Ksn,input$Smax),
                                   c("Surface flow coefficient","Groundwater flow lag coefficient",
                                     "Snow-melt driven runoff coefficient","Maximum soil moisture storage(mm)"))
        Parameter_set = `colnames<-`(Parameter_set,c("Parameters","Value","Explanation"))
        #---------------Quarterly accuracy-------------------------------
        name = c("Quater","Simulated mean annual  runoff(mm)","Recorded mean annual  runoff(mm)","Relative error(%)")
        si = NULL
        re = NULL
        ny = data[,1][length(data[,1])] - data[,1][1] + 1  #number of years
        si[1] = sum(data[,6][data[,2] == 12|data[,2] == 1|data[,2] == 2],na.rm = T)/ny
        re[1] = sum(data[,7][data[,2] == 12|data[,2] == 1|data[,2] == 2],na.rm = T)/ny
        si[2] = sum(data[,6][data[,2] == 3|data[,2] == 4|data[,2] == 5],na.rm = T)/ny
        re[2] = sum(data[,7][data[,2] == 3|data[,2] == 4|data[,2] == 5],na.rm = T)/ny
        si[3] = sum(data[,6][data[,2] == 6|data[,2] == 7|data[,2] == 8],na.rm = T)/ny
        re[3] = sum(data[,7][data[,2] == 6|data[,2] == 7|data[,2] == 8],na.rm = T)/ny
        si[4] = sum(data[,6][data[,2] == 9|data[,2] == 10|data[,2] == 11],na.rm = T)/ny
        re[4] = sum(data[,7][data[,2] == 9|data[,2] == 10|data[,2] == 11],na.rm = T)/ny
        Relativeerror = round((si-re)/re*100,digits = 2)
        si = round(si,digits = 1)
        re = round(re,digits = 1)
        QA = data.frame(c("12~2","3~5","6~8","9~11"), 
                        si, re, Relativeerror)
        Quarterly_accuracy = `colnames<-`(QA,name)
        #--------------------------------------------------------------------------
        switch(input$dataset,
               "Simulation Result" = Simulation_Result,
               "Simulation accuracy" = Simulation_accuracy,
               "Parameter set" = Parameter_set,
               "Quarterly accuracy" = Quarterly_accuracy)
        
    })  #select dataset to download
    
    #----- Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = paste(input$dataset,".csv", sep = ""),
        content = function(file) {
            write.csv(datasetOutput(), file, row.names = FALSE, quote = F, append = F, sep = ",")
        }
    ) 
    #-------------Quantitative attribution-------------------
    data_annual <- reactive({
        req(input$Dataset)
        dat = datasetraw()
        #`colnames<-`(dataset,c("year","month","P","Er","Tem","Discharge"))
        P = NULL
        R = NULL
        E = NULL
        Tem = NULL
        x1 = aggregate(P~year, data = dat, FUN = sum)
        y = x1[ ,1]
        P = x1[ ,2]
        x1 = aggregate(Er~year, data = dat, FUN = sum)
        E = x1[,2]
        x1 = aggregate(Tem~year, data = dat, FUN = mean)
        Tem = x1[,2]
        x1 = aggregate(Discharge~year, data = dat, FUN = mean)
        A = input$A
        R = x1[,2] * 365.3 *24*3.6/A
        out = data.frame(y, P, E, R, Tem)
        `colnames<-`(out, 
                     c("year","P","E","R","Tem"))
    })
    
    output$Relationship <- renderPlot({
        req(input$Dataset)
        
        dat = data_annual()
        #********base stage***********
        baseline1 = input$period[1]
        baseline2 = input$period[2]
        logic2 = dat$year >= baseline1 & dat$year <= baseline2
        data0 = dat[logic2,]
        #*********change stage****************
        a = input$period2[1]
        b = input$period2[2]
        logic1 = dat$year >= a & dat$year <=b
        data1= dat[logic1,]
        
        period = c(paste0(baseline1,"-",baseline2),paste0(a,"-",b))
        layout(matrix(c(1,2),ncol = 2))
        plot(data0[,2],data0[,4],tck = 0.02,xlab = "Annual precipitation(mm)",ylab = "Annual Runoff(mm)",type = "p",pch =16,col = "steelblue4")
        points(data1[,2],data1[,4],pch =24 ,col = "darkred")
        abline(lm(data0[,4]~data0[,2]),lty = 6,col = "steelblue4")
        abline(lm(data1[,4]~data1[,2]),lty = 1 ,col = "darkred")
        legend("topleft",period,pch = c(16,24),lty = c(6,1),col = c("steelblue4","darkred"),bty = "n")
        
        plot(data0[,5],data0[,4],tck = 0.02,xlab = "Annual temperature(Degree)",ylab = "NULL",type = "p",pch =16,col = "steelblue4",yaxt = "n")
        points(data1[,5],data1[,4],pch = 24,col = "darkred")
        abline(lm(data0[,4]~data0[,5]),lty = 6,col = "steelblue4")
        abline(lm(data1[,4]~data1[,5]),lty = 1,col = "darkred")
    })
    
    
    output$quantitativeimpacts <- renderTable({
        req(input$Dataset)
        
        dataset = datasetraw() #input dataset (monthly)
        Ks = input$Ks
        Kg = input$Kg
        Ksn = input$Ksn
        Smax = input$Smax
        S0 = input$S0
        SN0 = input$SN0
        A = input$A
        unit = input$unit
        Null = input$Null
        
        a = input$period2[1]  #change period start
        b = input$period2[2]  #change period end
        logic = dataset$year >= a & dataset$year <=b
        dataset = dataset[logic,]
        result = SIMULATE(dataset$year,dataset$month,
                          dataset$P, dataset$Er, dataset$Discharge, 
                          dataset$Tem, 
                          Ks, Kg, Ksn, Smax, S0, SN0, A, unit, Null)
        
        t_year = a:b
        nyear = length(t_year)
        Qsim = NULL
        Qre = NULL
        REM = NULL
        for (i in 1:nyear){
            Qsim[i] = sum(result$R_simulated[result[,1] == t_year[i]], na.rm = T)
            Qre[i] = sum(result$R_recorded[result[,1] == t_year[i]], na.rm = T) 
        }
        
        data = data()   #simulation results for baseline period
        baseline1 = input$period[1]
        baseline2 = input$period[2]
        Q_baseline_re = sum(data$R_recorded)/length(baseline1:baseline2)  #annual baseline period recorded runoff
        Q_baseline_si = sum(data$R_simulated)/length(baseline1:baseline2) #annual baseline period simulated runoff
        
        period = c(paste0(baseline1,"-",baseline2),paste0(a,"-",b))
        Re_runoff = c(Q_baseline_re,sum(Qre)/nyear)  #sum(Qre)/nyear : annual selected period recorded runoff
        Si_runoff = c(Q_baseline_si,sum(Qsim)/nyear) #sum(Qsim)/nyear: annual selected period simulated runoff
        total_reduction = sum(Qre)/nyear - Q_baseline_re
        #WH = sum(Qsim)/nyear - sum(Qre)/nyear
        WC = sum(Qsim)/nyear - Q_baseline_re
        WH = total_reduction - WC
        #percentange_WH = WH / total_reduction*100
        #percentange_WC = WC / total_reduction*100
        percentange_WH = WH / Q_baseline_re*100
        percentange_WC = WC / Q_baseline_re*100
        
        out = data.frame(period,round(Re_runoff,digits = 2),round(Si_runoff,digits = 2),c("-",round(total_reduction,digits = 2))
                         ,c("-",round(WH,digits = 2)),c("-",round(percentange_WH,digits = 2)),
                         c("-",round(WC,digits = 2)),c("-",round(percentange_WC,digits = 2)))
        `colnames<-`(out,c("Period","Recorded Runoff(Whr,mm)","Simulated Runoff(Whn,mm)","Total Runoff Reduction(mm)",
                           "Human-Induced(mm)","Human-Induced(%)","Climate-Induced(mm)","Climate-Induced(%)"))
    })
    #---------------sensitivity analysis---------------------------
    sensitivity <- reactive({
        req(input$Dataset)
        dat = datasetraw()
        Ks = input$Ks
        Kg = input$Kg
        Ksn = input$Ksn
        Smax = input$Smax
        S0 = input$S0
        SN0 = input$SN0
        A = input$A
        unit = input$unit
        Null = input$Null
        
        a = input$period[1]
        b = input$period[2]
        logic = dat[,1] >= a & dat[,1] <=b
        year = dat[logic,1]
        month = dat[logic,2]
        P = dat[logic,3]
        Er = dat[logic,4]
        Tem = dat[logic,5]
        Discharge = dat[logic,6]
        
        
        P_scenario = P*(1 + input$Pin/100)
        Tem_scenario = Tem + input$Tin
        lm = lm(log(Er) ~ Tem)  #caliberate the relationship between T and E
        c1 = exp(lm$coefficients[1])
        c2 = lm$coefficients[2]
        
        Er = c1 * exp(c2 * Tem)
        Er_scenario = c1 * exp(c2 * Tem_scenario)
        
        result = SIMULATE(year, month,
                          P, Er, Discharge, Tem,
                          Ks, Kg, Ksn, Smax, S0, SN0, A, unit, Null)
        result_scenario = SIMULATE(year, month, 
                                   P_scenario, Er_scenario, Discharge, Tem_scenario, 
                                   Ks, Kg, Ksn, Smax, S0, SN0, A, unit, Null)
        sensitivity_result = data.frame(result[,1]
                                        ,result[,2]
                                        ,result[,6]
                                        ,result_scenario[,6]
                                        ,result[,5]
                                        ,result_scenario[,5])
        `colnames<-`(sensitivity_result,c("year","month","Raw_runoff","Scenario_runoff","Raw_E","Scenario_E"))
    })
    
    output$scenario1 <- renderTable({
        req(input$Dataset)
        data = sensitivity() 
        nyear = data[,1][length(data[,1])] - data[,1][1] + 1 #number of years
        re = sum(data[,3])/nyear
        si = sum(data[,4])/nyear
        Re = (si-re)/re*100
        out = data.frame(si,re,Re)
        `colnames<-`(out,c("Scenario runoff(mm)","Baseline runoff(mm)","Change(%)"))
    })
    output$scenario2 <- renderTable({
        req(input$Dataset)
        data = sensitivity() 
        name = c("Quater","Mean annual rescaled runoff(mm)","Mean annual baseline runoff(mm)","Change(%)")
        si = NULL
        re = NULL
        ny = data[,1][length(data[,1])] - data[,1][1] + 1
        si[1] = sum(data[,4][data[,2] == 12|data[,2] == 1|data[,2] == 2],na.rm = T)/ny
        re[1] = sum(data[,3][data[,2] == 12|data[,2] == 1|data[,2] == 2],na.rm = T)/ny
        si[2] = sum(data[,4][data[,2] == 3|data[,2] == 4|data[,2] == 5],na.rm = T)/ny
        re[2] = sum(data[,3][data[,2] == 3|data[,2] == 4|data[,2] == 5],na.rm = T)/ny
        si[3] = sum(data[,4][data[,2] == 6|data[,2] == 7|data[,2] == 8],na.rm = T)/ny
        re[3] = sum(data[,3][data[,2] == 6|data[,2] == 7|data[,2] == 8],na.rm = T)/ny
        si[4] = sum(data[,4][data[,2] == 9|data[,2] == 10|data[,2] == 11],na.rm = T)/ny
        re[4] = sum(data[,3][data[,2] == 9|data[,2] == 10|data[,2] == 11],na.rm = T)/ny
        Re = round((si-re)/re*100,digits = 2)
        si = round(si,digits = 1)
        re = round(re,digits = 1)
        QA = data.frame(c("12~2","3~5","6~8","9~11"),si,re,Re)
        `colnames<-`(QA,name)
    })
    output$mongthlyrunoffplot <- renderPlot({
        data = sensitivity() 
        ny = data[,1][length(data[,1])] - data[,1][1] + 1
        re_monthly_runoff = NULL
        si_monthly_runoff = NULL
        
        for (i in 1:12) {
            si_monthly_runoff[i] = sum(data[,4][data[,2] == i], na.rm = T)/ny
            re_monthly_runoff[i] = sum(data[,3][data[,2] == i], na.rm = T)/ny
            
        }
        Change = (si_monthly_runoff - re_monthly_runoff)/re_monthly_runoff*100
        Change = round(Change, digits = 1)
        y = Change
        x = 1:12
        data = data.frame(as.factor(x), y)
        colnames(data) = c("x", "y")
        
        ggplot(data, aes(x, y, fill = y) ) + 
            geom_bar(stat = "identity") + 
            geom_abline(intercept = 0, slope = 0, size = 1.5, colour = 'gray')+
            geom_text(aes(label = y), hjust = 0.5, vjust= -0.5 ) +
            #scale_y_continuous(limits=c(-3.8,4.2))+
            labs(x = 'month', y = 'Response of mean monthly runoff (%)') + 
            theme(
                axis.text.x = element_text(size = 10, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold")
            )
        
    })
    output$E_Tem <- renderPlot({
        req(input$Dataset)
        
        dat = datasetraw()
        a = input$period[1]
        b = input$period[2]
        logic = dat[,1] >= a & dat[,1] <=b
        Er = dat[logic, 4]
        Tem = dat[logic, 5]
        
        lm = lm(log(Er) ~ Tem)  #caliberate the relationship between T and E
        c1 = exp(lm$coefficients[1])
        c2 = lm$coefficients[2]
        x = Tem
        y = c1 * exp(Tem * c2)
        plot(Tem, Er, 
             type = "p", col = "black", pch = 16, 
             tck = 0.02,
             xlab = "Temperature", ylab = "Monthly Evaporation(mm)")
        points(x, y, 
               type = "l", col = "red")
        c1 = round(c1, digits = 1)
        c2 = round(c2, digits = 1)
        tt = paste0("E =", c1, "exp(", c2, "T)" )
        text(median(Tem), 4/5*max(Er), tt, col = "blue")
    })
    
    Parameteranalysisdataset <- reactive({
        req(input$Dataset)
        dat = datasetraw()
        
        Ks = input$Ks
        Kg = input$Kg
        Ksn = input$Ksn
        Smax = input$Smax
        S0 = input$S0
        SN0 = input$SN0
        A = input$A
        unit = input$unit
        Null = input$Null
        
        a = input$period_scenario2[1]
        b = input$period_scenario2[2]
        logic = dat[,1] >= a & dat[,1] <= b
        year = dat[logic,1]
        month = dat[logic,2]
        P = dat[logic,3]
        Er = dat[logic,4]
        Tem = dat[logic,5]
        Discharge = dat[logic,6]
        
        
        In = seq(0.5,1.5,0.1) #changes of parameters
        #***********Ks**************
        Out_Ks = as.data.frame(matrix(c(1:(11*4)),ncol = 4))
        Out_Ks = `colnames<-`(Out_Ks,c("R2","Re","Change","Parameter"))
        Out_Ks$Parameter <- rep("Ks",11)
        Out_Ks$Change <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
        for (i in 1:length(In)) {
            data = SIMULATE(year,month,
                            P,Er,Discharge,Tem,
                            Ks*In[i], Kg, Ksn, Smax, S0, SN0, A, unit, Null)
            Out_Ks[i,1] = round(1-sum((data$R_recorded-data$R_simulated)^2)/sum((data$R_recorded-mean(data$R_recorded))^2),digits = 4)*100
            Out_Ks[i,2] = round(sum(data$R_simulated-data$R_recorded)/sum(data$R_recorded),digits = 4)*100 #Percentage mark system
        }
        
        #***********Smax**********
        Out_Smax = as.data.frame(matrix(c(1:(11*4)),ncol = 4))
        Out_Smax = `colnames<-`(Out_Ks,c("R2","Re","Change","Parameter"))
        Out_Smax$Parameter <- rep("Smax",11)
        Out_Smax$Change <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
        for (i in 1:length(In)) {
            data = SIMULATE(year, month, 
                            P,Er,Discharge,Tem,
                            Ks,Kg,Ksn,Smax*In[i],S0,SN0,A,unit,Null)
            Out_Smax[i,1] = round(1-sum((data$R_recorded-data$R_simulated)^2)/sum((data$R_recorded-mean(data$R_recorded))^2),digits = 4)*100
            Out_Smax[i,2] = round(sum(data$R_simulated-data$R_recorded)/sum(data$R_recorded),digits = 4)*100 #Percentage mark system
        }
        #***********Kg**********
        Out_Kg = as.data.frame(matrix(c(1:(11*4)),ncol = 4))
        Out_Kg = `colnames<-`(Out_Ks,c("R2","Re","Change","Parameter"))
        Out_Kg$Parameter <- rep("Kg",11)
        Out_Kg$Change <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
        for (i in 1:length(In)) {
            data = SIMULATE(year,month,
                            P,Er,Discharge,Tem,
                            Ks,Kg*In[i],Ksn,Smax,S0,SN0,A,unit,Null)
            Out_Kg[i,1] = round(1-sum((data$R_recorded-data$R_simulated)^2)/sum((data$R_recorded-mean(data$R_recorded))^2),digits = 4)*100
            Out_Kg[i,2] = round(sum(data$R_simulated-data$R_recorded)/sum(data$R_recorded),digits = 4)*100 #Percentage mark system
        }
        #***********Ksn**********
        Out_Ksn = as.data.frame(matrix(c(1:(11*4)),ncol = 4))
        Out_Ksn = `colnames<-`(Out_Ks,c("R2","Re","Change","Parameter"))
        Out_Ksn$Parameter <- rep("Ksn",11)
        Out_Ksn$Change <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
        for (i in 1:length(In)) {
            data = SIMULATE(year,month,
                            P,Er,Discharge,Tem,
                            Ks,Kg,Ksn*In[i],Smax,S0,SN0,A,unit,Null)
            Out_Ksn[i,1] = round(1-sum((data$R_recorded-data$R_simulated)^2)/sum((data$R_recorded-mean(data$R_recorded))^2),digits = 4)*100
            Out_Ksn[i,2] = round(sum(data$R_simulated-data$R_recorded)/sum(data$R_recorded),digits = 4)*100 #Percentage mark system
        }
        
        x = rep(c(-50,-40,-30,-20,-10,0,10,20,30,40,50),8)
        y = c(Out_Smax[,1],Out_Smax[,2],
              Out_Ks[,1],Out_Ks[,2],
              Out_Kg[,1],Out_Kg[,2],
              Out_Ksn[,1],Out_Ksn[,2])
        Label = rep(c(rep("R2",11),rep("Re",11)),4)
        Item = c(rep("Smax",22),
                 rep("Ks",22),
                 rep("Kg",22),
                 rep("Ksn",22))
        graph = data.frame(x,y,Label,Item)
        graph
    })
    output$Parameter_analysis_plot1 <- renderPlot({
        req(input$Dataset)
        graph = Parameteranalysisdataset()
        ggplot(data = graph[graph[,3] == "R2",],
               aes(x = x, y = y, shape = Item, colour = Item,linetype = Item)) + 
        geom_point(size = 1.5)+geom_line(size = 0.9)+
            xlab("Changes of parameters(%)")+ylab("NSE(%)")+
            theme_bw()+
            theme(axis.ticks = element_blank(),panel.grid.major = element_line(linetype = 3,colour = "black"))+
            labs(shape = NULL,colour = NULL,linetype = NULL) + 
            geom_vline(aes(xintercept = 0))+
            scale_x_continuous(breaks = c(-50,-40,-30,-20,-10,0,10,20,30,40,50),labels=c(-50,-40,-30,-20,-10,0,10,20,30,40,50))
        
    })
    output$Parameter_analysis_plot2 <- renderPlot({
        req(input$Dataset)
        graph = Parameteranalysisdataset()
        ggplot(data = graph[graph[,3] == "Re",],
               aes(x = x,y = y,shape = Item,colour = Item,linetype = Item))+
            geom_point(size = 1.5) + 
            geom_line(size = 0.9)+
            xlab("Changes of parameters(%)")+ylab("Bias(%)") + 
            theme_bw()+
            theme(axis.ticks = element_blank(), panel.grid.major = element_line(linetype = 3,colour = "black"))+
            labs(shape = NULL,colour = NULL,linetype = NULL) + 
            geom_vline(aes(xintercept = 0)) +
            scale_x_continuous(breaks = c(-50,-40,-30,-20,-10,0,10,20,30,40,50),labels=c(-50,-40,-30,-20,-10,0,10,20,30,40,50))
        
    })
    
    output$Parameter_analysis_dataset <- downloadHandler(
        filename = paste("Parameter_analysis.csv", sep = ""),
        content = function(file) {
            write.table(Parameteranalysisdataset(), file,col.names = T, 
                        row.names = FALSE,quote = F,append = F,sep = ",")
        }
    )
    
    #-------------------download image-----------------------
    output$Parameter_analysis_down <- downloadHandler(  #graph download
        filename = paste0("Parameter_analysis_down.emf"),
        content = function(file) {
          win.metafile(filename = file) #win.metafile
            #************************
            graph = Parameteranalysisdataset()
            ggplot(data = graph,aes(x = x,y = y,shape = Label,colour = Label)) + 
                geom_point() + 
                geom_line() +
                facet_wrap(~Item,scales = "free") + 
                labs(colour = NULL,shape = NULL) +
                theme(axis.ticks = element_blank(), panel.grid.major = element_line(linetype = 3,colour = "black"))
            
            #************************
            dev.off()
            # write.csv(datasetInput(),file, row.names = FALSE,quote = F,append = F,sep = ",")
        }
        ,contentType = NA
    )
    output$simulation_down <- downloadHandler(  
        filename = paste("simulation period",Sys.Date(),".png",sep = ""),
        content = function(file) {
            png(filename = file)
            #*************************************
            data = data()    #attach reactive data
            plot(data$R_simulated, type = "l", lty = 2, lwd = 2,
                 tck = 0.01, xlab = paste(input$period[1], "-", input$period[2]),
                 ylim = c(0, max(data$R_recorded) + 5), ylab = "Discharge(mm)", col = "blue")
            points(data$R_recorded, type = "l", lty = 1, col = "black")
            legend("topleft", c("simulated", "recorded"), lty = c(2, 1), col = c("blue", "black"), bty="n")
            #*************************************
            dev.off()
        }
        ,contentType = NA
    )
    output$SumKi_down <- downloadHandler(  
        filename = paste("SumKi", Sys.Date(), ".png", sep = ""),
        content = function(file) {
            png(filename = file)
            #*******************************************
            dataset = datasetraw()
            Ks = input$Ks
            Kg = input$Kg
            Ksn = input$Ksn
            Smax = input$Smax
            S0 = input$S0
            SN0 = input$SN0
            A = input$A
            unit = input$unit
            Null = input$Null
            result = SIMULATE(dataset$year, dataset$month, dataset$P, dataset$Er, 
                              dataset$Discharge, dataset$Tem, 
                              Ks,Kg,Ksn,Smax,S0,SN0,A,unit,Null)
            
            qi = dataset[1,1]
            zhi = dataset[,1][length(dataset[,1])]
            t_year = (qi:zhi)
            nyear = length(t_year)
            Qsim = NULL
            Qre = NULL
            for (i in 1:nyear){
                Qsim[i] = sum(result$R_simulated[result[,1] == t_year[i]], na.rm = T)
                Qre[i] = sum(result$R_recorded[result[,1] == t_year[i]], na.rm = T)
            }
            sig = function(x,y){
                if(x > y) {return(1)}
                else if (x == y) {return(0)}
                else {return(-1)}
            }
            K = NULL
            for (i in 1:nyear){
                K[i] = sig(Qsim[i],Qre[i])
            }
            
            SumKi = NULL
            SumKi[1] = K[1]
            for (i in 2:nyear) {
                SumKi[i] = SumKi[i-1]+K[i]
            }
            graph = data.frame(t_year,SumKi)
            graph = `colnames<-`(graph,c("year","SumKi"))
            ggplot(data = graph,aes(x= year,y = SumKi)) + 
                geom_point()
            #*******************************************
            dev.off()
            
        }
        ,contentType = NA
    )
    
    output$Relationship_down <- downloadHandler(  
        filename = paste("Relationship",".png",sep = ""),
        content = function(file) {
            png(filename = file)
            #*************************************
            dataset = datasetraw()
            
            P = NULL
            R = NULL
            E = NULL
            Tem = NULL
            y = dataset[,1][1]:dataset[,1][length(dataset[,1])]
            for (i in 1:length(y)){
                P[i] = sum(dataset[,3][dataset[,1] == y[i]],na.rm = T)
                E[i] = sum(dataset[,4][dataset[,1] == y[i]],na.rm = T)
                R[i] = sum(dataset[,5][dataset[,1] == y[i]],na.rm = T)*30*2.4*36/input$A
                Tem[i] = mean(dataset[,6][dataset[,1] == y[i]],na.rm = T)
            }
            dat = data.frame(y,P,E,R,Tem)
            dat = `colnames<-`(dat,c("year","P","E","R","Tem"))
            
            #********base stage***********
            baseline1 = input$period[1]
            baseline2 = input$period[2]
            logic2 = dat$year >= baseline1 & dat$year <= baseline2
            data0 = dat[logic2,]
            #*********first stage****************
            a = input$period2[1]
            b = input$period2[2]
            logic1 = dat$year >= a & dat$year <=b
            data1= dat[logic1,]
            
            period = c(paste0(baseline1,"-",baseline2),paste0(a,"-",b))
            layout(matrix(c(1,2),ncol = 2))
            plot(data0[,2],data0[,4],tck = 0.02,xlab = "Annual precipitation(mm)",ylab = "Annual Runoff(mm)",type = "p",pch =16,col = "steelblue4")
            points(data1[,2],data1[,4],pch =24 ,col = "darkred")
            abline(lm(data0[,4]~data0[,2]),lty = 6,col = "steelblue4")
            abline(lm(data1[,4]~data1[,2]),lty = 1 ,col = "darkred")
            legend("topleft",period,pch = c(16,24),lty = c(6,1),col = c("steelblue4","darkred"),bty = "n")
            
            plot(data0[,5],data0[,4],tck = 0.02,
                 xlab = "Annual temperature(Degree)",ylab = NULL,
                 type = "p",pch =16,col = "steelblue4",yaxt = "n")
            points(data1[,5],data1[,4],pch = 24,col = "darkred")
            abline(lm(data0[,4]~data0[,5]),lty = 6,col = "steelblue4")
            abline(lm(data1[,4]~data1[,5]),lty = 1,col = "darkred")
            #*************************************
            dev.off()
        }
        ,contentType = NA
    )
    
    
    output$E_Tem_down <- downloadHandler(  
        filename = paste("E_Tem",Sys.Date(),".png",sep = ""),
        content = function(file) {
            png(filename = file)
            #*************************************
            E = Evaporation()
            Tem = Temperature()
            Er = E[,3]
            Tem = Tem[,3]
            lm = lm(log(Er)~Tem)  #caliberate the relationship between T and E
            c1 = exp(lm$coefficients[1])
            c2 = lm$coefficients[2]
            x = Tem
            y = c1*exp(Tem*c2)
            plot(Tem,Er,type = "p",col = "black",pch = 16,tck = 0.02,xlab = "Temperature",ylab = "Monthly Evaporation(mm)")
            points(x,y,type = "l",col = "red")
            c1 = round(c1,digits = 1)
            c2 = round(c2,digits = 1)
            tt = paste("E =",c1,"exp(",c2,"T)",sep = "0" )
            text(2,4/5*max(Er),tt,col = "blue")
            #*************************************
            dev.off()
        }
        ,contentType = NA
    )
    periodanalysis <- reactive({
        dataset = datasetraw()
        Ks = input$Ks
        Kg = input$Kg
        Ksn = input$Ksn
        Smax = input$Smax
        S0 = input$S0
        SN0 = input$SN0
        A = input$A
        unit = input$unit
        Null = input$Null
        
        a = input$period2[1]
        b = input$period2[2]
        logic = dataset$year >= a & dataset$year <=b
        dataset = dataset[logic,]
        SIMULATE(dataset$year,dataset$month,
                 dataset$P,dataset$Er,dataset$Discharge,dataset$Tem,
                 Ks,Kg,Ksn,Smax,S0,SN0,A,unit,Null)
        
    })
    output$analysis_down <- downloadHandler(
        filename = paste("Relationship.csv", sep = ""),
        content = function(file) {
            write.table(periodanalysis(), file, row.names = FALSE,quote = F,append = F,sep = ",")
        }
    ) 
}

# Run the application 
shinyApp(ui = ui, server = server)


