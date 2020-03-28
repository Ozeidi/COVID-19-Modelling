

####################################
library(shiny)
library(shinydashboard)
library(flexdashboard)
#library(shinyBS)
library(tidyverse)
library(magrittr)
#library(plotly)
library(EpiModel)

theme_set(theme_bw(base_size =  18))  
## app.R ##

ui <- dashboardPage(
  dashboardHeader(title = "Modelling COVID-19 Outbreak", titleWidth = 350),
  dashboardSidebar( width = 350,
                    sidebarMenu(
                      menuItem("Model Settings", icon = icon("th"), tabName = "SimSettings", startExpanded = TRUE,
                               
                               menuItem("Outbreak Statistics", startExpanded = TRUE,
                                        
                                        numericInput("pop", "Population Headcount", 1000),
                                        sliderInput("confirmed.cases", "Number of Confirmed Cases", min = 1 , max =1000, value =90),
                                        sliderInput("recovered.cases", "Number of Recovered Cases", min = 0 , max =200, value =17),
                                        sliderInput("sim.duration", "No of days to run", min = 30 , max =1000, value =500),
                                        sliderInput("trans.rate", div("Transmissible Acts Rate \n",style = "font-size:15px;font-weight: bold",
                                                                      p("Average number of transmissible acts, like shaking hands.",style = "font-size:12px"),
                                                                      div(style = "float:left;", "Lock Down"),
                                                                      div(style = "float:right;", "Normal Life")),
                                                    min = 0 , max =10, value =4)
                               ),
                               
                               menuItem("Pandemic Parameters", startExpanded = TRUE,
                                        
                                        sliderInput("inf.prob", div("Infection Propability \n",style = "font-size:15px;font-weight: bold",
                                                                    p("between a susceptible and infected person",style = "font-size:12px")),
                                                    min = 0, max = 0.05, value =0.05),
                                        
                                        
                                        sliderInput("rec.rate", div("Recovery Time \n",style = "font-size:15px;font-weight: bold",
                                                                    p("Average recovery time (days) for an individual",style = "font-size:12px")),
                                                    min = 10 , max =30, value =20),
                                        
                                        
                                        sliderInput("fat.rate", div("Fatality Rate \n",style = "font-size:15px;font-weight: bold",
                                                                    p("fatality rate of infected people/1000",style = "font-size:12px")),
                                                    min = 0 , max =(10), value =(5.8))
                                        
                                        
                               )
                               
                               
                      )
                    )
                    #bsTooltip("pop", "The wait times will be broken into this many equally spaced bins","right", options = list(container = "body"))
  ),
  dashboardBody(
    
    fluidRow(
      #infoBoxOutput("sus"),
      infoBoxOutput("infected"),
      infoBoxOutput("recovered"),
      infoBoxOutput("dead")
      ),
      
    fluidRow(
      box(title ="Pandemic Breakout Timeline" , plotOutput("plot2")),
      box(title ="Social Distansing Sensitivity" , plotOutput("plot1"))
      
    )
    )
    
    # box(
    #   title = "Controls",
    #   sliderInput("slider", "Number of observations:", 1, 100, 50)
    # )
    
  )


server <- function(input, output) {
  
  showModal(modalDialog(
    title = "Introduction",
    includeHTML('www/Intro.htm'),
    width = 370, height = "auto",
    easyClose = TRUE
  ))
  

  
  
  df  <- reactive({
    sim(input)
  })
  
  pop <- reactive({
    pop <-input$pop + input$confirmed.cases + input$recovered.cases 
  })
  
  end_df <- reactive({
    mydf <- df()
    mydf <- mydf[nrow(mydf)-1,]
    print(mydf)
    mydf
  })
  

  output$sus <- renderInfoBox({
    mydf <- end_df()
    total_sus <- round(mydf$s.num,0)
    infoBox(title = "Total Susciptible",
            tags$p(total_recovered, style = "font-size: 190%;"), icon = icon("list"),color = "red")
  })
  
  output$infected <- renderInfoBox({
    mydf <- end_df()
    tot_infected <- round(mydf$i.num + mydf$r.num + mydf$dead,0)
    infoBox(title = "Total who got Infected",
            tags$p(tot_infected, style = "font-size: 190%;"), icon = icon("stethoscope"),color = "yellow")
  })
  
  
  output$recovered <- renderInfoBox({
    mydf <- end_df()
    total_recovered <- round(mydf$r.num,0)
    infoBox(title = "Total Recovered",
            tags$p(total_recovered, style = "font-size: 190%;"), icon = icon("child"),color = "red")
  })

  output$dead <- renderInfoBox({
    mydf <- end_df()
    total_dead <- round(mydf$dead,0)
    infoBox(title = "Death Toll",
            tags$p(total_dead, style = "font-size: 190%;"), icon = icon("ambulance"),color = "blue")
  })
  
  
  
  output$plot1<- renderPlot({
    mydf <- df()
    g <- ggplot() +theme(legend.position = "none" ,panel.border = element_blank(),
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      xlab("Time Since First Case")+ ylab("Number Infected")+
      ggtitle(" Social Distancing Scenarios Sensitivity")
    
    
    # Sensitivity Curves
    g +geom_line(data = mydf,aes(x= time, y=i.num,  color=rgb(158/255,158/255,161/255), group = (trans.rate), alpha=rev(trans.rate)),
                 size= .1, linetype = "solid")+
      
      # Selected Scenraios with 
      geom_line(data = mydf[mydf$trans.rate == last(mydf$trans.rate),],aes(x= time, y=i.num,  color=rgb(85/255,158/255,161/255)),size= 1.0, linetype = "solid")+
      geom_hline(aes(yintercept = 5027), linetype = "dashed")+
      geom_text(aes(x=input$sim.duration/2, y= 5027, label = "Oman Health Care Capacity \n Number of Beds", vjust = -.2), size =5)
    
    
  })
  
  output$plot2 <- renderPlot({
    mydf <- df()
    print(paste(last(mydf$s.num), last(mydf$i.num), last(mydf$r.num), last(mydf$dead)))
    pop <- pop()
    print(colnames(mydf))
    mydf <- mydf %>% filter(trans.rate== last(trans.rate)) %>%  select(c(2:6)) %>%  rename("Susciptilbe" = s.num, "Infected" = i.num, "Recovered" = r.num, "Dead" = dead) %>% 
      gather(variable ,total, 2:5, factor_key = T)
    mydf %<>% mutate(total = total / pop*100)
    
    print(pop)
    
    
    line_col <- c("Susciptilbe" = "orange", "Infected" ="red", "Recovered" = "green", "Dead"= "black")
    
    ggplot(mydf) +theme(legend.position = "bottom" ,panel.border = element_blank(),
                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      xlab("Time Since First Case")+ ylab("Percentage (%)")+
      geom_line(aes(x=time, y=total, group = variable, colour = variable), size=1.5)+
      scale_color_manual(values = line_col)
    
  })
}


sim <- function(input){
  pop <-input$pop + input$confirmed.cases + input$recovered.cases 
  rec.rate <- 1/input$rec.rate
  nsteps <- input$sim.duration
  init <- init.dcm(s.num = input$pop, i.num =input$confirmed.cases, r.num = input$recovered.cases)
  control <- control.dcm(type = "SIR", nsteps = nsteps, dt = 0.5)
  
  
  
  # Run simulations and draw lines.
  mod.df <- list()
  i <- 1
  fat.rate <- input$fat.rate /1000/365
  trans.rates <- seq(10, 0, by=-2)
  trans.rates <- c(trans.rates, input$trans.rate)
  for (rt in trans.rates) {
    
    param <- param.dcm(inf.prob = input$inf.prob, act.rate = rt, rec.rate = rec.rate,
                       a.rate = 0, ds.rate = 0, di.rate = fat.rate, dr.rate = 0)
    mod <- dcm(param, init, control)
    mod.df[[i]]<- as.data.frame(mod)[,c(seq(1:4),11)]
    i=i+1
    
  }
  names(mod.df) <- as.factor(trans.rates)
  
  mod.df.b <- bind_rows(mod.df, .id='trans.rate')
  mod.df.b$trans.rate <- (as.factor(mod.df.b$trans.rate))
  
  mod.df.b %<>%   mutate(dead = pop- num) %>%  select(-c(num))
  print(colnames(mod.df.b))
  mod.df.b
  
  #########################
  
  
}
shinyApp(ui, server)