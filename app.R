library(ggplot2)
library(markdown)
library(cowplot)
library(pracma)

red <- "#E41A1C"
blue <- "#377EB8"

dnorm_segment <- function(x, mean = 0, sd = 1, c=1, factor1 = 1, factor2 = 1) 
  dnorm(x, mean = mean, sd = sd)*ifelse(x<=c, factor1, factor2)

ui <- fluidPage(
  theme = "bootstrap.min.css",
  
  fluidRow(
    column(6, offset = 3,
           includeMarkdown("intro.md")
    )
  ),
  
  fluidRow(
    column(3,
           sliderInput("meanSignal","sensitivity (d')",-3,3,1,0.1),
           # sliderInput("varSignal","signal variance",0.5,2,1,0.1),
           sliderInput("c","decision criterion (c)",-3,3,0.5,0.1),
           sliderInput("scale","confidence scale",0,2,0.5,0.05)
    ),
    column(6,plotOutput("plot1")),
    column(3,plotOutput("ROCplot"))
  ),
  hr(),
  
  fluidRow(
    column(6, offset = 3,
           withMathJax(includeMarkdown("meta.md"))
    )
  ),
  
  fluidRow(
    column(3,
           sliderInput("metascale","confidence scale",0,2,0.5,0.05),
           sliderInput("metad", "meta d'",-3,3,1,0.1),
           sliderInput("metac","meta criterion",-3,3,0.5,0.1)
    ),
    column(6, plotOutput("plot2")),
    column(3, plotOutput("ROCplot2"))
  ),
  hr(),
  
  h4("Matan Mazor 2020")
  
)

server <- function(input, output) {
  
  c_vec <- reactive({input$scale*seq(-5,5)+input$c})
  
  sample1 <- reactive({data.frame(condition = rep(c("noise","signal"),each=12),
                                  response = factor(rep(c("no,6","no,5","no,4","no,3","no,2","no,1","yes,1","yes,2","yes,3","yes,4","yes,5","yes,6"),2),
                                                    levels = c("no,6","no,5","no,4","no,3","no,2","no,1","yes,1","yes,2","yes,3","yes,4","yes,5","yes,6")),
                                  count=c(hist(rnorm(600), breaks = c(-10,c_vec(),10))$counts,
                                          hist(rnorm(600,input$meanSignal), breaks = c(-10,c_vec(),10))$counts))})
  
  
  cumResps <- reactive({data.frame(F=c(1,pnorm(c_vec(),0,1, lower.tail=FALSE),0),
                                   H = c(1,pnorm(c_vec(),input$meanSignal,lower.tail=FALSE),0))})
  
  FA <- reactive(pnorm(input$c, lower.tail = FALSE))
  H <-reactive(pnorm(input$c, input$meanSignal, lower.tail = FALSE))
  
  type2cumResps <- reactive({data.frame(F=c(0,pnorm(c_vec()[seq(11,7,-1)],0,1, lower.tail=FALSE)/FA(),1)/2+
                                        c(0,pnorm(c_vec()[1:5],input$meanSignal,1)/(1-H()),1)/2,
                                        H = c(0,pnorm(c_vec()[seq(11,7,-1)],input$meanSignal,lower.tail=FALSE)/H(),1)/2+
                                        c(0,pnorm(c_vec()[1:5],0,1)/(1-FA()),1)/2
                                        )})
  
  metac_vec <- reactive({input$metascale*seq(-5,5)+input$metac})
  
  metaFA <- reactive(pnorm(input$metac, lower.tail = FALSE))
  metaH <-reactive(pnorm(input$metac, input$metad, lower.tail = FALSE))
  
  hit_factor <- reactive({H()/metaH()})
  
  miss_factor <- reactive({(1-H())/(1-metaH())})
  
  FA_factor <- reactive({FA()/metaFA()})
  
  CR_factor <- reactive({(1-FA())/(1-metaFA())})
  
  sample2 <-reactive({data.frame(condition = sample1()$condition,
                                 response = sample1()$response,
                                 count=c(hist(rnorm(600), breaks = c(-10,metac_vec(),10))$counts*
                                           rep(c(CR_factor(), FA_factor()),each=6),
                                         hist(rnorm(600,input$metad), breaks = c(-10,metac_vec(),10))$counts*
                                           rep(c(miss_factor(), hit_factor()),each=6)))})
  
  metacumResps <- reactive({data.frame(F=c(1,1-CR_factor()*pnorm(metac_vec()[seq(1,6)],0),
                                           FA_factor()*pnorm(metac_vec()[seq(7,11)],0,lower.tail = FALSE),0),
                                       H = c(1,1-miss_factor()*pnorm(metac_vec()[seq(1,6)],input$metad),
                                             hit_factor()*pnorm(metac_vec()[seq(7,11)],input$metad,lower.tail=FALSE),0))})
  
  metatype2cumResps <- reactive({data.frame(F=c(0,pnorm(metac_vec()[seq(11,7,-1)],0,1, lower.tail=FALSE)/metaFA(),1)/2+
                                          c(0,pnorm(metac_vec()[1:5],input$metad,1)/(1-metaH()),1)/2,
                                        H = c(0,pnorm(metac_vec()[seq(11,7,-1)],input$metad,lower.tail=FALSE)/metaH(),1)/2+
                                          c(0,pnorm(metac_vec()[1:5],0,1)/(1-metaFA()),1)/2
  )})
  
  output$plot1 <- renderPlot({
    
    dist_plot1 <- ggplot(data.frame(x=c(0, 2)), aes(x)) +
      stat_function(fun = dnorm, args = list(mean = input$meanSignal), xlim= c(-5,5),
                    color = blue, geom = "area", fill = blue, size = 1, alpha = 0.5)+
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1),color = red ,geom = "area", fill = red, size = 1, alpha = 0.5) + 
      geom_vline(xintercept = c_vec(),alpha=0.5) +geom_vline(xintercept = input$c,size=0.8) +
      ylim(0,0.5) + xlim(-5,5) + labs(x="Strength of Evidence", y = "Density")
    
    sample_plot1 <-  ggplot(data=sample1(),aes(x=response,y=count,fill=condition))+geom_bar(stat="identity",color="black",position=position_dodge())+
      scale_fill_manual(values=c(red,blue))+theme(legend.position="none")
    
    plot_grid(dist_plot1,sample_plot1, nrow=2)
    
  })
  
  output$ROCplot <- renderPlot({
    
    ROC1 <- ggplot(data=cumResps(), aes(x=F,y=H,group=1))+ 
      geom_line() + geom_point() + coord_fixed(ratio=1) + labs(title=paste("Type-1 ROC\nFA rate:", round(FA(),2),
                                                                           "| hit rate: ", round(H(),2),
                                                                           "\nAUC:", -round(trapz(cumResps()$F,cumResps()$H),2)),
                                                               x = "False Alarms", y="Hits")+
      geom_point(aes(x=pnorm(input$c, lower.tail = FALSE ), 
                     y=pnorm(input$c,input$meanSignal, lower.tail = FALSE)),size=5) +
      geom_abline(slope=1,linetype='dotted') + theme_bw() +
      geom_rect(data=cumResps(), mapping=aes(xmin=0, xmax=1, ymin=0, ymax=1), color="black", alpha=0.5, fill = NA)+
      theme(plot.title = element_text(size=11))
    
    ROC2 <- ggplot(data=type2cumResps(), aes(x=F,y=H))+ 
      geom_line() + geom_point() + coord_fixed(ratio=1) + 
      labs(title=paste("Type-2 ROC\nAUC2:", round(trapz(type2cumResps()$F,type2cumResps()$H),2)),
      x = "p(conf|incorrect)", y="p(conf|correct)")+
      geom_abline(slope=1,linetype='dotted') + theme_bw() +
      geom_rect(data=type2cumResps(), mapping=aes(xmin=0, xmax=1, ymin=0, ymax=1), color="black", alpha=0.5, fill = NA)+
      theme(legend.position = "none")+
      theme(plot.title = element_text(size=11))
    
    
    plot_grid(ROC1, ROC2, nrow=2)
  })
  
  output$plot2 <- renderPlot({
    dist2 <- ggplot(data.frame(x=c(0, 2)), aes(x)) +
      stat_function(fun = dnorm_segment, args = list(c = input$metac, mean = input$metad, factor1 = miss_factor(), factor2 = hit_factor()), xlim= c(-5,5),
                    n=1000, color = blue, geom = "area", fill = blue, size = 1, alpha = 0.5) +
      stat_function(fun = dnorm_segment, args = list(c = input$metac, factor1 = CR_factor(), factor2 = FA_factor()), xlim= c(-5,5),
                    n=1000,color = red, geom = "area", fill = red, size = 1, alpha = 0.5) +
      geom_vline(xintercept = metac_vec(),alpha=0.5) +geom_vline(xintercept = input$metac,size=0.8) +
      xlim(-5,5) + labs(x="Strength of Evidence", y = "Density") 
    sample2 <- ggplot(data=sample2(),aes(x=response,y=count,fill=condition))+geom_bar(stat="identity",color="black",position=position_dodge())+
      scale_fill_manual(values=c(red,blue))+theme(legend.position="none")
    plot_grid(dist2, sample2, nrow=2)
    
  })
  
  
  output$ROCplot2 <- renderPlot({
      ROC1 <- ggplot(data=metacumResps(), aes(x=F,y=H,group=1))+ 
      geom_line() + geom_point() + coord_fixed(ratio=1) +  labs(title=paste("ROC curve\nM ratio: ", round(input$metad/input$meanSignal,2),
                                                                            "\nAUC:", -round(trapz(metacumResps()$F,metacumResps()$H),2)),
                                                                x = "False Alarms", y="Hits")+
      geom_point(aes(x=pnorm(input$c, lower.tail = FALSE ), 
                     y=pnorm(input$c,input$meanSignal, lower.tail = FALSE)),size=5) +
      geom_abline(slope=1,linetype='dotted') + theme_bw() +
      geom_rect(data=cumResps(), mapping=aes(xmin=0, xmax=1, ymin=0, ymax=1), color="black", alpha=0.5, fill = NA)+
      theme(plot.title = element_text(size=11))
      ROC2 <- ggplot(data=metatype2cumResps(), aes(x=F,y=H))+ 
      geom_line() + geom_point() + coord_fixed(ratio=1) + 
      labs(title=paste("Type-2 ROC\nAUC2:", round(trapz(metatype2cumResps()$F,metatype2cumResps()$H),2)),
           x = "p(conf|incorrect)", y="p(conf|correct)")+
      geom_abline(slope=1,linetype='dotted') + theme_bw() +
      geom_rect(data=metatype2cumResps(), mapping=aes(xmin=0, xmax=1, ymin=0, ymax=1), color="black", alpha=0.5, fill = NA)+
      theme(legend.position = "none", plot.title = element_text(size=11))
    
      plot_grid(ROC1, ROC2, nrow=2)
  })
  
}



shinyApp(ui, server)