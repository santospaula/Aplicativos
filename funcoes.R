library(shiny)

runApp(display.mode = "showcase",
       list(choi <- c("Linear"="Linear",
                      "Quadratica"="Quadratica",
                      "Exponencial"="Exponencial",
                      "Modular"="Modular",
                      "Logaritmica"="Logaritmica"),
            
            ui=shinyUI(
              fluidPage(
                titlePanel(span("Estudo de funções")), 
                sidebarPanel(
                  selectInput(inputId="funcao",
                              label="Escolha uma função:",
                              choices=choi),
                  uiOutput("ui"),
                  
                  selectInput(inputId = "Cor", 
                              label=span("Escolha uma cor para o gráfico:"), 
                              choices = colors(),
                              selected = "blue")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Gráfico", plotOutput("plot")),
                    tabPanel("Atividades propostas",
                             h5("Questão 1: Construa o gráfico das seguintes funções e classifique-as em crescente ou decrescente:"),
                             h5("a) y = 5x + 8"),
                             h5("b) y = x + 2"),
                             h5("c) y = -3 - x"),
                             h5("d) y = 9 + 3x"),
                             h5("e) y = -3x"),   
                             h5("Questão 2: Construa o gráfico da função definida por f(x) = 5x - 3 pelo aplicativo de função. A partir dele, responda as questões abaixo:"),
                             h5("a) Verifique se a funço é crescente ou decrescente."),
                             h5("b) O zero da função."),
                             h5("c) O ponto onde a função intersecta o eixo y."),
                             h5("Questão 3: Seja a função f(x) = 2x + 1. Construa o gráfico de f(x) no aplicativo. A seguir, execute cada comando e anote o que acontece com o gráfico de f(x)."),
                             h5("a) Multiplique f por -1"),
                             h5("b) Multiplique f por 2"),
                             h5("c) Subtraia 1 em h"),
                             h5("d) Subtraia 2 em h"),
                             h5("e) Some 3 em h ")
                             
                    )
                  )
                  
                )
              )
            ),
            server=shinyServer(
              function(input, output){
                output$ui <- renderUI({
                  if(is.null(input$funcao)){
                    return()}
                  switch(input$funcao, 
                         "Linear"={
                           output$plot <- renderPlot({
                             curve(input$a*x + input$b,ylim=c(-10,10),xlim=c(-10,10),
                                   lwd=2,col=input$Cor,ylab="f(x)", main=expression(paste("y = f(x) = ", a*x+b)))
                             abline(v=-10:10,h=-10:10,col="gray", lwd=1,lty=2);  abline(v=0,col="black", lwd=2,lty=2);abline(h=0,col="black", lwd=2,lty=2)
                           })
                           wellPanel(sliderInput("a","Valor de a",min = -10,max = 10,step = 0.5,value = 1),
                                     sliderInput("b","Valor de b",min = -10,max = 10,step = 0.5,value = 1),
                                     helpText("Lembre-se:"),
                                     helpText("a e b são números reais."),
                                     helpText("Se a=0 temos uma função constante.")) },
                         
                         
                         "Quadratica"={
                           output$plot <- renderPlot({
                             curve(input$a*x^2 + input$b*x + input$c,ylim=c(-10,10),xlim=c(-10,10),
                                   lwd=2,col=input$Cor,ylab="f(x)", main=expression(paste("y = f(x) = ", a*x^2+b*x+c)))
                             abline(v=-10:10,h=-10:10,col="gray", lwd=1,lty=2);abline(v=0,col="black", lwd=2,lty=2);abline(h=0,col="black", lwd=2,lty=2)
                           })
                           wellPanel(sliderInput("a","Valor de a",min = -10,max = 10,step = 0.5,value = 1),
                                     sliderInput("b","Valor de b",min = -10,max = 10,step = 0.5,value = 1),
                                     sliderInput("c","Valor de c",min = -10,max = 10,step = 0.5,value = 1),
                                     helpText("Lembre se:"),
                                     helpText("a, b e c são números reais."),
                                     helpText("Além de a ser diferente de zero.")) },
                         
                         "Exponencial"={
                           output$plot <- renderPlot({
                             curve(input$a^x,ylim=c(-10,10),xlim=c(-10,10),
                                   lwd=2,col=input$Cor,ylab="f(x)",  main=expression(paste("y = f(x) = ", a^x)))
                             abline(v=-10:10,h=-10:10,col="gray", lwd=1,lty=2);abline(v=0,col="black", lwd=2,lty=2);abline(h=0,col="black", lwd=2,lty=2)
                           })
                           wellPanel(sliderInput("a","Valor de a",min = -10,max = 10,step = 0.1,value = 1),
                                     helpText("Lembre-se:"),
                                     helpText("a deve ser um número real maior que zero e diferente de 1.")) },
                         
                         "Modular"={
                           output$plot <- renderPlot({
                             curve(input$a*abs(x),ylim=c(-10,10),xlim=c(-10,10),
                                   lwd=2,col=input$Cor,ylab="f(x)", main=expression(paste("y = f(x) = ", a*group("|", x, "|"))))
                             abline(v=-10:10,h=-10:10,col="gray", lwd=1,lty=2);abline(v=0,col="black", lwd=2,lty=2);abline(h=0,col="black", lwd=2,lty=2)
                           })
                           wellPanel(sliderInput("a","Valor de a",min = -10,max = 10,step = 0.5,value = 1),
                                     helpText("Lembre-se:"),
                                     helpText("a deve ser um número real diferente de zero.")) },
                         
                         "Logaritmica"={
                           output$plot <- renderPlot({
                             curve(log(x,input$a),ylim=c(-10,10),xlim=c(-10,10),
                                   lwd=2,col=input$Cor,ylab="f(x)",  main=expression(paste("y = f(x) = " (log[a]*x) )))
                             abline(v=-10:10,h=-10:10,col="gray", lwd=1,lty=2);abline(v=0,col="black", lwd=2,lty=2);abline(h=0,col="black", lwd=2,lty=2)
                           })
                           wellPanel(sliderInput("a","Valor de a",min = -10,max = 10,step = 0.5,value = 1),
                                     helpText("Lembre-se:"),
                                     helpText("O valor de a deve ser um número real maior que zero e diferente de 1."))
                         })
                })
                
                
              }
            ))
       )

