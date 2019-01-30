library(shiny)

runApp(display.mode = "showcase",
       list(
            ui=shinyUI(
              fluidPage(
                titlePanel(span("Gráfico de barras e setores")),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = "file" , label = 
                                'Use o botão "Browse" para localizar o arquivo com extensão
                              .txt (bloco de notas) que contém os dados.'),
                    checkboxInput(inputId = 'header', label = 'Seus dados tem cabeçalho? Se sim, escolha essa opção.', value = FALSE),
                    br(),
                    selectInput("paula","Escolha um tipo de gráfico:",
                                c("Barras"="Barras","Setores"="Setores")),
                    
                    uiOutput("variavel"),
                    
                    selectInput(inputId = "Cor", 
                                label=span("Escolha uma cor para o gráfico:"), 
                                choices = colors(),
                                selected = "yellow")
                    ),
                  
                  mainPanel(
                    plotOutput("grafico"))
                )
              )),
            server=shinyServer(
              function(input, output) {
                dados <- reactive({
                  file1 <- input$file
                  if(is.null(file1)){return(NULL)}
                  read.table(file=file1$datapath, sep = input$sep,header=input$header)
                })
                
                output$variavel <- renderUI({
                  df <- dados()
                  if (is.null(df)) return(NULL)
                  items=names(df)
                  names(items)=items
                  selectInput("variavel","Escolha uma variável:",
                              items,
                              multiple=FALSE)
                })
                
                library(plotrix)
                output$grafico<- renderPlot({
                  df <- dados()
                  dados<- data.frame(df)
                  if (input$paula=="Barras") barplot(table(dados[,input$variavel]), 
                                                     main=paste("Gráfico de barras da variável", input$variavel),
                                                     ylim = c(0,1.2*max(table(dados[,input$variavel]))),
                                                     col=input$Cor, xlab=input$variavel, ylab="Frequência")
                  else pie(table(dados[,input$variavel]), radius = 0.9, main=paste("Gráfico de setores da variável", input$variavel))
                })
                
              })
              ))

