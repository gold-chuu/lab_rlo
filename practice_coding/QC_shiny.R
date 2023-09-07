library(shiny)
library(xtable)
library(tidyverse)
library(nord)
library(DT)

DATA <- read.csv("~/DATA_0411_v1.csv",header = T, fileEncoding = "euc-kr", stringsAsFactors = FALSE)
data_class <- as.data.frame(lapply(DATA, class))
data_all <- rbind(data_class,DATA)
chr <-DATA[which(data_all[1,] == 'character')]
num <-DATA[which(data_all[1,] != 'character')] 
num <- cbind(DATA[1:4],num)


ui <- fluidPage(
  #title panel 설정
  titlePanel("QC data"),
  
  #sidebarlayout 설정
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      
      #uploads contents
      
      conditionalPanel(condition = "input.tabs == 'contents'",
                       
                       # Input: Select a file ----
                       fileInput("file", "Choose CSV File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       
                       # Horizontal line ----
                       tags$hr(),
                       
                       # Input: Checkbox if file has header ----
                       checkboxInput("header", "Header", TRUE),
                       
                       # Input: Select separator ----
                       radioButtons("sep", "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = ","),
                       
                       # Horizontal line ----
                       tags$hr(),
                       
                       # Input: Select number of rows to display ----
                       radioButtons("disp", "Display",
                                    choices = c(Head = "head",
                                                All = "all",
                                                Character = 'chr',
                                                numeric = 'num'),
                                    selected = "all"),
                       
                       #content에서 table col 제외 기능
                       conditionalPanel(condition = 'input.disp == "all"',
                                        textInput("text","제외할 열 입력(대소문자 구분, sep ';')",
                                                  value = "enter the name of the col to exclude in table."))
      ),
      
      
      #num과 chr로 나뉜 data
      conditionalPanel(condition = "input.tabs == 'plot'",
                       
                       h2("Plot option"),
                       
                       #plot 설정
                       selectInput("select_plot","choose plot type",choices = list("point","box","violin","jitter")),
                       hr(),
                       
                       h2("Data option"),
                       #point plot
                       conditionalPanel(condition = "input.select_plot == 'point'",
                                        #x축 설정
                                        radioButtons('x_data_type','choose x data type :', choices = list("chr","num")),
                                        
                                        conditionalPanel(condition = "input.x_data_type == 'chr'",
                                                         #일단입력해준 data에서 col제외하고 select input 출력하는 ui만들기
                                                         selectizeInput("text_x","chr 목록에서 보고싶은 열 선택 : ",choices = names(chr), 
                                                                        selected = names(select(chr, contains('ID') | contains('name'))),multiple = TRUE),
                                                         selectInput("col_xc","select input",choices = names(chr))),
                                        
                                        
                                        conditionalPanel(condition = "input.x_data_type == 'num'",
                                                         selectInput(inputId = 'col_xn', 'choose x col:', choices =names(num[,-1:-4]))),
                                        br(),
                                        
                                        #y축 설정
                                        radioButtons("y_data_type","choose y data type : ", choices = list("chr","num")),
                                        conditionalPanel(condition = "input.y_data_type == 'chr'",
                                                         selectInput(inputId = 'col_yc','choose y col:', choices = names(chr),selected = "SEX")),
                                        conditionalPanel(condition = "input.y_data_type == 'num'",
                                                         selectInput(inputId = 'col_yn', 'choose y col:', choices =names(num[,-1:-4]))),
                                        br(),
                                        
                                        #plot에서 그리드 설정원하는지
                                        radioButtons("yn","grid 설정", c("yes","no"),selected = 'no'),
                                        
                                        #그리드 설정
                                        conditionalPanel(condition = "input.yn == 'yes'",
                                                         selectInput(inputId = "grid", label = "choose grid", choices = names(chr[,-1:-4]),selected = 'SEX'))),
                       
                       #boxplot
                       conditionalPanel(condition = "input.select_plot == 'box'",
                                        #x축 설정
                                        radioButtons('xb_data_type','choose x data type :', choices = list("chr","num")),
                                        
                                        conditionalPanel(condition = "input.xb_data_type == 'chr'",
                                                         #일단입력해준 data에서 col제외하고 selectinput 출력하는 ui만들기
                                                         selectizeInput("text_xb","chr 목록에서 보고싶은 열 선택 : ",choices = names(chr), 
                                                                        selected = names(select(chr, contains('ID') | contains('name'))) , multiple = TRUE),
                                                         selectInput("col_xbc","select input",choices = names(chr))),
                                        
                                        conditionalPanel(condition = "input.xb_data_type == 'num'",
                                                         selectInput(inputId = 'col_xbn', 'choose x col:', choices =names(num[,-1:-4]))),
                                        br(),
                                        
                                        #y축 설정
                                        radioButtons("yb_data_type","choose y data type : ", choices = list("chr","num")),
                                        conditionalPanel(condition = "input.yb_data_type == 'chr'",
                                                         selectInput(inputId = 'col_ybc','choose y col:', choices = names(chr),selected = "SEX")),
                                        conditionalPanel(condition = "input.yb_data_type == 'num'",
                                                         selectInput(inputId = 'col_ybn', 'choose y col:', choices =names(num[,-1:-4]))),
                                        br(),
                                        
                                        #plot에서 그리드 설정원하는지
                                        radioButtons("yn2","grid 설정", c("yes","no"),selected = 'no'),
                                        
                                        #그리드 설정
                                        conditionalPanel(condition = "input.yn2 == 'yes'",
                                                         selectInput(inputId = "fill", label = "choose grid", choices = names(chr[,-1:-4])))),
                       
                       #violin설정
                       conditionalPanel(condition = "input.select_plot == 'violin'",
                                        radioButtons('xv_data_type','choose x data type :', choices = list("chr","num")),
                                        
                                        conditionalPanel(condition = "input.xv_data_type == 'chr'",
                                                         #일단입력해준 data에서 col제외하고 selectinput 출력하는 ui만들기
                                                         selectizeInput("text_xv","chr 목록에서 보고싶은 열 선택 : ",choices = names(chr), 
                                                                        selected = names(select(chr, contains('ID') | contains('name'))) , multiple = TRUE),
                                                         selectInput("col_xvc","select input",choices = names(chr))),
                                        
                                        conditionalPanel(condition = "input.xv_data_type == 'num'",
                                                         selectInput(inputId = 'col_xvn', 'choose x col:', choices =names(num[,-1:-4]))),
                                        br(),
                                        
                                        #y축 설정
                                        radioButtons("yv_data_type","choose y data type : ", choices = list("chr","num")),
                                        conditionalPanel(condition = "input.yv_data_type == 'chr'",
                                                         selectInput(inputId = 'col_yvc','choose y col:', choices = names(chr),selected = "SEX")),
                                        conditionalPanel(condition = "input.yv_data_type == 'num'",
                                                         selectInput(inputId = 'col_yvn', 'choose y col:', choices =names(num[,-1:-4])))),
                       #jitter 설정
                       conditionalPanel(condition = "input.select_plot == 'jitter'",
                                        radioButtons('xj_data_type','choose x data type :', choices = list("chr","num")),
                                        
                                        conditionalPanel(condition = "input.xj_data_type == 'chr'",
                                                         #일단입력해준 data에서 col제외하고 selectinput 출력하는 ui만들기
                                                         selectizeInput("text_xj","chr 목록에서 보고싶은 열 선택 : ",choices = names(chr), 
                                                                        selected = names(select(chr, contains('ID') | contains('name'))) , multiple = TRUE),
                                                         selectInput("col_xjc","select input",choices = names(chr))),
                                        
                                        conditionalPanel(condition = "input.xj_data_type == 'num'",
                                                         selectInput(inputId = 'col_xjn', 'choose x col:', choices =names(num[,-1:-4]))),
                                        br(),
                                        
                                        #y축 설정
                                        radioButtons("yj_data_type","choose y data type : ", choices = list("chr","num")),
                                        conditionalPanel(condition = "input.yj_data_type == 'chr'",
                                                         selectInput(inputId = 'col_yjc','choose y col:', choices = names(chr),selected = "SEX")),
                                        conditionalPanel(condition = "input.yj_data_type == 'num'",
                                                         selectInput(inputId = 'col_yjn', 'choose y col:', choices =names(num[,-1:-4])))),
                       hr(),
                       h2("Plot size option"),
                       
                       sliderInput(inputId = "opt.height",
                                   label = "Figure size (height)",                            
                                   min = 100, max = 1500, step = 50, value = 500),
                       sliderInput(inputId = "opt.width",
                                   label = "Figure size (width)",                            
                                   min = 100, max = 1500, step = 50, value = 800)
      )),
    mainPanel(tabsetPanel(id="tabs",
                          tabPanel("contents",DT::dataTableOutput("contents")),
                          tabPanel("plot", plotOutput("plot")))
    )
  )
)

server <- function(input,output,session){
  
  #upload content page
  input_text <- reactive({input$text})
  
  output$contents <- DT::renderDataTable({
    DT::datatable({
      
      req(input$file)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file$datapath,
                         header = input$header,
                         sep = input$sep,
                         fileEncoding = "euc-kr", stringsAsFactors = FALSE)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
      
      if(input$disp == "head") {
        return(head(df))
      }
      else if (input$disp == 'chr') {
        return(df[which(as.data.frame(lapply(DATA,class))[1,] == 'character')])
      }
      else if (input$disp == 'num') {
        return(df[which(as.data.frame(lapply(DATA,class))[1,] != 'character')])
      }
      else {
        return(df %>% select( -one_of(str_split(input_text(),";") %>% unlist())))
      }
      
    })})
  
  
  #ui upadate 보기
  observe({
    x_name <- input$text_x
    
    # Can use character(0) to remove all choices
    if (is.null(x_name))
      x_name <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "col_xc",
                      label = paste("plot x축 설정 :  ", length(x_name)),
                      choices = x_name,
                      selected = tail(x_name, 1)
    )
    
    xb_name <- input$text_xb
    
    # Can use character(0) to remove all choices
    if (is.null(xb_name))
      xb_name <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "col_xbc",
                      label = paste("plot x축 설정 :  ", length(xb_name)),
                      choices = xb_name,
                      selected = tail(xb_name, 1)
    )
    
    xj_name <- input$text_xj
    
    # Can use character(0) to remove all choices
    if (is.null(xj_name))
      xj_name <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "col_xjc",
                      label = paste("plot x축 설정 :  ", length(xj_name)),
                      choices = xj_name,
                      selected = tail(xj_name, 1)
    )
    
    xv_name <- input$text_xv
    
    # Can use character(0) to remove all choices
    if (is.null(xv_name))
      xv_name <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "col_xvc",
                      label = paste("plot x축 설정 :  ", length(xv_name)),
                      choices = xv_name,
                      selected = tail(xv_name, 1)
    )
  })
  
  
  #plot page
  x<- reactive({
    switch(input$x_data_type,chr =input$col_xc, num= input$col_xn)})
  
  y<-reactive({
    switch(input$y_data_type,chr= input$col_yc, num = input$col_yn)})
  
  grid <- reactive({input$grid})
  
  col_xb <- reactive({
    switch(input$xb_data_type, chr =input$col_xbc, num = input$col_xbn)})
  
  col_yb <- reactive({
    switch(input$yb_data_type, chr =input$col_ybc, num = input$col_ybn)})
  
  fill<- reactive({input$fill})
  
  col_xv <- reactive({
    switch(input$xv_data_type, chr =input$col_xvc, num = input$col_xvn)})
  
  col_yv <- reactive({
    switch(input$yv_data_type, chr =input$col_yvc, num = input$col_yvn)})
  
  col_xj <- reactive({
    switch(input$xj_data_type, chr =input$col_xjc, num = input$col_xjn)})
  
  col_yj <- reactive({
    switch(input$yj_data_type, chr =input$col_yjc, num = input$col_yjn)})
  
  ratio_x <- reactive({input$opt.cex})
  
  #render plot
  
  output$plot <- renderPlot({
    if(input$select_plot == 'point'){
      if (input$yn == 'yes') {
        ggplot(DATA) + geom_point(aes_string(x=x(), y= y(),colour = y()) , na.rm = TRUE) + facet_grid(grid())
      }else{
        ggplot(DATA) + geom_point(aes_string(x=x(), y= y(),colour = y()) , na.rm = TRUE)
      }
    }else if (input$select_plot == 'box') {
      if (input$yn2 == 'yes') {
        ggplot(DATA,aes_string(x=col_xb(), y= col_yb(),fill = col_xb()), na.rm = TRUE) + geom_boxplot() +facet_grid(fill())
      }else{
        ggplot(DATA,aes_string(x=col_xb(), y= col_yb(),fill= col_xb()), na.rm = TRUE) + geom_boxplot() 
      }
    }else if (input$select_plot == 'violin') {
      ggplot(DATA, aes_string(x=col_xv(), y=col_yv(),fill=col_xv()))+ scale_fill_nord('aurora') + geom_violin()
      
    }else if (input$select_plot == 'jitter') {
      ggplot(DATA, aes_string(x=col_xj(), y=col_yj(),fill=col_xj())) +scale_fill_nord('baie_mouton') + geom_violin() + geom_jitter()
    }
  }, height = exprToFunction(as.numeric(input$opt.height)), width = exprToFunction(as.numeric(input$opt.width)))
}


shinyApp(ui,server)
