library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)         # enable and disable action button function is here
library(clipr)           # To copy and paste data as dataset
library(purrr)           # is_empty function is in this package
library(ggplot2)         # ggplot2 
library(gridExtra)       # making grid for multiple plots
library(stringr)         # string wrap function is here str_wrap
library(dplyr)           # select functions are covered in the library
library(readr)           # clipboard() function is here

#Button formatting function
#################################################################################
#style function for Action button default 50px and width 180px; you can change
#################################################################################
styleButtonBlue<- function(xheight="50px",xwidth="180px",xcolor='#4682B4'){
  paste("white-space: normal;
  text-align:center;
  color: #ffffff;
        background-color:",xcolor,";",
        "border-color: #ffffff;
        border-width:2px;
        height:",xheight,";
        width:",xwidth,";
        font-size: 11px;")
}

# Not In Negate function
'%!in%' <- Negate('%in%')  # define 'not in' function



#################################################################################
# function to convert number to percentage, result will be a character
#################################################################################
fnpercentZeroDigit <- function(x, digits = 0, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

fnpercent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}



fnDropdownGear <- function(){
  tags$h2("Dropdown Button")
  br()
  dropdown(
    tags$h4("Import Datasets - Two Options"),
    
    # Input: Select a file ----
    tags$hr(style="height:2px;border-width:0;color:gray;background-color:steelblue"),
    fileInput("file",
              label = "Select: csv,txt, xls, xlsx, rds ",
              multiple = FALSE,
              accept = c("text/csv/txt/Excel",
                         "text/comma-separated-values,text/plain/excel",
                         ".csv",".txt",".xls",".xlsx",".rds")),
    div(style = "margin-top:-10px"),
    actionLink(inputId = "mpasteDataset",label = HTML(paste("Another Option","<br>", "Paste Dataset from Excel..!"))),
    HTML(paste('<h6><i>',"click outside to close..!",'<h5></i>')),
    style = "unite", icon = icon("cog"),
    status = "danger", width = "300px",
    animate = animateOptions(
      enter = animations$fading_entrances$fadeInLeftBig,
      exit = animations$fading_exits$fadeOutRightBig
    )
  )
}




####################################
#UI starts here
####################################

ui <- fluidPage(
  useShinyalert(),
  box(
    width = 12,
    height = '525px',
    title ="Mean   ::   Standard Devision   ::   Confidence Interval with Variables Grouped",
    align='center',
    column(style = "border: 4px double red;height: 500px;overflow-y: auto;",
           width = 4,
           br(),
           fnDropdownGear(),
           br(),
           
           tags$head(
             tags$style("#mGroupVar ~ .selectize-control .option:nth-child(odd) {background-color: #a4dded;}
                        #mGroupVar ~ .selectize-control .option:nth-child(even) {background-color: #e8f4f8;}"
             )),
           
           tags$head(
             tags$style("#mSubGroupVar ~ .selectize-control .option:nth-child(odd) {background-color: #a4dded;}
                        #mSubGroupVar ~ .selectize-control .option:nth-child(even) {background-color: #e8f4f8;}"
             )),
           tags$head(
             tags$style("#mStatVariable ~ .selectize-control .option:nth-child(odd) {background-color: #a4dded;}
                        #mStatVariable ~ .selectize-control .option:nth-child(even) {background-color: #e8f4f8;}"
             )),
           selectInput(inputId = 'mGroupVar',label = 'Group Variable',choices = NULL,selected=NULL,multiple = FALSE),
           awesomeCheckbox(inputId = "mSubGroupRequired",label = "Check if Sub-Group Required", value = TRUE,status = "danger"),
           selectInput(inputId = 'mSubGroupVar',label = 'Sub-Group Variable',choices = NULL,selected=NULL,multiple = FALSE),
           div(style = "margin-top:-15px"),
           selectInput(inputId = 'mStatVariable',label = 'Reporting Variable',choices = NULL,multiple = FALSE),
           splitLayout(cellWidths = c('33%','33%','33%'),
                       actionButton(inputId = 'mMeanSDShowTableBtn',label = 'Mean & SD Table',style=styleButtonBlue(xheight = '50px',xwidth = '90px')),
                       actionButton(inputId = 'mMeanSDShowPlotBtn',label = 'Mean & SD Plot',style=styleButtonBlue(xheight = '50px',xwidth = '90px')),
                       actionButton(inputId = 'mCiPlotBtn',label = 'Mean & CI Plot',style=styleButtonBlue(xheight = '50px',xwidth = '90px'))
           ),
           br(),
           HTML(paste('<h5><i>',"Dataset should have minimum of one character and one numeric variables.",  "
                      Good to have atlest two character variable to select as Group and Sub-group",'<h5></i>'))
           ),

    column(style = "border: 4px double red;height: 500px;overflow-y: auto;",
           width = 8,
           align='center',
           tags$head(
             tags$style(
               paste0("#mDtTableMeanSd{color:black; font-size:14px; font-style:bold;text-align: justify; margin: 5px 5px 5px 5px;
                                                      width: '100%';height: 425px;max-height: 425px; background: #ffffff;}")
             )
           ),
           uiOutput(outputId = 'mshowMeanSDTableORPlot')
    ) #column Closure
  ) #box closure
  
  
)


####################################
#Server starts here
####################################

server <- function(input, output, session) {
  vmy <- reactiveValues(mydata=NULL,zz=NULL)
  clipr::clear_clip()
  
  #This option would increase the memorary limit to 30MB facilitating to import big dataset
  options(shiny.maxRequestSize=30*1024^2) 
  
  #This option prevents printing scientific notation
  options(scipen=999)  

  
  ####################################
  # importing file into shiny app
  ####################################
  observeEvent(input$file,{
    #### file import code start
    ext <- tools::file_ext(input$file$name)
    
    if (ext != "csv" & ext !='rds' & ext != 'xlsx' & ext != 'xlsx'){
      shinyalert("Oops!", "valid files are csv, txt, excel and rds only", type = "error")
      return()
    }
    else if (ext == "rds"){
      vmy$mydata <- as.data.frame(readRDS(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "xls" || ext == 'xlsx'){
      vmy$mydata <- as.data.frame(readxl::read_excel(input$file$datapath))  # got from https://readxl.tidyverse.org/
    }
    else if (ext == "csv" || ext == 'txt'){
      tryCatch({            #avoid rscript showing error initially before execution,Skipping error in for-loop ::I got from this site; Thank you:https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
        Comma = ","
        Double_Quote = '"'
        
        vmy$mydata <- as.data.frame(
          read.csv(input$file$datapath,
                   header = TRUE,
                   sep = Comma,
                   quote = Double_Quote
          )
        )
      },error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))})
    }
    #### delete NA rows code start
    vmy$mydata <- na.omit(vmy$mydata)
    vmy$mydata <- vmy$mydata[complete.cases(vmy$mydata), ]
    mNoofCharacterVar <- length(names(dplyr::select_if(vmy$mydata,is.character)))
    
    if (mNoofCharacterVar==0 ){
      shinyalert("Oops!", "Dataset should have one or more character variables", type = "error")
      return()
    }
    
    updateSelectInput(session,inputId = 'mGroupVar',choices = c("Select variable",names(dplyr::select_if(vmy$mydata,is.character))),selected = NULL)
    updateSelectInput(session,inputId = 'mSubGroupVar',choices = c("Select variable",names(dplyr::select_if(vmy$mydata,is.character))),selected = NULL)
    updateSelectInput(session,inputId = 'mStatVariable',choices = names(dplyr::select_if(vmy$mydata,is.numeric)),selected = NULL)
      
    #### file import code End
  })
  

  observeEvent(input$mGroupVar,{
    if (length(input$file)==0){
      return()
    }
    if (input$mSubGroupVar != "Select variable" & input$mGroupVar==input$mSubGroupVar){
      shinyalert(title = 'Error..!',text = 'Group and Sub-group cannot be same Variable')
      return()
    }
    n <- which( colnames(vmy$mydata)==input$mGroupVar)
    updateSelectInput(session,inputId = 'mSubGroupVar',choices = c("Select variable",names(dplyr::select_if(vmy$mydata[-n],is.character))),selected = NULL)
    })


  observeEvent(input$mpasteDataset,{
    tryCatch(
      {
        if (clipboard()==""){
          shinyalert(title = "Oops!",text = "Nothing in the Clipboard, you have not copied the table..!", type = "error")
          return()
        }
        if (length(read_clip_tbl())==1){
          shinyalert(title = "Oops!",text = "Data copied might not be dataset or it has only one column. You should have minimum two columns. Please Check...!", type = "error")
          return()
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    ask_confirmation(
      inputId = "myconfirmation",text = "Do you really want to load new dataset ?",
      title = "Confirm...!"
    )
    
  })
  
  
  read.excel <- function(header=TRUE,...) {
    read.table("clipboard",sep="\t",header=header,...)
  }
  observeEvent(input$myconfirmation, {
    if (isTRUE(input$myconfirmation)) {
      vmy$mydata <- read_clip_tbl(x = read_clip(),header=TRUE, sep = "\t")
      updateSelectInput(session,inputId = 'mGroupVar',choices = c("Select variable",names(dplyr::select_if(vmy$mydata,is.character))),selected = NULL)
      updateSelectInput(session,inputId = 'mSubGroupVar',choices = c("Select variable",names(dplyr::select_if(vmy$mydata,is.character))),selected = NULL)
      updateSelectInput(session,inputId = 'mStatVariable',choices = names(dplyr::select_if(vmy$mydata,is.numeric)),selected = NULL)
    } 
    else{
      return()
    }
  }, ignoreNULL = TRUE)
  
  
  
  
  #####################################################################################
  # From here Mean and SD Table
  #####################################################################################
  observeEvent(input$mSubGroupRequired,{
    if (input$mSubGroupRequired==FALSE){
      disable("mSubGroupVar")
    }else{
      enable("mSubGroupVar")
    }
  })
  
  observeEvent(input$mMeanSDShowTableBtn,{
    if (input$mSubGroupVar != "Select variable" & input$mGroupVar==input$mSubGroupVar){
      shinyalert(title = 'Error..!',text = 'Group and Sub-group cannot be same Variable')
      return()
    }
    output$mshowMeanSDTableORPlot <- renderUI({
      column(
        width = 12,
        HTML(paste('<h5><b>',"Mean and Standard Deviation for",input$mStatVariable,'<h5></b>')),
        DT::dataTableOutput('mDtTableMeanSd')
      )
    })
    
    fnzzprocessforMeanSD()
    output$mDtTableMeanSd <- DT::renderDataTable({
      st <- vmy$zz %>%
        group_by(vmy$zz['Group']) %>%
        mutate(count=1:n()) %>%
        ungroup %>%
        mutate(Group = ifelse(count==1, as.character(Group), NA)) %>%
        dplyr::select(-count)
      
      st['Group'][is.na(st['Group'])] <- ""
      st$percent <- fnpercent(st$percent,digits = 1)
      
      st$mean <- paste(sprintf('%.2f',st$mean),"\u00B1",sprintf('%.2f',st$sd))
      
      if (input$mSubGroupRequired ==FALSE){
        st<- st[-3]
      }else{
        st<- st[-4]
      }
      
      n<- ncol(st)-1
      DT::datatable(st,caption =NULL,
                    #class ='cell-border stripe compact white-space: nowrap', #::I got from this site; Thank you this multiple classes: https://rstudio.github.io/DT/
                    escape=FALSE,
                    editable = FALSE,
                    rownames = FALSE,
                    selection = list(mode = "single", target = 'row'),
                    options = list(#scrollY = '300px',
                      dom = 't',
                      ordering=FALSE,
                      pageLength = -1,
                      class="compact",
                      autoWidth = FALSE,
                      columnDefs = list(list(width ='25px', targets = c(0:n)),
                                        list(className = 'dt-center', targets = c(0:n))
                      )
                    )
                    
      )%>% 
        DT::formatStyle( columns=names(st), target= 'row',color = 'black', 
                         backgroundColor = '#ffffed',
                         fontWeight ='normal','font-size' = '11px',lineHeight='75%')
    })
  })
  
  
  
  #####################################################################################
  # From here Mean and SD Plot
  #####################################################################################
  observeEvent(input$mMeanSDShowPlotBtn,{
    if (input$mSubGroupVar != "Select variable" & input$mGroupVar==input$mSubGroupVar){
      shinyalert(title = 'Error..!',text = 'Group and Sub-group cannot be same Variable')
      return()
    }
    output$mshowMeanSDTableORPlot <- renderUI({
      if (input$mSubGroupRequired ==FALSE){
        mheight = 250
        mwidth = 650
      }else{
        nn <- length(unique(vmy$mydata[,input$mGroupVar]))
        mheight = nn *250
        mwidth = 650
      }
      column(
        width = 12,
        HTML(paste('<h5><b>',"Mean and Standard Deviation Plot for",input$mStatVariable,'<h5></b>')),
        plotOutput(outputId = 'mMeanandSDPlotUI', height = paste0(mheight,"px"),width = paste0(mwidth,"px"))
        
      )
    })
    
    fnzzprocessforMeanSD()
    
    if (input$mSubGroupRequired ==FALSE){
      vmy$zz[,'sd'] <- ifelse(is.na(vmy$zz[,'sd'])==TRUE,0,vmy$zz[,'sd'])
      vmy$zz['freq'] <- vmy$zz[,'Group']
      vmy$zz[,'Group'] <- ""
      vmy$zz['low'] <- vmy$zz$mean-vmy$zz$sd
      vmy$zz['up'] <- vmy$zz$mean+vmy$zz$sd     
    }else{
      vmy$zz[,'sd'] <- ifelse(is.na(vmy$zz[,'sd'])==TRUE,0,vmy$zz[,'sd'])
      colnames(vmy$zz)[2] <- 'freq'
      vmy$zz['low'] <- vmy$zz$mean-vmy$zz$sd
      vmy$zz['up'] <- vmy$zz$mean+vmy$zz$sd
    }
    
    output$mMeanandSDPlotUI <- renderPlot({
      fnPlotOutput(vmy$zz)  
    })
  })
  
  
  
  
  #####################################################################################
  # From here Mean and Confidence Interval Plot
  #####################################################################################
  observeEvent(input$mCiPlotBtn,{
    if (input$mSubGroupVar != "Select variable" & input$mGroupVar==input$mSubGroupVar){
      shinyalert(title = 'Error..!',text = 'Group and Sub-group cannot be same Variable')
      return()
    }
    output$mshowMeanSDTableORPlot <- renderUI({
      if (input$mSubGroupRequired ==FALSE){
        mheight = 250
        mwidth = 650
      }else{
        nn <- length(unique(vmy$mydata[,input$mGroupVar]))
        mheight = nn *250
        mwidth = 650
      }
      column(
        width = 12,
        HTML(paste('<h5><b>',"Mean and Confidence Interval Plot for ",input$mStatVariable,'<h5></b>')),
        plotOutput(outputId = 'mCiPlotUI', height = paste0(mheight,"px"),width = paste0(mwidth,"px"))
      )
    })
    
    fndfCiProcessforCiPlot()
    
    output$mCiPlotUI <- renderPlot({
      fnPlotOutput(vmy$dfCI)
    })
  })
  
  
  
  fnzzprocessforMeanSD <- function(){
    set.seed(123)
    assign("Group",input$mGroupVar)
    
    if (input$mSubGroupRequired ==FALSE){
      xx <- data.frame(vmy$mydata[input$mGroupVar],vmy$mydata[input$mStatVariable])
      colnames(xx)[1] <- c('Group')
      yy <- xx %>% group_by(xx['Group']) %>% dplyr::summarise_each(funs(mean, sd))
      ww <- xx %>% group_by(xx['Group']) %>% tally()
      ss <- xx %>% group_by(xx['Group']) %>% tally()
      zz <- data.frame(yy,observations=ww$n)
      joined <- dplyr::left_join(zz, ss, by = "Group")    # where you got: https://stackoverflow.com/questions/51328147/looking-up-values-from-another-dataframe-in-r
      joined['percent'] <- joined$observations/joined$n
      vv <-joined[-5]
    }else{
      xx <- data.frame(vmy$mydata[input$mGroupVar],vmy$mydata[input$mSubGroupVar],vmy$mydata[input$mStatVariable])
      colnames(xx)[1] <- c('Group')
      yy <- xx %>% group_by(xx['Group'],xx[input$mSubGroupVar]) %>% dplyr::summarise_each(funs(mean, sd))
      ww <- xx %>% group_by(xx['Group'],xx[input$mSubGroupVar]) %>% tally()
      ss <- xx[-2] %>% group_by(xx['Group']) %>% tally()
      zz <- data.frame(yy,observations=ww$n)
      joined <- dplyr::left_join(zz, ss, by = "Group")    # where you got: https://stackoverflow.com/questions/51328147/looking-up-values-from-another-dataframe-in-r
      joined['percent'] <- joined$observations/joined$n
      vv <-joined[-6]
    }
    
    vv$mean <- round(vv$mean,2)
    vv$sd  <- round(vv$sd,2)
    vmy$zz <- vv
    
  }
  
  
  
  fndfCiProcessforCiPlot <- function(){
    if (input$mSubGroupRequired ==FALSE){
      xx <- data.frame(Group="",freq=vmy$mydata[input$mGroupVar],values=vmy$mydata[input$mStatVariable])
      colnames(xx) <- c("Group",'freq','values')
    }else{
      xx <- data.frame(Group=vmy$mydata[input$mGroupVar],freq=vmy$mydata[input$mSubGroupVar],values=vmy$mydata[input$mStatVariable])
      colnames(xx) <- c("Group",'freq','values')
    }
    
    dfCI <- data.frame(Group = as.character(),
                       freq = as.character(),
                       mean = as.numeric(),
                       low = as.numeric(),
                       up = as.numeric()
    )
    
    colnames(xx) <- c("Group",'freq','values')
    
    set.seed(123)
    mgrpnames <- unique(xx$Group)
    for (g in mgrpnames){
      dd <- xx%>%subset(Group == g)
      for (i in unique(dd$freq)){
        ee <- dd%>%subset(freq == i)
        
        # Calculate the mean and standard error
        model <- lm(values ~ 1, ee)
        
        # Find the confidence interval
        cc<-confint(model, level=0.95)
        dfCI <- dfCI%>%add_row(
          Group = g,
          freq = i,
          mean = mean(ee$values,na.rm = T),
          low = cc[[1]],
          up = cc[[2]]
        )
      }
    }
    vmy$dfCI  <- dfCI[order(dfCI$Group, dfCI$freq), ] 
  }
  
  
  fnPlotOutput <- function(zz){
    library(gridExtra)
    library(ggplot2)
    
    mgrp <- unique(zz$Group)
    n <- length(mgrp)
    r <- n
    c <- 1
    par(mfrow=c(r,c)) #get in 3 rows and 3 columns (first one is row)
    my_plots <- list()
    
    par(mar = c(3, 3, 3, 3))
    
    theme_set(
      theme_grey(12)
    )
    
    for (i in 1:n){
      dd <- dplyr::filter(zz,Group==mgrp[i])
      Charttitle <- mgrp[i]
      yUpylim <- ceiling(max(dd['up'])*1.10)
      yLowylim <- floor(min(dd['low'])/1.10)
      
      pl2 <- ggplot(data = dd) 
      pl2 <- pl2 + geom_line(aes(x=freq, y=mean), group = 1)
      pl2 <- pl2 + geom_point(aes(x=freq, y=mean), color= "black")
      pl2 <- pl2 + geom_errorbar(aes(x=freq, ymin=low, ymax= up), width = 0.05, color ="red", size = 0.5)
      pl2 <- pl2 + geom_text(aes(x=freq, y=low, label = round(low,1)), size= 4, vjust = 1.8)
      pl2 <- pl2 + geom_text(aes(x=freq, y=up, label = round(up,1)), size= 4, vjust = -1)
      pl2 <- pl2 + geom_text(aes(x=freq, y=mean, label = round(mean,1)), size= 4, vjust = -0.2,hjust=-0.2)
      pl2 <- pl2 + theme_classic()
      pl2 <- pl2 + labs(title = Charttitle) 
      pl2 <- pl2 + ylim(yLowylim,yUpylim)
      pl2 <- pl2 + theme(
        plot.title = element_text(color="black", size=14, face="bold"),   
        axis.title.y = element_text(color="black", size=11, face="bold"),
        axis.title.x = element_blank(),
        axis.text=element_text(size=11)
      )
      pl2 <- pl2 +scale_x_discrete(labels = function(x) str_wrap(x, width = 14))
      my_plots[[i]] <- pl2
      
    }
    grid.arrange(grobs =my_plots,nrow=r, ncol=c)
  }
  
  
  
  
} # Server Closure

shinyApp(ui, server)