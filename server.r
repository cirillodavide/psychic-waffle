# install and launch required packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "xlsx", "xtable", "crosstalk", "DT")

library(shiny)
library(xlsx)
library(xtable)
library(DT)

  shinyServer(function(input, output, session) {

    # xlsx input
    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data <- read.xlsx(inFile$datapath,sheetName="Sheet1",header=TRUE) # sheetName must be "Sheet1"
      colnames(data)[1]<-"TIME" # just in case the name differs...
      data
      })

    # check boxes
    output$choose_columns <- renderUI({
      data_sets <- myData()
      if(is.null(data_sets)) return()
      colnames <- colnames(data_sets)[-1]
      checkboxGroupInput("columns", "Choose columns", choices  = colnames)
      })

    # data table
    
    selectedData <- reactive({

      # average log of selected columns
      data_sets <- myData()
      if(is.null(input$columns)) return()
      data_sets <- na.omit(data_sets[,c("TIME",input$columns), drop=FALSE])
      x<-data_sets$TIME
      y<-rowMeans(log(data_sets[,-1, drop=FALSE]))
      signif(data.frame(x,y),3)
      })

    derivateData <- reactive({

      # smoothed derivative of curve
      set.seed(123)
      selectedData <- selectedData()
      if (is.null(selectedData)) return(NULL)
      x <- selectedData$x
      y <- selectedData$y
      mid <- x
      strike <- y
      deriv <- function(x, y) diff(y) / diff(x)
      middle_pts <- function(x) x[-1] - diff(x) / 2
      second_d <- deriv(middle_pts(mid), deriv(mid, strike))
      smooth_second_d <- smooth.spline(second_d ~ middle_pts(middle_pts(mid)))
      n <- input$wind
      smooth_second_d <- data.frame(y=filter(second_d , rep(1/n, n)), x=filter(middle_pts(middle_pts(mid)) , rep(1/n, n)))
      dx <- smooth_second_d$x
      dy <- smooth_second_d$y
      na.omit(data.frame(dx, dy))
      })

    linearRegions <- reactive({

      # splopes of linear intervals
      selectedData <- selectedData()
      if (is.null(derivateData)) return(NULL)
      derivateData <- derivateData()
      if (is.null(derivateData)) return(NULL)

      slope <- vector()
      up <- input$zero
      down <- -input$zero
      x = selectedData$x
      y = selectedData$y
      xx = which(derivateData$dy<up & derivateData$dy>down)
      start = c(1, which(diff(xx) != 1 & diff(xx) != 0) + 1)
      end = c(start - 1, length(xx))
      x1<-x[xx[start]]
      x2<-x[xx[end]]
      for(i in 1:length(x1)){
        new.x <- subset(x, x>=x1[i] & x<=x2[i])
        new.y <- y[which(x%in%new.x)]
        fitmodel <- lm(new.y~new.x)
        slope <- append(slope, signif(fitmodel$coefficients[2],3))
      }
      span <- signif(x2-x1+1,3)
      df <-data.frame("start"=x1,"end"=x2, "slope"=slope, "span"=span)
      dff <- df[df$span>input$span,]
      dff[order(dff$slope, decreasing=TRUE),]
      })

    # O.D. range choice

    ODInterval <- reactive({
      selectedData <- selectedData()
      if (is.null(selectedData)) return(NULL)
      selectedData[selectedData$y<=input$od1 & selectedData$y>=input$od2,]
    })

    # display first plot

    output$w2 <- renderPlot({

      selectedData <- selectedData()
      if (is.null(selectedData)) return(NULL)
      ODInterval <- ODInterval()
      if (is.null(ODInterval)) return(NULL)
      
      print(max(ODInterval$x)-min(ODInterval$x)+1)

      par(mar = c(4, 4, 1, .1))
      plot(selectedData$x,selectedData$y,type='l', col='grey80', ylab="ln O.D.", xlab="time", lwd = 3)
      points(ODInterval$x, ODInterval$y, type='l', col='blue', lwd = 4)
      abline(h=input$od1,lty='dashed',col='grey')
      abline(h=input$od2,lty='dashed',col='grey')
      fitmodel <- lm(ODInterval$y~ODInterval$x)
      abline(fitmodel,lwd=1,lty='dashed', col=1)
      legend("topleft",paste(
        paste("Intercept = ",signif(fitmodel$coefficients[1],3),"\n"),
        paste("Slope = ",signif(fitmodel$coefficients[2],3),"\n"),
        paste("Doubling time = ",signif(log(2)/fitmodel$coefficients[2],3),"\n"),
        sep="\n"), bty="n")
      })

    # display first table

    output$x1 <- DT::renderDataTable(linearRegions(), 
      server = FALSE, 
      rownames= FALSE, 
      selection = list(mode = 'single', selected = 1), 
      options=list(bFilter=0), 
      caption="Linear regions (Time range)")

    selectedInterval <- reactive({
      input$x1_rows_selected
      })

    # display second plot

    output$x2 <- renderPlot({

      linearRegions <- linearRegions()
      if (is.null(linearRegions)) return(NULL)
      selectedData <- selectedData()
      if (is.null(selectedData)) return(NULL)
      derivateData <- derivateData()
      if (is.null(derivateData)) return(NULL)
      
      par(mar = c(4, 4, 1, .1))
      miny <- min(c(selectedData$y,derivateData$dy))
      maxy <- max(c(selectedData$y,derivateData$dy))
      plot(selectedData$x,selectedData$y,ylim=c(miny,maxy),type='l', col='blue', ylab="ln O.D.", xlab="time", lwd = 3)
      points(derivateData$dx,derivateData$dy,type='l',col=3)
      abline(h=0,col=1)
      abline(h=input$zero,lty='dashed',col=1)
      abline(h=-input$zero,lty='dashed',col=1)
      span<-linearRegions$span
      x1<-linearRegions$start
      x2<-linearRegions$end
      invisible(sapply(which(span>input$span),function(x) polygon( c(x2[x],x2[x],x1[x],x1[x]), c(miny,maxy,maxy,miny),col=rgb(0.75,0.75,0.75,0.2), border='grey')))
      s <- selectedInterval()
      if (is.null(s)) s <- 1
        x1<-linearRegions[s, , drop = FALSE]$start
        x2<-linearRegions[s, , drop = FALSE]$end
        polygon( c(x2,x2,x1,x1), c(miny,maxy,maxy,miny),col=rgb(0.25,0.40,0.88,0.2), border='grey')
      })

    # display third plot

    output$x3 <- renderPlot({

      linearRegions <- linearRegions()
      if (is.null(linearRegions)) return(NULL)
      selectedData <- selectedData()
      if (is.null(selectedData)) return(NULL)

      x = selectedData$x
      y = selectedData$y
      plot(x, y, type='l', col='blue', lwd=3, xlab="time", ylab="ln O.D.")
      range <- input$range
      new.x <- subset(x, x>=range[1] & x<=range[2])
      new.y <- y[which(x%in%new.x)]
      fitmodel <- lm(new.y~new.x)
      abline(fitmodel,lwd=1,lty='dashed')
      legend("topleft",paste(
        paste("Intercept = ",signif(fitmodel$coefficients[1],3),"\n"),
        paste("Slope = ",signif(fitmodel$coefficients[2],3),"\n"),
        paste("Doubling time = ",signif(log(2)/fitmodel$coefficients[2],3),"\n"),
        sep="\n"), bty="n")
      })

    # update slider values for second plot

    observe({
      
      linearRegions <- linearRegions()
      if (is.null(linearRegions)) return(NULL)
      
      s <- selectedInterval()
        x1<-linearRegions[s, , drop = FALSE]$start
        x2<-linearRegions[s, , drop = FALSE]$end
        updateSliderInput(session, "range",
          value = c(x1,x2))
    })

    # download data

    output$downloadData <- downloadHandler(
  
      filename = function() { paste('mydata.csv', sep='') },
      content = function(file) {
        ic <- paste(input$columns,collapse=";")
        df <- linearRegions()[selectedInterval(), , drop = FALSE]
        df <- cbind("Data"=ic,df,"CloseToZero"=input$zero,"SmoothWind"=input$wind,"SpanCutoff"=input$span)
        write.csv(df, file, row.names=FALSE)
    })

    })
