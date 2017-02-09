  library(shiny)
  library(xlsx)
  library(xtable)

  shinyServer(function(input, output) {

    # xlsx input
    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data <- read.xlsx(inFile$datapath,sheetName="Sheet1",header=TRUE) # sheetName must be "Sheet1"
      colnames(data)[1]<-"TIME" # just in case the name differs...
      data
      })

    # Check boxes
    output$choose_columns <- renderUI({
      data_sets <- myData()
      if(is.null(data_sets)) return()
      colnames <- colnames(data_sets)[-1] # every column but the first ("TIME")
      checkboxGroupInput("columns", "Choose columns", choices  = colnames)
      })

    # Graph optimal
    output$distPlot1 <- renderPlot({
      data_sets <- myData()
      if(is.null(input$columns)) return()

      data_sets <- na.omit(data_sets[,c("TIME",input$columns), drop=FALSE])

      x<-data_sets$TIME
      y<-log(data_sets[,-1, drop=FALSE])
      y<-rowMeans(y)

      set.seed(123)

      mid <- x
      strike <- y
      deriv <- function(x, y) diff(y) / diff(x)
      middle_pts <- function(x) x[-1] - diff(x) / 2
      second_d <- deriv(middle_pts(mid), deriv(mid, strike))
      n <- input$wind
      smooth_second_d<-data.frame(y=filter(second_d , rep(1/n, n)),x=filter(middle_pts(middle_pts(mid)) , rep(1/n, n)))

      y_conv <- smooth_second_d$y
      x_conv <- smooth_second_d$x

      data<-na.omit(y_conv)
      up <- input$zero
      down <- -input$zero
      xx = which(y_conv<up & y_conv>down)
      start = c(1, which(diff(xx) != 1 & diff(xx) != 0) + 1)
      end = c(start - 1, length(xx))
      x1<-x[xx[start]]
      x2<-x[xx[end]]
      span <- x2-x1+1

      miny <- min(c(y,na.omit(y_conv)))
      maxy <- max(c(y,na.omit(y_conv)))

      plot(x,y,ylim=c(miny,maxy),type='l', col='blue', ylab="ln O.D.", xlab="time", lwd = 3)
      points(x_conv,y_conv,type='l',col=3)
      abline(h=0,col=1)
      abline(h=up,lty='dashed',col=1)
      abline(h=down,lty='dashed',col=1)
      invisible(sapply(which(span>input$span),function(x) polygon( c(x2[x],x2[x],x1[x],x1[x]), c(miny,maxy,maxy,miny),col=rgb(0.75,0.75,0.75,0.2), border='grey')))

      })

    # table linear regions

    output$table <- renderTable({

      data_sets <- myData()
      if(is.null(input$columns)) return()

      data_sets <- na.omit(data_sets[,c("TIME",input$columns), drop=FALSE])

      x<-data_sets$TIME
      y<-log(data_sets[,-1, drop=FALSE])
      y<-rowMeans(y)

      set.seed(123)

      mid <- x
      strike <- y
      deriv <- function(x, y) diff(y) / diff(x)
      middle_pts <- function(x) x[-1] - diff(x) / 2
      second_d <- deriv(middle_pts(mid), deriv(mid, strike))
      smooth_second_d <- smooth.spline(second_d ~ middle_pts(middle_pts(mid)))
      n <- input$wind
      smooth_second_d <- data.frame(y=filter(second_d , rep(1/n, n)), x=filter(middle_pts(middle_pts(mid)) , rep(1/n, n)))

      y_conv <- smooth_second_d$y
      x_conv <- smooth_second_d$x

      slope <- vector()
      data<-na.omit(y_conv)
      up <- input$zero
      down <- -input$zero
      xx = which(y_conv<up & y_conv>down)
      start = c(1, which(diff(xx) != 1 & diff(xx) != 0) + 1)
      end = c(start - 1, length(xx))
      x1<-x[xx[start]]
      x2<-x[xx[end]]
      for(i in 1:length(x1)){
        new.x <- subset(x, x>=x1[i] & x<=x2[i])
        new.y <- y[which(x%in%new.x)]
        fitmodel <- lm(new.y~new.x)
        slope <- append(slope, fitmodel$coefficients[2])
      }
      span <- x2-x1+1

      df <-data.frame("start"=x1,"end"=x2, "slope"=slope, "span"=span)
    
      dff <- signif(df[df$span>input$span,],3)

      dff[order(dff$slope, decreasing=TRUE),]

    }, caption = "Linear regions (Time range)",
       caption.placement = getOption("xtable.caption.placement", "top"), 
       caption.width = getOption("xtable.caption.width", NULL)
    )

    # download output

    output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(output$table, file)
    }
    )

    # Graph costumized
    output$distPlot2 <- renderPlot({
      data_sets <- myData()
      if(is.null(input$columns)) return()

      data_sets <- na.omit(data_sets[,c("TIME",input$columns), drop=FALSE])

      x<-data_sets$TIME
      y<-log(data_sets[,-1, drop=FALSE])
      matplot(x, y, type='l', col='grey', lty='dashed', lwd=2, xlab="time", ylab="ln O.D.")
      
      y<-rowMeans(y)
      range <- input$range
      new.x <- subset(x, x>=range[1] & x<=range[2])
      new.y <- y[which(x%in%new.x)]
      fitmodel <- lm(new.y~new.x)
      points(x, y, type='l', col='blue', lwd=3)
      abline(h=range, lty='dotted',col="tomato")
      abl<-abline(fitmodel,lwd=1,lty='dashed')
      legend("topleft",paste(
        paste("Intercept = ",signif(fitmodel$coefficients[1],3),"\n"),
        paste("Slope = ",signif(fitmodel$coefficients[2],3),"\n"),
        paste("Doubling time = ",signif(log(2)/fitmodel$coefficients[2],3),"\n"),
        sep="\n"), bty="n")

 })

  })
