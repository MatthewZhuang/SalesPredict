library(forecast)
library(TTR)
library(shiny)
library(dygraphs)
library(xts)
library(ggplot2)
library(ggthemes)
library(zoo)

#salesTrend
salesTrend_sale <- read.csv("data/sale.csv", header = T)
salesTrend_sale$time <- as.Date(salesTrend_sale$time, "%m/%d/%Y")

#skuDistribution
sk<-read.csv("data/SKU.csv",header = T)

#predict
model_inv<-read.csv("data/SKU_week_sales.csv")

#promotion
daily<-read.csv("data/SKU_day_sales.csv")
model_inv<-read.csv("data/SKU_week_sales.csv")

#inventory
AC202B<-read.csv("data/AC202B.csv",header = T)
AC202B$Date<-as.Date(AC202B$Date,"%m/%d/%Y")
AC201<-read.csv("data/AC201.csv",header = T)
AC201$Date<-as.Date(AC201$Date,"%m/%d/%Y")

#inventoryImprove
inventory_AC201<-read.csv("data/AC201_inventory.csv")
inventory_AC202B<-read.csv("data/AC202B_inventory.csv")
inventory_AC840<-read.csv("data/AC840_inventory.csv")
inventory_CA2014RB<-read.csv("data/CA2014RB_inventory.csv")
sku_sales<-read.csv("data/SKU_week_salesamount.csv")


promotionWeeks<-function(dates, date, days){
    #max promotion days: 5
    #case 1: all promotion days in one week
    #case 2: promotion days in two weeks
    bingo = which(dates >= date)[1]
    first = dates[bingo]
    firstDays = as.Date(first) - as.Date(date) + 1
    endDay = as.Date(date) + days -1
    if(endDay > first){
        weeks = c(first)
        firstDays = as.Date(first) - as.Date(date) + 1
        weekdays = c(as.double(firstDays))
        nextWeek = as.Date(first) + 7
        weeks = append(weeks, nextWeek)
        weekdays = append(weekdays, (days-weekdays[1]))
    }else{
        weeks = c(first)
        weekdays = c(as.double(days))
    }
    
    return(data.frame(weeks, weekdays))
}




shinyServer(function(input, output) {
	
    ########################salesTrend########################
	output$salesTrendplot <- renderDygraph({
        
		sale <- salesTrend_sale
        #sale$time <- as.Date(sale$time, "%m/%d/%Y")

		newdata <- subset(sale, time >= input$dates[1] & time <= input$dates[2])
		ll <- lm(Overall.Sales ~ time, data = newdata)

		xlab = paste("from  ", input$dates[1], "   to  ", input$dates[2])
		date = newdata$time
		sales = newdata$Overall.Sales
		lm = ll$fitted.values
		x = data.frame(sales, date, lm)

		y = cbind(sales = xts(x$sales, x$date), lm = xts(x$lm, x$date))
		dygraph(y, main = "Overall Sales Trend", group = "q", width = 1200) %>%
			dySeries("sales", label = "overall sales", drawPoints = TRUE, color = "black") %>% 
			dySeries("lm", label = "Linear regression", drawPoints = TRUE, color = "red", strokePattern = "dashed") %>% 
			dyAxis("y", label = "Dollar amount") %>% 
			dyAxis("x", label = xlab) %>% 
			dyOptions(axisLineWidth = 2) %>% 
			dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>% 
			dyRoller(rollPeriod = 1)

	})
    ########################salesTrend  end########################
    
    
    ########################salesDistribution########################
    output$skuDistributionsummary <- renderPrint({
        
        newdata<-subset(sk,total.sale >= input$slider[1] & total.sale <=  input$slider[2])
        #summary(newdata$total.sale)
        cat(paste("max:    ",max(newdata$total.sale)),"\n")
        cat(paste("min:    ", min(newdata$total.sale)),"\n")
        cat(paste("mean:   ", mean(newdata$total.sale)),"\n")
        cat(paste("median: ", median(newdata$total.sale)),"\n")
        
    })
    
    
    output$skuDistributionplot<-renderPlot({
        newdata<-subset(sk,total.sale >= input$slider[1] & total.sale <=  input$slider[2])
        #newdata<-newdata[order(newdata$total.sale),]
        ggplot(newdata,aes(reorder(SKU, total.sale),total.sale,fill=SKU,xlab=" "))+geom_bar(stat = "identity")+
        xlab(" ") + ylab("Dollar amount")
        
        
    })
    ########################salesDistribution  end########################
    
    
    ########################predict########################
    skuInput <- reactive({
        
        data<-c(input$predictsku)
        
    })
    
    output$predictdygraph <- renderDygraph({
        
        SKU<-model_inv[,c("X",skuInput())]
        SKU$X<-as.Date(SKU$X, "%m/%d/%Y")
        SKU<-SKU[order(SKU$X),]
        dates = SKU$X
        
        SKU<-SKU[1:64,]
        # generate the time series
        ts_SKU<- ts(SKU[2],frequency=52,start=c(2015,02))
        # get all predict, when do promotions, just change the relevant fitted value
        b<-HoltWinters(ts_SKU,beta=FALSE, gamma=FALSE);
        
        sales = SKU[2][1:64,]
        date = SKU$X
        date =date[1:64]
        predSales = b$fitted[1:63]
        predSales = c(sales[1], predSales)
        x = data.frame(sales,date,predSales)
        
        y=cbind(sales=xts(x$sales,x$date),
        predSales=xts(x$predSales,x$date))
        dygraph(y, main = "Real & Pred Sales", group = "q", width=1200) %>%
			dySeries('sales', drawPoints = TRUE, color = 'black') %>%
			dySeries('predSales', drawPoints = TRUE, color = 'red') %>%
			dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
			dyAxis("y", label = "Dollar amount") %>%
			dyOptions(axisLineWidth = 2) %>% 
			dyRoller(rollPeriod=1)
    })
    ########################predict  end########################
    
    
    ########################promotion########################
    output$promotionplot<-renderDygraph({
        benefit = input$benefit
        
        SKU<-model_inv[,c("X",input$sku)]
        SKU$X<-as.Date(SKU$X, "%m/%d/%Y")
        SKU<-SKU[order(SKU$X),]
        dates = SKU$X
        
        #calculate the week to decide the num
        #plot all weeks, not just the promotion week
        SKU<-SKU[1:64,]
        # generate the time series
        ts_SKU<- ts(SKU[2],frequency=52,start=c(2015,02))
        # get all predict, when do promotions, just change the relevant fitted value
        b<-HoltWinters(ts_SKU,beta=FALSE, gamma=FALSE);
        
        SKU_Daily<-daily[,c("X",input$sku)]
        SKU_Daily$X<-as.Date(SKU_Daily$X, "%m/%d/%Y")
        SKU_Daily<-SKU_Daily[order(SKU_Daily$X),]
        
        weeks = promotionWeeks(dates, input$date, input$num)
        row = nrow(weeks)
        from = as.Date(weeks[1, 1]) - 7
        to = as.Date(weeks[row, 1]) + 7
        range = c(as.character(from), as.character(to))
        #get the 5 average sales before the input$date
        end = which(SKU_Daily$X == input$date)
        begin = end - 5
        base = sum(SKU_Daily[begin:end-1,][2])/5
        ADPIR = 0.1
        
        
        if(input$sku == 'AC201'){
            ADPIR = 0.12
        }else if(input$sku == 'AC840'){
            ADPIR = 2.57
        }else if(input$sku == 'CA2014RB'){
            ADPIR = 0.09
        }else if(input$sku == 'AC202B'){
            ADPIR = 0.2
        }
        
        if(base == 0){
            base = 10
        }
        if(base < 0){
            base = 0
        }
        
        dpb = base*ADPIR
        
        if(!is.na(benefit)){
            dpb = as.numeric(benefit)
        }
        min = 0
        max = 0
        for(i in 1:row){
            firstWeek = weeks[i,1]
            index = which(SKU$X == firstWeek)
            #predict , should calculate the first week predict value. so first fit the model
            sales = b$fitted[index - 1]
            promotiondays = weeks[i,2]
            predictValue = sales + dpb*promotiondays
            # easy way, fitted all the week, and change the predict data of the promotion week
            #chang the fitted value
            b$fitted[index-1] = predictValue
            if(predictValue > max){
                max = predictValue+1000
            }
        }
        
        #plot the rec
        pos1 = floor((which(SKU$X == weeks[1,1])-1)*19.23)
        pos2 = floor((which(SKU$X == weeks[row,1])+1)*19.23)
        xleft = 0
        xright = 0
        if(pos1 > 1000){
            pos1 = pos1 - 1000
            pos2 = pos2 - 1000
            if(pos1 < 100){
                xleft = paste("2016.0", pos1, sep="")
            }else{
                xleft = paste("2016.", pos1, sep="")
            }
            if(pos2 < 100){
                xright = paste("2016.0", pos2, sep="")
            }else{
                xright = paste("2016.", pos2, sep="")
            }
        }else{
            if(pos2 > 1000){
                pos2 = pos2 - 1000
                if(pos2 < 100){
                    xright = paste("2016.0", pos2, sep="")
                }else{
                    xright = paste("2016.", pos2, sep="")
                }
                if(pos1 < 100){
                    xleft = paste("2015.0", pos1, sep="")
                }else{
                    xleft = paste("2015.", pos1, sep="")
                }
            }else{
                if(pos1 < 100){
                    xleft = paste("2015.0", pos1, sep="")
                }else{
                    xleft = paste("2015.", pos1, sep="")
                }
                if(pos2 < 100){
                    xright = paste("2015.0", pos2, sep="")
                }else{
                    xright = paste("2015.", pos2, sep="")
                }
            }
            
        }
        ylow = min
        ytop = max
        
        sales = b$x[1:64,]
        date = dates[1:64]
        predSales = c(sales[1], b$fitted[1:63])
        x = data.frame(sales,date,predSales)
        
        y=cbind(sales=xts(x$sales,x$date),
        predSales=xts(x$predSales,x$date))
        dygraph(y, main = "Promotion Sales", group = "q", width=1200) %>%
			dySeries('sales', label = 'origin sales', drawPoints = TRUE, color = 'black') %>%
			dySeries('predSales', label = 'promotion sales', drawPoints = TRUE, color = 'red') %>%
			dyShading(from = range[1], to = range[2], color = "#CCEBD6") %>%
			dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
			dyAxis("y", label = "Dollar amount") %>%
			dyOptions(axisLineWidth = 2) %>% 
			dyRoller(rollPeriod=1)
    })
    ########################promotion  end########################
    
    
    ########################inventory########################
    output$inventoryplot1<-renderDygraph({
        date<-as.Date(AC202B$Date)
        sales<-AC202B$week_sale
        invQty<-AC202B$week_InvQty
        x = data.frame(sales,date,invQty)
        y=cbind(sales=xts(x$sales,x$date),
        invQty=xts(x$invQty,x$date))
        dygraph(y, main = "Example of a 'rising' SKU(AC202B) ", group = "q", width=1200) %>%
        dySeries('sales', drawPoints = TRUE, color = 'black') %>%
        dySeries('invQty', drawPoints = TRUE, color = 'red') %>%
        dyAxis("y", label = "Quantities") %>%
		dyOptions(axisLineWidth = 2) %>% 
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyRoller(rollPeriod=1)
    })
    
    output$inventoryplot2<-renderDygraph({
        
        date<-as.Date(AC201$Date)
        sales<-AC201$week_sale
        invQty<-AC201$week_InvQty
        x = data.frame(sales,date,invQty)
        y=cbind(sales=xts(x$sales,x$date),
        invQty=xts(x$invQty,x$date))
        dygraph(y, main = "Example of a 'falling' SKU(AC201) ", group = "q", width=1200) %>%
        dySeries('sales', drawPoints = TRUE, color = 'black') %>%
        dySeries('invQty', drawPoints = TRUE, color = 'red') %>%
        dyAxis("y", label = "Quantities") %>%
		dyOptions(axisLineWidth = 2) %>% 
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
        dyRoller(rollPeriod=1)
        
    })
    
    ########################inventory  end########################
    
    ########################inventory  improve########################
    output$inventoryImproveplot<-renderDygraph({
        
        SKU<-sku_sales[,c("X",input$improvesku)]
        SKU$X<-as.Date(SKU$X, "%m/%d/%Y")
        SKU<-SKU[order(SKU$X),]
        dates = SKU$X
        
        #calculate the week to decide the num
        #plot all weeks, not just the promotion week
        SKU<-SKU[1:64,]
        # generate the time series
        ts_SKU<- ts(SKU[2],frequency=52,start=c(2015,02))
        # get all predict, when do promotions, just change the relevant fitted value
        b<-HoltWinters(ts_SKU,beta=FALSE, gamma=FALSE);
        last = predict(b, n.ahead = 1)[1,]
        
        inventory = inventory_AC201
        if(input$improvesku == "AC201"){
            inventory = inventory_AC201
        }else if(input$improvesku == "AC840"){
            inventory = inventory_AC840
        }else if(input$improvesku == "CA2014RB"){
            inventory = inventory_CA2014RB
        }else if(input$improvesku == "AC202B"){
            inventory = inventory_AC202B
        }
        
        inv<-inventory[,c("Date","InvQty","POQty")]
        inv = inv[1:64,]
        inv$Date<-as.Date(inv$Date, "%m/%d/%Y")
        inv<-inv[order(inv$Date),]
        inv$sum = inv$InvQty + inv$POQty
        date = inv$Date
        inv_ori = inv$sum
        inv_pre = c()
        row = nrow(b$fitted)
        pre = b$fitted
        firstWeek = inv[1,2]
        nextWeek = 0
        
        #promotion   change value 1.8 based on the increase rate
        for(i in 1:row){
            if(i == 1){
                nextWeek = firstWeek
            }
            p = pre[i]*2
            thisWeek = 0
            if(nextWeek >= p){
                inv_pre = append(inv_pre, nextWeek)
                thisWeek = nextWeek
            }else{
                inv_pre = append(inv_pre, p)
                thisWeek = p
            }
            nextWeek = SKU[i+1,2] - thisWeek
        }
        
        p = last*2
        if(nextWeek  >= p){
            inv_pre = append(inv_pre, nextWeek )
        }else{
            inv_pre = append(inv_pre, p)
        }
        
        res = data.frame(inv_ori, inv_pre)
        
        origin = res$inv_ori
        predict = res$inv_pre
        date = dates
        sales = SKU[2][1:63,]
        sales = c(0, sales)
        x = data.frame(origin, date, predict, sales)
        
        y=cbind(origin=xts(x$origin,x$date),
        predict=xts(x$predict,x$date),
        sales=xts(x$sales,x$date))
        dygraph(y, main = "Optimized Inventory", group = "q", width=1200) %>%
			dySeries('origin', label = 'actual inventory', drawPoints = TRUE, color = 'blue') %>%
			dySeries('predict', label = 'optimized inventory', drawPoints = TRUE, color = 'red') %>%
			dySeries('sales', label = 'Sales', drawPoints = TRUE, color = 'black') %>%
			dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
			dyAxis("y", label = "Quantities") %>%
			dyOptions(axisLineWidth = 2) %>% 
			dyRoller(rollPeriod=1)
    })
    
    
    ########################inventory  improve  end########################
    
})
