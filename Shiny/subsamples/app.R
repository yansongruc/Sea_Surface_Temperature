library(shiny)
library(fields)
library(geoR)
library(MaxPro)
library(ggplot2)
library(readr)

SST=read_csv('SST.csv')

# Define UI for application that draws a plot
ui = shinyUI(pageWithSidebar(
    headerPanel("Sea Surface Tenperatures of Day 10"),
    sidebarPanel(
        selectInput("method", "Choose a subsampling method:", 
                    choices = c("Random", "Deep and Wide", "MaxPro")),
        
        sliderInput("m", "Number of subsamples to view (*10):", 
                    min=1,max=30,value=10)
    ),
    
    mainPanel(
        plotOutput("view")
    )
))

server = shinyServer(function(input, output) {
    geoSST10=as.geodata(SST[,-1])
    sst10=data.frame(loc.x1=geoSST10$coords[,1],loc.x2=geoSST10$coords[,2],y=geoSST10$data)
    
    subsamples=function(k,type)
    {
        iR   = sample(1:nrow(sst10),10*k)
        R    = sst10[iR,]
        
        ic=sample(1:nrow(sst10),5)
        Dis=rdist(sst10[,-3])
        inb=matrix(0,5,round(10*k/5))
        for(i in 1:5)
        {
            rk=rank(Dis[ic[i],],ties.method = "first")
            inb[i,]=which(rk>1 & rk<=round(10*k/5)+1)
        }
        inr=unique(as.vector(inb))
        DaW=sst10[inr,]
        
        map=MaxProLHD(10*k,2)$Design
        Map=map
        Map[,1]=map[,1]*(max(sst10$loc.x1)-min(sst10$loc.x1))+min(sst10$loc.x1)
        Map[,2]=map[,2]*(max(sst10$loc.x2)-min(sst10$loc.x2))+min(sst10$loc.x2)
        Mdist=rdist(Map,sst10[,-3])
        imap=rep(0,10*k)
        for(i in 1:(10*k))
        {
            rkm=rank(Mdist[i,],ties.method = "first")
            imap[i]=which(rkm==1)
        }
        imap=unique(as.vector(imap))
        MAP=sst10[imap,]
        
        switch(type,"Random"=R,"Deep and Wide"=DaW, "MaxPro"=MAP)
        
    }
    
    output$view=renderPlot({
        subs=subsamples(input$m,input$method)
        p_plot=ggplot(subs,aes(x=subs[,1],y=subs[,2],color=subs[,3]))+geom_point()+theme_light()+
            scale_color_gradientn(colours = tim.colors(25))+labs(x="Longitude",y="Latitude",color="Sea Surface Temperatures",title = "Subsamples")+
            scale_x_continuous(limits =range(sst10$loc.x1))+scale_y_continuous(limits =range(sst10$loc.x2))
        p_plot
        
    })
    
})


# Run the application 
shinyApp(ui = ui, server = server)
