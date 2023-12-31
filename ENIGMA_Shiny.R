
## Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby 

##############################################################################################
# This file is part of the Shiny app for the ENIGMA HPAI model version 1.0.   
                                                                                            
# The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                   
                                                                                            
# The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                                                

# You should have received a copy of the GNU General Public License along with the ENIGMA HPAI model. If not, see <https://www.gnu.org/licenses/>.
##############################################################################################



# load required packages
library(tidyverse)
library(sf)
library(surveillance)
library(shiny)
library(shinydashboard)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(grid)
library(spdep)
library(fanplot)
library(qs)

### READ IN newest OIE DATA ###
##link to where files are - the below code will pick the newest file in the folder
tt <- tempfile()
#download.file("http://www.costmodds.org/ENIGMA2023/for_ai.car", tt, mode="wb")
download.file("http://www.enigmahpai.org/ENIGMA2023/for_ai.car", tt, mode="wb")
qs::qload(tt)
updateDate <-strftime(as.Date(substring(filename, 7,14), format='%Y%m%d'),format = '%d/%m/%Y')
file.remove(tt)

## FUNCTION TO MAKE FOOTNOTES IN DOWNLOADED PLOTS FROM SHINY ##
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(1, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

## SHINY APP ##
shinyApp(
   ui <- fluidPage(
     # for Carstens tracker
    tags$head(includeHTML("head.html"),includeHTML("head_ljkjaer.html")),
    # Title and logo
    titlePanel(title=div(img(src="ku_logo_uk_v.png",height = 100, width = 80),"ENIGMA HPAI model version 1.0"), windowTitle = "ENIGMA HPAI model"),
       # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        #data range input
          dateRangeInput("dateRange", paste0("Select Date Range (needs to be within 27/09/2021 and ",strftime(endDate, format = '%d/%m/%Y'),", see model description for details about date range)."),
                       min=as.Date(mindate), max=as.Date(maxdate),
                       start = "2021-09-27",
                       end = strftime(endDate, format = '%Y-%m-%d'), format='dd/mm/yyyy'),
        
        
        
        # buttons to picks which graphs/maps to see
        radioButtons("graph", "Select:",
                     c("Map of detections"="hpai_map",
                       "Timeseries" = "hpai_timeseries",
                       "Model fit" = "country_predictions",
                       "Forecasting" = "simulation"),selected="hpai_timeseries"),
        
        #if predictions are chose, here you select country
        conditionalPanel(
          condition = "input.graph == 'country_predictions'",
          selectInput("predictions_options","Select country (only countries with > 50 detections total are shown)", choices=districts2plot2, selected=("Denmark"))
        ),
        
        # if forecasting is chosen, here you pick country
        conditionalPanel(
          condition = "input.graph == 'simulation'",
          selectInput("forecasting_options","Select country (only countries with > 50 detections total are shown)", choices=districts2plot2, selected=("Denmark"))
        ),
        #text under the panel
        HTML('<hr style = "border-color: #800000; height: 5px;width: 100%">'),
        div(span("The purpose of this webpage is to show the predictions of the ENIGMA HPAI model. This model has been developed during research at the University of Copenhagen in "),span(a("the ENIGMA project, " , href="https://ivh.ku.dk/forskning/dyrevelfaerd-og-sygdomsbekaempelse/projektside/enigma/",target="_blank")), span("2021-2024. The ENIGMA project is based in "), span(a("the Avian Influenza Epidemiology Subgroup ", href="https://ivh.ku.dk/english/research/animal-welfare-and-disease-control/avian-influenza-epidemiology/",target="_blank")),span("at the University of Copenhagen and is part of the "),span(a("Danish Veterinary Contingency Consortium (DKVET) ", href="https://dkvet.dk/english/about/", target="_blank")),span("funded by the Danish Food and Veterinary Administration. R code and data used for the ENIGMA HPAI model is available at "), span(a("GitHub.", href="https://github.com/ckirkeby/enigma/", target="_blank"))),
        br(),
        h5(HTML("<i>Disclaimer: The model and data reflects the state of knowledge available on the date of dispatch. The University of Copenhagen and DKVET cannot be held liable for any errors, inaccuracies or inconsistencies with regard to text and/or data contained therein. Therefore, the University of Copenhagen and DKVET accept no responsibility or liability arising out of, or in connection with the information provided.</i>")),
        HTML('<hr style = "border-color: #800000; height: 5px;width: 100%">'),
        strong(a("Contact Admin", 
          href="mailto:ckir@sund.ku.dk")),
        br(),
        br(),
        p(strong(em(paste0("WOAH-WAHIS data and model updated: ", updateDate)))),
        br(),
        br(),
        p("Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby",style = "font-size:80%;"),
        br(),
        div(img(src="gplv3-with-text-136x68.png",height = 40, width = 80),
            br(),
            span("The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See ",style = "font-size:80%;"), span(a("the GNU General Public License", href="https://www.gnu.org/licenses/",target="_blank",style = "font-size:80%;")), span(" for more details.",style = "font-size:80%;"))
      ),
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Tabset w/ model description and model output panel ----
        tabsetPanel(
          tabPanel("Model description",
                   
                   h3("Endemic-epidemic modelling of highly pathogenic avian influenza in Europe"),
                   p("The ENIGMA HPAI model results and graphs presented in this shiny app are based on the 2021-2022 model described in Kjær et al. (2023). In this study, we utilized readily available data from the World Organization for Animal Health (WOAH-WAHIS)* on highly pathogenic avian influenza (HPAI) H5 detections in wild and domestic birds together with a time-series modelling framework (Meyer et al. 2014) to predict HPAI H5 detections within Europe. This framework decomposes time series data into endemic and epidemic components, where the epidemic component can take into account within-country transmission and between-country transmission as well as short-distance (from directly neighouring countries) and long-distance (transmission follows a distance-decay algorithm) transmission."),
                   p("Looking at the WOAH-WAHIS data, we noticed a shift in the seasonality between 2016-late 2021, and late 2021-now, with outbreaks during the summer periods in recent years. Thus, we created different models for these time periods, and the results presented here are from the model fitted to more recent data. Therefore, the earliest date of data in this model is from late 2021. This model includes long-range transmission, and seasonality in the epidemic component, but assumes a constant contribution from endemic transmission within each country. The original model suggests that 12.2% of HPAI detections are endemic in nature, with 87.8% being epidemic in nature (73.3% within-country and 14.5% between-country transmission). This may change as more data on detections are added"),
                   p("Due to the model being based on more recent HPAI H5 detection data, the earliest date that can be chosen in this app is Monday in week 39, 2021 and the latest date that can be chosen is the last date, from which we have HPAI H5 data from WOAH-WAHIS (updated weekly). Forecasting will always be 4 weeks ahead from the week before the last date chosen (to account for delays in WOAH-WAHIS reporting). For more details see Kjær et al. (2023) and Meyer et al. (2014)."),
                   br(),
                   br(),
                   p(a("Kjær, L. J., M. P. Ward, A. E. Boklund, L. E. Larsen, C. K. Hjulsager, and C. T. Kirkeby. Using surveillance data for early modelling of highly pathogenic avian influenza in Europe reveals a seasonal shift in transmission, 2016-2022. 2023. Scientific Reports 13:15396.",href="https://doi.org/10.1038/s41598-023-42660-7",target="_blank",style = "font-size:85%;")),
                   p(a("Meyer, S., L. Held, and M. Höhle. 2014. Spatio-Temporal Analysis of Epidemic Phenomena Using the R Package surveillance. J. Stat. Softw. 77.", href= "https://doi.org/10.18637/jss.v077.i11", target="_blank",style = "font-size:85%;")),
                   br(),
                   br(),
                   div(
                     h4(HTML("<b>*Copyright of data extracted from WAHIS</b>"),style = " 
                          background-color: #800000; width: 100%; color:white;font-size:85%;"),
                     h5(HTML("<b>Disclaimer and caption</b>"), style="text-align:left;background-color: #ffffff;width: 50%;font-size:85%;"),
                     p(HTML("Any person accessing and/or using this SharePoint Communication Website is allowed to extract data to the extent that the following caption is displayed: <i>“World Organisation for Animal Health (WOAH) (YEAR). – [NAME OF Periodical data extractions WEBPAGE]. Retrieved on [DATE] from [LINK WEBPAGE]. Data extracted by [NAME/ORGANISATION]. WOAH bears no responsibility for the integrity or accuracy of the data contained herein, but not limited to, any deletion, manipulation, or reformatting of data that may have occurred beyond its control. For some events, incorrect data have recently been detected in the figures containing the quantitative information of the outbreaks. WOAH is currently undertaking considerable efforts to solve these issues at the source and provide a dataset that is fully consistent with that reported by countries. The remaining fields of the table are not impacted by these issues. We will keep our users informed as the situation develops.”</i>"),style="text-align:justify;background-color: #ffffff;width: 100%;font-size:85%;"))
             
                   ),
          tabPanel("Model output", downloadButton("downloadPlot", "Download"),plotOutput("graph",height = 500, width = 900), textOutput("info"))
        )
      )
    )
  ),
  
  
  # Define server 
  server <- function(input, output) {
    ## REACTIVE FUNCTIONS ##
    # reactive functions take user inputs fx. date or graph type and produce outputs or variables based on this

    # many of the below functions just create timevariables out of the dates chosen. These variables are used for plotting
    startWeek <- reactive({
      startweekno<- isoweek(input$dateRange[1])
      startyearno<- isoyear(input$dateRange[1])
      if(startyearno==2023 && startweekno==53){
        if(input$dateRange[1] < '2024-01-01'){
          startweekno<-52
          
        } 
        else if(input$dateRange[1] >='2024-01-01'){
          startweekno<- 1
        }
        
      }
      return(as.numeric(startweekno))
    })
    
    startYear <- reactive({
      startyearno<-isoyear(input$dateRange[1])
      startweekno <- isoweek(input$dateRange[1])
      if(startyearno==2023 && startweekno==53){
        if(input$dateRange[1] < '2024-01-01'){
          startyearno<- 2023
          
        } 
        else if(input$dateRange[1] >='2024-01-01'){
          startyearno <- 2024
        }
      }
      return(as.numeric(startyearno))
    })
    
    endWeek <- reactive({
      endweekno <-isoweek(input$dateRange[2])
      endyearno <- isoyear(input$dateRange[2])
      if(endyearno==2023 && endweekno==53){
        if(input$dateRange[2] < '2024-01-01'){
          endweekno<-52
          
        } 
        else if(input$dateRange[2] >='2024-01-01'){
          endweekno<- 1
        }
        
      }
      return(as.numeric(endweekno))
    })
    
    endYear <- reactive({
      endyearno<-isoyear(input$dateRange[2])
      endweekno <-isoweek(input$dateRange[2])
      if(endyearno==2023 && endweekno==53){
        if(input$dateRange[2] < '2024-01-01'){
          endyearno<- 2023
          
        } 
        else if(input$dateRange[2] >='2024-01-01'){
          endyearno <- 2024
        }
      }
      return(as.numeric(endyearno))
    })
    
    startYearWeek <- reactive ({
      yearweek(paste0(startYear(), ' ', paste0('W',startWeek())))
    })
    
    endYearWeek <-  reactive({
      yearweek(paste0(endYear(), ' ', paste0('W',endWeek())))
    })
    
    start <- reactive ({
      as.numeric(floor(difftime(input$dateRange[1], '2021-09-27',units="weeks")+1))   
    })
    
    end <-  reactive({
      as.numeric(floor(difftime(input$dateRange[2], '2021-09-27',units="weeks")+1))   
    })
    
    #now we calculate number of outbreaks for each country for the time period chosen for plotting
    europe_mapData1 <-reactive ({
      europe_data_weekly %>%
        filter(yearweek>=startYearWeek() & yearweek <=endYearWeek()) %>% 
        group_by(ADM0_A3) %>% summarise(no_outbreaks = sum(no_outbreaks, na.rm=T))
    })
    
    #Merge with shapefiles of Europe for plotting
    europe_mapData<- reactive ({
      t1 <- merge(europeanCountries, europe_mapData1(),by="ADM0_A3")
      t1$outbreaksArea <- t1$no_outbreaks/(t1$area_sqkm/10000)
      return(st_as_sf(t1))
    })
    ## simulations 3 weeks ahead of the last week of the time period chosen. 
    sim <- reactive({
      startSim <-end()-1
      endSim<-startSim+4
      y.start <- observed(AI_sts)[startSim, ] 
      AI_stsSim <- simulate(final_model,
                            nsim = 500, seed = 1, subset =(startSim+1):endSim,y.start = y.start)
      return(AI_stsSim)
    })
    # her we have reactive functions to create each of the graphs/maps
    #map
    hpai_map <- reactive({
      myMap1 <- ggplot() +
        geom_sf(data= europe_mapData(),aes(fill = no_outbreaks),color="black")+
        theme_void()+
        scale_fill_gradientn(colours=hcl.colors(10, "Reds",rev=T)) +
        theme(legend.position = c(0.91, 0.98),
              legend.justification=c(0.5, 1),legend.title=element_text(size=9),plot.margin=unit(c(1,-0.5,1,1), "cm"))+
        labs(fill=expression(paste("HPAI total")))
      
      myMap2 <- ggplot() +
        geom_sf(data= europe_mapData(),aes(fill = outbreaksArea),color="black")+
        theme_void()+
        scale_fill_gradientn(colours=hcl.colors(10, "Reds",rev=T)) +
          theme(legend.position = c(0.95, 0.99),
              legend.justification=c(0.5, 1),legend.title=element_text(size=9),plot.margin=unit(c(1,1,1,-0.5), "cm"))+
        labs(fill=expression(paste("HPAI/10,000 km"^"2")))
      
      print(ggarrange(myMap1,myMap2, ncol=2))
      recordPlot()
    })

    # time series
    hpai_timeseries <- reactive({
      par(mar = c(7, 5, 4.1, 2.1))
        myPlot <- plot(AI_sts[start():end(),], type = observed ~ time, ylab="No. detected cases", xlab="Years and quarters", ylim=c(0,max(rowSums(observed(AI_sts[start():end(),]),na.rm=T))+100))
        print(myPlot)
      recordPlot()
    })
    
    # model fit, summed all countries  
    allCountry_predictions<- reactive({
        myPlot <-plot(final_model, type = "fitted", total = TRUE,hide0s = TRUE, ylab="No. detected cases", xlab="Years and quarters",ylim=c(0,(max(rowSums(observed(final_model$stsObj[start():end(), ]), na.rm=T)))+50),par.settings = list(mar = c(7, 5, 4.1, 2.1)), xaxis=list(epochsAsDate=TRUE),start=c(startYear(),startWeek()), end=c(endYear(),endWeek()),col=c('brown2','#4292C6','orange'),names="Summed all countries", legend.args=list(x="topright", inset=c(-0.1,0.003), horiz=T,legend = c("Epidemic component,between-country","Epidemic component,within-country", "Endemic"),col=c('orange','#4292C6','brown2')))
        print(myPlot)
      recordPlot()
    })
    
    # model fit, individual countries  
    country_predictions <- reactive({
      unit<-  which(districts2plot2==input$predictions_options) 
      myPlot <-plot(final_model, type = "fitted", units = districts2plot[unit], hide0s = TRUE,  names=districts2plot2[unit],ylab="No. deteced cases",xlab="Years and quarters",ylim=c(0,(max(observed(final_model$stsObj)[start():end(),districts2plot[unit]], na.rm=T))+7),xaxis=list(epochsAsDate=TRUE),par.settings = list(mar = c(7, 5, 4.1, 2.1)),  start=c(startYear(),startWeek()), end=c(endYear(),endWeek()),legend.args=list(x="topright",inset=c(-0.1,0.003), horiz=T,legend = c("Epidemic component,between-country","Epidemic component,within-country", "Endemic"),col=c('orange','#4292C6','brown2')),col=c('brown2','#4292C6','orange'))
        print(myPlot)
        recordPlot()
    })
    # forecasting, summed all countries  
    AllCountry_simulation <- reactive({
          myPlot <-plot(sim(), observed=FALSE,ylab="",xlab="", main="Summed all countries",type="fan", means.args = list(),xaxis=list(epochsAsDate=TRUE, xaxis.tickFreq=list("%d"=atChange, "%m"=atChange),xaxis.labelFreq=list("%d"=atMedian), xaxis.labelFormat="%G\n\n%d-%b"), ylim=range(sim())*0.7,fan.args=list(ln = c(10,50,90)),par.settings=list(pch=1,cex=0.8,mar = c(7, 5, 4.1, 2.1)))
        title(xlab="Time", line=1)
        title(ylab="No. of detected/predicted cases", line=3)
        grid()
        print(myPlot)
      recordPlot()
    })
    
    # forecasting, individual countries  
    country_simulations <- reactive({
      unit<-  which(districts2plot2==input$forecasting_options) 
        myPlot <-plot(sim()[,districts2plot[unit],],  observed=FALSE,ylab="", xlab="",main=districts2plot2[unit], type="fan", ylim=c(0, quantile(sim()[,districts2plot[unit],], 0.99)),means.args = list(),xaxis=list(epochsAsDate=TRUE, xaxis.tickFreq=list("%d"=atChange, "%m"=atChange),xaxis.labelFreq=list("%d"=atMedian), xaxis.labelFormat="%G\n\n%d-%b"), fan.args=list(ln = c(10,50,90)),par.settings=list(pch=1,cex=0.8,mar = c(7, 5, 4.1, 2.1)))
      title(xlab="Time", line=1)
      title(ylab="No. of detected/predicted cases", line=3)
      grid()
        print(myPlot)
        recordPlot()
    })
    
   
    #functions to get plot names dependent on user input - this will be used when downloading files to create file name
    getPlotName <- reactive({
      if(input$graph=="country_predictions"){
        if(input$predictions_options== "Summed all countries"){filename<- "allCountry_predictions()"} else {filename <- "country_predictions()"}
      }else
        if(input$graph=="simulation"){
          if(input$forecasting_options== "Summed all countries"){filename <-"AllCountry_simulation()"}else{filename <- "country_simulations()"}
        }else{filename <- paste0(input$graph,"()") }
      return(filename)
    })
    ## RENDER PLOT ###
    # this creates the actual graphs/maps on the output tab using the respective reactive functions for these graphs/maps
        output$graph <- renderPlot({
      if(input$graph=="hpai_map"){
        replayPlot(req(hpai_map()))
      }
      if(input$graph=="hpai_timeseries") {replayPlot(req(hpai_timeseries()))}
      if(input$graph=="country_predictions"){
        if(input$predictions_options== "Summed all countries"){replayPlot(req(allCountry_predictions()))
          } else {replayPlot(req(country_predictions()))}
      }
      if(input$graph=="simulation"){
        if(input$forecasting_options== "Summed all countries"){replayPlot(req(AllCountry_simulation()))
          }else{replayPlot(req(country_simulations()))}
      }
    })
    ## RENDER TEXT ##
    # here we create figure text for each graph/map
    output$info <- renderText({
      if(input$graph=="hpai_map"){
        paste0("Number of reported highly pathogenic avian influenza (H5 subtype) detections between ",strftime(input$dateRange[1], format='%d/%m/%Y'), " and ", strftime(input$dateRange[2],format='%d/%m/%Y'), ", summed over 37 European countries shown geographically as total number of detections (left) and number of detections per 10,000 km² (right).")
      } else
        if(input$graph=="hpai_timeseries") {
          paste0("Number of reported highly pathogenic avian influenza (H5 subtype) detections shown over time between ",strftime(input$dateRange[1], format= '%d/%m/%Y'), " and ", strftime(input$dateRange[2],format='%d/%m/%Y'), ", summed over 37 European countries.")
          
        } else
          if(input$graph=="country_predictions"){
            if(input$predictions_options== "Summed all countries"){
              paste0("Overall model fit aggregated over all the 37 countries shown for ", strftime(input$dateRange[1], format= '%d/%m/%Y'), " to ", strftime(input$dateRange[2],format='%d/%m/%Y'),". The plot shows the relative contribution of model components based on the final multivariate time-series model in Kjær et al. (2023). Dots show the actual counts of reported highly pathogenic avian influenza (H5 subtype) detections in domestic and wild birds. Note that zero/missing detections have been omitted.")
            } else {
              unit<-  which(districts2plot2==input$predictions_options) 
              paste0("Model fit for ", districts2plot2[unit]," shown for ", strftime(input$dateRange[1], format='%d/%m/%Y'), " to ", strftime(input$dateRange[2],format='%d/%m/%Y'), ". The plot shows the relative contribution of model components based on the final multivariate time-series model in Kjær et al. (2023). Dots show the actual counts of reported highly pathogenic avian influenza (H5 subtype) detections in domestic and wild birds. Note that zero/missing detections have been omitted.")
            }
          } else 
            if(input$graph=="simulation"){
              if(input$forecasting_options== "Summed all countries") {
                paste0("Simulation-based 4 week forecast using the multivariate time-series model in Kjær et al. (2023). The plots show weekly number of predicted highly pathogenic avian influenza (H5 subtype) detections aggregated over all 37 countries included in the model. The fan chart represents the 10%, 50% and 90% quantiles of the simulations (N=500) each week; their mean is displayed as a white line. The black dot to the left of the graph depicts the number of detections from week ", isoweek(input$dateRange[2])-1, " in ", isoyear(input$dateRange[2]), ". As forecasting consists of sequential calls to the negative binomial distributions developed in the model, the mean at each time point is determined by using the parameter estimates and the counts simulated at the previous time point. Thus, we used the second to last week of data (to account for delays in WOAH-WAHIS reporting) to forecast 4 weeks ahead. For further details, see Kjær et al. (2023).")
              }else{
                unit<-  which(districts2plot2==input$forecasting_options) 
                paste0("Simulation-based 4 week forecast using the multivariate time-series model in Kjær et al. (2023). The plots show weekly number of predicted highly pathogenic avian influenza (H5 subtype) detections for ",districts2plot2[unit],". The fan chart represents the 10%, 50% and 90% quantiles of the simulations (N=500) each week; their mean is displayed as a white line. The black dot to the left of the graph depicts the number of detections from week ", isoweek(input$dateRange[2])-1, " in ", isoyear(input$dateRange[2]), ". As forecasting consists of sequential calls to the negative binomial distributions developed in the model, the mean at each time point is determined by using the parameter estimates and the counts simulated at the previous time point. Thus, we used the second to last week of data (to account for delays in WOAH-WAHIS reporting) to forecast 4 weeks ahead. For further details, see Kjær et al. (2023).")
              }
            }
    })
    
    ## DOWNLOAD GRAPHS/MAPS ###
    output$downloadPlot <- downloadHandler(
      
      filename = function() { 
        if(input$graph=="simulation" & input$forecasting_options!= "Summed all countries"){
          paste0(substr(getPlotName(),1,nchar(getPlotName())-2), "_", input$forecasting_options,"_", Sys.Date(),".png")
        }else if (input$graph=="country_predictions" & input$predictions_options!= "Summed all countries"){
          paste0(substr(getPlotName(),1,nchar(getPlotName())-2), "_", input$predictions_options,"_", Sys.Date(),".png")
        } else{paste0(substr(getPlotName(),1,nchar(getPlotName())-2), "_",  Sys.Date(),".png")}},
      content = function(file) {
          png(file,width = 13, height = 8, units = 'in', res = 300)
          replayPlot(eval(parse(text = getPlotName())))
          makeFootnote(paste0("ENIGMA model results based on model from Kjær et al. (2023), using WOAH-WAHIS data from ", strftime(input$dateRange[1],format='%d/%m/%Y')," to ",strftime(input$dateRange[2],format='%d/%m/%Y'), ". Downloaded on ", format(Sys.time(), "%d/%m/%Y")))
          dev.off()
      }
      
      )
  }
  
)
# Create Shiny app ----
shinyApp(ui, server)
