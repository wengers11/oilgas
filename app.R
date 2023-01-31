#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##add tabs to Rshiny app
#https://shiny.rstudio.com/articles/tabsets.html

##https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r

##ggmap setup and tutorial
#https://rpubs.com/jcraggy/841199

# install.packages(c("shiny", "shinyWidgets","dplyr","tidyr","lubridate","ggplot2","ggmap","stringr",
#                     "data.table","Metrics","zoo","factoextra","cluster","randomForest",
#                     "earth","class","sp","rgeos","geosphere","gstat","EnvStats",
#                     "gplots","caret","tidyselect","RColorBrewer","RSQLite"))
# install.packages(c("shinyWidgets","stringr",
#                    "data.table","Metrics","zoo","factoextra","cluster","randomForest",
#                    "earth","class","sp","rgeos","geosphere","gstat","EnvStats",
#                    "gplots","caret","RColorBrewer","RSQLite"))

library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(stringr)
library(data.table) ##for shift function
library(Metrics) ##rmse function
library(zoo) #rolling means
library(factoextra) ##cluster visualization
library(cluster) ##for Gap Statistic
library(randomForest)
library(earth)
library(class)##for knn
library(sp) ##for spatial analysis, kriging
library(rgeos) ## for spatial analysis
library(geosphere) ## for spatial analysis
library(gstat)##for kriging
library(EnvStats) ##for pareto distribution
library("gplots") ##for plotmeans graphing function
library(gridExtra) ##Add table to plot
library(caret) ##bayesian modeling
library(tidyselect)
library(RColorBrewer)
library(gbm)
library(glmnet)

#setwd("C:/Users/Wesley Engers/Documents/Eagle Ford Oil and Gas Project/Monthly Data/Oil App")
#setwd("~/Eagle Ford Oil and Gas Project/Monthly Data/Oil App/Oil_Prediction_App")

eagle.data2<-readRDS("Eagle Data 2.rds")
cluster.data<-readRDS("cluster data2.rds")
cluster.data<-cluster.data%>%left_join(select(eagle.data2,API,Surf.Lat,Surf.Lon,API.Gravity))%>%
  mutate(Avg.Prop.per.GPI=Avg.Prop.per.Lateral,Avg.Fluid.per.GPI=Avg.Fluid.per.Lateral)%>%
  select(-Avg.Prop.per.Lateral,-Avg.Fluid.per.Lateral)

eagle.data3<-eagle.data2%>%left_join(select(cluster.data,API,Leading.Coef,Power.Coef))

joined.data.price<-readRDS("joined price data.rds")
prod.length<-readRDS("product length.rds")
eagle.rev.cost.data<-readRDS("eagle rev cost data.rds")%>%
  left_join(prod.length)%>%filter(Max.Month>24)

oil.gas.data<-readRDS("Oil and Gas Monthly prod.rds")
first.6.BOE<-filter(oil.gas.data,Months.On.Production<7)%>%mutate(Month.ID=paste0("Month.",Months.On.Production,".BOE"))%>%
   select(API,Month.ID,Month.BOE)%>%
   pivot_wider(names_from = Month.ID,values_from=Month.BOE)
cum.6mon.data<-oil.gas.data%>%group_by(API)%>%summarise(BOE_Cum_6mon=sum(Month.BOE))

prod.6mon.data<-joined.data.price%>%
   mutate(Month.BOE=Product.per.day..normalized.Oil*30+Product.per.day..normalized.Gas*5)%>%
   filter(Months.On.Production<=6)%>%group_by(API)%>%summarise(BOE_Prod_6mon=sum(Month.BOE))

rf.prod.data<-readRDS("RF_Production_pred_data2.rds")%>%
                  mutate(On.Stream.Month=month(On.Stream),On.Stream.Year=year(On.Stream),
                           Proppant.per.GPI..lb.ft.=as.numeric(Proppant.per.GPI..lb.ft.),
                           Fluid.per.GPI..gal.ft.=as.numeric(Fluid.per.GPI..gal.ft.),
                           Fluid.Type=as.factor(Fluid.Type))%>%
                  left_join(first.6.BOE)%>%
                  left_join(cum.6mon.data)

krig.sample.data<-read.csv("Kriging Sample File.csv")

pl.vars.df<-data.frame(API=character(),Month_try=numeric(),Lead.Coef=numeric(),Power.Coef=numeric(),
                       Cum_error=numeric(),Abs_Percent_Error=numeric())

#setwd("C:/Users/Wesley Engers/Documents/Eagle Ford Oil and Gas Project/Monthly Data/Oil App")
#https://shiny.rstudio.com/reference/shiny/latest/varSelectInput.html
#https://shiny.rstudio.com/articles/layout-guide.html


#if (interactive()) {
my_password="oil&gas"
# Define UI for application that draws a histogram
ui <- fluidPage(
   passwordInput("password", "Password:"),
   actionButton("go", "Submit"),
   # Application title
   titlePanel("Eagle Ford Well Production Analyzer"),
   
      mainPanel(
        
        tabsetPanel(type="tabs",
                    
            tabPanel("Inputs & Info",
                sidebarPanel(
                   textInput("API_num", "API #: ", "4201334338"),
                ),
                
                mainPanel(
                   h3(textOutput("info_header")),
                   textOutput("info"),
                   uiOutput("info_link"),
                   h3(textOutput("info_chart_header")),
                   plotOutput("PLPlot")
                )
                     ),

            tabPanel("Data Summaries",
                     sidebarPanel(
                        
                        numericInput("bins", "Bin Width:", value = 0.2),
                        varSelectInput("hist_var2", "Histogram Variable:", select(eagle.data3,-API), selected = NULL, multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL),
                        varSelectInput("map_var", "Map Gradient Variable:", select(rf.prod.data,-API,-Well.Name,-Operator,-Current.Operator,-County), selected = "TVD", multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL),
                     ),
                     
                     mainPanel(
                        plotOutput("histPlot"),
                        plotOutput("MapPlot"),
                        plotOutput("MapPlotLog")
                     )
                ),
            
            tabPanel("Clustering",
                     sidebarPanel(
                        downloadButton("downloadClust", "Download Cluster Data txt"),
                        downloadButton("downloadClustcsv", "Download Cluster Data csv"),
                        downloadButton("downloadClusterMap", "Download Cluster Map csv"),
                        varSelectInput("clust_var", "Cluster Variables:", select(rf.prod.data,-API,-Fluid.Type,-On.Stream,-Well.Name,-Operator,-Current.Operator,-County), selected = c("Thickness","TVD"), multiple = TRUE,
                                       selectize = TRUE, width = NULL, size = NULL),
                        numericInput("clust_num", "Number of Clusters:", value = 10),
                        numericInput("clust_invest", "Cluster Detail #:", value = 1),
                        selectizeInput("clust_map","Select Cluster to Display on Map",seq(1,2),multiple = TRUE),
                        varSelectInput("density_var", "Density Chart Variable:", select(rf.prod.data,Oil_Prod_2yr:TVD,Lateral..ft.:API.Gravity,Month.1.BOE:BOE_Cum_6mon), selected =c("BOE_Prod_2yr"), multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL),
                        varSelectInput("rf_var", "Variable Importance Input Variables:", select(rf.prod.data,-API,-Well.Name), selected = c("TVD","TOC"), multiple = TRUE,
                                       selectize = TRUE, width = NULL, size = NULL),
                        varSelectInput("rf_target_var", "Variable Importance Target Variable:", select(rf.prod.data,BOE_Prod_2yr,Oil_Prod_2yr,Gas_Prod_2yr,Month.1.BOE:BOE_Cum_6mon,Cumulative.Oil.Production...Mstb.,Cumulative.Gas.Production...MMscf.), selected = "BOE_Prod_2yr", multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL),
                        numericInput("cl_tree_num","Number of Trees for RF and GB", value = 500)
                     ),
                     
                     mainPanel(
                        h3(textOutput("import_krig_table_name")),
                        tableOutput("import_krig_display"),
                        plotOutput("clust_fviz"),
                        plotOutput("clust_map"),
                        plotOutput("density_clust_plot"),
                        h3(textOutput("clust_table_name")),
                        tableOutput("clust_summary"),
                        plotOutput("rand_forest"),
                        plotOutput("cl_bayes.boe.importance"),
                        plotOutput("cl_gbm.boe.importance"),
                        h3(textOutput("clust_table2_name")),
                        tableOutput("ClustTable")
                     )
                  ),
            
            tabPanel("Rev/Cost Analysis",
                     sidebarPanel(
                       numericInput("C_vd", "Cost of Vertical Depth($/ft):", value = 100),
                       numericInput("C_hd", "Cost of Horizontal Depth($/ft):", value = 200),
                       numericInput("C_stage", "Cost of Each Stage($/stage):", value = 200000),
                       numericInput("C_fluid", "Cost of Fluid($/gallon):", value = 0.1),
                       numericInput("C_prop", "Cost of Proppant($/pound):", value = 0.1),
                       numericInput("C_opex", "Cost of OPEX ($/month):", value = 200),
                       numericInput("C_process", "Cost of Gas Process ($/1000s ft^3):", value = 1),
                       numericInput("C_disposal", "Cost of water disposal ($/Mstb):", value = 5000),
                       numericInput("disc_fact","Annual Discount Factor (for NPV):",value=.03,step = .01)),

                     mainPanel(
                       plotOutput("cum.profit.well.plot"),
                       plotOutput("cum.profit.clust.plot"),
                       plotOutput("profit.well.plot"),
                       plotOutput("profit.clust.plot"),
                       plotOutput("cum.rev.plot"),
                       plotOutput("rev.plot"),
                       plotOutput("oil.price.plot"),
                       plotOutput("gas.price.plot"),
                       plotOutput("clust.rev.plot"),
                       plotOutput("cum.clust.rev.plot")
                     )),
            tabPanel("Power Law Fits",
                     
                     sidebarPanel(
                       textInput("pl_api","Well API to View",rf.prod.data$API[1]),
                       numericInput("num_month_fit", "Number of Months to fit PL/Hyp:", value = 12),
                       numericInput("num_sample_pl", "Number of Wells to Sample", value=50),
                       numericInput("min_pl_test", "Min Months for PL testing",value=3),
                       numericInput("max_pl_test", "Max Months for PL testing",value=12),
                       numericInput("max_error_months","Max Months for Error Calc",value=60),
                       numericInput("pl_b_start","Lowest Hyp b value to try:",0.01),
                       numericInput("pl_b_end","Highest Hyp b value to try:",2.01),
                       numericInput("pl_b_step","Step Size for Hyp b:", .25),
                       numericInput("pl_a_i_step","Step Size for Hyp a_i:",.005),
                       varSelectInput("pl_density_var", "Density Chart Variable:", select(pl.vars.df,Lead.Coef,Power.Coef,Abs_Percent_Error), selected = c("Lead.Coef"), multiple = FALSE,
                                      selectize = TRUE, width = NULL, size = NULL),
                       numericInput("pl_min_month_density","Min Months for Density Plot",12),
                       numericInput("pl_max_month_density","Max Months for Density Plot",12),
                       prettyToggle(
                          inputId = "pl_toggle", value = FALSE,
                          label_on = "Run Power Law Errors", icon_on = icon("check"),
                          label_off = "Don't run Power Law Errors", icon_off = icon("remove"))
                     ),
                     
                     mainPanel(
                       tableOutput("PL.Error.Table"),
                       tableOutput("PL.Well.Summary"),
                       plotOutput("PL_density"),
                       tableOutput("PL.API.Table"),
                       plotOutput("PL.BOE.well.plot"),
                       plotOutput("PL.BOE.well.error"),
                       plotOutput("PL.oil.well.plot"),
                       plotOutput("PL.oil.well.error"),
                       plotOutput("PL.gas.well.plot"),
                       plotOutput("PL.gas.well.error"),
                       plotOutput("PL.BOE.clust.plot"),
                       plotOutput("PL.BOE.clust.error"),
                       plotOutput("PL.oil.clust.plot"),
                       plotOutput("PL.oil.clust.error"),
                       plotOutput("PL.gas.clust.plot"),
                       plotOutput("PL.gas.clust.error")
                     )
                     ),
            
            tabPanel("ML Production Pred",
                     sidebarPanel(
                       varSelectInput("rf_prod_var", "Random Forest Input Variables:", select(rf.prod.data,-(API:BOE_Prod_2yr),-Cumulative.Oil.Production...Mstb.,-Cumulative.Gas.Production...MMscf.,-On.Stream), selected = c("Thickness","TVD"), multiple = TRUE,
                                      selectize = TRUE,width = NULL, size = NULL),
                       numericInput("train_prop","Training Proportion of Data",value = 0.7, step = .01),
                       numericInput("tree_num","Tree Number View (for download only)", value=2,step=1),
                       downloadButton("Download.rf.boe.error.train.data","Download Train Data & RF Errors"),
                       downloadButton("Download.rf.boe.error.test.data","Download Test Data & RF Errors"),
                       prettyToggle(
                         inputId = "rf_toggle", value = FALSE,
                         label_on = "Run Random Forest&Bayes!", icon_on = icon("check"),
                         label_off = "Don't run Random Forest&Bayes", icon_off = icon("remove")),
                       numericInput("rf_max_oil","Max Oil Prod Well to Include",value = 500000,step=1000),
                       numericInput("rf_min_oil","Min Oil Prod Well to Include",value = 0,step=1000),
                       numericInput("rf_max_gas","Max Gas Prod Well to Include",value = 2500000,step=1000),
                       numericInput("rf_min_gas","Min Gas Prod Well to Include",value = 0,step=1000),
                       numericInput("rf_error_bins", "Error # of Bins:", value = 30),
                       numericInput("max_hist_error", "Maximum Error for Histogram Display:",value = 100),
                       downloadButton("downloadTrainError", "Download RF Cluster Train Errors"),
                       downloadButton("downloadTestError", "Download RF Cluster Test Errors"),
                       downloadButton("downloadTLErrors", "Download TL Error Table"),
                       downloadButton("downloadTL_RF_Var_Imp", "Download TL RF Var Importance Table"),
                       downloadButton("downloadTL_Bayes_Var_Imp", "Download TL Bayes Var Importance Table"),
                       downloadButton("rf.clust.tree.dl","Download Tree View"),
                       numericInput("TL_min","Min TL Clusters",value=5, step=1),
                       numericInput("TL_max","Max TL Clusters",value=5, step=1),
                       varSelectInput("rf_clust_var", "Cluster Variables:", select(rf.prod.data,-API,-Oil_Prod_2yr,-Gas_Prod_2yr,-BOE_Prod_2yr,-Cumulative.Oil.Production...Mstb.,-Cumulative.Gas.Production...MMscf.,-Well.Name,-Operator,-Current.Operator,-County), selected = c("Thickness","TVD"), multiple = TRUE,
                                      selectize = TRUE, width = NULL, size = NULL),
                       numericInput("rf_clust_sil_num","Silhoutte Max Cluster Check:",value = 10),
                       prettyToggle(
                          inputId = "sil_toggle", value = FALSE,
                          label_on = "Run Silhoutte Check!", icon_on = icon("check"),
                          label_off = "Don't Run Silhoutte Check", icon_off = icon("remove")),
                       prettyToggle(
                          inputId = "pca_clust_toggle", value = FALSE,
                          label_on = "Run PCA before clustering", icon_on = icon("check"),
                          label_off = "No PCA before clustering", icon_off = icon("remove")),
                       numericInput("rf_clust_num", "Number of Clusters:", value = 5),
                       numericInput("rf_train_clust","Training Cluster:",value = 1),
                       numericInput("rf_test_clust","Testing Cluster:",value = 2),
                       numericInput("num_train_clust","# of Training Wells",value=10000),
                       selectizeInput("rf_clust_map","Select Clusters to Display on Map",seq(1,20),selected=seq(1,5),multiple = TRUE),
                       prettyToggle( ##Use this toggle to prevent clusters from being created too soon
                         inputId = "rf_toggle2", value = TRUE,
                         label_on = "Run Cluster Random Forest!", icon_on = icon("check"),
                         label_off = "Don't run Cluster Random Forest", icon_off = icon("remove"))
                     ),
                     
                     mainPanel(
                       tableOutput("rf.boe.prod.error"),
                       plotOutput("rf.boe.train.hist.error"),
                       plotOutput("rf.boe.test.hist.error"),
                       plotOutput("rf.boe.importance"),
                       plotOutput("bayes.boe.importance"),
                       plotOutput("RF_Clust_silhoutte"),
                       plotOutput("rf_clust_fviz"),
                       plotOutput("rf_clust_map"),
                       plotOutput("rf.clust.boe.importance"),
                       plotOutput("bayes.clust.boe.importance"),
                       tableOutput("rf.clust.boe.prod.error"),
                       plotOutput("rf_clust_train_scatter"),
                       plotOutput("rf.clust.boe.train.hist.error"),
                       plotOutput("rf_clust_test_scatter"),
                       plotOutput("rf.clust.boe.test.hist.error"),
                       tableOutput("TL_errors"),
                       tableOutput("TL_RF_Var_Imp"),
                       tableOutput("TL_Bayes_Var_Imp"),
                       textOutput("rf_clust_table_name"),
                       tableOutput("rf.prod.clust.summary")
                     )
            ),
            
         tabPanel("Cluster TL",
                     sidebarPanel(
                        #c(-101, 27.5, -96, 31) corners for entire eagle ford basin
                        numericInput("train_Lat_LL","Field A Lat Lower Left",27.5),
                        numericInput("train_Lon_LL","Field A Lon Lower Left",-101),
                        numericInput("train_Lat_UR","Field A Lat Upper Right",31),
                        numericInput("train_Lon_UR","Field A Lon Upper Right",-96),
                        
                        numericInput("test_Lat_LL","Field B Lat Lower Left",27.5),
                        numericInput("test_Lon_LL","Field B Lon Lower Left",-101),
                        numericInput("test_Lat_UR","Field B Lat Upper Right",31),
                        numericInput("test_Lon_UR","Field B Lon Upper Right",-96),
                        
                        varSelectInput("tl_prod_var", "Transfer Learning Input Variables:", select(rf.prod.data,-(API:BOE_Prod_2yr),-Cumulative.Oil.Production...Mstb.,-Cumulative.Gas.Production...MMscf.,-On.Stream), selected = c("Thickness","TVD"), multiple = TRUE,
                                       selectize = TRUE,width = NULL, size = NULL),
                        selectizeInput("tl_field_A","Select Clusters for Field A",seq(1,20),selected=seq(1,2),multiple = TRUE),
                        selectizeInput("tl_field_B","Select Clusters for Field B",seq(1,20),selected=seq(3,4),multiple = TRUE),
                        numericInput("tl_train_prop","Train Proportion from Field A",1),
                        numericInput("tl_validation_prop","Validation Proportion from Field B",.3),
                        numericInput("tl_test_prop","Train Prop from Field B excl. Validation",.95),
                        prettyToggle( ##Use this toggle to prevent maps and TL from being created too soon
                           inputId = "tl_toggle", value = FALSE,
                           label_on = "Run Cluster TL", icon_on = icon("check"),
                           label_off = "Don't Run Cluster TL", icon_off = icon("remove"))
                     ),
                  
                     mainPanel(
                        plotOutput("TL_train_map"),
                        plotOutput("TL_test_map"),
                        tableOutput("TL_Well_Counts"),
                        plotOutput("tl_train_hist"),
                        plotOutput("tl_train_scatter"),
                        plotOutput("tl_valid_hist"),
                        plotOutput("tl_valid_scatter"),
                        tableOutput("tl_error_table")
                  )
      ),
      
      tabPanel("Kriging TL",
               sidebarPanel(
                  #c(-101, 27.5, -96, 31) corners for entire eagle ford basin
                  #distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine) in meters
                  fileInput("krig_file", "Upload Kriging Data",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  downloadButton("downloadKrig_SampleData","Sample Krig Download"),
                  uiOutput('krig_Lat_LL_UI'),
                  uiOutput('krig_Lon_LL_UI'),
                  uiOutput('krig_Lat_UR_UI'),
                  uiOutput('krig_Lon_UR_UI'),
                  numericInput("krig_step","Number of Steps per side for Kriging grid",100),
                  numericInput("krig_spacing", "Spacing Between Wells (in acres):",120),
                  numericInput("krig_num_wells","Number of Wells for Kriging",100),
                  selectInput("krig_vario_display","Variogram Display Variable",
                              choices = c("TOC.scale","Permeability.scale","Brittleness.scale",
                                          "Porosity.scale","Thickness.scale","Vclay.scale","TVD.scale",
                                          "API.Gravity.scale"),
                              multiple = FALSE),
                  selectInput("krig_display_var","Kriging Display Variable",
                              choices = c("TOC","Permeability","Brittleness",
                                          "Porosity","Thickness","Vclay","TVD",
                                          "API.Gravity"),
                              multiple = FALSE),
                  numericInput("cert_num","Certainty Cutoff",value=0.5,step=.01),
                  numericInput("krig_prop",paste0("Prop Per GPI to Use in RF Model (",round(quantile(rf.prod.data$Proppant.per.GPI..lb.ft.,.05)[[1]],0),",",round(quantile(rf.prod.data$Proppant.per.GPI..lb.ft.,.95)[[1]],0),")"),
                               value = round(median(rf.prod.data$Proppant.per.GPI..lb.ft.),0),
                               min = round(quantile(rf.prod.data$Proppant.per.GPI..lb.ft.,.05)[[1]],0),
                               max = round(quantile(rf.prod.data$Proppant.per.GPI..lb.ft.,.95)[[1]],0)),
                  numericInput("krig_fluid",paste0("Fluid Per GPI to Use in RF Model (",round(quantile(rf.prod.data$Fluid.per.GPI..gal.ft.,.05)[[1]],0),",",round(quantile(rf.prod.data$Fluid.per.GPI..gal.ft.,.95)[[1]],0),")"),
                               value = round(median(rf.prod.data$Fluid.per.GPI..gal.ft.),0),
                               min = round(quantile(rf.prod.data$Fluid.per.GPI..gal.ft.,.05)[[1]],0),
                               max = round(quantile(rf.prod.data$Fluid.per.GPI..gal.ft.,.95)[[1]],0)),
                  numericInput("krig_gpi",paste0("GPI to Use in RF Model (",round(quantile(rf.prod.data$GPI,.05)[[1]],0),",",round(quantile(rf.prod.data$GPI,.95)[[1]],0),")"),
                               value = median(rf.prod.data$GPI),
                               min = round(quantile(rf.prod.data$GPI,.05)[[1]],0),
                               max = round(quantile(rf.prod.data$GPI,.95)[[1]],0)),
                  numericInput("krig_ll",paste0("Lateral Length to Use in RF Model (",round(quantile(rf.prod.data$Lateral..ft.,.05)[[1]],0),",",round(quantile(rf.prod.data$Lateral..ft.,.95)[[1]],0),")"),
                               value = median(rf.prod.data$Lateral..ft.),
                               min = round(quantile(rf.prod.data$Lateral..ft.,.05)[[1]],0),
                               max = round(quantile(rf.prod.data$Lateral..ft.,.95)[[1]],0)),
                  varSelectInput("krig_rf_vars","Variables to Use in RF",select(rf.prod.data,TOC:TVD,API.Gravity,Lateral..ft.,GPI,Proppant.per.GPI..lb.ft.,Fluid.per.GPI..gal.ft.), selected = c("Thickness","TVD"), multiple = TRUE,
                              selectize = TRUE,width = NULL, size = NULL),
                  numericInput("krig_rf_Lat_LL","RF Lat Lower Left",27.5),
                  numericInput("krig_rf_Lon_LL","RF Lon Lower Left",-101),
                  numericInput("krig_rf_Lat_UR","RF Lat Upper Right",31),
                  numericInput("krig_rf_Lon_UR","RF Lon Upper Right",-96),
                  numericInput("krig_rf_num_wells","Number of Wells for Random Forest",500),
                  numericInput("krig_clust_num","Number of Cluster in RF field",10),
                  varSelectInput("krig_clust_vars","Variables to Use in Clustering",select(rf.prod.data,TOC:TVD,API.Gravity), selected = c("Thickness","TVD"), multiple = TRUE,
                                 selectize = TRUE,width = NULL, size = NULL),
                  numericInput("krig_manually_clust","Cluster to use in RF",1),
                  prettyToggle(
                     inputId = "krig_clust_toggle", value = FALSE,
                     label_on = "Use Manual Input Cluster", icon_on = icon("check"),
                     label_off = "Use Closest Cluster", icon_off = icon("remove")),
                  numericInput("krig_pred_lat","Point Pred Lat:",29),
                  numericInput("krig_pred_lon","Point Pred Lon:",-99),
                  prettyToggle(
                     inputId = "krig_rf_toggle", value = FALSE,
                     label_on = "Run Kriging & Random Forest", icon_on = icon("check"),
                     label_off = "Don't run Kriging & Random Forest", icon_off = icon("remove")),
                  prettyToggle(
                     inputId = "clust_toggle", value = FALSE,
                     label_on = "Import Krig_TL Clusters to Other Tabs as Clusters", icon_on = icon("check"),
                     label_off = "Using Clustering Inputs", icon_off = icon("remove")),
               ),
               
               mainPanel(
                  h3(textOutput("krig_head_name")),
                  tableOutput("krig_upload_data"),
                  tableOutput("krig_size"),
                  plotOutput("krig_data_map"),
                  plotOutput("krig_variogram"),
                  plotOutput("krig_pred_map"),
                  plotOutput("krig.rf.boe.importance"),
                  h3(textOutput("krig_dist_table_heading")),
                  tableOutput("krig_dist_clust_table"),

                  h3(textOutput("krig_field_summary_name")),
                  tableOutput("krig_rf_summary"),
                  
                  h3(textOutput("krig_sensitivity_name")),
                  tableOutput("krig_rf_sense_summary"),
                  
                  h3(textOutput("krig_point_name")),
                  tableOutput("krig_point_pred"),
                  h3(textOutput("krig_table_name")),
                  tableOutput("krig_rf_data_table"),
                  plotOutput("krig_MapPlot"),
                  plotOutput("krig_boe_pred_map")
               )
      ),
      
      tabPanel("Ensemble Learning",
               sidebarPanel(

                 varSelectInput("el_filter_var1", "Filter Var 1", select(rf.prod.data,TOC,Permeability...d.,Brittleness,Porosity,Thickness,Vclay,TVD,GPI,Lateral..ft.,Proppant.per.GPI..lb.ft.,Fluid.per.GPI..gal.ft.,API.Gravity),
                                selected = "TOC", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                 varSelectInput("el_filter_var2", "Filter Var 2", select(rf.prod.data,TOC,Permeability...d.,Brittleness,Porosity,Thickness,Vclay,TVD,GPI,Lateral..ft.,Proppant.per.GPI..lb.ft.,Fluid.per.GPI..gal.ft.,API.Gravity),
                                selected = "TOC", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                 varSelectInput("el_filter_var3", "Filter Var 3", select(rf.prod.data,TOC,Permeability...d.,Brittleness,Porosity,Thickness,Vclay,TVD,GPI,Lateral..ft.,Proppant.per.GPI..lb.ft.,Fluid.per.GPI..gal.ft.,API.Gravity),
                                selected = "TOC", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                 varSelectInput("el_target_stress", "Stress Test Target Variable", select(rf.prod.data,BOE_Prod_2yr,Oil_Prod_2yr,Gas_Prod_2yr,Month.1.BOE:BOE_Cum_6mon),
                                selected = "BOE_Prod_2y", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                 varSelectInput("el_target_var", "Ensemble Learning Target Variable", select(rf.prod.data,BOE_Prod_2yr,Oil_Prod_2yr,Gas_Prod_2yr,Month.1.BOE:BOE_Cum_6mon),
                                selected = "BOE_Prod_2y", multiple = FALSE,
                                selectize = TRUE, width = NULL, size = NULL),
                 numericInput("el_low","Cut Low (Stress Test only)", value = .6),
                 numericInput("el_high","Cut High (Stress Test only)", value = 0.4),
                 
                 numericInput("el_start","Cut Start", value = .1),
                 numericInput("el_end","Cut End", value = 0.9),
                 numericInput("el_step", "Cut Steps", value=0.1),
                 varSelectInput("el_formula_var", "Input Variables for Models:", select(rf.prod.data,-(API:GOR_2yr),-(Fluid.Type:County)), selected = c("TVD","TOC"), multiple = TRUE,
                                selectize = TRUE, width = NULL, size = NULL),
                 numericInput("el_weight_step","Weighting Step",value = 0.1),
                 numericInput("el_num_tree","Tree Numbers for RF & GB",value = 1000,min = 0,step = 100),
                 numericInput("el_train_prop","Training Prop for Overlap Data",value = 0.7,min=0,max=1),
                 
                 prettyToggle(
                   inputId = "EL_toggle", value = FALSE,
                   label_on = "Run Ensemble Learning", icon_on = icon("check"),
                   label_off = "Don't run Ensemble Learning", icon_off = icon("x")),
                 
                 downloadButton("download_EL_Best_Weights","Download EL Best Weights"),
                 
                 prettyToggle(
                   inputId = "EL_toggle_st", value = FALSE,
                   label_on = "Run Stress Test", icon_on = icon("check"),
                   label_off = "Don't run Stress Test", icon_off = icon("x")),
                 
                 downloadButton("download_EL_Stress_Test","Download Stress Test Errors")
                ),

               mainPanel(
                 h3(textOutput("EL_low_text")),
                 tableOutput("EL_Low_df"),
                 h3(textOutput("EL_high_text")),
                 tableOutput("EL_High_df"),
                 h3(textOutput("EL_table_text")),
                 tableOutput("EL_Table"),
                 h3(textOutput("EL_Stress_Test_text")),
                 tableOutput("EL_Strees_Test_Table")
               )
      )
      
   )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   ##Info tab
   output$info_header<-renderText({
      "About the Eagle Ford Analyzer"
   })
   
   output$info<-renderText({
      "This application is used to analyze Oil and Gas Production from the Eagle Ford field in Texas."
   })
   
   url <- a("Oil App Information", href="https://docs.google.com/document/d/1SALS3FKkuqM97dKChQqXyLBWq0OC0-IZ/edit")
   output$info_link <- renderUI({
     tagList("Read Me:", url)
   })
   
   output$info_chart_header<-renderText({
      "Example Power Law Model"
   })
   
  output$clust_table_name<-renderText({req(input$password==my_password&input$go)
                                      "Cluster Summary Table"})
  output$clust_table2_name<-renderText({req(input$password==my_password&input$go)
                                       paste0("Cluster ",input$clust_invest," Details")})
  output$clust_num<-reactive({input$clust_num})  
  
  # output$dense_name<-renderText({req(input$password==my_password&input$go)
  #   paste0("Density Cluster ",input$clust_invest," Details")})
  
  output$rf_train_clust_table_name<-renderText({req(input$password==my_password&input$go)
    "Training Cluster Summary Table"})
  
  output$rf_test_clust_table_name<-renderText({req(input$password==my_password&input$go)
    "Testing Cluster Summary Table"})
  
  output$rf_clust_table_name<-renderText({req(input$password==my_password&input$go)
    "RF Cluster Summary Table"})
  
   output$histPlot <- renderPlot({
     req(input$password==my_password&input$go)
      my_hist<-ggplot(eagle.data3, aes(!!input$hist_var2)) + geom_histogram(binwidth = input$bins)+
          ggtitle(paste0("Histogram of ",input$hist_var2))+
          xlab(input$hist_var2)+
          theme(text = element_text(size=20))
      my_hist
   })
   
   output$PLPlot<-renderPlot({
      req(input$password==my_password&input$go)
      model.data<-rev.cost.joined.data()%>%filter(API==input$API_num, Months.On.Production>=2)%>%
         mutate(BOE=Model_Oil_Produced+Model_Gas_Produced/6,ln.BOE=log(BOE))
      BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
      pred.data<-rev.cost.joined.data()%>%filter(API==input$API_num,Months.On.Production>=2)%>%
         mutate(Model_BOE_Produced=Model_Oil_Produced+Model_Gas_Produced/6,
                Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2])%>%
         select(Months.On.Production,Model_BOE_Produced,Pred)%>%mutate(BOE_Produced=Model_BOE_Produced)%>%select(-Model_BOE_Produced)
      graph.data<-pred.data%>%pivot_longer(-Months.On.Production, names_to= "Output_Type", values_to="BOE.per.Month")
      PL.BOE.well.plot<-ggplot(graph.data,aes(x=Months.On.Production,y=BOE.per.Month,colour=Output_Type))+
         geom_point(size=2)+geom_line(linewidth=1.2)+
         theme(text = element_text(size=20),legend.position="bottom")+
         ggtitle(paste0("BOE Power Law Fit using All Months\nAPI#",input$API_num,":\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
      PL.BOE.well.plot
   })
   
   
   gg<-reactive({
     req(input$password==my_password&input$go)
     ##Maps
     ##Maps of Coefficients
     loc <- c(-101, 27.5, -96, 31)
     
     tx_map_gmaps <- get_map(location=loc, source="google", maptype="terrain")
     gg <- ggmap(tx_map_gmaps)
     gg
   })
   
   output$MapPlot<-renderPlot({
     req(input$password==my_password&input$go)
     map_plot<-gg()+geom_point(data=rf.prod.data, aes(x=Surf.Lon,  y=Surf.Lat,color=!!input$map_var),size=2) +
          ggtitle(paste0(input$map_var, " Gradient Color on Map"))+scale_color_gradientn(colours = topo.colors(5))+
          theme(text = element_text(size=20))
     map_plot
     
   })
   
   output$MapPlotLog<-renderPlot({
     req(input$password==my_password&input$go)
     map_plot<-gg()+geom_point(data=rf.prod.data, aes(x=Surf.Lon,  y=Surf.Lat,color=log(abs(!!input$map_var))),size=2) +
       ggtitle(paste0(input$map_var, " Log Gradient Color on Map"))+scale_color_gradientn(colours = topo.colors(5))+
       theme(text = element_text(size=20))
     map_plot
     
   })

##Clustering tab
   ClustScale<-reactive({
     req(input$password==my_password&input$go)
     if (length(input$clust_var) == 0){
       cluster.scale<-rf.prod.data%>%select(TVD,Thickness)%>%scale()
     } else {
       cluster.scale<-scale(rf.prod.data %>% dplyr::select(!!!input$clust_var))
     }
   })
   
   ClustData<-reactive({
     req(input$password==my_password&input$go)
     set.seed(277)
     my_cluster_scale<-kmeans(ClustScale(),centers = input$clust_num)
     
   })
   updateSelectizeInput(session, 'clust_map', choices =seq(1,20),selected=seq(1,10), server = TRUE)
   
   ClustData2<-reactive({
     req(input$password==my_password&input$go)
     cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))%>%
       filter(Cluster==input$clust_invest)%>%
       select(API,Cluster,Oil_Prod_2yr:API.Gravity,Cumulative.Oil.Production...Mstb.:On.Stream.Year,Month.1.BOE:BOE_Cum_6mon)
     if(input$clust_toggle){ ##imports Clusters from Kriging TL tab if that toggle is selected on Kriging TL tab
        cluster.data2<-import_krig_clust()%>%
           filter(Cluster==input$clust_invest)
     }
     cluster.data2
   })
   
   ClustData3<-reactive({ ##Goes into Cost/Rev tab
     req(input$password==my_password&input$go)
     cluster.data3<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))%>%
       select(API,Cluster)
     if(input$clust_toggle){  ##imports Clusters from Kriging TL tab if that toggle is selected on Kriging TL tab
        cluster.data3<-import_krig_clust()%>%
           select(API,Cluster)
     }
     cluster.data3
   })
   
   output$ClustTable<-renderTable({
     req(input$password==my_password&input$go)
     cluster.data2<-ClustData2()
     if(input$clust_toggle){
        cluster.data2<-import_krig_clust()%>%filter(Cluster==input$clust_invest)%>%
            select(API,Cluster,Oil_Prod_2yr:BOE_Cum_6mon)
     }
     cluster.data2
   })
   
   output$downloadClust<-downloadHandler(
     filename = function() {
       paste0("Cluster ", input$clust_invest, " Data.txt")
     },
     content=function(file) {
       write.table(ClustData2(),file,row.names=FALSE)
     }
   )
   
   output$downloadClustcsv<-downloadHandler(
     filename = function() {
       paste0("Cluster ", input$clust_invest, " Data.csv")
     },
     content=function(file) {
       write.csv(ClustData2(),file,row.names = FALSE)
     }
   )
   
   output$downloadClusterMap<-downloadHandler(
     filename = function() {
       paste0("Cluster", " Map.csv")
     },
     content=function(file) {
       write.csv(ClustData3(),file,row.names = FALSE)
     }
   )
   
   import_krig_clust<-reactive({
      #req(input$password==my_password&input$go&input$clust_toggle)
      krig_import_df<-krig_rf_data()%>%select(API)%>%left_join(rf.prod.data,by=c("API"="API"))%>%
         mutate(Cluster=factor(krig_clust_model()$cluster))
      krig_import_df
   })
   
   output$import_krig_table_name<-renderText({
      req(input$password==my_password&input$go&input$clust_toggle)
      "Head of Imported Kriging Data"})
   
   output$import_krig_display<-renderTable({
      req(input$password==my_password&input$go&input$clust_toggle)
      head(import_krig_clust())
   })
   
   output$clust_fviz<-renderPlot({
     req(input$password==my_password&input$go)
     fviz_plot<-fviz_cluster(ClustData(),data = ClustScale())+
       ggtitle(paste0("PCA Cluster Plot \nVariables: ",paste(sapply(input$clust_var,paste),collapse = " ")))+theme(text = element_text(size=20))
     if(input$clust_toggle){
        fviz_plot<-fviz_cluster(krig_clust_model(),data = filter(krig_scale_clust_data(),Type=="EagleFord")%>%select(-Type))+
           ggtitle(paste0("PCA Cluster Plot from Kriging TL \nVariables: ",paste(sapply(input$krig_clust_vars,paste),collapse = " ")))+theme(text = element_text(size=20))
     }
     fviz_plot
   })
   
   ##Create map of clusters on google maps
   output$clust_map<-renderPlot({
     req(input$password==my_password&input$go)
     cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))%>%filter(Cluster %in% input$clust_map)
     if(input$clust_toggle){
        cluster.data2<-import_krig_clust()%>%filter(Cluster %in% input$clust_map)
     }
     gg_clust_map<-gg()+geom_point(data=cluster.data2, aes(x=Surf.Lon,  y=Surf.Lat,color=Cluster),size=2) +
       ggtitle("Clusters on Map") + theme(text = element_text(size=20))
     gg_clust_map
   })
   
   output$density_clust_plot<-renderPlot({
      req(input$password==my_password&input$go)
      cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))
      if(input$clust_toggle){
         cluster.data2<-import_krig_clust()%>%filter(Cluster %in% input$clust_map)
      }
      density.cluster<-ggplot(cluster.data2, aes(x=!!input$density_var,color=factor(Cluster))) + 
         geom_density(alpha=.2,size=1) +ggtitle(paste0("Density of ",input$density_var, " by Cluster"))+ theme(text = element_text(size=20))
      density.cluster
   })
   
   output$clust_summary<-renderTable({
     req(input$password==my_password&input$go)

     cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))
     
     if(input$clust_toggle){
        cluster.data2<-import_krig_clust()
     }
     
     cluster.summary<-cluster.data2%>%group_by(Cluster)%>%
       summarise(Well.Count=n(),
                 Avg.TVD=mean(TVD),
                 Avg.Thickness=mean(Thickness),
                 Avg.Vclay=mean(Vclay),
                 Avg.TOC=mean(TOC),
                 Avg.GPI=mean(GPI),
                 Avg.Porosity=mean(Porosity),
                 Avg.Permeability=mean(Permeability...d.),
                 Avg.Brittleness=mean(Brittleness),
                 Avg.Lateral.Length=mean(Lateral..ft.),
                 Avg.Prop.per.GPI=mean(Proppant.per.GPI..lb.ft.,na.rm=TRUE),
                 Avg.Fluid.per.GPI=mean(Fluid.per.GPI..gal.ft.,na.rm=TRUE),
                 Avg.BOE.2yr=mean(BOE_Prod_2yr),
                 Avg.Oil.2yr=mean(Oil_Prod_2yr),
                 Avg.Gas.2yr=mean(Gas_Prod_2yr),
                 Avg.Month.1.BOE=mean(Month.1.BOE),
                 Avg.Month.2.BOE=mean(Month.2.BOE),
                 Avg.Month.3.BOE=mean(Month.3.BOE),
                 Avg.Month.4.BOE=mean(Month.4.BOE),
                 Avg.Month.5.BOE=mean(Month.5.BOE),
                 Avg.Month.6.BOE=mean(Month.6.BOE),
                 Avg.BOE_Cum_6mon=mean(BOE_Cum_6mon))

     cluster.summary
   })
   

   ##Variable importance of random forest
   randForest<-reactive({
     req(input$password==my_password&input$go)
     
     if(input$clust_toggle){
        cluster.data2<-import_krig_clust()
     }
     else{
       cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))
     }
     
     
     model.data<-cluster.data2%>%dplyr::select(!!!input$rf_target_var,Cluster,!!!input$rf_var)%>%
        filter(Cluster==input$clust_invest)%>%
        select(-Cluster)
     model.data<-na.omit(model.data)
     
     #BOE_Prod_2yr,Oil_Prod_2yr,Gas_Prod_2yr,Cumulative.Oil.Production...Mstb.,Cumulative.Gas.Production...MMscf.,BOE_Cum_6mon
     withProgress(message = 'Calculating Random Forest Importance', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       set.seed(107)
       rf.oil.prod<-randomForest(as.formula(paste(input$rf_target_var,"~ .")), data=model.data, ntree=input$cl_tree_num)
       
       incProgress(1/n, detail = paste(1, " of ", n))
     })
     rf.oil.prod
   })  
   
   output$rand_forest<-renderPlot({
     req(input$password==my_password&input$go)
      
      rf.var.imp<-varImp(randForest())
      rf.summary<-data.frame(var=rownames(rf.var.imp),
                                  rel.inf=rf.var.imp$Overall)%>%
         arrange(rel.inf)
      rf.summary<-rf.summary%>%
         mutate(var=factor(rf.summary$var, levels=rf.summary$var))
      
      rf_rel_inf_plot<-ggplot(data = rf.summary,aes(x=var,y=rel.inf))+
         geom_bar(stat="identity",color="black",fill="cornflowerblue")+
         geom_text(aes(label=round(rel.inf,1)), hjust=1.5, color="white",
                   position = position_dodge(0.9), size=3.5)+
         theme_bw()+
         xlab("Variable")+ylab("Variable Importance")+
         ggtitle(paste0("Cluster ",input$clust_invest," Random Forest Variable Importance Plot\n(Target: ",input$rf_target_var," )"))+
         theme(text = element_text(size=17))+
         coord_flip()
      rf_rel_inf_plot
     })
   
   cl_bayes.boe.model<-reactive({
     req(input$password==my_password&input$go)
     
     if(input$clust_toggle){
       cluster.data2<-import_krig_clust()
     }
     else{
       cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))
     }
     
     model.data<-cluster.data2%>%dplyr::select(!!!input$rf_target_var,Cluster,!!!input$rf_var)%>%
       filter(Cluster==input$clust_invest)%>%
       select(-Cluster)
     model.data<-na.omit(model.data)
     
     #BOE_Prod_2yr,Oil_Prod_2yr,Gas_Prod_2yr,Cumulative.Oil.Production...Mstb.,Cumulative.Gas.Production...MMscf.,BOE_Cum_6mon
     withProgress(message = 'Calculating Bayesian Importance', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       set.seed(107)
       bayes.boe.prod.model <- train(as.formula(paste(input$rf_target_var,"~ .")),
                                     data = model.data,
                                     method = "bayesglm"  # now we're using the bayesian glm method
       )
       incProgress(1/n, detail = paste(1, " of ", n))
     })
     bayes.boe.prod.model
   })
   
   output$cl_bayes.boe.importance<-renderPlot({
     bayes.var.imp<-varImp(cl_bayes.boe.model())
     bayes.summary<-data.frame(var=rownames(bayes.var.imp$importance),
                               rel.inf=bayes.var.imp$importance$Overall)%>%
       arrange(rel.inf)
     bayes.summary<-bayes.summary%>%
       mutate(var=factor(bayes.summary$var, levels=bayes.summary$var))
     
     bayes_rel_inf_plot<-ggplot(data = bayes.summary,aes(x=var,y=rel.inf))+
       geom_bar(stat="identity",color="black",fill="cornflowerblue")+
       geom_text(aes(label=round(rel.inf,1)), hjust=1.5, color="white",
                 position = position_dodge(0.9), size=3.5)+
       theme_bw()+
       xlab("Variable")+ylab("Relative Importance")+
       ggtitle(paste0("Cluster ",input$clust_invest," Bayesian GLM Relative Importance Plot\n(Target: ",input$rf_target_var," )"))+
       theme(text = element_text(size=17))+
       coord_flip()
     bayes_rel_inf_plot
   })
   
   cl_gbm.boe.model<-reactive({
     req(input$password==my_password&input$go)
     
     if(input$clust_toggle){
       cluster.data2<-import_krig_clust()
     }
     else{
       cluster.data2<-mutate(rf.prod.data,Cluster=factor(ClustData()$cluster))
     }
     
     model.data<-cluster.data2%>%dplyr::select(!!!input$rf_target_var,Cluster,!!!input$rf_var)%>%
       filter(Cluster==input$clust_invest)%>%
       select(-Cluster)
     model.data<-na.omit(model.data)
     
     #BOE_Prod_2yr,Oil_Prod_2yr,Gas_Prod_2yr,Cumulative.Oil.Production...Mstb.,Cumulative.Gas.Production...MMscf.,BOE_Cum_6mon
     withProgress(message = 'Calculating Gradient Boosting Importance', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       set.seed(107)
       gbm.boe.prod.model <- gbm(as.formula(paste(input$rf_target_var,"~ .")),
                                     data = model.data, n.trees = input$cl_tree_num)
       incProgress(1/n, detail = paste(1, " of ", n))
     })
     
     gbm.boe.prod.model
   })
   
   output$cl_gbm.boe.importance<-renderPlot({
     gbm.var.imp<-summary.gbm(cl_gbm.boe.model())
     gbm.summary<-data.frame(var=gbm.var.imp$var,
                               rel.inf=gbm.var.imp$rel.inf)%>%
       arrange(rel.inf)
     gbm.summary<-gbm.summary%>%
       mutate(var=factor(gbm.summary$var, levels=gbm.summary$var))
     
     gbm_rel_inf_plot<-ggplot(data = gbm.summary,aes(x=var,y=rel.inf))+
       geom_bar(stat="identity",color="black",fill="cornflowerblue")+
       geom_text(aes(label=round(rel.inf,1)), hjust=1.5, color="white",
                 position = position_dodge(0.9), size=3.5)+
       theme_bw()+
       xlab("Variable")+ylab("Relative Importance")+
       ggtitle(paste0("Cluster ",input$clust_invest," Gradient Boosting Relative Importance Plot\n(Target: ",input$rf_target_var," )"))+
       theme(text = element_text(size=17))+
       coord_flip()
     gbm_rel_inf_plot
   })
   
##Rev/Cost tab
   rev.cost.joined.data<-reactive({
     req(input$password==my_password&input$go)
     rev.cost.joined.data<-joined.data.price%>%left_join(select(ClustData3(),API,Cluster))%>%
       left_join(select(eagle.rev.cost.data,API,Lateral.Length,Total.Fluid,Total.Proppant.Volume,Cumulative.Water.Production...Mstb.))%>%
       mutate(Oil_Produced=30*Product.per.day..normalized.Oil,
              Gas_Produced=30*Product.per.day..normalized.Gas,
              Model_Oil_Produced=if_else(Months.On.Production==2,Oil_Produced+shift(Oil_Produced,1,type="lag"),Oil_Produced),
              Model_Gas_Produced=if_else(Months.On.Production==2,Gas_Produced+shift(Gas_Produced,1,type="lag"),Gas_Produced),
              ln.Month=log(Months.On.Production),
              ln.Oil=log(Model_Oil_Produced),
              ln.Gas=log(Model_Gas_Produced),
              Oil_Revenue_dollars=30*Product.per.day..normalized.Oil*oil.barrel.price,
              Gas_Revenue_dollars=30*Product.per.day..normalized.Gas*gas.price.dollars.per.mbtu,
              Total_Month_Revenue=Oil_Revenue_dollars+Gas_Revenue_dollars,
              Drill_Cost=if_else(Months.On.Production==1,input$C_vd*TVD+input$C_hd*Lateral.Length,0),
              Completion_Cost=if_else(Months.On.Production==1,ceiling(GPI/250)*input$C_stage+Total.Fluid*input$C_fluid+Total.Proppant.Volume*input$C_prop,0),
              OPEX=input$C_opex+Gas_Produced*input$C_process+(Cumulative.Water.Production...Mstb./12)*input$C_disposal,
              Total_Month_Cost=Drill_Cost+Completion_Cost+OPEX,
              Total_Month_Profit=Total_Month_Revenue-Total_Month_Cost,
              NPV_Total_Month_Profit=Total_Month_Profit/((1+input$disc_fact/12)^(Months.On.Production-1)))
     rev.cost.joined.data
   })
   
    output$rev.cost.table<-renderTable({
      head(rev.cost.joined.data())
    })
   
   output$oil.price.plot<-renderPlot({
     req(input$password==my_password&input$go)
     joined.well.data<-rev.cost.joined.data()%>%filter(API==input$API_num)
     oil.price.plot<-ggplot(joined.well.data, aes(x=Current.Date, y=oil.barrel.price)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Oil Price per Barrel Over\nLifetime of Well API ",input$API_num))+
       ylab("Oil Price ($/barrel)")
     oil.price.plot
   })
   
   output$gas.price.plot<-renderPlot({
     req(input$password==my_password&input$go)
     joined.well.data<-rev.cost.joined.data()%>%filter(API==input$API_num)
     gas.price.plot<-ggplot(joined.well.data, aes(x=Current.Date, y=gas.price.dollars.per.mbtu)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Gas Price per Mbtu Over\nLifetime of Well API ",input$API_num))+
       ylab("Gas Price ($/Mbtu)")
     gas.price.plot
   })
   
   output$rev.plot<-renderPlot({
     req(input$password==my_password&input$go)
     rev.well.data<-rev.cost.joined.data()%>%filter(API==input$API_num)
     rev.plot<-ggplot(rev.well.data, aes(x=Months.On.Production, y=Total_Month_Revenue)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Revenue by Production Month \nfor Well API ",input$API_num))+
       ylab("Revenue ($)")
     rev.plot
   })
   
   output$cum.rev.plot<-renderPlot({
     req(input$password==my_password&input$go)
     rev.well.data<-rev.cost.joined.data()%>%filter(API==input$API_num)%>%mutate(Cum.Revenue=cumsum(Total_Month_Revenue))
     cum.rev.plot<-ggplot(rev.well.data, aes(x=Months.On.Production, y=Cum.Revenue)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20),legend.position="bottom")+
       ggtitle(paste0("Cumulative Revenue by Production \nMonth for Well API ",input$API_num))+
       ylab("Cumulative Revenue ($)")
     cum.rev.plot
   })
  
   output$clust.rev.plot<-renderPlot({
     req(input$password==my_password&input$go)
     rev.clust.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
       group_by(Months.On.Production)%>%summarise(Clust.Avg.Month.Rev.per.well=mean(Total_Month_Revenue,na.rm=TRUE))
     clust.rev.plot<-ggplot(rev.clust.data, aes(x=Months.On.Production, y=Clust.Avg.Month.Rev.per.well)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Revenue by Production Month \nfor Cluster ",input$clust_invest))+
       ylab("Revenue per well($/well)")
     clust.rev.plot
   }) 
   
   output$cum.clust.rev.plot<-renderPlot({
     req(input$password==my_password&input$go)
     rev.clust.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
       group_by(Months.On.Production)%>%summarise(Clust.Avg.Month.Rev.per.well=mean(Total_Month_Revenue,na.rm=TRUE))%>%
       mutate(Cum.Avg.Revenue.per.well=cumsum(Clust.Avg.Month.Rev.per.well))
     cum.clust.rev.plot<-ggplot(rev.clust.data, aes(x=Months.On.Production, y=Cum.Avg.Revenue.per.well)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20),legend.position="bottom")+
       ggtitle(paste0("Cumulative Revenue by Production \nMonth for Cluster ",input$clust_invest))+
       ylab("Cumulative Revenue ($)")
     cum.clust.rev.plot
   })
   
   output$profit.well.plot<-renderPlot({
     req(input$password==my_password&input$go)
     graph.data<-rev.cost.joined.data()%>%filter(API==input$API_num)
     profit.plot<-ggplot(graph.data, aes(x=Months.On.Production, y=Total_Month_Profit)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Profit by Production Month \nfor Well API ",input$API_num))+
       ylab("Profit ($)")
     profit.plot
   })
   
   output$cum.profit.well.plot<-renderPlot({
     req(input$password==my_password&input$go)
     graph.data<-rev.cost.joined.data()%>%filter(API==input$API_num)%>%
       mutate(cumProfit=cumsum(Total_Month_Profit))
     npv<-round(sum(graph.data$NPV_Total_Month_Profit),2)
     profit.plot<-ggplot(graph.data, aes(x=Months.On.Production, y=cumProfit)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Cumlative Profit by Production Month \nfor Well API ",input$API_num,"\nNPV: $",npv))+
       ylab("Cumlative Profit ($)")
     profit.plot
   })
   
   output$profit.clust.plot<-renderPlot({
     req(input$password==my_password&input$go)
     graph.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
       group_by(Months.On.Production)%>%summarise(Clust_Month_Profit=mean(Total_Month_Profit,na.rm=TRUE),Clust_Month_NPV=mean(NPV_Total_Month_Profit,na.rm=TRUE))
     npv<-round(sum(graph.data$Clust_Month_NPV),2)
     profit.plot<-ggplot(graph.data, aes(x=Months.On.Production, y=Clust_Month_Profit)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Avg. Profit by Production Month \nfor Cluster ",input$clust_invest,"\nAvg. NPV: $",npv))+
       ylab("Avg. Profit ($/well)")
     profit.plot
   })
   
   output$cum.profit.clust.plot<-renderPlot({
     req(input$password==my_password&input$go)
     graph.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
       group_by(Months.On.Production)%>%summarise(Clust_Month_Profit=mean(Total_Month_Profit,na.rm=TRUE))%>%
       mutate(cumProfit=cumsum(Clust_Month_Profit))
     profit.plot<-ggplot(graph.data, aes(x=Months.On.Production, y=cumProfit)) + 
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       ggtitle(paste0("Cumulative Profit by Production Month \nfor Cluster ",input$clust_invest))+
       ylab("Cumulative Avg. Profit ($/well)")
     profit.plot
   })
   
##PL tab
   
   pl.data.table<-reactive({
      req(input$password==my_password&input$go&input$pl_toggle)
      set.seed(606)
      pl.samp<-slice_sample(eagle.rev.cost.data,n=input$num_sample_pl)
      pl.joined.data<-joined.data.price%>%filter(API %in% pl.samp$API)%>%
         left_join(select(eagle.rev.cost.data,API,Lateral.Length,Total.Fluid,Total.Proppant.Volume,Cumulative.Water.Production...Mstb.))%>%
         mutate(Oil_Produced=30*Product.per.day..normalized.Oil,
                Gas_Produced=30*Product.per.day..normalized.Gas,
                Model_Oil_Produced=if_else(Months.On.Production==2,Oil_Produced+shift(Oil_Produced,1,type="lag"),Oil_Produced),
                Model_Gas_Produced=if_else(Months.On.Production==2,Gas_Produced+shift(Gas_Produced,1,type="lag"),Gas_Produced),
                Model_BOE_Produced=Model_Oil_Produced+Model_Gas_Produced/6,
                ln.Month=log(Months.On.Production),
                ln.Oil=log(Model_Oil_Produced),
                ln.Gas=log(Model_Gas_Produced),
                ln.BOE=log(Model_BOE_Produced)
         )
      
      api.samp<-pl.samp$API
      pl.tune <- expand.grid(API=api.samp, Month_try=seq(input$min_pl_test,input$max_pl_test))
      pl.results<-data.frame(API=character(),Month_try=numeric(),Lead.Coef=numeric(),Power.Coef=numeric(),
                             Cum_error=numeric(),Abs_Percent_Error=numeric())
      
      withProgress(message = 'Calculating Power Laws', value = 0, {
        # Number of times we'll go through the loop
        n <- dim(pl.tune)[[1]]
        
        for (i in 1:dim(pl.tune)[[1]]){
           this.well.data<-filter(pl.joined.data,API==pl.tune$API[i])%>%select(API,Months.On.Production,Model_BOE_Produced,ln.Month,ln.BOE)
           model.data<-filter(this.well.data,Months.On.Production>=2,Months.On.Production<=pl.tune$Month_try[i])
           BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
           pred.data<-this.well.data%>%filter(Months.On.Production>=2)%>%
              mutate(Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
                     Error=Model_BOE_Produced-Pred)%>%
              filter(Months.On.Production<=input$max_error_months)
           this.result<-data.frame(API=pl.tune$API[i],Month_try=pl.tune$Month_try[i],
                                   Lead.Coef=exp(coef(BOE.model))[1],Power.Coef=(coef(BOE.model))[2],
                                   Cum_error=sum(pred.data$Pred)-sum(pred.data$Model_BOE_Produced))%>%
              mutate(Abs_Percent_Error=100*abs(Cum_error)/sum(pred.data$Model_BOE_Produced))
           pl.results<-rbind(pl.results,this.result)
           
           incProgress(1/n, detail = paste(i, " of ", n))
        }
      })

      pl.result.summary<-pl.results%>%group_by(Month_try)%>%summarise(Med_Abs_Percent_Error_BOE=median(Abs_Percent_Error))

      pl.output<-list(result.summary=pl.result.summary,coef.data=pl.results,data=pl.joined.data)
   })
   
   output$PL_density<-renderPlot({
      req(input$password==my_password&input$go&input$pl_toggle)
      pl.density.data<-pl.data.table()$coef.data%>%
               filter(Month_try>=input$pl_min_month_density,Month_try<=input$pl_max_month_density)
      density.pl<-ggplot(pl.density.data, aes(x=!!input$pl_density_var)) + 
         geom_density(alpha=.2,size=1) +
         ggtitle(paste0(input$pl_density_var," Density for Sample between\n",input$pl_min_month_density," and ",input$pl_max_month_density," Months"))+
         theme(text = element_text(size=20))
      density.pl
   })
   
   output$PL.Error.Table<-renderTable({
      req(input$password==my_password&input$go&input$pl_toggle)
      pl.data.table()$result.summary
   })
   
   output$PL.API.Table<-renderTable({
      head(data.frame(API=unique(pl.data.table()$data$API)),10)
   })
   
   output$PL.Well.Summary<-renderTable({
      req(input$password==my_password&input$go&input$pl_toggle)
      PL.well.summary<-pl.data.table()$data%>%group_by(API)%>%
         summarise(Well.Age=max(Months.On.Production))%>%
         summarise(Min_Age=min(Well.Age),
                   Mean_Age=mean(Well.Age),
                   Median_Age=median(Well.Age),
                   Max_Age=max(Well.Age))
      PL.well.summary
   })
   
   output$PL.BOE.well.plot<-renderPlot({
      req(input$password==my_password&input$go&input$pl_toggle)
      model.data<-rev.cost.joined.data()%>%filter(API==input$pl_api)%>%
         mutate(BOE=Model_Oil_Produced+Model_Gas_Produced/6,ln.BOE=log(BOE),
                Prev.Month.BOE.Prod=if_else(Months.On.Production==1,0,shift(BOE,1,type = "lag")),
                delta_q_t=BOE-Prev.Month.BOE.Prod,
                a_t=-delta_q_t/BOE)%>%
         filter(Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
      BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
      ##hyperbolic decline curve
      peak.prod<-max(model.data$BOE)
      q_i<-filter(model.data,BOE==peak.prod)$BOE[[1]]
      
      a_i<-filter(model.data,BOE==peak.prod)$a_t[[1]]
      a_i<-ifelse(a_i>0.8|a_i<.05,mean(filter(model.data,Months.On.Production<=4)$a_t),a_i)
      a_i<-ifelse(a_i>0.8|a_i<0.05,.275,a_i)
      
      this_peak_month<-filter(model.data,BOE==peak.prod)$Months.On.Production[[1]]
      
      b.grid<-expand.grid(b_try=seq(input$pl_b_start,input$pl_b_end,input$pl_b_step),a_i_try=seq(a_i-.05,a_i+.05,input$pl_a_i_step))
      b.result<-data.frame(b_try=numeric(),a_i_try=numeric(),Cum_Abs_Error=numeric())
      months.to.use<-input$num_month_fit
      
      withProgress(message = 'Calculating BOE Plot Curves', value = 0, {
        # Number of times we'll go through the loop
        n <- dim(b.grid)[[1]]
        
        for (i in 1:dim(b.grid)[[1]]){
           ##subtract "this_peak_month" from Months because peak production is month "this_peak_month" and decline estimate starts in month "this_peak_month"
           this.data<-model.data%>%
              mutate(Hyp_Pred=q_i/(1+b.grid$b_try[i]*a_i*(Months.On.Production-this_peak_month))^(1/b.grid$b_try[i]),
                     Error=BOE-Hyp_Pred, Abs_Error=abs(Error))%>%
              filter(Months.On.Production>=this_peak_month)
           this.result<-data.frame(b_try=b.grid$b_try[i],a_i_try=b.grid$a_i_try[i],Cum_Abs_Error=abs(sum(this.data$Hyp_Pred)-sum(this.data$Month.BOE.Prod)))
           b.result<-rbind(b.result,this.result)
        }
        incProgress(1/n, detail = paste(i, " of ", n))
      })
      
      b.result<-b.result%>%arrange((Cum_Abs_Error))
      b.best<-b.result$b_try[1]
      best.data<-rev.cost.joined.data()%>%filter(API==input$pl_api,Months.On.Production>=2)%>%
                              mutate(Model_BOE_Produced=Model_Oil_Produced+Model_Gas_Produced/6,
                                     PL_Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
                                     Hyp_Pred=q_i/(1+b.best*a_i*(Months.On.Production-this_peak_month))^(1/b.best),
                                     Hyp_Error=Model_BOE_Produced-Hyp_Pred, 
                                     Hyp_Abs_Error=abs(Hyp_Error), 
                                     Hyp_Abs_Per_Error=Hyp_Abs_Error/Model_BOE_Produced)%>%
                              filter(Months.On.Production>=this_peak_month)%>%
                              select(API,Months.On.Production,Model_BOE_Produced,PL_Pred,Hyp_Pred,Hyp_Error,Hyp_Abs_Error,Hyp_Abs_Per_Error)%>%
                              mutate(BOE_Produced=Model_BOE_Produced)%>%
                              select(-Model_BOE_Produced)
      
      best.data.graph<-best.data%>%select(API,Months.On.Production,BOE_Produced,PL_Pred,Hyp_Pred)%>%
         pivot_longer(BOE_Produced:Hyp_Pred,names_to="Output_Type",values_to="Prod_Value")
      
      cum.hyp.per.error<-abs(sum(best.data$Hyp_Pred)-sum(best.data$BOE_Produced))/sum(best.data$BOE_Produced)
      cum.pl.per.error<-abs(sum(best.data$PL_Pred)-sum(best.data$BOE_Produced))/sum(best.data$BOE_Produced)
      
      best.b.plot<-ggplot(best.data.graph,aes(x=Months.On.Production,y=Prod_Value,colour=Output_Type))+
         geom_point(size=2)+geom_line(linewidth=1.2)+
         theme(text = element_text(size=20),legend.position="bottom")+
         annotate("text", x = quantile(best.data.graph$Months.On.Production,.75), y = max(best.data.graph$Prod_Value)*.85, size=6,
                  label = paste0("Hyp. b value: ",b.best,"\n","Abs.Per.Error PL: ",round(cum.pl.per.error,4)*100,"%\n",
                                 "Abs.Per.Error Hyp: ",round(cum.hyp.per.error,4)*100,"%"))+
         ggtitle(paste0("Well Actual BOE Production, Power Law and\nHyperbolic Prediction for API#",input$pl_api,"\n",
                        "BOE Power Law Fit using ",input$num_month_fit," Months:\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
      best.b.plot
   })
   
   output$PL.BOE.well.error<-renderPlot({
      req(input$password==my_password&input$go&input$pl_toggle)
      model.data<-rev.cost.joined.data()%>%filter(API==input$pl_api, Months.On.Production>=2,Months.On.Production<=input$num_month_fit)%>%
         mutate(BOE=Model_Oil_Produced+Model_Gas_Produced/6,ln.BOE=log(BOE))
      BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
      pred.data<-rev.cost.joined.data()%>%filter(API==input$pl_api,Months.On.Production>=2)%>%
         mutate(Model_BOE_Produced=Model_Oil_Produced+Model_Gas_Produced/6,
                Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2])%>%
         select(Months.On.Production,Model_BOE_Produced,Pred)%>%mutate(BOE_Produced=Model_BOE_Produced)%>%select(-Model_BOE_Produced)
      error.data<-pred.data%>%mutate(Error_Month=Pred-BOE_Produced,Abs_Error_Month=abs(Error_Month),Abs_Percent_Error=Abs_Error_Month/BOE_Produced,Percent_Error=Error_Month/BOE_Produced)
      PL.BOE.well.error<-ggplot(error.data,aes(x=Months.On.Production,y=Percent_Error))+
         geom_point(size=2)+geom_line(linewidth=1.2)+
         theme(text = element_text(size=20))+
         scale_y_continuous(labels = scales::percent)+
         ggtitle(paste0("BOE Power Law Error using ", input$num_month_fit," Months\nAPI#",input$pl_api,":\nCumulative Percent Error: ",100*round(sum(error.data$Error_Month)/sum(error.data$BOE_Produced),3),"%"))
      PL.BOE.well.error
   })
   
   output$PL.oil.well.plot<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     ##For ease of copying code I have just redefined the BOE variable to be only oil for this
     model.data<-rev.cost.joined.data()%>%filter(API==input$pl_api)%>%
        mutate(BOE=Model_Oil_Produced,ln.BOE=log(BOE),
               Prev.Month.BOE.Prod=if_else(Months.On.Production==1,0,shift(BOE,1,type = "lag")),
               delta_q_t=BOE-Prev.Month.BOE.Prod,
               a_t=-delta_q_t/BOE)%>%
        filter(Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
     BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
     ##hyperbolic decline curve
     peak.prod<-max(model.data$BOE)
     q_i<-filter(model.data,BOE==peak.prod)$BOE[[1]]
     
     a_i<-filter(model.data,BOE==peak.prod)$a_t[[1]]
     a_i<-ifelse(a_i>0.8|a_i<.05,mean(filter(model.data,Months.On.Production<=4)$a_t),a_i)
     a_i<-ifelse(a_i>0.8|a_i<0.05,.275,a_i)
     
     this_peak_month<-filter(model.data,BOE==peak.prod)$Months.On.Production[[1]]
     
     b.grid<-expand.grid(b_try=seq(input$pl_b_start,input$pl_b_end,input$pl_b_step),a_i_try=seq(a_i-.05,a_i+.05,input$pl_a_i_step))
     b.result<-data.frame(b_try=numeric(),a_i_try=numeric(),Cum_Abs_Error=numeric())
     months.to.use<-input$num_month_fit
     
     withProgress(message = 'Calculating Oil Plot Curves', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(b.grid)[[1]]
       
       for (i in 1:dim(b.grid)[[1]]){
          ##subtract "this_peak_month" from Months because peak production is month "this_peak_month" and decline estimate starts in month "this_peak_month"
          this.data<-model.data%>%
             mutate(Hyp_Pred=q_i/(1+b.grid$b_try[i]*a_i*(Months.On.Production-this_peak_month))^(1/b.grid$b_try[i]),
                    Error=BOE-Hyp_Pred, Abs_Error=abs(Error))%>%
             filter(Months.On.Production>=this_peak_month)
          this.result<-data.frame(b_try=b.grid$b_try[i],a_i_try=b.grid$a_i_try[i],Cum_Abs_Error=abs(sum(this.data$Hyp_Pred)-sum(this.data$Month.BOE.Prod)))
          b.result<-rbind(b.result,this.result)
       }
       incProgress(1/n, detail = paste(i, " of ", n))
     })
     
     b.result<-b.result%>%arrange((Cum_Abs_Error))
     b.best<-b.result$b_try[1]
     best.data<-rev.cost.joined.data()%>%filter(API==input$pl_api,Months.On.Production>=2)%>%
        mutate(Model_BOE_Produced=Model_Oil_Produced,
               PL_Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
               Hyp_Pred=q_i/(1+b.best*a_i*(Months.On.Production-this_peak_month))^(1/b.best),
               Hyp_Error=Model_BOE_Produced-Hyp_Pred, 
               Hyp_Abs_Error=abs(Hyp_Error), 
               Hyp_Abs_Per_Error=Hyp_Abs_Error/Model_BOE_Produced)%>%
        filter(Months.On.Production>=this_peak_month)%>%
        select(API,Months.On.Production,Model_BOE_Produced,PL_Pred,Hyp_Pred,Hyp_Error,Hyp_Abs_Error,Hyp_Abs_Per_Error)%>%
        mutate(B_Oil_Produced=Model_BOE_Produced)%>%
        select(-Model_BOE_Produced)
     
     best.data.graph<-best.data%>%select(API,Months.On.Production,B_Oil_Produced,PL_Pred,Hyp_Pred)%>%
        pivot_longer(B_Oil_Produced:Hyp_Pred,names_to="Output_Type",values_to="Prod_Value")
     
     cum.hyp.per.error<-abs(sum(best.data$Hyp_Pred)-sum(best.data$B_Oil_Produced))/sum(best.data$B_Oil_Produced)
     cum.pl.per.error<-abs(sum(best.data$PL_Pred)-sum(best.data$B_Oil_Produced))/sum(best.data$B_Oil_Produced)
     
     best.b.plot<-ggplot(best.data.graph,aes(x=Months.On.Production,y=Prod_Value,colour=Output_Type))+
        geom_point(size=2)+geom_line(linewidth=1.2)+
        theme(text = element_text(size=20),legend.position="bottom")+
        annotate("text", x = quantile(best.data.graph$Months.On.Production,.75), y = max(best.data.graph$Prod_Value)*.85, size=6,
                 label = paste0("Hyp. b value: ",b.best,"\n","Abs.Per.Error PL: ",round(cum.pl.per.error,4)*100,"%\n",
                                "Abs.Per.Error Hyp: ",round(cum.hyp.per.error,4)*100,"%"))+
        ggtitle(paste0("Well Actual Oil Production, Power Law and\nHyperbolic Prediction for API#",input$pl_api,"\n",
                       "Oil Power Law Fit using ",input$num_month_fit," Months:\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
     best.b.plot
   })
   
   output$PL.oil.well.error<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     model.data<-rev.cost.joined.data()%>%filter(API==input$pl_api, Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
     oil.model<-lm(ln.Oil~ln.Month,data=model.data)
     pred.data<-rev.cost.joined.data()%>%filter(API==input$pl_api,Months.On.Production>=2)%>%
       mutate(Pred=(exp(coef(oil.model))[1])*(Months.On.Production)^(coef(oil.model))[2])%>%
       select(Months.On.Production,Model_Oil_Produced,Pred)%>%mutate(Oil_Produced=Model_Oil_Produced)%>%select(-Model_Oil_Produced)
     error.data<-pred.data%>%mutate(Error_Month=Pred-Oil_Produced,Abs_Error_Month=abs(Error_Month),Abs_Percent_Error=Abs_Error_Month/Oil_Produced,Percent_Error=Error_Month/Oil_Produced)
     PL.oil.well.error<-ggplot(error.data,aes(x=Months.On.Production,y=Percent_Error))+
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       scale_y_continuous(labels = scales::percent)+
       ggtitle(paste0("Oil Power Law Error using ", input$num_month_fit," Months\nAPI#",input$pl_api,":\nCumulative Percent Error: ",100*round(sum(error.data$Error_Month)/sum(error.data$Oil_Produced),3),"%"))
     PL.oil.well.error
     })
   
   output$PL.gas.well.plot<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     ##For ease of copying I have redefine the BOE to be Model_Gas_Produced/6
     model.data<-rev.cost.joined.data()%>%filter(API==input$pl_api)%>%
        mutate(BOE=Model_Gas_Produced/6,ln.BOE=log(BOE),
               Prev.Month.BOE.Prod=if_else(Months.On.Production==1,0,shift(BOE,1,type = "lag")),
               delta_q_t=BOE-Prev.Month.BOE.Prod,
               a_t=-delta_q_t/BOE)%>%
        filter(Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
     BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
     ##hyperbolic decline curve
     peak.prod<-max(model.data$BOE)
     q_i<-filter(model.data,BOE==peak.prod)$BOE[[1]]
     
     a_i<-filter(model.data,BOE==peak.prod)$a_t[[1]]
     a_i<-ifelse(a_i>0.8|a_i<.05,mean(filter(model.data,Months.On.Production<=4)$a_t),a_i)
     a_i<-ifelse(a_i>0.8|a_i<0.05,.275,a_i)
     
     this_peak_month<-filter(model.data,BOE==peak.prod)$Months.On.Production[[1]]
     
     b.grid<-expand.grid(b_try=seq(input$pl_b_start,input$pl_b_end,input$pl_b_step),a_i_try=seq(a_i-.05,a_i+.05,input$pl_a_i_step))
     b.result<-data.frame(b_try=numeric(),a_i_try=numeric(),Cum_Abs_Error=numeric())
     months.to.use<-input$num_month_fit
     
     withProgress(message = 'Calculating Gas Plot Curves', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(b.grid)[[1]]
       
       for (i in 1:dim(b.grid)[[1]]){
          ##subtract "this_peak_month" from Months because peak production is month "this_peak_month" and decline estimate starts in month "this_peak_month"
          this.data<-model.data%>%
             mutate(Hyp_Pred=q_i/(1+b.grid$b_try[i]*a_i*(Months.On.Production-this_peak_month))^(1/b.grid$b_try[i]),
                    Error=BOE-Hyp_Pred, Abs_Error=abs(Error))%>%
             filter(Months.On.Production>=this_peak_month)
          this.result<-data.frame(b_try=b.grid$b_try[i],a_i_try=b.grid$a_i_try[i],Cum_Abs_Error=abs(sum(this.data$Hyp_Pred)-sum(this.data$Month.BOE.Prod)))
          b.result<-rbind(b.result,this.result)
       }
       incProgress(1/n, detail = paste(i, " of ", n))
     })
     
     b.result<-b.result%>%arrange((Cum_Abs_Error))
     b.best<-b.result$b_try[1]
     best.data<-rev.cost.joined.data()%>%filter(API==input$pl_api,Months.On.Production>=2)%>%
        mutate(Model_BOE_Produced=Model_Gas_Produced/6,
               PL_Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
               Hyp_Pred=q_i/(1+b.best*a_i*(Months.On.Production-this_peak_month))^(1/b.best),
               Hyp_Error=Model_BOE_Produced-Hyp_Pred, 
               Hyp_Abs_Error=abs(Hyp_Error), 
               Hyp_Abs_Per_Error=Hyp_Abs_Error/Model_BOE_Produced)%>%
        filter(Months.On.Production>=this_peak_month)%>%
        select(API,Months.On.Production,Model_BOE_Produced,PL_Pred,Hyp_Pred,Hyp_Error,Hyp_Abs_Error,Hyp_Abs_Per_Error)%>%
        mutate(B_Gas_Produced=Model_BOE_Produced)%>%
        select(-Model_BOE_Produced)
     
     best.data.graph<-best.data%>%select(API,Months.On.Production,B_Gas_Produced,PL_Pred,Hyp_Pred)%>%
        pivot_longer(B_Gas_Produced:Hyp_Pred,names_to="Output_Type",values_to="Prod_Value")
     
     cum.hyp.per.error<-abs(sum(best.data$Hyp_Pred)-sum(best.data$B_Gas_Produced))/sum(best.data$B_Gas_Produced)
     cum.pl.per.error<-abs(sum(best.data$PL_Pred)-sum(best.data$B_Gas_Produced))/sum(best.data$B_Gas_Produced)
     
     best.b.plot<-ggplot(best.data.graph,aes(x=Months.On.Production,y=Prod_Value,colour=Output_Type))+
        geom_point(size=2)+geom_line(linewidth=1.2)+
        theme(text = element_text(size=20),legend.position="bottom")+
        annotate("text", x = quantile(best.data.graph$Months.On.Production,.75), y = max(best.data.graph$Prod_Value)*.85, size=6,
                 label = paste0("Hyp. b value: ",b.best,"\n","Abs.Per.Error PL: ",round(cum.pl.per.error,4)*100,"%\n",
                                "Abs.Per.Error Hyp: ",round(cum.hyp.per.error,4)*100,"%"))+
        ggtitle(paste0("Well Actual Gas Production, Power Law and\nHyperbolic Prediction for API#",input$pl_api,"\n",
                       "Gas Power Law Fit using ",input$num_month_fit," Months:\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
     best.b.plot
   })
   
   output$PL.gas.well.error<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     model.data<-rev.cost.joined.data()%>%filter(API==input$pl_api, Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
     gas.model<-lm(ln.Gas~ln.Month,data=model.data)
     pred.data<-rev.cost.joined.data()%>%filter(API==input$pl_api,Months.On.Production>=2)%>%
       mutate(Pred=(exp(coef(gas.model))[1])*(Months.On.Production)^(coef(gas.model))[2])%>%
       select(Months.On.Production,Model_Gas_Produced,Pred)%>%mutate(Gas_Produced=Model_Gas_Produced)%>%select(-Model_Gas_Produced)
     error.data<-pred.data%>%mutate(Error_Month=Pred-Gas_Produced,Abs_Error_Month=abs(Error_Month),Abs_Percent_Error=Abs_Error_Month/Gas_Produced,Percent_Error=Error_Month/Gas_Produced)
     PL.gas.well.error<-ggplot(error.data,aes(x=Months.On.Production,y=Percent_Error))+
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       scale_y_continuous(labels = scales::percent)+
       ggtitle(paste0("Gas Power Law Error using ", input$num_month_fit," Months\nAPI#",input$pl_api,":\nCumulative Percent Error: ",100*round(sum(error.data$Error_Month)/sum(error.data$Gas_Produced),3),"%"))
     PL.gas.well.error
   })
   
   output$PL.BOE.clust.plot<-renderPlot({
      req(input$password==my_password&input$go&input$pl_toggle)
      
      ##For ease of copying I have redefine the BOE to be the average for the cluster
      model.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
         mutate(BOE_temp=Model_Oil_Produced+Model_Gas_Produced/6)%>%
         group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE_temp,na.rm=TRUE))%>%
         mutate(ln.Month=log(Months.On.Production),ln.BOE=log(Clust_Avg_BOE_Prod),
                BOE=Clust_Avg_BOE_Prod,
                Prev.Month.BOE.Prod=if_else(Months.On.Production==1,0,shift(BOE,1,type = "lag")),
                delta_q_t=BOE-Prev.Month.BOE.Prod,
                a_t=-delta_q_t/BOE)%>%
         filter(Months.On.Production>=2,Months.On.Production<=input$num_month_fit)

      BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
      
      ##hyperbolic decline curve
      peak.prod<-max(model.data$BOE)
      q_i<-filter(model.data,BOE==peak.prod)$BOE[[1]]

      a_i<-filter(model.data,BOE==peak.prod)$a_t[[1]]
      a_i<-ifelse(a_i>0.8|a_i<.05,mean(filter(model.data,Months.On.Production<=4)$a_t),a_i)
      a_i<-ifelse(a_i>0.8|a_i<0.05,.275,a_i)

      this_peak_month<-filter(model.data,BOE==peak.prod)$Months.On.Production[[1]]

      b.grid<-expand.grid(b_try=seq(input$pl_b_start,input$pl_b_end,input$pl_b_step),a_i_try=seq(a_i-.05,a_i+.05,input$pl_a_i_step))
      b.result<-data.frame(b_try=numeric(),a_i_try=numeric(),Cum_Abs_Error=numeric())
      months.to.use<-input$num_month_fit
      
      withProgress(message = 'Calculating Cluster BOE Plot Curves', value = 0, {
        # Number of times we'll go through the loop
        n <- dim(b.grid)[[1]]

        for (i in 1:dim(b.grid)[[1]]){
           ##subtract "this_peak_month" from Months because peak production is month "this_peak_month" and decline estimate starts in month "this_peak_month"
           this.data<-model.data%>%
              mutate(Hyp_Pred=q_i/(1+b.grid$b_try[i]*a_i*(Months.On.Production-this_peak_month))^(1/b.grid$b_try[i]),
                     Error=BOE-Hyp_Pred, Abs_Error=abs(Error))%>%
              filter(Months.On.Production>=this_peak_month)
           this.result<-data.frame(b_try=b.grid$b_try[i],a_i_try=b.grid$a_i_try[i],Cum_Abs_Error=abs(sum(this.data$Hyp_Pred)-sum(this.data$BOE)))
           b.result<-rbind(b.result,this.result)
        }
        incProgress(1/n, detail = paste(i, " of ", n))
      })

      b.result<-b.result%>%arrange((Cum_Abs_Error))
      b.best<-b.result$b_try[1]
      best.data<-rev.cost.joined.data()%>%
         mutate(BOE_temp=Model_Oil_Produced+Model_Gas_Produced/6)%>%
         filter(Cluster==input$clust_invest,Months.On.Production>=2)%>%
         group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE_temp,na.rm=TRUE))%>%
         mutate(Model_BOE_Produced=Clust_Avg_BOE_Prod,
                PL_Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
                Hyp_Pred=q_i/(1+b.best*a_i*(Months.On.Production-this_peak_month))^(1/b.best),
                Hyp_Error=Model_BOE_Produced-Hyp_Pred,
                Hyp_Abs_Error=abs(Hyp_Error),
                Hyp_Abs_Per_Error=Hyp_Abs_Error/Model_BOE_Produced)%>%
         filter(Months.On.Production>=this_peak_month)%>%
         select(Months.On.Production,Model_BOE_Produced,PL_Pred,Hyp_Pred,Hyp_Error,Hyp_Abs_Error,Hyp_Abs_Per_Error)%>%
         mutate(BOE_Produced=Model_BOE_Produced)%>%
         select(-Model_BOE_Produced)

      best.data.graph<-best.data%>%select(Months.On.Production,BOE_Produced,PL_Pred,Hyp_Pred)%>%
         pivot_longer(BOE_Produced:Hyp_Pred,names_to="Output_Type",values_to="Prod_Value")

      cum.hyp.per.error<-abs(sum(best.data$Hyp_Pred)-sum(best.data$BOE_Produced))/sum(best.data$BOE_Produced)
      cum.pl.per.error<-abs(sum(best.data$PL_Pred)-sum(best.data$BOE_Produced))/sum(best.data$BOE_Produced)

      best.b.plot<-ggplot(best.data.graph,aes(x=Months.On.Production,y=Prod_Value,colour=Output_Type))+
         geom_point(size=2)+geom_line(linewidth=1.2)+
         theme(text = element_text(size=20),legend.position="bottom")+
         annotate("text", x = quantile(best.data.graph$Months.On.Production,.75), y = max(best.data.graph$Prod_Value)*.85, size=6,
                  label = paste0("Hyp. b value: ",b.best,"\n","Abs.Per.Error PL: ",round(cum.pl.per.error,4)*100,"%\n",
                                 "Abs.Per.Error Hyp: ",round(cum.hyp.per.error,4)*100,"%"))+
         ggtitle(paste0("Cluster Actual Avg. BOE Production, Power Law and\nHyperbolic Prediction for Cluster#",input$clust_invest,"\n",
                        "BOE Power Law Fit using ",input$num_month_fit," Months:\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
      best.b.plot
   })
   
   output$PL.BOE.clust.error<-renderPlot({
      req(input$password==my_password&input$go&input$pl_toggle)
      model.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest, Months.On.Production>=2,Months.On.Production<=input$num_month_fit)%>%
         mutate(BOE=Model_Oil_Produced+Model_Gas_Produced/6)%>%
         group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE,na.rm=TRUE))%>%
         mutate(ln.Month=log(Months.On.Production),ln.BOE=log(Clust_Avg_BOE_Prod))
      BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
      pred.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest, Months.On.Production>=2)%>%
         mutate(BOE=Model_Oil_Produced+Model_Gas_Produced/6)%>%
         group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE,na.rm=TRUE))%>%
         mutate(Model_BOE_Produced=Clust_Avg_BOE_Prod,
                Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2])%>%
         select(Months.On.Production,Model_BOE_Produced,Pred)%>%mutate(BOE_Produced=Model_BOE_Produced)%>%select(-Model_BOE_Produced)
      error.data<-pred.data%>%mutate(Error_Month=Pred-BOE_Produced,Abs_Error_Month=abs(Error_Month),Abs_Percent_Error=Abs_Error_Month/BOE_Produced,Percent_Error=Error_Month/BOE_Produced)
      PL.BOE.clust.error<-ggplot(error.data,aes(x=Months.On.Production,y=Percent_Error))+
         geom_point(size=2)+geom_line(linewidth=1.2)+
         theme(text = element_text(size=20))+
         scale_y_continuous(labels = scales::percent)+
         ggtitle(paste0("BOE Power Law Error using ", input$num_month_fit," Months\nCluster#",input$clust_invest,":\nCumulative Percent Error: ",100*round(sum(error.data$Error_Month)/sum(error.data$BOE_Produced),3),"%"))
      PL.BOE.clust.error
   })
   
   output$PL.oil.clust.plot<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)

     ##For ease of copying I have redefine the BOE to be the average Oil for the cluster
     model.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
        mutate(BOE_temp=Model_Oil_Produced)%>%
        group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE_temp,na.rm=TRUE))%>%
        mutate(ln.Month=log(Months.On.Production),ln.BOE=log(Clust_Avg_BOE_Prod),
               BOE=Clust_Avg_BOE_Prod,
               Prev.Month.BOE.Prod=if_else(Months.On.Production==1,0,shift(BOE,1,type = "lag")),
               delta_q_t=BOE-Prev.Month.BOE.Prod,
               a_t=-delta_q_t/BOE)%>%
        filter(Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
     
     BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
     
     # ##hyperbolic decline curve
     peak.prod<-max(model.data$BOE)
     q_i<-filter(model.data,BOE==peak.prod)$BOE[[1]]
     
     a_i<-filter(model.data,BOE==peak.prod)$a_t[[1]]
     a_i<-ifelse(a_i>0.8|a_i<.05,mean(filter(model.data,Months.On.Production<=4)$a_t),a_i)
     a_i<-ifelse(a_i>0.8|a_i<0.05,.275,a_i)
     
     this_peak_month<-filter(model.data,BOE==peak.prod)$Months.On.Production[[1]]
     
     b.grid<-expand.grid(b_try=seq(input$pl_b_start,input$pl_b_end,input$pl_b_step),a_i_try=seq(a_i-.05,a_i+.05,input$pl_a_i_step))
     b.result<-data.frame(b_try=numeric(),a_i_try=numeric(),Cum_Abs_Error=numeric())
     months.to.use<-input$num_month_fit
     
     withProgress(message = 'Calculating Cluster Oil Plot Curves', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(b.grid)[[1]]
       
       for (i in 1:dim(b.grid)[[1]]){
          ##subtract "this_peak_month" from Months because peak production is month "this_peak_month" and decline estimate starts in month "this_peak_month"
          this.data<-model.data%>%
             mutate(Hyp_Pred=q_i/(1+b.grid$b_try[i]*a_i*(Months.On.Production-this_peak_month))^(1/b.grid$b_try[i]),
                    Error=BOE-Hyp_Pred, Abs_Error=abs(Error))%>%
             filter(Months.On.Production>=this_peak_month)
          this.result<-data.frame(b_try=b.grid$b_try[i],a_i_try=b.grid$a_i_try[i],Cum_Abs_Error=abs(sum(this.data$Hyp_Pred)-sum(this.data$BOE)))
          b.result<-rbind(b.result,this.result)
       }
       incProgress(1/n, detail = paste(i, " of ", n))
     })
     
     b.result<-b.result%>%arrange((Cum_Abs_Error))
     b.best<-b.result$b_try[1]
     best.data<-rev.cost.joined.data()%>%
        mutate(BOE_temp=Model_Oil_Produced)%>%
        filter(Cluster==input$clust_invest,Months.On.Production>=2)%>%
        group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE_temp,na.rm=TRUE))%>%
        mutate(Model_BOE_Produced=Clust_Avg_BOE_Prod,
               PL_Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
               Hyp_Pred=q_i/(1+b.best*a_i*(Months.On.Production-this_peak_month))^(1/b.best),
               Hyp_Error=Model_BOE_Produced-Hyp_Pred,
               Hyp_Abs_Error=abs(Hyp_Error),
               Hyp_Abs_Per_Error=Hyp_Abs_Error/Model_BOE_Produced)%>%
        filter(Months.On.Production>=this_peak_month)%>%
        select(Months.On.Production,Model_BOE_Produced,PL_Pred,Hyp_Pred,Hyp_Error,Hyp_Abs_Error,Hyp_Abs_Per_Error)%>%
        mutate(Oil_Produced=Model_BOE_Produced)%>%
        select(-Model_BOE_Produced)
     
     best.data.graph<-best.data%>%select(Months.On.Production,Oil_Produced,PL_Pred,Hyp_Pred)%>%
        pivot_longer(Oil_Produced:Hyp_Pred,names_to="Output_Type",values_to="Prod_Value")
     
     cum.hyp.per.error<-abs(sum(best.data$Hyp_Pred)-sum(best.data$Oil_Produced))/sum(best.data$Oil_Produced)
     cum.pl.per.error<-abs(sum(best.data$PL_Pred)-sum(best.data$Oil_Produced))/sum(best.data$Oil_Produced)
     
     best.b.plot<-ggplot(best.data.graph,aes(x=Months.On.Production,y=Prod_Value,colour=Output_Type))+
        geom_point(size=2)+geom_line(size=1.2)+
        theme(text = element_text(size=20),legend.position="bottom")+
        annotate("text", x = quantile(best.data.graph$Months.On.Production,.75), y = max(best.data.graph$Prod_Value)*.85, size=6,
                 label = paste0("Hyp. b value: ",b.best,"\n","Abs.Per.Error PL: ",round(cum.pl.per.error,4)*100,"%\n",
                                "Abs.Per.Error Hyp: ",round(cum.hyp.per.error,4)*100,"%"))+
        ggtitle(paste0("Cluster Actual Avg. Oil Production, Power Law and\nHyperbolic Prediction for Cluster#",input$clust_invest,"\n",
                       "Oil Power Law Fit using ",input$num_month_fit," Months:\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
     best.b.plot
   })
   
   output$PL.oil.clust.error<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     model.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest, Months.On.Production>=2,Months.On.Production<=input$num_month_fit)%>%
       group_by(Months.On.Production)%>%summarise(Clust_Avg_Oil_Prod=mean(Model_Oil_Produced,na.rm=TRUE))%>%
       mutate(ln.Month=log(Months.On.Production),ln.Oil=log(Clust_Avg_Oil_Prod))
     oil.model<-lm(ln.Oil~ln.Month,data=model.data)
     pred.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest, Months.On.Production>=2)%>%
       group_by(Months.On.Production)%>%summarise(Clust_Avg_Oil_Prod=mean(Model_Oil_Produced,na.rm=TRUE))%>%
       mutate(Pred=(exp(coef(oil.model))[1])*(Months.On.Production)^(coef(oil.model))[2])
     error.data<-pred.data%>%mutate(Error_Month=Pred-Clust_Avg_Oil_Prod,Abs_Error_Month=abs(Error_Month),Abs_Percent_Error=Abs_Error_Month/Clust_Avg_Oil_Prod,Percent_Error=Error_Month/Clust_Avg_Oil_Prod)
     PL.oil.clust.error<-ggplot(error.data,aes(x=Months.On.Production,y=Percent_Error))+
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
       scale_y_continuous(labels = scales::percent)+
       ggtitle(paste0("Oil Power Law Error using ", input$num_month_fit," Months\nCluster#",input$clust_invest,":\nCumulative Percent Error: ",100*round(sum(error.data$Error_Month)/sum(error.data$Clust_Avg_Oil_Prod),3),"%"))
     PL.oil.clust.error
   })
   
   output$PL.gas.clust.plot<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     
     ##For ease of copying I have redefine the BOE to be the average Gas for the cluster
     model.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest)%>%
        mutate(BOE_temp=Model_Gas_Produced/6)%>%
        group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE_temp,na.rm=TRUE))%>%
        mutate(ln.Month=log(Months.On.Production),ln.BOE=log(Clust_Avg_BOE_Prod),
               BOE=Clust_Avg_BOE_Prod,
               Prev.Month.BOE.Prod=if_else(Months.On.Production==1,0,shift(BOE,1,type = "lag")),
               delta_q_t=BOE-Prev.Month.BOE.Prod,
               a_t=-delta_q_t/BOE)%>%
        filter(Months.On.Production>=2,Months.On.Production<=input$num_month_fit)
     
     BOE.model<-lm(ln.BOE~ln.Month,data=model.data)
     
     ##hyperbolic decline curve
     peak.prod<-max(model.data$BOE)
     q_i<-filter(model.data,BOE==peak.prod)$BOE[[1]]
     
     a_i<-filter(model.data,BOE==peak.prod)$a_t[[1]]
     a_i<-ifelse(a_i>0.8|a_i<.05,mean(filter(model.data,Months.On.Production<=4)$a_t),a_i)
     a_i<-ifelse(a_i>0.8|a_i<0.05,.275,a_i)
     
     this_peak_month<-filter(model.data,BOE==peak.prod)$Months.On.Production[[1]]
     
     b.grid<-expand.grid(b_try=seq(input$pl_b_start,input$pl_b_end,input$pl_b_step),a_i_try=seq(a_i-.05,a_i+.05,input$pl_a_i_step))
     b.result<-data.frame(b_try=numeric(),a_i_try=numeric(),Cum_Abs_Error=numeric())
     months.to.use<-input$num_month_fit
     
     withProgress(message = 'Calculating Cluster Gas Plot Curves', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(b.grid)[[1]]
     
       for (i in 1:dim(b.grid)[[1]]){
          ##subtract "this_peak_month" from Months because peak production is month "this_peak_month" and decline estimate starts in month "this_peak_month"
          this.data<-model.data%>%
             mutate(Hyp_Pred=q_i/(1+b.grid$b_try[i]*a_i*(Months.On.Production-this_peak_month))^(1/b.grid$b_try[i]),
                    Error=BOE-Hyp_Pred, Abs_Error=abs(Error))%>%
             filter(Months.On.Production>=this_peak_month)
          this.result<-data.frame(b_try=b.grid$b_try[i],a_i_try=b.grid$a_i_try[i],Cum_Abs_Error=abs(sum(this.data$Hyp_Pred)-sum(this.data$BOE)))
          b.result<-rbind(b.result,this.result)
       }
       incProgress(1/n, detail = paste(i, " of ", n))
     })
     
     b.result<-b.result%>%arrange((Cum_Abs_Error))
     b.best<-b.result$b_try[1]
     best.data<-rev.cost.joined.data()%>%
        mutate(BOE_temp=Model_Gas_Produced/6)%>%
        filter(Cluster==input$clust_invest,Months.On.Production>=2)%>%
        group_by(Months.On.Production)%>%summarise(Clust_Avg_BOE_Prod=mean(BOE_temp,na.rm=TRUE))%>%
        mutate(Model_BOE_Produced=Clust_Avg_BOE_Prod,
               PL_Pred=(exp(coef(BOE.model))[1])*(Months.On.Production)^(coef(BOE.model))[2],
               Hyp_Pred=q_i/(1+b.best*a_i*(Months.On.Production-this_peak_month))^(1/b.best),
               Hyp_Error=Model_BOE_Produced-Hyp_Pred,
               Hyp_Abs_Error=abs(Hyp_Error),
               Hyp_Abs_Per_Error=Hyp_Abs_Error/Model_BOE_Produced)%>%
        filter(Months.On.Production>=this_peak_month)%>%
        select(Months.On.Production,Model_BOE_Produced,PL_Pred,Hyp_Pred,Hyp_Error,Hyp_Abs_Error,Hyp_Abs_Per_Error)%>%
        mutate(Gas_Produced=Model_BOE_Produced)%>%
        select(-Model_BOE_Produced)
     
     best.data.graph<-best.data%>%select(Months.On.Production,Gas_Produced,PL_Pred,Hyp_Pred)%>%
        pivot_longer(Gas_Produced:Hyp_Pred,names_to="Output_Type",values_to="Prod_Value")
     
     cum.hyp.per.error<-abs(sum(best.data$Hyp_Pred)-sum(best.data$Gas_Produced))/sum(best.data$Gas_Produced)
     cum.pl.per.error<-abs(sum(best.data$PL_Pred)-sum(best.data$Gas_Produced))/sum(best.data$Gas_Produced)
     
     best.b.plot<-ggplot(best.data.graph,aes(x=Months.On.Production,y=Prod_Value,colour=Output_Type))+
        geom_point(size=2)+geom_line(linewidth=1.2)+
        theme(text = element_text(size=20),legend.position="bottom")+
        annotate("text", x = quantile(best.data.graph$Months.On.Production,.75), y = max(best.data.graph$Prod_Value)*.85, size=6,
                 label = paste0("Hyp. b value: ",b.best,"\n","Abs.Per.Error PL: ",round(cum.pl.per.error,4)*100,"%\n",
                                "Abs.Per.Error Hyp: ",round(cum.hyp.per.error,4)*100,"%"))+
        ggtitle(paste0("Cluster Actual Avg. Gas Production, Power Law and\nHyperbolic Prediction for Cluster#",input$clust_invest,"\n",
                       "Gas Power Law Fit using ",input$num_month_fit," Months:\n",round(exp(coef(BOE.model))[1],2),"*(Months.On.Production)^",round((coef(BOE.model))[2],2),")"))
     best.b.plot
   })
   
   output$PL.gas.clust.error<-renderPlot({
     req(input$password==my_password&input$go&input$pl_toggle)
     model.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest, Months.On.Production>=2,Months.On.Production<=input$num_month_fit)%>%
       group_by(Months.On.Production)%>%summarise(Clust_Avg_Gas_Prod=mean(Model_Gas_Produced,na.rm=TRUE))%>%
       mutate(ln.Month=log(Months.On.Production),ln.Gas=log(Clust_Avg_Gas_Prod))
     gas.model<-lm(ln.Gas~ln.Month,data=model.data)
     pred.data<-rev.cost.joined.data()%>%filter(Cluster==input$clust_invest, Months.On.Production>=2)%>%
       group_by(Months.On.Production)%>%summarise(Clust_Avg_Gas_Prod=mean(Model_Gas_Produced,na.rm=TRUE))%>%
       mutate(Pred=(exp(coef(gas.model))[1])*(Months.On.Production)^(coef(gas.model))[2])
     error.data<-pred.data%>%mutate(Error_Month=Pred-Clust_Avg_Gas_Prod,Abs_Error_Month=abs(Error_Month),Abs_Percent_Error=Abs_Error_Month/Clust_Avg_Gas_Prod,Percent_Error=Error_Month/Clust_Avg_Gas_Prod)
     PL.gas.clust.error<-ggplot(error.data,aes(x=Months.On.Production,y=Percent_Error))+
       geom_point(size=2)+geom_line(linewidth=1.2)+
       theme(text = element_text(size=20))+
        scale_y_continuous(labels = scales::percent)+
       ggtitle(paste0("Gas Power Law Error using ", input$num_month_fit," Months\nCluster#",input$clust_invest,":\nCumulative Percent Error: ",100*round(sum(error.data$Error_Month)/sum(error.data$Clust_Avg_Gas_Prod),3),"%"))
     PL.gas.clust.error
   })
   
## ML Production Pred tab
   
   rf.prod.data2<-reactive({
     req(input$password==my_password&input$go)
     rf.prod.data2<-rf.prod.data%>%filter(Oil_Prod_2yr>=input$rf_min_oil,
                                          Oil_Prod_2yr<=input$rf_max_oil,
                                          Gas_Prod_2yr>=input$rf_min_gas,
                                          Gas_Prod_2yr<=input$rf_max_gas)
     rf.prod.data2
   })
   
   train.prod<-reactive({
     req(input$password==my_password&input$go)
     set.seed(564)
     train<-rf.prod.data2()%>%slice_sample(prop = input$train_prop)
     train
   })
   
   test.prod<-reactive({
     req(input$password==my_password&input$go)
     test<-filter(rf.prod.data2(),!(API %in% train.prod()$API))
     test
   })
   
   rf.boe.model<-reactive({
     req(input$password==my_password&input$go&input$rf_toggle)
     train.data<-train.prod()%>%select(BOE_Prod_2yr,!!!input$rf_prod_var)
     
     rf.boe.prod.model<-randomForest(BOE_Prod_2yr ~ .,data=train.data)
     rf.boe.prod.model
   })
   
   output$rf.boe.importance<-renderPlot({
     varImpPlot(rf.boe.model(),main="BOE RF Variance Importance")
   })
   
   bayes.boe.model<-reactive({
      req(input$password==my_password&input$go&input$rf_toggle)
      train.data<-train.prod()%>%select(BOE_Prod_2yr,!!!input$rf_prod_var)
      bayes.boe.prod.model <- train(BOE_Prod_2yr ~ .,
                               data = train.data,
                               method = "bayesglm"  # now we're using the bayesian glm method
      )
      bayes.boe.prod.model
   })
   
   output$bayes.boe.importance<-renderPlot({
      bayes.var.imp<-varImp(bayes.boe.model())
      bayes.summary<-data.frame(var=rownames(bayes.var.imp$importance),
                                    rel.inf=bayes.var.imp$importance$Overall)%>%
         arrange(rel.inf)
      bayes.summary<-bayes.summary%>%
         mutate(var=factor(bayes.summary$var, levels=bayes.summary$var))
      
      bayes_rel_inf_plot<-ggplot(data = bayes.summary,aes(x=var,y=rel.inf))+
         geom_bar(stat="identity",color="black",fill="cornflowerblue")+
         geom_text(aes(label=round(rel.inf,1)), hjust=1.5, color="white",
                   position = position_dodge(0.9), size=3.5)+
         theme_bw()+
         xlab("Variable")+ylab("Relative Importance")+
         ggtitle("Bayesian GLM Relative Importance Plot for BOE")+
         theme(text = element_text(size=17))+
         coord_flip()
      bayes_rel_inf_plot
   })
   
   rf.boe.error.train.data<-reactive({
     req(input$password==my_password&input$go)
     pred.train<-predict(rf.boe.model(),newdata=train.prod())
     df.train<-train.prod()%>%mutate(RF_BOE_Pred=pred.train,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                     Percent_Error=100*Abs_Error/BOE_Prod_2yr)
     df.train
   })
   
   bayes.boe.error.train.data<-reactive({
      req(input$password==my_password&input$go)
      pred.train<-predict(bayes.boe.model(),newdata=train.prod())
      df.train<-train.prod()%>%mutate(Bayes_BOE_Pred=pred.train,Abs_Error=abs(Bayes_BOE_Pred-BOE_Prod_2yr),
                                      Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      df.train
   })
   
   output$Download.rf.boe.error.train.data<-downloadHandler(
     filename = function() {
       paste0("Train Proportion ", input$train_prop, " Error Data.csv")
     },
     content=function(file) {
       write.table(rf.boe.error.train.data(),file,row.names = FALSE)
     }
   )
   
   rf.boe.error.test.data<-reactive({
     req(input$password==my_password&input$go)
     pred.test<-predict(rf.boe.model(),newdata=test.prod())
     df.test<-test.prod()%>%mutate(RF_BOE_Pred=pred.test,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                   Percent_Error=100*Abs_Error/BOE_Prod_2yr)
     df.test
   })
   
   bayes.boe.error.test.data<-reactive({
      req(input$password==my_password&input$go)
      pred.test<-predict(bayes.boe.model(),newdata=test.prod())
      df.test<-test.prod()%>%mutate(Bayes_BOE_Pred=pred.test,Abs_Error=abs(Bayes_BOE_Pred-BOE_Prod_2yr),
                                    Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      df.test
   })
   
   output$Download.rf.boe.error.test.data<-downloadHandler(
     filename = function() {
       paste0("Test Proportion ", round(1-input$train_prop,2) , " Error Data.csv")
     },
     content=function(file) {
       write.table(rf.boe.error.test.data(),file,row.names = FALSE)
     }
   )
   
   output$rf.boe.prod.error<-renderTable({
     req(input$password==my_password&input$go)
     df.rf.train<-rf.boe.error.train.data()
     df.rf.test<-rf.boe.error.test.data()
     df.bayes.train<-bayes.boe.error.train.data()
     df.bayes.test<-bayes.boe.error.test.data()
     df.error<-data.frame(RF_Med_Train_BOE_Error=round(median(df.rf.train$Percent_Error),2),
                          RF_Med_Test_BOE_Error=round(median(df.rf.test$Percent_Error),2),
                          Bayes_Med_Train_BOE_Error=round(median(df.bayes.train$Percent_Error),2),
                          Bayes_Med_Test_BOE_Error=round(median(df.bayes.test$Percent_Error),2))
     df.error
   })

   output$rf.boe.train.hist.error<-renderPlot({
     req(input$password==my_password&input$go)
     hist.data<-filter(rf.boe.error.train.data(),Percent_Error<=input$max_hist_error)
     hist.rf.train.error<-ggplot(data=hist.data, aes(Percent_Error)) + 
       geom_histogram(bins = input$rf_error_bins)+
       theme(text = element_text(size=20))+
       ggtitle("Histogram of Percent Train Error of BOE Prod \nbetween Actual and Random Forest Model")
     hist.rf.train.error
   })
   
   output$rf.boe.test.hist.error<-renderPlot({
     req(input$password==my_password&input$go)
     hist.data<-filter(rf.boe.error.test.data(),Percent_Error<=input$max_hist_error)
     hist.rf.test.error<-ggplot(data=hist.data, aes(Percent_Error)) + 
       geom_histogram(bins = input$rf_error_bins)+
       theme(text = element_text(size=20))+
       ggtitle("Histogram of Percent Test Error of BOE Prod \nbetween Actual and Random Forest Model")
     hist.rf.test.error
   })
   
   ##Begin work on Random Forest Training on individual Clusters
   rf.prod.data3<-reactive({ ##Using rf.prod.data3 as a sample, replace with rf.prod.data2 for actual implementation
     #set.seed(3)
     rf.prod.data3<-rf.prod.data2()
     rf.prod.data3
   }) 
   
   RF_ClustScale<-reactive({
     req(input$password==my_password&input$go)
     
     rf.prod.data4<-rf.prod.data3()
     if(input$clust_toggle){ ##imports Clusters from Kriging TL tab if that toggle is selected on Kriging TL tab
       rf.prod.data4<-import_krig_clust()
     }
     
      if (input$pca_clust_toggle){
         cluster.prcomp<-prcomp(rf.prod.data4  %>% dplyr::select(!!!input$rf_clust_var))
         cluster.scale<-cluster.prcomp$x
      }
      else {
         cluster.scale<-data.frame(scale(rf.prod.data4  %>% dplyr::select(!!!input$rf_clust_var)))
      }
     cluster.scale
     ##need to modify this too from kringing

   })
   
   output$RF_Clust_silhoutte<-renderPlot({
      req(input$password==my_password&input$go&input$sil_toggle)
      sil.plot<-fviz_nbclust(RF_ClustScale(), kmeans, method = "silhouette", k.max = input$rf_clust_sil_num)+
                  theme(text = element_text(size=20))
      sil.plot
   })
   
   RF_ClustData<-reactive({
     req(input$password==my_password&input$go&input$rf_toggle2)
     set.seed(465)
     my_cluster_scale<-kmeans(RF_ClustScale(),centers = input$rf_clust_num)
   })
   
   RF_Clust_TL<-reactive({
     req(input$password==my_password&input$go&input$rf_toggle2)
     
     error.results<-data.frame(Cluster_Number=numeric(),Train_ID=numeric(),Test_ID=numeric(),
                         Train_Error=numeric(),Test_Error=numeric())
     
     start_loop<-input$TL_min
     end_loop<-input$TL_max
     clust_flag<-input$rf_clust_num
     if(input$clust_toggle){
       start_loop<-input$krig_clust_num
       end_loop<-input$krig_clust_num
       clust_flag<-input$krig_clust_num
     }
     
     withProgress(message = 'Calculating Transfer Learning Clusters', value = 0, {
       # Number of times we'll go through the loop
       n <- (end_loop-start_loop)+1
     
       for (clust_num in start_loop:end_loop){
         set.seed(465)
         clust_model<-kmeans(RF_ClustScale(), centers = clust_num)
         
         if (clust_num==clust_flag){
           my_cluster_scale<-clust_model
         }
         
         
         if(input$clust_toggle){
           cluster.data2<-mutate(RF_ClustScale(),Cluster=factor(clust_model$cluster))
         }
         else{
           cluster.data2<-mutate(rf.prod.data3(),Cluster=factor(clust_model$cluster))
         }
         
         ##consider importing clusters from Kriging tab here
         if(input$clust_toggle){ ##imports Clusters from Kriging TL tab if that toggle is selected on Kriging TL tab
           cluster.data2<-import_krig_clust()
         }
         
         for (train_id in 1:clust_num){
           train<-cluster.data2%>%filter(Cluster==train_id)%>%select(Cluster,BOE_Prod_2yr,!!!input$rf_prod_var)
           set.seed(466)
           train<-train%>%slice_sample(n=min(input$num_train_clust,dim(train)[[1]]))
           ##Create RF model and Bayesian
           set.seed(466)
           rf.prod.model<-randomForest(BOE_Prod_2yr ~ .,data=select(train,-Cluster)) 
           bayes.prod.model <- train(BOE_Prod_2yr ~ ., data = select(train,-Cluster),
                                         method = "bayesglm")
           
           if (clust_num==clust_flag & train_id==input$rf_train_clust){
             display.rf.prod.model<-rf.prod.model
             display.bayes.prod.model<-bayes.prod.model
           }
           
           pred.train<-predict(rf.prod.model,newdata=train)
           df.train<-train%>%mutate(RF_BOE_Pred=pred.train,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                    Percent_Error=100*Abs_Error/BOE_Prod_2yr)
           pred.bayes.train<-predict(bayes.prod.model,newdata=train)
           df.bayes.train<-train%>%mutate(Bayes_BOE_Pred=pred.train,Bayes_Abs_Error=abs(Bayes_BOE_Pred-BOE_Prod_2yr),
                                    Bayes_Percent_Error=100*Bayes_Abs_Error/BOE_Prod_2yr)
           
           bayes.var.imp<-varImp(bayes.prod.model)$importance
           if (train_id==1 & clust_num==start_loop){
              var.imp.results<-data.frame(RF_Var_Names=row.names(rf.prod.model$importance),Value1=as.integer(length(rf.prod.model$importance)+1-rank(rf.prod.model$importance)))
              var.imp.bayes.results<-data.frame(Bayes_Var_Names=row.names(bayes.var.imp),Value1=as.integer(dim(bayes.var.imp)[[1]]+1-rank(bayes.var.imp)))
              i.count<-1
              }
           else {
              var.imp.results<-data.frame(cbind(var.imp.results,as.integer(length(rf.prod.model$importance)+1-rank(rf.prod.model$importance)))) ##rank 1 is most important
              var.imp.bayes.results<-data.frame(cbind(var.imp.bayes.results,as.integer(dim(bayes.var.imp)[[1]]+1-rank(bayes.var.imp))))
              i.count=i.count+1
           }
           
           names(var.imp.results)[i.count+1]<-paste0("Num_Clust_",clust_num,"_Train_ID_", train_id)
           names(var.imp.bayes.results)[i.count+1]<-paste0("Num_Clust_",clust_num,"_Train_ID_", train_id)
           
           for (test_id in 1:clust_num){
             
             test<-cluster.data2%>%filter(Cluster==test_id)
             pred.test<-predict(rf.prod.model,newdata=test)
             
             df.test<-test%>%mutate(RF_BOE_Pred=pred.test,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                    Percent_Error=100*Abs_Error/BOE_Prod_2yr)
             if(clust_num==clust_flag & train_id==input$rf_train_clust &test_id==input$rf_test_clust){
               ##Store the results needed on dashboard
               train.result<-df.train
               test.result<-df.test
             }
             
             df.results<-c(clust_num,train_id,test_id,
                           round(median(df.train$Percent_Error),5),
                           round(median(df.test$Percent_Error),5))
             
             error.results<-rbind(error.results,df.results)
             print(c(clust_num,train_id,test_id))
           }
           
         }
         incProgress(1/n, detail = paste("# Clusters", clust_num, " of ", input$TL_max))
       }
       
       
     })
     
     names(error.results)<-c("Cluster_Number","Train_ID","Test_ID",
                       "Train_Error","Test_Error")
     error.results<-error.results%>%mutate(flag=(Train_ID==Test_ID))%>%filter(flag==FALSE)%>%select(-flag)%>%
        mutate(Cluster_Number=as.integer(Cluster_Number),Train_ID=as.integer(Train_ID),Test_ID=as.integer(Test_ID),
               Test_Train_Ratio=Test_Error/Train_Error)%>%
       arrange(Test_Error)
     
     results.output<-list(RF_Var_Imp=var.imp.results,Bayes_Var_Imp=var.imp.bayes.results,
                          Results=error.results,RF_ClustData=my_cluster_scale,
                          RF_model=display.rf.prod.model,Bayes_model=display.bayes.prod.model,
                          RF_error_train=train.result,RF_error_test=test.result)
   })
   
   
   output$TL_errors<-renderTable({
     RF_Clust_TL()$Results
   })
   
   output$downloadTLErrors<-downloadHandler(
     filename = function() {
       paste0(" Transfer Learning Clusters ", input$TL_min, " to ",input$TL_max, "Error Data.csv")
     },
     content=function(file) {
       write.table(RF_Clust_TL()$Results,file,row.names = FALSE)
     }
   )
   
   output$TL_RF_Var_Imp<-renderTable({
     RF_Clust_TL()$RF_Var_Imp
   })
   
   output$TL_Bayes_Var_Imp<-renderTable({
      RF_Clust_TL()$Bayes_Var_Imp
   })
   
   output$downloadTL_RF_Var_Imp<-downloadHandler(
     filename = function() {
       paste0(" Transfer Learning Clusters ", input$TL_min, " to ",input$TL_max, " RF Var Importance.csv")
     },
     content=function(file) {
       write.table(RF_Clust_TL()$RF_Var_Imp,file,row.names = FALSE)
     }
   )
   
   output$downloadTL_Bayes_Var_Imp<-downloadHandler(
     filename = function() {
       paste0(" Transfer Learning Clusters ", input$TL_min, " to ",input$TL_max, " Bayes Var Importance.csv")
     },
     content=function(file) {
       write.table(RF_Clust_TL()$Bayes_Var_Imp,file,row.names = FALSE)
     }
   )
   
   output$rf_clust_fviz<-renderPlot({
     req(input$password==my_password&input$go&input$rf_toggle2)
     
     if(input$clust_toggle){
       fviz_plot<-fviz_cluster(krig_clust_model(),data = filter(krig_scale_clust_data(),Type=="EagleFord")%>%select(-Type))+
         ggtitle(paste0("PCA Cluster Plot from Kriging TL \nVariables: ",paste(sapply(input$krig_clust_vars,paste),collapse = " ")))+theme(text = element_text(size=20))
     }
     else{
       fviz_plot<-fviz_cluster(RF_Clust_TL()$RF_ClustData,data = rf.prod.data3()  %>% dplyr::select(!!!input$rf_clust_var)) +
         ggtitle(paste0("PCA Cluster Plot \nVariables: ",paste(sapply(input$rf_clust_var,paste),collapse = " ")))+theme(text = element_text(size=20))
     }
     fviz_plot
   })
   
   RF_ClustData2<-reactive({
     req(input$password==my_password&input$go&input$rf_toggle2)
     
     if(input$clust_toggle){ ##imports Clusters from Kriging TL tab if that toggle is selected on Kriging TL tab
       cluster.data2<-import_krig_clust()
     }
     else{
       cluster.data2<-mutate(rf.prod.data3(),Cluster=factor(RF_Clust_TL()$RF_ClustData$cluster))
     }
     cluster.data2
   })
   
   train.prod.clust<-reactive({
     req(input$password==my_password&input$go&input$rf_toggle2)
     train<-RF_ClustData2()%>%filter(Cluster==input$rf_train_clust)
     train
   })
   
   test.prod.clust<-reactive({
     req(input$password==my_password&input$go&input$rf_toggle2)
     test<-RF_ClustData2()%>%filter(Cluster==input$rf_test_clust)
     test
   })
   
   output$rf.prod.clust.summary<-renderTable({
     req(input$password==my_password&input$go&input$rf_toggle2)
     rf.clust.summary.table<-RF_ClustData2()%>%group_by(Cluster)%>%
       summarise(Well_Count=n(),Avg.Oil=mean(Oil_Prod_2yr),
                 Avg.Gas=mean(Gas_Prod_2yr), Avg.BOE=mean(BOE_Prod_2yr), Avg.TVD=mean(TVD),
                 Avg.Thickness=mean(Thickness), Avg.API.Gravity=mean(API.Gravity), Avg.Vclay=mean(Vclay),
                 Avg.Brittleness=mean(Brittleness), Avg.TOC=mean(TOC), Avg.GPI=mean(GPI),
                 Avg.Prop=mean(Proppant.per.GPI..lb.ft.), Avg.Fluid=mean(Fluid.per.GPI..gal.ft.),
                 Avg.Lateral.Length=mean(Lateral..ft.))
     rf.clust.summary.table
   })
   
   output$rf_clust_map<-renderPlot({
     req(input$password==my_password&input$go&input$rf_toggle2)
     cluster.data2<-RF_ClustData2()%>%filter(Cluster %in% input$rf_clust_map)
     gg_clust_map<-gg()+geom_point(data=cluster.data2, aes(x=Surf.Lon,  y=Surf.Lat,color=Cluster),size=2) +
       ggtitle("Clusters on Map") + theme(text = element_text(size=20))
     gg_clust_map
   })
   
   output$rf.clust.boe.importance<-renderPlot({
     varImpPlot(RF_Clust_TL()$RF_model,main="BOE RF Variance Importance")
   })
   
   output$bayes.clust.boe.importance<-renderPlot({
      bayes.var.imp<-varImp(RF_Clust_TL()$Bayes_model)
      bayes.summary<-data.frame(var=rownames(bayes.var.imp$importance),
                                rel.inf=bayes.var.imp$importance$Overall)%>%
         arrange(rel.inf)
      bayes.summary<-bayes.summary%>%
         mutate(var=factor(bayes.summary$var, levels=bayes.summary$var))
      
      bayes_rel_inf_plot<-ggplot(data = bayes.summary,aes(x=var,y=rel.inf))+
         geom_bar(stat="identity",color="black",fill="cornflowerblue")+
         geom_text(aes(label=round(rel.inf,1)), hjust=1.5, color="white",
                   position = position_dodge(0.9), size=3.5)+
         theme_bw()+
         xlab("Variable")+ylab("Relative Importance")+
         ggtitle("Bayesian GLM Relative Importance Plot for BOE")+
         theme(text = element_text(size=17))+
         coord_flip()
      bayes_rel_inf_plot
   })
   
   rf.clust.tree.react<-reactive({
      req(input$password==my_password&input$go)
      this.tree<-getTree(RF_Clust_TL()$RF_model, input$tree_num, labelVar=TRUE)
      row.id<-seq(1,dim(this.tree)[[1]])
      this.tree<-data.frame(cbind(row.id,this.tree))
      this.tree
   })
   
   output$rf.clust.tree.dl<-downloadHandler(
      filename = function() {
         paste0(" Tree Number ", input$tree_num, " View.csv")
      },
      content=function(file) {
         write.table(rf.clust.tree.react(),file,row.names = FALSE)
      }
   )
   
   output$rf.clust.boe.prod.error<-renderTable({
     req(input$password==my_password&input$go)
     clust_flag<-input$rf_clust_num
     if(input$clust_toggle){
       clust_flag<-input$krig_clust_num
     }
     df.error<-RF_Clust_TL()$Results%>%filter(Train_ID==input$rf_train_clust,Test_ID==input$rf_test_clust,Cluster_Number==clust_flag)
     df.error
   })
   
   output$rf_clust_train_scatter<-renderPlot({
      req(input$password==my_password&input$go)
      df.train<-data.frame(RF_BOE_Pred=RF_Clust_TL()$RF_error_train$RF_BOE_Pred,BOE_Prod_2yr=RF_Clust_TL()$RF_error_train$BOE_Prod_2yr)#tl_model_data()%>%mutate(RF_BOE_Pred=pred.train)
      scatter.lm<-lm(BOE_Prod_2yr~RF_BOE_Pred,data=df.train)
      r.squared<-summary(scatter.lm)$r.squared
      scatter.train.error<-ggplot(data=df.train, aes(x=RF_BOE_Pred,y=BOE_Prod_2yr)) + 
         geom_point()+xlab("Predicted")+ylab("Actual")+
         geom_abline(slope = 1,intercept = 0)+
         geom_smooth(method = "lm")+
         annotate("text",x=quantile(df.train$RF_BOE_Pred,0.2),y=max(df.train$BOE_Prod_2yr)*.8,
                  label=paste0("Actual = ",round(coef(scatter.lm)[[1]],2)," + ", 
                               round(coef(scatter.lm)[[2]],2),"*Predicted \nR^2= ",round(r.squared,4)), 
                  hjust=0,size=5)+
         theme(text = element_text(size=20))+
         ggtitle(paste0("Cross-Validate Scatter Train Data"))
      scatter.train.error
   })
   
   output$rf.clust.boe.train.hist.error<-renderPlot({
     req(input$password==my_password&input$go)
     hist.data<-filter(RF_Clust_TL()$RF_error_train,Percent_Error<=input$max_hist_error)
     hist.rf.train.error<-ggplot(data=hist.data, aes(Percent_Error)) + 
       geom_histogram(bins = input$rf_error_bins)+
       theme(text = element_text(size=20))+
       ggtitle("Histogram of Percent Train Error \nof BOE Prod between Actual and \nCluster Random Forest Model")
     hist.rf.train.error
   })
   
   output$rf_clust_test_scatter<-renderPlot({
      req(input$password==my_password&input$go)
      df.test<-data.frame(RF_BOE_Pred=RF_Clust_TL()$RF_error_test$RF_BOE_Pred,BOE_Prod_2yr=RF_Clust_TL()$RF_error_test$BOE_Prod_2yr)
      scatter.lm<-lm(BOE_Prod_2yr~RF_BOE_Pred,data=df.test)
      r.squared<-summary(scatter.lm)$r.squared
      scatter.test.error<-ggplot(data=df.test, aes(x=RF_BOE_Pred,y=BOE_Prod_2yr)) + 
         geom_point()+xlab("Predicted")+ylab("Actual")+
         geom_abline(slope = 1,intercept = 0)+
         geom_smooth(method = "lm")+
         annotate("text",x=quantile(df.test$RF_BOE_Pred,0.2),y=max(df.test$BOE_Prod_2yr)*.8,
                  label=paste0("Actual = ",round(coef(scatter.lm)[[1]],2)," + ", 
                               round(coef(scatter.lm)[[2]],2),"*Predicted \nR^2= ",round(r.squared,4)), 
                  hjust=0,size=5)+
         theme(text = element_text(size=20))+
         ggtitle(paste0("Cross-Validate Scatter Test Data"))
      scatter.test.error
   })
   
   output$rf.clust.boe.test.hist.error<-renderPlot({
     req(input$password==my_password&input$go)
     hist.data<-filter(RF_Clust_TL()$RF_error_test,Percent_Error<=input$max_hist_error)
     hist.rf.test.error<-ggplot(data=hist.data, aes(Percent_Error)) + 
       geom_histogram(bins = input$rf_error_bins)+
       theme(text = element_text(size=20))+
       ggtitle("Histogram of Percent Test Error \nof BOE Prod between Actual and \nCluster Random Forest Model")
     hist.rf.test.error
   })
   
   output$downloadTrainError<-downloadHandler(
     filename = function() {
       paste0(" Train Cluster ", input$rf_train_clust, "Error Data.csv")
     },
     content=function(file) {
       write.table(RF_Clust_TL()$RF_error_train,file,row.names = FALSE)
     }
   )
   
   output$downloadTestError<-downloadHandler(
     filename = function() {
       paste0(" Test Cluster ", input$rf_test_clust, "Error Data.csv")
     },
     content=function(file) {
       write.table(RF_Clust_TL()$RF_error_test,file,row.names = FALSE)
     }
   )
   
##Cluster TL tab
   
   tl.prod.data.train<-reactive({
      req(input$password==my_password&input$go&input$tl_toggle)
      ## map coords for entire field c(-101, 27.5, -96, 31)
      rf_min_oil=0
      rf_max_oil=500000
      rf_min_gas=0
      rf_max_gas=2500000
      # RF_ClustData2 rf.prod.data
      rf.prod.data2<-RF_ClustData2()%>%filter(Oil_Prod_2yr>=rf_min_oil,
                                           Oil_Prod_2yr<=rf_max_oil,
                                           Gas_Prod_2yr>=rf_min_gas,
                                           Gas_Prod_2yr<=rf_max_gas)
      set.seed(504)
      tl.prod.data.train<-rf.prod.data2%>%filter(Surf.Lat>input$train_Lat_LL,
                                                 Surf.Lat<input$train_Lat_UR,
                                                 Surf.Lon>input$train_Lon_LL,
                                                 Surf.Lon<input$train_Lon_UR,
                                                 Cluster %in% input$tl_field_A)%>%
                           slice_sample(prop = input$tl_train_prop)
         
      tl.prod.data.train
   })
   
   tl.prod.data.test<-reactive({
      req(input$password==my_password&input$go&input$tl_toggle)
      ## map coords for entire field c(-101, 27.5, -96, 31)
      rf_min_oil=0
      rf_max_oil=500000
      rf_min_gas=0
      rf_max_gas=2500000
      # RF_ClustData2 rf.prod.data
      rf.prod.data2<-RF_ClustData2 ()%>%filter(Oil_Prod_2yr>=rf_min_oil,
                                           Oil_Prod_2yr<=rf_max_oil,
                                           Gas_Prod_2yr>=rf_min_gas,
                                           Gas_Prod_2yr<=rf_max_gas)
      tl.prod.data.test<-rf.prod.data2%>%filter(Surf.Lat>input$test_Lat_LL,
                                                 Surf.Lat<input$test_Lat_UR,
                                                 Surf.Lon>input$test_Lon_LL,
                                                 Surf.Lon<input$test_Lon_UR,
                                                Cluster %in% input$tl_field_B)
                     
      tl.prod.data.test
   })
      
   output$TL_train_map<-renderPlot({
      req(input$password==my_password&input$go&input$tl_toggle)
      ##c(LL.Lon,LL.Lat,UR.Lon,UR.Lat)
      train_loc2<- c(input$train_Lon_LL-.1,input$train_Lat_LL-.1,input$train_Lon_UR+.1,input$train_Lat_UR+.1)
      train_map_gmaps <- get_map(location=train_loc2, source="google", maptype="terrain")
      gg_train <- ggmap(train_map_gmaps)
      
      tl.prod.train.graph<-mutate(tl.prod.data.train(),Surf.Lon=round(Surf.Lon,2),Surf.Lat=round(Surf.Lat,2))
      gg_wells_train<-gg_train+geom_point(data=tl.prod.train.graph, aes(x=Surf.Lon,  y=Surf.Lat),size=2) +
         ggtitle("Field A Wells on Map") + theme(text = element_text(size=20))
      gg_wells_train
   })
   
   output$TL_test_map<-renderPlot({
      req(input$password==my_password&input$go&input$tl_toggle)
      ##c(LL.Lon,LL.Lat,UR.Lon,UR.Lat)
      test_loc2<- c(input$test_Lon_LL-.1,input$test_Lat_LL-.1,input$test_Lon_UR+.1,input$test_Lat_UR+.1)
      test_map_gmaps <- get_map(location=test_loc2, source="google", maptype="terrain")
      gg_test <- ggmap(test_map_gmaps)
      
      tl.prod.test.graph<-mutate(tl.prod.data.test(),Surf.Lon=round(Surf.Lon,2),Surf.Lat=round(Surf.Lat,2))
      gg_wells_test<-gg_test+geom_point(data=tl.prod.test.graph, aes(x=Surf.Lon,  y=Surf.Lat),size=2) +
         ggtitle("Field B Wells on Map") + theme(text = element_text(size=20))
      gg_wells_test
   })
   
   tl_valid.data<-reactive({
      req(input$password==my_password&input$go&input$tl_toggle)
      set.seed(505)
      valid.data<-tl.prod.data.test()%>%slice_sample(prop = input$tl_validation_prop)
   })
   
   tl_test.data<-reactive({
      req(input$password==my_password&input$go&input$tl_toggle)
      set.seed(506)
      test.data<-tl.prod.data.test()%>%filter(!(API %in% tl_valid.data()$API))%>%
         slice_sample(prop = input$tl_test_prop)
   })
   
   tl_model_data<-reactive({
      req(input$password==my_password&input$go&input$tl_toggle)
      model_data<-rbind(tl.prod.data.train(),tl_test.data())%>%
         select(BOE_Prod_2yr,!!!input$tl_prod_var)
      model_data
   })
   
   output$TL_Well_Counts<-renderTable({
      req(input$password==my_password&input$go&input$tl_toggle)
      train.count<-tl.prod.data.train()%>%summarise(Count=n())
      test.count<-tl.prod.data.test()%>%summarise(Count=n())
      valid.count<-tl_valid.data()%>%summarise(Count=n())
      model.count<-tl_model_data()%>%summarise(Count=n())
      tl.well.counts<-data.table(Field_A_Count=train.count$Count,Field_B_Count=test.count$Count,
                                 Validation_Count=valid.count$Count,Model_Train_Count=model.count$Count)
      tl.well.counts
   })
   
   tl_rf_model<-reactive({
      req(input$password==my_password&input$go&input$tl_toggle)
      set.seed(507)
      withProgress(message = 'Calculating Random Forest', value = 0, {
        # Number of times we'll go through the loop
        n <- 1
        rf_model<-randomForest(BOE_Prod_2yr ~ .,data=tl_model_data())
        incProgress(1/n, detail = paste(1, " of ", n))
      })
      rf_model
   })
   
   output$tl_train_hist<-renderPlot({
      req(input$password==my_password&input$go&input$tl_toggle)
      pred.train<-predict(tl_rf_model(),newdata=tl_model_data())
      df.train<-tl_model_data()%>%mutate(RF_BOE_Pred=pred.train,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                             Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      hist.train.error<-ggplot(data=filter(df.train,Percent_Error<100), aes(Percent_Error)) + 
         geom_histogram(bins=25)+
         theme(text = element_text(size=20))+
         ggtitle(paste0("Histogram of Percent Error on Field A with\n", 100*input$tl_test_prop*(1-input$tl_validation_prop),"% Field B (i.e. Train Data) added of BOE Prod"))
      hist.train.error
   })
   
   output$tl_train_scatter<-renderPlot({
      req(input$password==my_password&input$go&input$tl_toggle)
      pred.train<-predict(tl_rf_model(),newdata=tl_model_data())
      df.train<-tl_model_data()%>%mutate(RF_BOE_Pred=pred.train)
      scatter.lm<-lm(BOE_Prod_2yr~RF_BOE_Pred,data=df.train)
      r.squared<-summary(scatter.lm)$r.squared
      scatter.train.error<-ggplot(data=df.train, aes(x=RF_BOE_Pred,y=BOE_Prod_2yr)) + 
         geom_point()+xlab("Predicted")+ylab("Actual")+
         geom_abline(slope = 1,intercept = 0)+
         geom_smooth(method = "lm")+
         annotate("text",x=quantile(df.train$RF_BOE_Pred,0.2),y=max(df.train$BOE_Prod_2yr)*.8,
                  label=paste0("Actual = ",round(coef(scatter.lm)[[1]],2)," + ", 
                               round(coef(scatter.lm)[[2]],2),"*Predicted \nR^2= ",round(r.squared,4)), 
                  hjust=0,size=5)+
         theme(text = element_text(size=20))+
         ggtitle(paste0("Cross-Validate Scatter Plot Field A with\n", 100*input$tl_test_prop*(1-input$tl_validation_prop),"% Field B (i.e. Train Data) added of BOE Prod"))
      scatter.train.error
   })
   
   output$tl_valid_hist<-renderPlot({
      req(input$password==my_password&input$go&input$tl_toggle)
      pred.valid<-predict(tl_rf_model(),newdata=tl_valid.data())
      df.valid<-tl_valid.data()%>%mutate(RF_BOE_Pred=pred.valid,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                         Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      hist.valid.error<-ggplot(data=filter(df.valid,Percent_Error<100), aes(Percent_Error)) + 
         geom_histogram(bins=25)+
         theme(text = element_text(size=20))+
         ggtitle(paste0("Histogram of Percent Error for\nValidation Set of BOE Prod"))
      hist.valid.error
   })
   
   output$tl_valid_scatter<-renderPlot({
      req(input$password==my_password&input$go&input$tl_toggle)
      pred.valid<-predict(tl_rf_model(),newdata=tl_valid.data())
      df.valid<-tl_valid.data()%>%mutate(RF_BOE_Pred=pred.valid)
      scatter.lm<-lm(BOE_Prod_2yr~RF_BOE_Pred,data=df.valid)
      r.squared<-summary(scatter.lm)$r.squared
      scatter.valid.error<-ggplot(data=df.valid, aes(x=RF_BOE_Pred,y=BOE_Prod_2yr)) + 
         geom_point()+xlab("Predicted")+ylab("Actual")+
         geom_abline(slope = 1,intercept = 0)+
         geom_smooth(method = "lm")+
         annotate("text",x=quantile(df.valid$RF_BOE_Pred,0.2),y=max(df.valid$BOE_Prod_2yr)*.8,
                  label=paste0("Actual = ",round(coef(scatter.lm)[[1]],2)," + ", 
                               round(coef(scatter.lm)[[2]],2),"*Predicted \nR^2= ",round(r.squared,4)), 
                  hjust=0,size=5)+
         theme(text = element_text(size=20))+
         ggtitle(paste0("Cross-Validate Scatter Plot for\nValidation Set of BOE Prod"))
      scatter.valid.error
   })
   
   output$tl_error_table<-renderTable({
      req(input$password==my_password&input$go&input$tl_toggle)
      pred.train<-predict(tl_rf_model(),newdata=tl_model_data())
      df.train<-tl_model_data()%>%mutate(RF_BOE_Pred=pred.train,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                         Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      pred.valid<-predict(tl_rf_model(),newdata=tl_valid.data())
      df.valid<-tl_valid.data()%>%mutate(RF_BOE_Pred=pred.valid,Abs_Error=abs(RF_BOE_Pred-BOE_Prod_2yr),
                                         Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      df.avg.model<-tl_valid.data()%>%mutate(BOE_Pred=mean(tl_model_data()$BOE_Prod_2yr),Abs_Error=abs(BOE_Pred-BOE_Prod_2yr),
                                             Percent_Error=100*Abs_Error/BOE_Prod_2yr)
      tl_error_table<-data.frame(Med_Train_Error=median(df.train$Percent_Error),
                                 Med_Validation_Error=median(df.valid$Percent_Error),
                                 Med_Avg_Model_Error=median(df.avg.model$Percent_Error))
      tl_error_table
   })
   
##Kriging TL tab
   ##Kriging calcs and outputs
   output$krig_head_name<-renderText({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      paste0("Top of Uploaded Data")
   })
   
   output$krig_upload_data <- renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      krig_file_out<-head(read.csv(input$krig_file$datapath))
      krig_file_out
   })
   
   krig_data_upload<-reactive({
      krig_data<-read.csv(input$krig_file$datapath)
      krig_data
   })
   
   output$downloadKrig_SampleData<-downloadHandler(
      filename = function() {
         paste0("Kriging Sample", " Data.csv")
      },
      content=function(file) {
         write.csv(krig.sample.data,file,row.names=FALSE)
      }
   )
      
   krig_train_data<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      set.seed(45)
      krig_train_df<-krig_data_upload()%>%filter(Surf.Lon>=input$krig_Lon_LL-.01,Surf.Lon<=input$krig_Lon_UR+.01,
                            Surf.Lat>=input$krig_Lat_LL-.01,Surf.Lat<=input$krig_Lat_UR+.01)%>%
         slice_sample(n=input$krig_num_wells)%>%
         mutate(TOC.scale=scale(TOC)[,1],Permeability.scale=scale(Permeability...d.)[,1],
                Brittleness.scale=scale(Brittleness)[,1], Porosity.scale=scale(Porosity)[,1],
                Thickness.scale=scale(Thickness)[,1],Vclay.scale=scale(Vclay)[,1],TVD.scale=scale(TVD)[,1],
                API.Gravity.scale=scale(API.Gravity)[,1])
      krig_train_df
   })
   
   coord_table<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      lon.dist<-distm(c(input$krig_Lon_LL,input$krig_Lat_LL),c(input$krig_Lon_UR,input$krig_Lat_LL),fun = distHaversine)[1,1]/1000 ##x
      lat.dist<-distm(c(input$krig_Lon_LL,input$krig_Lat_LL),c(input$krig_Lon_LL,input$krig_Lat_UR),fun = distHaversine)[1,1]/1000 ##y
      lon.avg<-lon.dist/(input$krig_step-1)
      lat.avg<-lat.dist/(input$krig_step-1)
      coord.table<-data.frame(Lon_Dist_Grid_Size_km=lon.avg,Lat_Dist_Grid_Size_km=lat.avg)%>%
         mutate(Area_acres=Lon_Dist_Grid_Size_km*Lat_Dist_Grid_Size_km*247.1,
                Wells_Per_Grid=Area_acres/input$krig_spacing)
   })
   
   output$krig_size<-renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      coord.table<-coord_table()
      coord.table
   })
   
   krig_model_pred<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      geo.list<-c("TOC.scale","Permeability.scale","Brittleness.scale","Porosity.scale",
                  "Thickness.scale","Vclay.scale","TVD.scale","API.Gravity.scale") #names(rf.prod.train)[28:34]
      krig_train_data2<-krig_train_data()
      coordinates(krig_train_data2)<- ~ Surf.Lon+Surf.Lat
      
      ##create grid to predict on
      start_lat <-  input$krig_Lat_LL ##27.5 #rf.prod.train@bbox[2,1]
      start_lng <- input$krig_Lon_LL ##-101 #rf.prod.train@bbox[1,1]
      end_lat <- input$krig_Lat_UR ##31
      end_lng <- input$krig_Lon_UR ##-96
      step.num=input$krig_step
      grid.pred <- expand.grid(latcoords = seq(from = start_lat, by = (end_lat-start_lat)/step.num, l = step.num),
                               lngcoords = seq(from = start_lng, by = (end_lng-start_lng)/step.num, l = step.num))
      grid.pred<-rbind(grid.pred,data.frame(latcoords=input$krig_pred_lat,lngcoords=input$krig_pred_lon)) ##add point estimate for prediction
      
      coordinates(grid.pred) <- ~ lngcoords + latcoords
      
      grid.pred.df<-as.data.frame(grid.pred)
      
      withProgress(message = 'Calculating Kriging', value = 0, {
        # Number of times we'll go through the loop
        n <- length(geo.list)
        
        for (i in 1:length(geo.list)){
           #consider guassian for TVD, Brittleness
           geo.vgm <- variogram(as.formula(paste(geo.list[i],"~ 1")), krig_train_data2) # calculates sample variogram values 
           geo.fit <- fit.variogram(geo.vgm, model=vgm(model="Sph")) # fit model,Bes,Sph and Cir, Pen work well
           
           geo.kriged <- krige(as.formula(paste(geo.list[i],"~ 1")), krig_train_data2, grid.pred, model=geo.fit)
           geo.kriged2 <- as.data.frame(geo.kriged)
           names(geo.kriged2)<-c("lngcoords","latcoords",paste0(geo.list[i],".pred"),paste0(geo.list[i],".var"))
           grid.pred.df<-left_join(grid.pred.df,geo.kriged2)
           
        }
        incProgress(1/n, detail = paste(i, " of ", n))
      })
      
      grid.pred.df<-mutate(grid.pred.df,TOC.pred=TOC.scale.pred*sd(krig_train_data()$TOC)+mean(krig_train_data()$TOC),
                           Permeability.pred=Permeability.scale.pred*sd(krig_train_data()$Permeability...d.)+mean(krig_train_data()$Permeability...d.),
                           Brittleness.pred=Brittleness.scale.pred*sd(krig_train_data()$Brittleness)+mean(krig_train_data()$Brittleness),
                           Porosity.pred=Porosity.scale.pred*sd(krig_train_data()$Porosity)+mean(krig_train_data()$Porosity),
                           Thickness.pred=Thickness.scale.pred*sd(krig_train_data()$Thickness)+mean(krig_train_data()$Thickness),
                           Vclay.pred=Vclay.scale.pred*sd(krig_train_data()$Vclay)+mean(krig_train_data()$Vclay),
                           TVD.pred=TVD.scale.pred*sd(krig_train_data()$TVD)+mean(krig_train_data()$TVD),
                           API.Gravity.pred=API.Gravity.scale.pred*sd(krig_train_data()$API.Gravity)+mean(krig_train_data()$API.Gravity),
                           
                           TOC.prob=1-TOC.scale.var/max(TOC.scale.var),
                           Permeability.prob=1-Permeability.scale.var/max(Permeability.scale.var),
                           Brittleness.prob=1-Brittleness.scale.var/max(Brittleness.scale.var),
                           Porosity.prob=1-Porosity.scale.var/max(Porosity.scale.var),
                           Thickness.prob=1-Thickness.scale.var/max(Thickness.scale.var),
                           Vclay.prob=1-Vclay.scale.var/max(Vclay.scale.var),
                           TVD.prob=1-TVD.scale.var/max(TVD.scale.var),
                           API.Gravity.prob=1-API.Gravity.scale.var/max(API.Gravity.scale.var),
                           max.prob=pmax(TOC.prob,Permeability.prob,Brittleness.prob,Porosity.prob,
                                         Thickness.prob,Vclay.prob,TVD.prob,API.Gravity.prob),
                           min.prob=pmin(TOC.prob,Permeability.prob,Brittleness.prob,Porosity.prob,
                                         Thickness.prob,Vclay.prob,TVD.prob,API.Gravity.prob))
      
      grid.pred.df
   })
   
   output$krig_Lat_LL_UI <- renderUI({ 
      # If missing input, return to avoid error later in function
      if(is.null(input$krig_file))
         return()
      numericInput("krig_Lat_LL","Kriging Lat Lower Left",round(min(krig_data_upload()$Surf.Lat),2))
   })
   
   output$krig_Lon_LL_UI <- renderUI({ 
      # If missing input, return to avoid error later in function
      if(is.null(input$krig_file))
         return()
      numericInput("krig_Lon_LL","Kriging Lon Lower Left",round(min(krig_data_upload()$Surf.Lon),2))
   })
   
   output$krig_Lat_UR_UI <- renderUI({ 
      # If missing input, return to avoid error later in function
      if(is.null(input$krig_file))
         return()
      numericInput("krig_Lat_UR","Kriging Lat Upper Right",round(max(krig_data_upload()$Surf.Lat),2))
   })
   
   output$krig_Lon_UR_UI <- renderUI({ 
      # If missing input, return to avoid error later in function
      if(is.null(input$krig_file))
         return()
      numericInput("krig_Lon_UR","Kriging Lon Upper Right",round(max(krig_data_upload()$Surf.Lon),2))
   })
   
   output$krig_data_map<-renderPlot({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      ##c(LL.Lon,LL.Lat,UR.Lon,UR.Lat)
      train_loc2<- c(input$krig_Lon_LL-.1,input$krig_Lat_LL-.1,input$krig_Lon_UR+.1,input$krig_Lat_UR+.1)
      train_map_gmaps <- get_map(location=train_loc2, source="google", maptype="terrain")
      gg_train <- ggmap(train_map_gmaps)
      
      krig.data<-krig_train_data()
      krig.prod.train.graph<-mutate(krig.data,Surf.Lon=round(Surf.Lon,2),Surf.Lat=round(Surf.Lat,2))
      gg_krig_train<-gg_train+geom_point(data=krig.prod.train.graph, aes(x=Surf.Lon,  y=Surf.Lat),size=2) +
         ggtitle("Kriging Wells on Map") + theme(text = element_text(size=20))
      gg_krig_train
   })
   
   output$krig_pred_map<-renderPlot({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      train_loc2<- c(input$krig_Lon_LL-.1,input$krig_Lat_LL-.1,input$krig_Lon_UR+.1,input$krig_Lat_UR+.1)
      train_map_gmaps <- get_map(location=train_loc2, source="google", maptype="terrain")
      gg_train <- ggmap(train_map_gmaps)
      
      map_data<-krig_model_pred()%>%select(lngcoords,latcoords,starts_with(input$krig_display_var),max.prob,min.prob)%>%
                     filter(min.prob>input$cert_num)
      
      #my_color <- enquo(paste0(input$krig_display_var,".pred"))

      kriged.map<- gg_train+geom_point(data=map_data, aes_string(x="lngcoords",  y="latcoords",color=paste0(input$krig_display_var,".pred"),size=1.5))+
         ggtitle(paste0("Kriged Data for ",input$krig_display_var," Map"))+
         scale_color_gradientn(colours = topo.colors(5))+
         theme(text = element_text(size=20))
      kriged.map
   })
   
   plot_variogram <- function(v, m, var.name) { 
      #https://stackoverflow.com/questions/51501256/plotting-empirical-and-fitted-semivariogram-in-ggplot
      preds = variogramLine(m, maxdist = max(v$dist))
      ggplot() + 
         geom_point(data = v, aes(x = dist, y = gamma, size=np), color="cornflowerblue") +
         geom_line(data = preds, aes(x = dist, y = gamma),linewidth=1)+
         ggtitle(paste0(var.name," Variogram"))+
         xlab("Distance")+
         ylab("gamma (semivariance)")+
         theme(text = element_text(size=20))
   }
   
   output$krig_variogram<-renderPlot({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      krig_train_data2<-krig_train_data()
      coordinates(krig_train_data2)<- ~ Surf.Lon+Surf.Lat
      
      geo.vgm <- variogram(as.formula(paste(input$krig_vario_display,"~ 1")), krig_train_data2) # calculates sample variogram values 
      geo.fit <- fit.variogram(geo.vgm, model=vgm(model="Sph")) # fit model,Bes,Sph and Cir, Pen work well
      krig_vario_plot<-plot_variogram(geo.vgm,geo.fit,input$krig_vario_display)
      krig_vario_plot
   })
   
   krig_rf_data<-reactive({
      #req(input$password==my_password&input$go&input$krig_rf_toggle)
      set.seed(47)
      krig_rf_df<-rf.prod.data%>%filter(Surf.Lon>=input$krig_rf_Lon_LL,Surf.Lon<=input$krig_rf_Lon_UR,
                                        Surf.Lat>=input$krig_rf_Lat_LL,Surf.Lat<=input$krig_rf_Lat_UR)%>%
                              select(API,BOE_Prod_2yr,!!!input$krig_rf_vars)%>%
                              slice_sample(n=input$krig_rf_num_wells)
      krig_rf_df
   })
   
   krig_scale_clust_data<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      scaled.clust.data<-rbind(rf.prod.data%>%filter(API %in% krig_rf_data()$API)%>%select(!!!input$krig_clust_vars)%>%mutate(Type="EagleFord"),
                                  select(krig_data_upload(),!!!input$krig_clust_vars)%>%mutate(Type="Target"))
      scaled.clust.data2<-select(scaled.clust.data,-Type)%>%scale()%>%as.data.frame()%>%
         mutate(Type=scaled.clust.data$Type)
      scaled.clust.data2
   })
   
   krig_clust_model<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
     withProgress(message = 'Calculating Kmeans Clustering', value = 0, {
       # Number of times we'll go through the loop
        n <- 1
        set.seed(277)
        clust_model<-kmeans(filter(krig_scale_clust_data(),Type=="EagleFord")%>%select(-Type), centers =input$krig_clust_num)
        
        incProgress(1/n, detail = paste(1, " of ", n))
        })  
      clust_model
   })
   
   krig_rf_cluster_dist<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      rf.clusters<-rf.prod.data%>%filter(API %in% krig_rf_data()$API)%>%
                  mutate(Cluster=krig_clust_model()$cluster)
      clust_sample<-kmeans(filter(krig_scale_clust_data(),Type=="Target")%>%select(-Type), centers =1)
      dist.data<-data.frame(krig_clust_model()$centers,Cluster=seq(1,input$krig_clust_num),Dist_Sq=-1)
      for (i in 1:input$krig_clust_num){
         dist.data$Dist_Sq[i]<-sum((krig_clust_model()$centers[i,]-clust_sample$centers)^2)
      }
      dist.data
      
      dist.output<-rbind(dist.data,mutate(as.data.frame(clust_sample$centers),Cluster="Target",Dist_Sq=0))%>%
                  arrange(Dist_Sq)
      dist.output
   })
   
   output$krig_dist_table_heading<-renderText({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      paste0("Scaled Cluster Centers and Squared Distance to Target")
   })
   
   output$krig_dist_clust_table<-renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      krig_rf_cluster_dist()
   })
   
   krig_rf_model<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      rf.clusters<-rf.prod.data%>%filter(API %in% krig_rf_data()$API)%>%
         mutate(Cluster=krig_clust_model()$cluster)
      
      rf_clust_centers<-krig_rf_cluster_dist()%>%filter(Cluster!="Target")
      closest_clust<-rf_clust_centers%>%filter(Dist_Sq==min(rf_clust_centers$Dist_Sq))
      if(input$krig_clust_toggle==T){
         closest_clust<-data.frame(Cluster=input$krig_manually_clust)
      }
      rf.clust.data<-rf.clusters%>%filter(Cluster==closest_clust$Cluster)
      
      rf_model_data<-krig_rf_data()%>%filter(API %in% rf.clust.data$API)
      withProgress(message = 'Calculating Random Forest', value = 0, {
        # Number of times we'll go through the loop
        n <- 1
        set.seed(107)
        rf_model<-randomForest(BOE_Prod_2yr ~ .,data=select(rf_model_data,-API))
        
        incProgress(1/n, detail = paste(1, " of ", n))
      })
      rf_model
   })
   
   output$krig.rf.boe.importance<-renderPlot({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      rf.clusters<-rf.prod.data%>%filter(API %in% krig_rf_data()$API)%>%
         mutate(Cluster=krig_clust_model()$cluster)
      
      rf_clust_centers<-krig_rf_cluster_dist()%>%filter(Cluster!="Target")
      closest_clust<-rf_clust_centers%>%filter(Dist_Sq==min(rf_clust_centers$Dist_Sq))
      if(input$krig_clust_toggle==T){
         closest_clust<-data.frame(Cluster=input$krig_manually_clust)
      }
      
      krig_rf.var.imp<-varImp(krig_rf_model())
      krig_rf.summary<-data.frame(var=rownames(krig_rf.var.imp),
                                rel.inf=krig_rf.var.imp$Overall)%>%
         arrange(rel.inf)
      krig_rf.summary<-krig_rf.summary%>%
         mutate(var=factor(krig_rf.summary$var, levels=krig_rf.summary$var))
      
      krig_rf_rel_inf_plot<-ggplot(data = krig_rf.summary,aes(x=var,y=rel.inf))+
         geom_bar(stat="identity",color="black",fill="cornflowerblue")+
         geom_text(aes(label=round(rel.inf,1)), hjust=1.5, color="white",
                   position = position_dodge(0.9), size=3.5)+
         theme_bw()+
         xlab("Variable")+ylab("Variable Importance")+
         ggtitle(paste0("Kriged RF Importance Plot for BOE using Cluster ",closest_clust$Cluster))+
         theme(text = element_text(size=17))+
         coord_flip()
      krig_rf_rel_inf_plot
   })
   
   krig_rf_pred<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      model_pred_data<-krig_model_pred()%>%mutate(Lateral..ft.=input$krig_ll,
                                                  GPI=input$krig_gpi,
                                                  Proppant.per.GPI..lb.ft.=input$krig_prop,
                                                  Fluid.per.GPI..gal.ft.=input$krig_fluid,
                                                  TOC=TOC.pred,
                                                  Permeability...d.=Permeability.pred,
                                                  Brittleness=Brittleness.pred,
                                                  Porosity=Porosity.pred,
                                                  Thickness=Thickness.pred,
                                                  Vclay=Vclay.pred,
                                                  TVD=TVD.pred,
                                                  API.Gravity=API.Gravity.pred
                                                  )
      model_pred<-predict(krig_rf_model(),newdata=model_pred_data)
      model_pred_results<-model_pred_data%>%mutate(RF_BOE_Pred=model_pred)
      model_pred_results
   })
   
   krig_rf_sensitivity<-reactive({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      
      rf.clusters<-rf.prod.data%>%filter(API %in% krig_rf_data()$API)%>%
         mutate(Cluster=krig_clust_model()$cluster)
      
      model_pred_sensitivity_data<-krig_model_pred()%>%mutate(Lateral..ft.=input$krig_ll,
                                                  GPI=input$krig_gpi,
                                                  Proppant.per.GPI..lb.ft.=input$krig_prop,
                                                  Fluid.per.GPI..gal.ft.=input$krig_fluid,
                                                  TOC=TOC.pred,
                                                  Permeability...d.=Permeability.pred,
                                                  Brittleness=Brittleness.pred,
                                                  Porosity=Porosity.pred,
                                                  Thickness=Thickness.pred,
                                                  Vclay=Vclay.pred,
                                                  TVD=TVD.pred,
                                                  API.Gravity=API.Gravity.pred
      )
      
      rf_sensitivity<-data.frame(Surf.Lat=numeric(),Surf.Lon=numeric(),Train_Clust=numeric(),Pred_BOE=numeric(),min.prob=numeric())
      
      withProgress(message = 'Calculating Sensitivity', value = 0, {
        # Number of times we'll go through the loop
        n <- input$krig_clust_num
        for (this_clust in 1:input$krig_clust_num){
           rf.clust.data<-rf.clusters%>%filter(Cluster==this_clust)
           rf_model_data<-krig_rf_data()%>%filter(API %in% rf.clust.data$API)
           set.seed(107)
           rf_model<-randomForest(BOE_Prod_2yr ~ .,data=select(rf_model_data,-API))
           ##predict with RF
           this_model_pred<-predict(rf_model,newdata=model_pred_sensitivity_data)
           this_model_pred_results<-model_pred_sensitivity_data%>%mutate(Train_Clust=this_clust,Pred_BOE=this_model_pred)
           this_rf_sensitivity<-this_model_pred_results%>%mutate(Surf.Lon=lngcoords,Surf.Lat=latcoords)%>%
              select(Surf.Lat,Surf.Lon,Train_Clust,Pred_BOE,min.prob)
           rf_sensitivity<-rbind(rf_sensitivity,this_rf_sensitivity)
        }
        incProgress(1/n, detail = paste("Cluster ", this_clust, " of ", n))
      })
      
      rf_sensitivity
   })
   
   output$krig_sensitivity_name<-renderText({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      paste0("Sensitivity Summary Table using Probability Cutoff of ",input$cert_num*100,"%")
   })
   
   output$krig_rf_sense_summary<-renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      coord.table<-coord_table()
      sense_summary<-krig_rf_sensitivity()%>%filter(min.prob>input$cert_num)%>%
                     mutate(Number_Wells=coord.table$Wells_Per_Grid, 
                         Area_BOE=Pred_BOE*Number_Wells)%>%
                     group_by(Train_Clust)%>%
                     summarise(Total_Pred_2yr_BOE=sum(Area_BOE))%>%
                     arrange(desc(Total_Pred_2yr_BOE))%>%
                     mutate(Total_Pred_2yr_BOE=as.character(round(Total_Pred_2yr_BOE,0)))
         
      sense_summary
   })
   
   output$krig_field_summary_name<-renderText({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      paste0("Field Output Summary Table")
   })

   output$krig_rf_summary<-renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      coord.table<-coord_table()
      user.pred.cutoff<-krig_rf_pred()%>%filter(min.prob>input$cert_num)%>%
         select(lngcoords,latcoords,RF_BOE_Pred,!!!input$krig_rf_vars)%>%
         mutate(Number_Wells=coord.table$Wells_Per_Grid, Area_BOE=RF_BOE_Pred*Number_Wells)
      low.pred.cutoff<-krig_rf_pred()%>%filter(min.prob>.9)%>%
         select(lngcoords,latcoords,RF_BOE_Pred,!!!input$krig_rf_vars)%>%
         mutate(Number_Wells=coord.table$Wells_Per_Grid, Area_BOE=RF_BOE_Pred*Number_Wells)
      med.pred.cutoff<-krig_rf_pred()%>%filter(min.prob>.5)%>%
         select(lngcoords,latcoords,RF_BOE_Pred,!!!input$krig_rf_vars)%>%
         mutate(Number_Wells=coord.table$Wells_Per_Grid, Area_BOE=RF_BOE_Pred*Number_Wells)
      high.pred.cutoff<-krig_rf_pred()%>%filter(min.prob>.1)%>%
         select(lngcoords,latcoords,RF_BOE_Pred,!!!input$krig_rf_vars)%>%
         mutate(Number_Wells=coord.table$Wells_Per_Grid, Area_BOE=RF_BOE_Pred*Number_Wells)
      summary.user<-data.frame(Pred.Type=paste0("User_",100*input$cert_num),
                               Total_2y_BOE_Pred=round(sum(user.pred.cutoff$Area_BOE),0),
                               Well_Count=round(sum(user.pred.cutoff$Number_Wells),0),
                               Percentage_Wells=100*round(sum(user.pred.cutoff$Number_Wells),0)/(coord.table$Wells_Per_Grid[[1]]*input$krig_step^2),
                               Total_Acres=coord.table$Area_acres*dim(user.pred.cutoff)[[1]])
      summary.low<-data.frame(Pred.Type="Low_90",
                              Total_2y_BOE_Pred=round(sum(low.pred.cutoff$Area_BOE),0),
                              Well_Count=round(sum(low.pred.cutoff$Number_Wells),0),
                              Percentage_Wells=100*round(sum(low.pred.cutoff$Number_Wells),0)/(coord.table$Wells_Per_Grid[[1]]*input$krig_step^2),
                              Total_Acres=coord.table$Area_acres*dim(low.pred.cutoff)[[1]])
      summary.med<-data.frame(Pred.Type="Medium_50",
                              Total_2y_BOE_Pred=round(sum(med.pred.cutoff$Area_BOE),0),
                              Well_Count=round(sum(med.pred.cutoff$Number_Wells),0),
                              Percentage_Wells=100*round(sum(med.pred.cutoff$Number_Wells),0)/(coord.table$Wells_Per_Grid[[1]]*input$krig_step^2),
                              Total_Acres=coord.table$Area_acres*dim(med.pred.cutoff)[[1]])
      summary.high<-data.frame(Pred.Type="High_10",
                               Total_2y_BOE_Pred=round(sum(high.pred.cutoff$Area_BOE),0),
                               Well_Count=round(sum(high.pred.cutoff$Number_Wells),0),
                               Percentage_Wells=100*round(sum(high.pred.cutoff$Number_Wells),0)/(coord.table$Wells_Per_Grid[[1]]*input$krig_step^2),
                               Total_Acres=coord.table$Area_acres*dim(high.pred.cutoff)[[1]])
      summary.table<-rbind(summary.user,summary.low,summary.med,summary.high)
           summary.table
   }, digits = 0)
   
   output$krig_point_name<-renderText({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      paste0("BOE Prediction for Input Coordinate")
   })
   
   output$krig_point_pred<-renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      coord.table<-coord_table()
      point_pred_table<-krig_rf_pred()%>%
         filter(lngcoords==input$krig_pred_lon,latcoords==input$krig_pred_lat)%>%
         select(lngcoords,latcoords,RF_BOE_Pred,!!!input$krig_rf_vars)%>%
         mutate(Number_Wells=coord.table$Wells_Per_Grid, Area_BOE=RF_BOE_Pred*Number_Wells)
      point_pred_table
   })
   
   output$krig_table_name<-renderText({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      paste0("Top 10 Highest BOE Prediction Locations")
   })
   
   output$krig_rf_data_table<-renderTable({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      coord.table<-coord_table()
      display.data<-krig_rf_pred()%>%filter(min.prob>input$cert_num)%>%
                           select(lngcoords,latcoords,RF_BOE_Pred,!!!input$krig_rf_vars)%>%
                           arrange(desc(RF_BOE_Pred))%>%
                           mutate(Number_Wells=coord.table$Wells_Per_Grid, Area_BOE=RF_BOE_Pred*Number_Wells)
      head(display.data,10)
   })
   
   output$krig_MapPlot<-renderPlot({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      krig_data<-krig_rf_pred()%>%filter(min.prob>input$cert_num)
      bins_data<-rbind(data.frame(BOE=krig_data$RF_BOE_Pred),data.frame(BOE=rf.prod.data$BOE_Prod_2yr))
      krig_map_data<-rf.prod.data%>%mutate(BOE_Category=if_else(BOE_Prod_2yr<=round(quantile(bins_data$BOE,0.2),-4),paste0("Less than ", round(quantile(bins_data$BOE,0.2),-4)),
                                                      if_else(BOE_Prod_2yr<=round(quantile(bins_data$BOE,0.4),-4),paste0("Between ", round(quantile(bins_data$BOE,0.2),-4)," and ", round(quantile(bins_data$BOE,0.4),-4)),
                                                      if_else(BOE_Prod_2yr<=round(quantile(bins_data$BOE,0.6),-4),paste0("Between ", round(quantile(bins_data$BOE,0.4),-4)," and ", round(quantile(bins_data$BOE,0.6),-4)),
                                                      if_else(BOE_Prod_2yr<=round(quantile(bins_data$BOE,0.8),-4),paste0("Between ", round(quantile(bins_data$BOE,0.6),-4)," and ", round(quantile(bins_data$BOE,0.8),-4)),
                                                                      paste0("Above ",round(quantile(bins_data$BOE,0.8),-4))      )))))
      
      map_plot<-gg()+geom_point(data=krig_map_data, aes(x=Surf.Lon,  y=Surf.Lat,color=BOE_Category),size=2) +
         ggtitle(paste0("Actual BOE Prod", " Discrete Color on Map"))+scale_color_discrete()+ #scale_color_gradientn(colours = topo.colors(5))+
         theme(text = element_text(size=20))
      map_plot
   })
   
   output$krig_boe_pred_map<-renderPlot({
      req(input$password==my_password&input$go&input$krig_rf_toggle)
      train_loc2<- c(input$krig_Lon_LL-.1,input$krig_Lat_LL-.1,input$krig_Lon_UR+.1,input$krig_Lat_UR+.1)
      train_map_gmaps <- get_map(location=train_loc2, source="google", maptype="terrain")
      gg_train <- ggmap(train_map_gmaps)
      
      map_data<-krig_rf_pred()%>%filter(min.prob>input$cert_num)
      krig_data<-krig_rf_pred()%>%filter(min.prob>input$cert_num)
      bins_data<-rbind(data.frame(BOE=krig_data$RF_BOE_Pred),data.frame(BOE=rf.prod.data$BOE_Prod_2yr))
      krig_map_data<-krig_data%>%mutate(BOE_Category=if_else(RF_BOE_Pred<=round(quantile(bins_data$BOE,0.2),-4),paste0("Less than ", round(quantile(bins_data$BOE,0.2),-4)),
                                                                if_else(RF_BOE_Pred<=round(quantile(bins_data$BOE,0.4),-4),paste0("Between ", round(quantile(bins_data$BOE,0.2),-4)," and ", round(quantile(bins_data$BOE,0.4),-4)),
                                                                        if_else(RF_BOE_Pred<=round(quantile(bins_data$BOE,0.6),-4),paste0("Between ", round(quantile(bins_data$BOE,0.4),-4)," and ", round(quantile(bins_data$BOE,0.6),-4)),
                                                                                if_else(RF_BOE_Pred<=round(quantile(bins_data$BOE,0.8),-4),paste0("Between ", round(quantile(bins_data$BOE,0.6),-4)," and ", round(quantile(bins_data$BOE,0.8),-4)),
                                                                                        paste0("Above ",round(quantile(bins_data$BOE,0.8),-4))      )))))
      
      boe.map<- gg_train+geom_point(data=krig_map_data, aes_string(x="lngcoords",  y="latcoords",color="BOE_Category"),size=1.5)+
         ggtitle(paste0("BOE Predictions from RF Model \nwith Kriged Inputs Map"))+
         scale_color_discrete()+ #scale_color_gradientn(colours = topo.colors(5))+
         theme(text = element_text(size=20))
      
      boe.map
   })
   
##Ensemble Learning
   
   cut_props<-reactive({
     cut_props<-data.frame(low.cut=seq(input$el_start,input$el_end,input$el_step))%>%mutate(high.cut=1-low.cut)
   })
   
   filter_var_df<-reactive({
     req(input$password==my_password&input$go&input$EL_toggle)
     
     cut_props<-cut_props()

     filter_df<-rf.prod.data%>%select(API,!!!input$el_filter_var1,!!!input$el_filter_var2,!!!input$el_filter_var3)
     filter_df1<-rf.prod.data%>%select(!!!input$el_filter_var1)
     filter_df2<-rf.prod.data%>%select(!!!input$el_filter_var2)
     filter_df3<-rf.prod.data%>%select(!!!input$el_filter_var3)
    
    overlap_data1<-rf.prod.data%>%filter((!!!input$el_filter_var1)<=quantile(filter_df1[,1],cut_props$low.cut[1]),
                                         (!!!input$el_filter_var1)>=quantile(filter_df1[,1],cut_props$high.cut[1]),
                                         (!!!input$el_filter_var2)<=quantile(filter_df2[,1],cut_props$low.cut[1]),
                                         (!!!input$el_filter_var2)>=quantile(filter_df2[,1],cut_props$high.cut[1]),
                                         (!!!input$el_filter_var3)<=quantile(filter_df3[,1],cut_props$low.cut[1]),
                                         (!!!input$el_filter_var3)>=quantile(filter_df3[,1],cut_props$high.cut[1]))
    
    overlap_low_df1<-overlap_data1%>%slice_sample(prop = 0.7)
    overlap_high_df1<-overlap_data1%>%anti_join(overlap_low_df1,by=c("API"="API"))
    
    low_df1<-rf.prod.data%>%anti_join(overlap_data1,by=c("API"="API"))%>%
      filter((!!!input$el_filter_var1)<quantile(filter_df1[,1],min(cut_props$low.cut[1],cut_props$high.cut[1])),
             (!!!input$el_filter_var2)<quantile(filter_df2[,1],min(cut_props$low.cut[1],cut_props$high.cut[1])),
             (!!!input$el_filter_var3)<quantile(filter_df3[,1],min(cut_props$low.cut[1],cut_props$high.cut[1])))%>%
      rbind(overlap_low_df1)%>%
      select(API,BOE_Prod_2yr,!!!input$el_formula_var)
    
    high_df1<-rf.prod.data%>%anti_join(overlap_data1,by=c("API"="API"))%>%
      filter((!!!input$el_filter_var1)>quantile(filter_df1[,1],max(cut_props$low.cut[1],cut_props$high.cut[1])),
             (!!!input$el_filter_var2)>quantile(filter_df2[,1],max(cut_props$low.cut[1],cut_props$high.cut[1])),
             (!!!input$el_filter_var3)>quantile(filter_df3[,1],max(cut_props$low.cut[1],cut_props$high.cut[1])))%>%
      rbind(overlap_high_df1)%>%
      select(API,BOE_Prod_2yr,!!!input$el_formula_var)
    
    list(low_df=(low_df1),high_df=(high_df1))
     })
   
   
   
   output$EL_low_text<-renderText({
     req(input$password==my_password&input$go&input$EL_toggle)
     paste0("Head of Low (Train) Data for Start Cut")
   })
   output$EL_Low_df<-renderTable({
     req(input$password==my_password&input$go&input$EL_toggle)
     head(filter_var_df()$low_df)
   })
   
   output$EL_high_text<-renderText({
     req(input$password==my_password&input$go&input$EL_toggle)
     paste0("Head of High (Test) Data for Start Cut")
   })
   output$EL_High_df<-renderTable({
     req(input$password==my_password&input$go&input$EL_toggle)
     head(filter_var_df()$high_df)
   })
   
   output$EL_table_text<-renderText({
     req(input$password==my_password&input$go&input$EL_toggle)
     paste0("Ensemble Learning Results")
   })
   
   output$EL_Stress_Test_text<-renderText({
     req(input$password==my_password&input$go&input$EL_toggle_st)
     paste0("All Weights Errors in Specified Cut")
   })
   
   EL_Strees_Test<-reactive({
     req(input$password==my_password&input$go&input$EL_toggle_st)
     
     filter_df<-rf.prod.data%>%select(API,!!!input$el_filter_var1,!!!input$el_filter_var2,!!!input$el_filter_var3)
     filter_df1<-rf.prod.data%>%select(!!!input$el_filter_var1)
     filter_df2<-rf.prod.data%>%select(!!!input$el_filter_var2)
     filter_df3<-rf.prod.data%>%select(!!!input$el_filter_var3)
     
     overlap_data1<-rf.prod.data%>%filter((!!!input$el_filter_var1)<=quantile(filter_df1[,1],input$el_low),
                                          (!!!input$el_filter_var1)>=quantile(filter_df1[,1],input$el_high),
                                          (!!!input$el_filter_var2)<=quantile(filter_df2[,1],input$el_low),
                                          (!!!input$el_filter_var2)>=quantile(filter_df2[,1],input$el_high),
                                          (!!!input$el_filter_var3)<=quantile(filter_df3[,1],input$el_low),
                                          (!!!input$el_filter_var3)>=quantile(filter_df3[,1],input$el_high))
     
     overlap_low_df1<-overlap_data1%>%slice_sample(prop = input$el_train_prop)
     overlap_high_df1<-overlap_data1%>%anti_join(overlap_low_df1,by=c("API"="API"))
     
     low_df1<-rf.prod.data%>%anti_join(overlap_data1,by=c("API"="API"))%>%
       filter((!!!input$el_filter_var1)<quantile(filter_df1[,1],min(input$el_low,input$el_high)),
              (!!!input$el_filter_var2)<quantile(filter_df2[,1],min(input$el_low,input$el_high)),
              (!!!input$el_filter_var3)<quantile(filter_df3[,1],min(input$el_low,input$el_high)))%>%
       rbind(overlap_low_df1)%>%
       select(!!!input$el_target_stress,!!!input$el_formula_var) ##Remove BOE 2yr
     
     high_df1<-rf.prod.data%>%anti_join(overlap_data1,by=c("API"="API"))%>%
       filter((!!!input$el_filter_var1)>quantile(filter_df1[,1],max(input$el_low,input$el_high)),
              (!!!input$el_filter_var2)>quantile(filter_df2[,1],max(input$el_low,input$el_high)),
              (!!!input$el_filter_var3)>quantile(filter_df3[,1],max(input$el_low,input$el_high)))%>%
       rbind(overlap_high_df1)%>%
       select(!!!input$el_target_stress,!!!input$el_formula_var) ##Remove BOE 2yr
     
     ##Data formatting for Ridge Regression
     low_ridge_df1_y<-select(low_df1,!!!input$el_target_stress)[,1]
     low_ridge_df1_x<-data.matrix(low_df1%>%select(-(!!!input$el_target_stress)))
     
     high_ridge_df1_y<-select(high_df1,!!!input$el_target_stress)[,1]
     high_ridge_df1_x<-data.matrix(high_df1%>%select(-(!!!input$el_target_stress)))

     
     ##Random Forest
     withProgress(message = 'Calculating Random Forest', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       
       rf_model1<-randomForest(as.formula(paste(input$el_target_stress,"~ .")),data = low_df1, ntree=input$el_num_tree)
       train.error.rf1<-rmse(select(low_df1,!!!input$el_target_stress)[,1],predict(rf_model1))
       test.error.rf1<-rmse(select(high_df1,!!!input$el_target_stress)[,1],predict(rf_model1,newdata = high_df1))
       incProgress(1/n, detail = paste(1, " of ", n))
     })
     
     ##Gradient Boosting
     withProgress(message = 'Calculating Gradient Boosting', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       
       gb_model1<-gbm(as.formula(paste(input$el_target_stress,"~ .")),data = low_df1, n.trees = input$el_num_tree)
       train.error.gb1<-rmse(select(low_df1,!!!input$el_target_stress)[,1],predict(gb_model1))
       test.error.gb1<-rmse(select(high_df1,!!!input$el_target_stress)[,1],predict(gb_model1,newdata = high_df1))
       incProgress(1/n, detail = paste(1, " of ", n))
     })
     
     ##Ridge Regression
     withProgress(message = 'Calculating Ridge Regression', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       
       cv_ridge_model1 <- cv.glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 0)
       ridge_model1_best_lambda <- cv_ridge_model1$lambda.min
       ridge_best_model1 <- glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 0, lambda = ridge_model1_best_lambda)
       train.error.ridge1<-rmse(low_ridge_df1_y,predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = low_ridge_df1_x))
       test.error.ridge1<-rmse(high_ridge_df1_y,predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = high_ridge_df1_x))
       incProgress(1/n, detail = paste(1, " of ", n))
     })
   
     ##Lasso Regression
     withProgress(message = 'Calculating Lasso Regression', value = 0, {
       # Number of times we'll go through the loop
       n <- 1
       
       cv_lasso_model1 <- cv.glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 1)
       lasso_model1_best_lambda <- cv_lasso_model1$lambda.min
       lasso_best_model1 <- glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 1, lambda = lasso_model1_best_lambda)
       train.error.lasso1<-rmse(low_ridge_df1_y,predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = low_ridge_df1_x))
       test.error.lasso1<-rmse(high_ridge_df1_y,predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = high_ridge_df1_x))
       incProgress(1/n, detail = paste(1, " of ", n))
      })
     
     ##create Output
     output1<-data.frame(Model_Name=c("RF Percentile","GB Percentile","Ridge Percentile","Lasso Percentile"),
                         Train_Error=c(train.error.rf1,train.error.gb1,train.error.ridge1,train.error.lasso1),
                         Test_Error=c(test.error.rf1,test.error.gb1,test.error.ridge1,test.error.lasso1))
     
     output_train<-data.frame(Actual_BOE=low_ridge_df1_y, 
                              RF_Pred=predict(rf_model1),
                              GB_Pred=predict(gb_model1),
                              X1=predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = low_ridge_df1_x)[,1],
                              X1.1=predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = low_ridge_df1_x)[,1])%>%
       mutate(Ridge_Pred=X1,Lasso_Pred=X1.1)%>%
       select(-(X1:X1.1))
     
     output_test<-data.frame(Actual_BOE=high_ridge_df1_y,
                             RF_Pred=predict(rf_model1,newdata = high_df1),
                             GB_Pred=predict(gb_model1,newdata = high_df1),
                             X1=predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = high_ridge_df1_x)[,1],
                             X1.1=predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = high_ridge_df1_x)[,1])%>%
       mutate(Ridge_Pred=X1,Lasso_Pred=X1.1)%>%
       select(-(X1:X1.1))
     
     ##create weightings for different aggregate models
     weights.df<-expand.grid(RF_wt=seq(0,1,input$el_weight_step),
                             GB_wt=seq(0,1,input$el_weight_step),
                             Ridge_wt=seq(0,1,input$el_weight_step),
                             Lasso_wt=seq(0,1,input$el_weight_step))%>%
       mutate(Total_Weight=RF_wt+GB_wt+Ridge_wt+Lasso_wt)%>%
       filter(Total_Weight==1)%>%select(-Total_Weight)
     weights.df<-weights.df%>%mutate(ID=seq(1,dim(weights.df)[[1]]))
     
     ##Loop through different weights for training errors
     output_train_errors<-data.frame(ID=numeric(),Train_Error=numeric())
     withProgress(message = 'Calculating Weighted Train Predictions', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(weights.df)[[1]]
       
       for (j in 1:dim(weights.df)[[1]]){
         temp_df<-output_train%>%
           mutate(Weighted_Pred=weights.df$RF_wt[j]*RF_Pred+weights.df$GB_wt[j]*GB_Pred+weights.df$Ridge_wt[j]*Ridge_Pred+weights.df$Lasso_wt[j]*Lasso_Pred)
         this_result<-data.frame(ID=j,Train_Error=rmse(output_train$Actual_BOE,temp_df$Weighted_Pred))
         output_train_errors<-rbind(output_train_errors,this_result)
         
         incProgress(1/n, detail = paste(j, " of ", n))
       }
     })
     output_train_errors<-output_train_errors%>%left_join(weights.df)%>%
       arrange(Train_Error)
     
     ##Loop through different weights for testing errors
     output_test_errors<-data.frame(ID=numeric(),Test_Error=numeric())
     withProgress(message = 'Calculating Weighted Test Predictions', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(weights.df)[[1]]
       
       for (k in 1:dim(weights.df)[[1]]){
         temp_df<-output_test%>%
           mutate(Weighted_Pred=weights.df$RF_wt[k]*RF_Pred+weights.df$GB_wt[k]*GB_Pred+weights.df$Ridge_wt[k]*Ridge_Pred+weights.df$Lasso_wt[k]*Lasso_Pred)
         this_result<-data.frame(ID=k,Test_Error=rmse(output_test$Actual_BOE,temp_df$Weighted_Pred))
         output_test_errors<-rbind(output_test_errors,this_result)
         incProgress(1/n, detail = paste(k, " of ", n))
       }
     })
     output_test_errors<-output_test_errors%>%left_join(weights.df)%>%
       arrange(Test_Error)
     
     #Create Output file with Train and Test Models based on Weightings
     output_errors1<-output_test_errors%>%left_join(select(output_train_errors,ID,Train_Error))%>%
       mutate(Well_Count_Train=dim(low_df1)[1],Well_Count_Test=dim(high_df1)[1],
              Total_Target_Output_Train=sum(low_ridge_df1_y),Total_Target_Output_Test=sum(high_ridge_df1_y),
              Avg_Target_Output_Train=Total_Target_Output_Train/Well_Count_Train,Avg_Target_Output_Test=Total_Target_Output_Test/Well_Count_Test,
              Percent_Error_Train=Train_Error/Avg_Target_Output_Train,Percent_Error_Test=Test_Error/Avg_Target_Output_Test
       )
     
   })
   
   output$EL_Strees_Test_Table<-renderTable({
     EL_Strees_Test()
   })
   
   EL_Best_Weights<-reactive({
     req(input$password==my_password&input$go&input$EL_toggle)
     
     cut_props<-cut_props()
     best_weightings<-data.frame(Test_Error=numeric(),RF_wt=numeric(),GB_wt=numeric(),
                                 Ridge_wt=numeric(),Lasso_wt=numeric(),Train_Error=numeric(),
                                 Well_Count_Train=numeric(),Well_Count_Test=numeric(),
                                 Total_Target_Output_Train=numeric(),Total_Target_Output_Test=numeric(),Avg_Target_Output_Train=numeric(),
                                 Avg_Target_Output_Test=numeric(),Percent_Error_Train=numeric(),Percent_Error_Test=numeric(),
                                 low.cut=numeric(),high.cut=numeric())
     
     withProgress(message = 'Calculating Models for Various Cuts', value = 0, {
       # Number of times we'll go through the loop
       n <- dim(cut_props)[1]
       
       for (i in 1:dim(cut_props)[1]){
         filter_df<-rf.prod.data%>%select(API,!!!input$el_filter_var1,!!!input$el_filter_var2,!!!input$el_filter_var3)
         filter_df1<-rf.prod.data%>%select(!!!input$el_filter_var1)
         filter_df2<-rf.prod.data%>%select(!!!input$el_filter_var2)
         filter_df3<-rf.prod.data%>%select(!!!input$el_filter_var3)
         
         overlap_data1<-rf.prod.data%>%filter((!!!input$el_filter_var1)<=quantile(filter_df1[,1],cut_props$low.cut[i]),
                                              (!!!input$el_filter_var1)>=quantile(filter_df1[,1],cut_props$high.cut[i]),
                                              (!!!input$el_filter_var2)<=quantile(filter_df2[,1],cut_props$low.cut[i]),
                                              (!!!input$el_filter_var2)>=quantile(filter_df2[,1],cut_props$high.cut[i]),
                                              (!!!input$el_filter_var3)<=quantile(filter_df3[,1],cut_props$low.cut[i]),
                                              (!!!input$el_filter_var3)>=quantile(filter_df3[,1],cut_props$high.cut[i]))
         
         overlap_low_df1<-overlap_data1%>%slice_sample(prop = input$el_train_prop)
         overlap_high_df1<-overlap_data1%>%anti_join(overlap_low_df1,by=c("API"="API"))
         
         low_df1<-rf.prod.data%>%anti_join(overlap_data1,by=c("API"="API"))%>%
           filter((!!!input$el_filter_var1)<quantile(filter_df1[,1],min(cut_props$low.cut[i],cut_props$high.cut[i])),
                  (!!!input$el_filter_var2)<quantile(filter_df2[,1],min(cut_props$low.cut[i],cut_props$high.cut[i])),
                  (!!!input$el_filter_var3)<quantile(filter_df3[,1],min(cut_props$low.cut[i],cut_props$high.cut[i])))%>%
           rbind(overlap_low_df1)%>%
           select(!!!input$el_target_var,!!!input$el_formula_var)
         
         high_df1<-rf.prod.data%>%anti_join(overlap_data1,by=c("API"="API"))%>%
           filter((!!!input$el_filter_var1)>quantile(filter_df1[,1],max(cut_props$low.cut[i],cut_props$high.cut[i])),
                  (!!!input$el_filter_var2)>quantile(filter_df2[,1],max(cut_props$low.cut[i],cut_props$high.cut[i])),
                  (!!!input$el_filter_var3)>quantile(filter_df3[,1],max(cut_props$low.cut[i],cut_props$high.cut[i])))%>%
           rbind(overlap_high_df1)%>%
           select(!!!input$el_target_var,!!!input$el_formula_var)
        
         ##Data formatting for Ridge Regression
         low_ridge_df1_y<-select(low_df1,!!!input$el_target_var)[,1]
         low_ridge_df1_x<-data.matrix(low_df1%>%select(-(!!!input$el_target_var)))
         
         high_ridge_df1_y<-select(high_df1,!!!input$el_target_var)[,1]
         high_ridge_df1_x<-data.matrix(high_df1%>%select(-(!!!input$el_target_var)))
  
         ##Random Forest
         rf_model1<-randomForest(as.formula(paste(input$el_target_var,"~ .")),data = low_df1, ntree=input$el_num_tree)
         train.error.rf1<-rmse(select(low_df1,!!!input$el_target_var)[,1],predict(rf_model1))
         test.error.rf1<-rmse(select(high_df1,!!!input$el_target_var)[,1],predict(rf_model1,newdata = high_df1))
         
         ##Gradient Boosting
         gb_model1<-gbm(as.formula(paste(input$el_target_var,"~ .")),data = low_df1, n.trees = input$el_num_tree)
         train.error.gb1<-rmse(select(low_df1,!!!input$el_target_var)[,1],predict(gb_model1))
         test.error.gb1<-rmse(select(high_df1,!!!input$el_target_var)[,1],predict(gb_model1,newdata = high_df1))
         
         ##Ridge Regression
         cv_ridge_model1 <- cv.glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 0)
         ridge_model1_best_lambda <- cv_ridge_model1$lambda.min
         ridge_best_model1 <- glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 0, lambda = ridge_model1_best_lambda)
         train.error.ridge1<-rmse(low_ridge_df1_y,predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = low_ridge_df1_x))
         test.error.ridge1<-rmse(high_ridge_df1_y,predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = high_ridge_df1_x))
         
         ##Lasso Regression
         cv_lasso_model1 <- cv.glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 1)
         lasso_model1_best_lambda <- cv_lasso_model1$lambda.min
         lasso_best_model1 <- glmnet(low_ridge_df1_x, low_ridge_df1_y, alpha = 1, lambda = lasso_model1_best_lambda)
         train.error.lasso1<-rmse(low_ridge_df1_y,predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = low_ridge_df1_x))
         test.error.lasso1<-rmse(high_ridge_df1_y,predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = high_ridge_df1_x))
         
         ##create Output
         output1<-data.frame(Model_Name=c("RF Percentile","GB Percentile","Ridge Percentile","Lasso Percentile"),
                             Train_Error=c(train.error.rf1,train.error.gb1,train.error.ridge1,train.error.lasso1),
                             Test_Error=c(test.error.rf1,test.error.gb1,test.error.ridge1,test.error.lasso1))
         
         output_train<-data.frame(Actual_BOE=low_ridge_df1_y,
                                  RF_Pred=predict(rf_model1),
                                  GB_Pred=predict(gb_model1),
                                  X1=predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = low_ridge_df1_x)[,1],
                                  X1.1=predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = low_ridge_df1_x)[,1])%>%
           mutate(Ridge_Pred=X1,Lasso_Pred=X1.1)%>%
           select(-(X1:X1.1))
         
         output_test<-data.frame(Actual_BOE=high_ridge_df1_y,
                                 RF_Pred=predict(rf_model1,newdata = high_df1),
                                 GB_Pred=predict(gb_model1,newdata = high_df1),
                                 X1=predict(ridge_best_model1, s = ridge_model1_best_lambda, newx = high_ridge_df1_x)[,1],
                                 X1.1=predict(lasso_best_model1, s = lasso_model1_best_lambda, newx = high_ridge_df1_x)[,1])%>%
           mutate(Ridge_Pred=X1,Lasso_Pred=X1.1)%>%
           select(-(X1:X1.1))
         
         ##create weightings for different aggregate models
         weights.df<-expand.grid(RF_wt=seq(0,1,input$el_weight_step),
                                 GB_wt=seq(0,1,input$el_weight_step),
                                 Ridge_wt=seq(0,1,input$el_weight_step),
                                 Lasso_wt=seq(0,1,input$el_weight_step))%>%
           mutate(Total_Weight=RF_wt+GB_wt+Ridge_wt+Lasso_wt)%>%
           filter(Total_Weight==1)%>%select(-Total_Weight)
         weights.df<-weights.df%>%mutate(ID=seq(1,dim(weights.df)[[1]]))
         
         ##Loop through different weights for training errors
         output_train_errors<-data.frame(ID=numeric(),Train_Error=numeric())
         for (j in 1:dim(weights.df)[[1]]){
           temp_df<-output_train%>%
             mutate(Weighted_Pred=weights.df$RF_wt[j]*RF_Pred+weights.df$GB_wt[j]*GB_Pred+weights.df$Ridge_wt[j]*Ridge_Pred+weights.df$Lasso_wt[j]*Lasso_Pred)
           this_result<-data.frame(ID=j,Train_Error=rmse(output_train$Actual_BOE,temp_df$Weighted_Pred))
           output_train_errors<-rbind(output_train_errors,this_result)
         }
         output_train_errors<-output_train_errors%>%left_join(weights.df)%>%
           arrange(Train_Error)
         
         ##Loop through different weights for testing errors
         output_test_errors<-data.frame(ID=numeric(),Test_Error=numeric())
         for (k in 1:dim(weights.df)[[1]]){
           temp_df<-output_test%>%
             mutate(Weighted_Pred=weights.df$RF_wt[k]*RF_Pred+weights.df$GB_wt[k]*GB_Pred+weights.df$Ridge_wt[k]*Ridge_Pred+weights.df$Lasso_wt[k]*Lasso_Pred)
           this_result<-data.frame(ID=k,Test_Error=rmse(output_test$Actual_BOE,temp_df$Weighted_Pred))
           output_test_errors<-rbind(output_test_errors,this_result)
           
         }
         output_test_errors<-output_test_errors%>%left_join(weights.df)%>%
           arrange(Test_Error)
         
         #Create Output file with Train and Test Models based on Weightings
         output_errors1<-output_test_errors%>%left_join(select(output_train_errors,ID,Train_Error))%>%
           mutate(Well_Count_Train=dim(low_df1)[1],Well_Count_Test=dim(high_df1)[1],
                  Total_Target_Output_Train=sum(low_ridge_df1_y),Total_Target_Output_Test=sum(high_ridge_df1_y),
                  Avg_Target_Output_Train=Total_Target_Output_Train/Well_Count_Train,Avg_Target_Output_Test=Total_Target_Output_Test/Well_Count_Test,
                  Percent_Error_Train=Train_Error/Avg_Target_Output_Train,Percent_Error_Test=Test_Error/Avg_Target_Output_Test
           )
  
          this_weight<-output_errors1[1,]%>%
            select(-ID)%>%mutate(low.cut=cut_props$low.cut[i],high.cut=cut_props$high.cut[i])
          best_weightings<-rbind(best_weightings,this_weight)
          
          incProgress(1/n, detail = paste("Cut ",i, " of ", n))
       }
     })
     best_weightings
     
   })
   
   output$EL_Table<-renderTable({
     req(input$password==my_password&input$go&input$EL_toggle)
     EL_Best_Weights()
   })
   
   
   output$download_EL_Best_Weights<-downloadHandler(
     filename = function() {
       paste0("Ensemble Learning", " Weightings Table.csv")
     },
     content=function(file) {
       write.csv(EL_Best_Weights(),file,row.names=FALSE)
     }
   )
   
   output$download_EL_Stress_Test<-downloadHandler(
     filename = function() {
       paste0("Stress Test LC ",input$el_low," HC ",input$el_high ," Weightings Error Table.csv")
     },
     content=function(file) {
       write.csv(EL_Strees_Test(),file,row.names=FALSE)
     }
   )
   
}

# Run the application 
shinyApp(ui = ui, server = server)
#}
