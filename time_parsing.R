library("readxl")
library(ggplot2)
library(DT)
library(lubridate)
library(dplyr)
require(tidyverse)
my_data <- read_excel("/Users/yujunming/Downloads/BCM_Bread_2024_V3_2024_07_16_06_42_00_351566.xlsx")
time_start<-substring(my_data$start,0,23)
time_end<-substring(my_data$end,0,23)
time_start<-ymd_hms(time_start)
time_end<-ymd_hms(time_end)
time_diff$name<-my_data$`1.2 Enumerator name (WFP)`
time_diff$time<-time_end-time_start
print(time_diff)
time_outlier<-which(time_diff %in% boxplot.stats(time_diff$time)$out)
print(time_outlier)
print(my_data$`1.2 Enumerator name (WFP)`[time_outlier])
hh_size<-my_data$`2.5 What is the household size of the respondent?`
print(boxplot.stats(hh_size)$out)
my_data$`1.3.a Do you agree to participate?`<- as.numeric(substring(my_data$`1.3.a Do you agree to participate?`,0,1))


#count each person's warning for long time
out_names<-data.frame(list(my_data$`1.2 Enumerator name (WFP)`[time_outlier]))
enu_names<-data.frame(my_data$`1.2 Enumerator name (WFP)`)
enu_names<-unique(enu_names)
names(enu_names)[1]<-paste("names1")
names(out_names)[1]<-paste("names1")
out_names<-data.frame(table(out_names))
print(enu_names)
enu_names<-(merge(enu_names,out_names,by="names1",all=TRUE))
enu_names$Freq[is.na(enu_names$Freq)]<-0
print(enu_names)


name_list<-list(my_data$`1.2 Enumerator name (WFP)`)
agg_df <- aggregate(time_diff$time, by=name_list, FUN=function(x) round(mean(x),2))
agg_df[3]<-aggregate(my_data$`1.3.a Do you agree to participate?`, by=name_list, FUN=sum)[2]
agg_df[4]<-merge(enu_names,agg_df,all=True)[2]
names(agg_df)[1]<-paste("Enumerator Name")
names(agg_df)[2]<-paste("Average Interview Time(min)")
names(agg_df)[3]<-paste("Successful Interview Numbers")
names(agg_df)[4]<-paste("Warning on Interview Duration")

time_diff_indi<-xtabs(~time+name,time_diff)
print(time_diff_indi)

library(shiny)

# Define UI ----
ui <- fluidPage(
  title = "Examples of DataTables",
    tabsetPanel(
      id = 'dataset',
      tabPanel("By Enumerators",   sidebarLayout(
        sidebarPanel(
          plotOutput("myplot_enu")
        ),
        mainPanel(
          DT::dataTableOutput("mytable1")
        )
      )),
      tabPanel("By BreadShops",   sidebarLayout(
        sidebarPanel(
          plotOutput("myplot_shop")
        ),
        mainPanel(
          DT::dataTableOutput("mytable2")
          )
        )
      )
    )
)

# Define server logic ----
server <- function(input, output) {
  output$mytable1 <- DT::renderDataTable({DT::datatable(agg_df)})
  output$mytable2 <- DT::renderDataTable({DT::datatable(agg_df)})
  output$myplot_enu <-renderPlot({boxplot(time_diff)})
  
}

# Run the app ----
shinyApp(ui = ui, server = server)










