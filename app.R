#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require("jsonlite")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(DT)
library(plotly)
library(crosstalk)
library(shinydashboard)

library(readr)
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2) 
library(treemapify)


#global variables
# Read in data

All_Studies_18409 <- read_delim("All Studies 18409.csv", ";", escape_double = FALSE, na = "empty", trim_ws = TRUE)
drug_categories <- read.csv("drug-regex.csv", header=T, sep=',')
model_categories <- read.csv("list-of-models-for-R.csv", header=T, sep=',')

# regexList <- colnames(All_Studies_18409)[-c(1:17)]
# modelRegex <- regexList[1:38]
# drugRegex <- regexList[39:154]


## Data preparation 
modelAllStudies <- All_Studies_18409[, 1:55]
removeModel_rows <- modelAllStudies[apply(modelAllStudies[c(18:55)], 1, function(z) any (z!=0)),]
removeModel <- removeModel_rows[, apply(removeModel_rows, 2, function(x) any(x!=0))]
# modelData <- data.frame(v1=setdiff(modelAllStudies[,1], removeModel[,1]), v2=setdiff(modelAllStudies[,2], removeModel[,2]))
# modelData <- modelAllStudies[!modelAllStudies$depID %in% removeModel$depID, ]


drugAllStudies <- All_Studies_18409[, c(1:17, 56:171)]
removeDrug_rows <- drugAllStudies[apply(drugAllStudies[c(18:133)], 1, function(z) any (z!=0)),]
removeDrug <- removeDrug_rows[, apply(removeDrug_rows, 2, function(x) any(x!=0))]
# drugData <- data.frame(v1=setdiff(drugAllStudies[,1], removeDrug[,1]), v2=setdiff(drugAllStudies[,2], removeDrug[,2]))
# drugData <- drugAllStudies[!drugAllStudies$depID %in% removeDrug$depID, ]



## change column types
removeModel$PdfPathStatus <- as.character(removeModel$PdfPathStatus)
removeDrug$PdfPathStatus <- as.character(removeDrug$PdfPathStatus)

removeModel$depID <- as.character(removeModel$depID)
removeDrug$depID <- as.character(removeDrug$depID)


## remove regex from column names
colnames(removeModel)[c(18:47)] <- sub("Regex.*", "", colnames(removeModel)[c(18:47)])
colnames(removeDrug)[c(18:111)] <- sub("Regex.*", "", colnames(removeDrug)[c(18:111)])


## remove regex from categories sheets
drug_categories$name <- sub("Regex.*", "", drug_categories$dictionaryNameHeader)
model_categories$name <- sub("Regex.*", "", model_categories$dictionaryNameHeader)

## make categories as factors
drug_categories$drugCategory <- as.factor(drug_categories$drugCategory)
model_categories$modelCategory <- as.factor(model_categories$modelCategory)





# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Set theme
  theme = shinytheme("spacelab"),
  
  # title 
  titlePanel("Animal Models of Depression"),
  
  tabsetPanel(
# model panel  
      tabPanel("Model", fluid = T, 
             sidebarLayout(
               sidebarPanel(
                 
                 helpText(h5("This is an interactive app to create meta-analysis sub-projects on Animal Models of Depression. 
                             Input your data into your project on", a(href ="https://app.syrf.org.uk", "SyRF.",target = "_blank"), "Authors: Alexandra Bannach-Brown")),
                 helpText(p("Please contact",
                            a(href ="https://twitter.com/aBannachBrown", "Alexandra on twitter",target = "_blank"),
                            "or see the",
                            a(href ="https://github.com/abannachbrown/visualisations_of_depression_data", "GitHub page",target = "_blank"),
                            " for more information.")),
                 br(),
                 # selectInput("modelOptions", "Select models to display", colnames(removeModel)[c(18:133)], 
                 #             selected = colnames(removeModel)[c(18:55)], multiple = TRUE)
                 pickerInput(inputId = "modelOptions", label= "Select models of depression to display",
                                    choices = colnames(removeModel)[c(18:47)],
                                    selected = colnames(removeModel)[c(18:47)],
                             options = list(`actions-box` = TRUE),
                             multiple = TRUE
                 )
               ), 
               
               mainPanel(helpText(h5("The graph below displays filtered results based on input in the sidebar.")),
                        # verbatimTextOutput("modelclick"), 
                         br(),
                         plotOutput("modelPlot", height = "500px" 
                                      #,click="click_treemap_model"
                                      ), 
                         br(), 
                         br(), 
                         helpText(h5("Download the filtered data by pressing the button below the table. Import this data directly into your project on",  a(href ="https://app.syrf.org.uk", "SyRF.",target = "_blank"), ".")),
                         br(),
                         DT::dataTableOutput("xModel", width="100%", height = "auto"), 
                         br(), 
                         br(), 
                         fluidRow(
                           p(class = 'text-center', downloadButton('xDownModel', 'Download Filtered Data'))
                         )) 
             ) 
    ), 
     

##drug panel
    
    tabPanel("Drug", fluid = T, 
             sidebarLayout(
               sidebarPanel(
                 
                 helpText(h5("This is an interactive app to create meta-analysis sub-projects on Animal Models of Depression. Input your data into your project on", a(href ="https://app.syrf.org.uk", "SyRF.",target = "_blank"),
                             "Authors: Alexandra Bannach-Brown")),
                 helpText(p("Please contact",
                            a(href ="https://twitter.com/aBannachBrown", "Alexandra on twitter",target = "_blank"),
                            "or see the",
                            a(href ="https://github.com/abannachbrown/visualisations_of_depression_data", "GitHub page",target = "_blank"),
                            " for more information.")),
                 br(), 
                 # selectInput("drugOptions", "Select drugs to display", colnames(removeDrug)[c(18:133)], 
                 #             selected = colnames(removeDrug)[c(18:133)], multiple = TRUE)
                 pickerInput(inputId = "drugOptions",
                             label = "Select the drugs to display",
                                    choices = colnames(removeDrug)[c(18:111)],
                                    selected = colnames(removeDrug)[c(18:111)],
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE
                 )
               ),
               mainPanel(helpText(h5("The graph below displays filtered results based on input in the sidebar.")),
                       #  verbatimTextOutput("drugclick"), 
                         br(),
                         plotOutput('drugPlot', height = '500px' 
                                      #,click="click_treemap_model"
                                      ), 
                         br(), 
                         br(), 
                         helpText(h5("Download the filtered data by pressing the button below the table. Import this data directly into your project on",  a(href ="https://app.syrf.org.uk", "SyRF",target = "_blank"), ".")),
                         br(),
                         DT::dataTableOutput("xDrug", width="100%", height = "auto"), 
                         br(), 
                         br(), 
                         fluidRow(
                           p(class = 'text-center', downloadButton('xDownDrug', 'Download Filtered Data'))
                         ))
             )
    ), 


tabPanel("All Data", fluid = T, helpText(h5("Download the filtered data by pressing the button below the table. Import this data directly into your project on",  a(href ="https://app.syrf.org.uk", "SyRF",target = "_blank"), ".")),
         br(),
         DT::dataTableOutput("xAllData", width="100%", height = "auto"), 
         br(),
         fluidRow(
           p(class = 'text-center', downloadButton('xDownAll', 'Download All Data'))
)
),

tabPanel("About", fluid = T, helpText(h2("
Preclinical Models of Depression"
)), 
helpText(h5("This app has been built in connection with a systematic review and meta-analysis of animal models of depression. 
The pre-registered protocol for this review is published",  
            a(href = "https://onlinelibrary.wiley.com/doi/pdf/10.1002/ebm2.24", "here.", target = "_blank"),  
            br(), 
            br(), 
            br()
)),
helpText(h3("
Regular Expression Dictionaties
", 
            br() )),
helpText(h5(
  "Regular Expression dictionaries are custom built and implemented on records using AutoAnnotation R package built by Jing Liao.", 
a(href = "https://github.com/shihikoo/AutoAnnotation/", "View GitHub documentation here.", target = "_blank"),
"The antidepressant dictionary was built based on the list of antidepressants Wikipedia ", 
a(href = "https://en.wikipedia.org/wiki/List_of_antidepressants", "page.", target = "_blank"), 
"from December, 2016."
)), 
br(), 
br() , 
helpText(h3(
            "Licensing Agreement & Data Usage", 
            br() 
)),
helpText(h5(
            "Copyright 2018 Alexandra Bannach-Brown", 
            br(), 

"Licensed under the Apache License, Version 2.0; you may not use this file except in compliance with the License.
Obtain a copy of the License",     a(href = "http://www.apache.org/licenses/LICENSE-2.0", "here.", target = "_blank"), 
br(), 
"Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.", 
"See the License for the specific language governing permissions and limitations under the License.", 
                                       
         br()
  ))
)
)
  #closes ui 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## define datatables based on user input
  modelValsGraph <- reactive({
        a <- removeModel[, c('depID', input$modelOptions)]
    # a <- subset(modelData, modelData[, 18:55] == input$modelOptions)
    a <- droplevels(a)
     return(a)
  })
  
  modelValsTable <- reactive({
    modelCols = colnames(removeModel)[c(18:47)]
    b <- subset(removeModel, subset = modelCols == input$modelOptions 
                #, select = modelData[,c(3:10, 12:13, 16:17)]
    )
    b <- droplevels(b)
    return(b)
  })
  
  
  drugValsGraph  <- reactive({
    b <- removeDrug[ , c('depID', input$drugOptions)]
    b <- droplevels(b)
    return(b)
    })
  
  drugValsTable <- reactive({
    drugCols = colnames(removeDrug)[c(18:111)]
    b <- subset(removeDrug, subset = drugCols == input$drugOptions 
                #, select = drugData[,c(3:10, 12:13, 16:17)]
    )
    b <- droplevels(b)
    return(b)
  })
  
  ## treemaps code
  
  output$modelPlot <- renderPlot({
    
    # wrangling the data
   
    model_gath <- gather(modelValsGraph(), model, freq, -depID)
    remModel <- which(model_gath$freq==0)
    model_gath <- model_gath[-remModel,]
    model_gath$model <- as.factor(model_gath$model)
    model_gath$freq <- as.numeric(model_gath$freq)
    model_categories$name <- as.factor(model_categories$name)
    mDep_group <- model_gath %>% group_by(model) %>% summarize(num_docs=n_distinct(depID), avg_freq=mean(freq))
    mDep_group$modelCategory <- model_categories$modelCategory[match(mDep_group$model, model_categories$name)]
    mDep_group$modelCategory <- as.factor(mDep_group$modelCategory)
    mDep_group$log_numdoc <- log(mDep_group$num_docs)
    
    ## handle clicks in plot for data filtering ??
    
    # ggplot code for the plot
    treeMapModel<- ggplot2::ggplot(mDep_group, ggplot2::aes(area = num_docs,
                                                      fill = avg_freq,
                                                      label = model,
                                                      subgroup = modelCategory))  +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "lightgrey") +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                   "white", fontface = "italic", min.size = 0) +
      geom_treemap_text(colour = "lightgray", place = "topleft", grow = T, reflow = T) +
      theme(legend.position = "bottom") +
      labs(
        title = "Frequency of Models in Depression Systematic Review Dataset",
        caption = "The area of each outcome measure is proportional to the number of documents the term appears in. 
        The colour of each model is proportional to the average frequency of the term in each document.",
        fill = "Average Frequency of Terms")
    
    treeMapModel+scale_fill_gradient(low="white", high="darkred")
  })
  
  output$drugPlot <- renderPlot({
    
    # wrangling the data
    drug_gath <- gather(drugValsGraph(), drug, freq, -depID)
    remDrug <- which(drug_gath$freq==0)
    drug_gath <- drug_gath[-remDrug,]
    drug_gath$drug <- as.factor(drug_gath$drug)
    drug_gath$freq <- as.numeric(drug_gath$freq)
    dDep_group <- drug_gath %>% group_by(drug) %>% summarize(num_docs=n_distinct(depID), avg_freq=mean(freq))
    dDep_group$drugCategory <- drug_categories$drugCategory[match(dDep_group$drug, drug_categories$name)]
    dDep_group$drugCategory <- as.factor(dDep_group$drugCategory)
    dDep_group$log_numdoc <- log(dDep_group$num_docs)
    
    ## handle clicks in plot for data filtering ??
    
    # ggplot code for the plot
    treeMapPlotDrug <- ggplot2::ggplot(dDep_group, 
                    ggplot2::aes(area = num_docs,
                                 fill = avg_freq,
                                 label = drug,
                                 subgroup = drugCategory)
    )  +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "grey") +
      geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                   "white", fontface = "italic", min.size = 0) +
      geom_treemap_text(colour = "lightyellow", place = "topleft", grow = T, reflow = T) +
      theme(legend.position = "bottom") +
      labs(
        title = "Frequency of Drugs in Depression Systematic Review Dataset",
        caption = "The area of each drug is proportional to the number of documents the term appears in. 
    The colour of each drug is proportional to the average frequency of the term in each document.",
        fill = "Average Frequency of Terms")
    
    
    print(treeMapPlotDrug+scale_fill_gradient(low= "white", high= "blue"))
    
  })
  
  
  ##clicks
  # output$modelclick <- renderPrint({
  #   d <- event_data("plotly_click")
  #   if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })
  # 
  # output$drugclick <- renderPrint({
  #   d <- event_data("plotly_click")
  #   if (is.null(d)) "Click events appear here (double-click to clear)" else d
  # })
  
  
  ## Table Views
  output$xModel <- DT::renderDataTable({
    #table displays what user has selected in sidebar
    modelTabData <- modelValsTable()
    datatable(
      modelTabData[,c(3:10, 12, 17)], extensions = 'FixedHeader', 
      options = list(pageLength = 10, fixedHeader = TRUE, orderClasses = TRUE,  
                     dom='Brtip'),  
      rownames = FALSE
    )
    
  })
  
  observe({
  output$xDrug <- renderDataTable({
    #table displays what user has selected in sidebar
  drugTabData <- drugValsTable()
    datatable(
      drugTabData[,c(3:10, 12, 17)]
      , extensions = 'FixedHeader', 
      options = list(pageLength = 10, fixedHeader = TRUE, orderClasses = TRUE
                     ,  dom='Brtip'
                     # , buttons ? 
                     ), 
      rownames = FALSE
      
    )
    
    
    
  })
  })
  
  
  output$xAllData <- DT::renderDataTable({
    #table displays what user has selected in sidebar
        datatable(
      All_Studies_18409[,c(3:10, 12, 17)], extensions = 'FixedHeader', 
      options = list(pageLength = 10, fixedHeader = TRUE, orderClasses = TRUE,  
                     dom='Brtip'),  
      rownames = FALSE
    )
  })
  
  
  #creating dataframe for tsv -> syrf download
  # output dataframes for filtered downloads
  

 # print(modelDownload)
  
  # download the filtered data
  output$xDownModel <- downloadHandler(    filename = function() {
    paste(
      "model-filtered_", Sys.Date(), ".txt", sep = ""
    )
  }, 
  content = function(file) {
    a <- modelValsTable() 
    print(a)
    
    removeNewline <- function(text){
      print("-- Start to remove \r \n \f \t")
      text <- gsub("\r|\n|\f|\t|(NULL)", " ", text)
      return(text)
    }
    
    modelDownload <- a %>%
      mutate(
        title = removeNewline(Title),
        surname = removeNewline(Author),
        firstname = "",
        csurname = "",
        cfirstname = "",
        cauthororder = "",
        publicationName = removeNewline(Journal),
        doi = "",
        url = URL,
        abstract = removeNewline(Abstract),
        keywords = "",
        URL = "",
        authorAddress = "",
        referenceType = "",
        pdfPath = pdfLink
      ) %>%
      select(
        title = title,
        firstname = firstname,
        surname = surname,
        csurname = csurname,
        cfirstname = cfirstname,
        cauthororder = cauthororder,
        publicationName = publicationName,
        alternateName = Journal,
        abstract = abstract,
        URL = URL,
        authorAddress = authorAddress,
        year = Year,
        DOI = doi,
        referenceType = referenceType,
        pdfPath = pdfPath,
        keywords = keywords
      )
    names(modelDownload) <- c( "Title",
                            "First Author First Name",
                            "First Author Surname",
                            "Corresponding Author First Name",
                            "Corresponding Author Surname",
                            "Corresponding Author Order",
                            "Publication Name",
                            "Alternate Name",
                            "Abstract",
                            "Url",
                            "Author Address",
                            "Year",
                            "DOI",
                            "Reference Type",
                            "PDF Relative Path",
                            "Keywords"
    )
       write.table(modelDownload,file, quote=FALSE, row.names = FALSE, sep="\t", na='' )
     }
  )
  
 # outputOptions(output, 'xDownModel', suspendWhenHidden=FALSE)
  
   output$xDownDrug = downloadHandler(
  filename = function() {
    paste(
      "drug-filtered_", Sys.Date(), ".txt", sep = ""
    )
  }, 
  content = function(file) {
    a <- drugValsTable() 
    summary(a)
    
    removeNewline <- function(text){
      print("-- Start to remove \r \n \f \t")
      text <- gsub("\r|\n|\f|\t|(NULL)", " ", text)
      return(text)
    }
    
    drugDownload <- a %>%
      mutate(
        title = removeNewline(Title),
        surname = removeNewline(Author),
        firstname = "",
        csurname = "",
        cfirstname = "",
        cauthororder = "",
        publicationName = removeNewline(Journal),
        doi = "",
        url = URL,
        abstract = removeNewline(Abstract),
        keywords = "",
        URL = "",
        authorAddress = "",
        referenceType = "",
        pdfPath = pdfLink
      ) %>%
      select(
        title = title,
        firstname = firstname,
        surname = surname,
        csurname = csurname,
        cfirstname = cfirstname,
        cauthororder = cauthororder,
        publicationName = publicationName,
        alternateName = Journal,
        abstract = abstract,
        URL = URL,
        authorAddress = authorAddress,
        year = Year,
        DOI = doi,
        referenceType = referenceType,
        pdfPath = pdfPath,
        keywords = keywords
      )
    names(drugDownload) <- c( "Title",
                               "First Author First Name",
                               "First Author Surname",
                               "Corresponding Author First Name",
                               "Corresponding Author Surname",
                               "Corresponding Author Order",
                               "Publication Name",
                               "Alternate Name",
                               "Abstract",
                               "Url",
                               "Author Address",
                               "Year",
                               "DOI",
                               "Reference Type",
                               "PDF Relative Path",
                               "Keywords"
    )
    write.table(drugDownload,file, quote=FALSE, row.names = FALSE, sep="\t", na='' )
  }
  )
  
  output$xDownAll = downloadHandler(filename = 
                                        function() {
                                          paste(
                                            "All_Depression_Data", ".txt", sep = ""
                                          ) 
                                        }
                                    , 
                                    content = function(file) {
                                      write.table(All_Studies_18409, file, quote=FALSE,   
                                                  #            col.names = c("Title", "First Author First Name",
                                                  # "First Author Surname", "Corresponding Author First Name", "Corresponding Author Surname",
                                                  # "Corresponding Author Order",	"Publication Name", "Alternative Name", "Abstract",
                                                  # "Url", "Author Address",	"Year", "DOI", "Reference Type", "PDF Relative Path", "Keywords"),
                                                  row.names = FALSE,  sep="\t", na='')
                                      }
  )
   outputOptions(output, 'xDownAll', suspendWhenHidden=FALSE)
  
    
}



# Run the application 
shinyApp(ui = ui, server = server)

