fluidPage(
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
    )
  )


shinyApp(ui, server)
nchar=8
map_f<-map_obj(nchar)
colnames(dt_out)<-c("id", "statemet", "selected_annot", "grep_id", "grep_id_name")
shiny_in=list(c1=c1, c2=c2, c3=c3, c4=c4, c5=c5)
shiny_in$c1
n=17

shiny_in$c5[[shiny_in$c1[n]]]
write.csv(file="shiny_test.csv", unlist(list("HAO:1"="dad", "HAO:2"="aa", "HAO:3"="as", "HAO:4"="ad")))
###########################################################################
ui <- dashboardPage(
  #fluidPage(
  #sidebarLayout(
  #sidebarPanel(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(width=400,

      hr(), fluidRow(column(10,
                                      textOutput("char"),
                                      verbatimTextOutput("txt"),
                                      textInput("text4", label = "Your IDs", value = ""),
                                      checkboxGroupInput("terms", "",
                                                         choices =list("HAO:1"="dad", "HAO:2"="aa", "HAO:3"="as", "HAO:4"="ad")))),
                hr(),
                actionButton("save_btn", label = "Save"),

                uiOutput("WidgetVectorDisplay"),
                verbatimTextOutput("show_last"),

                textInput("text3", label = h3("Text input"), value = "Enter text..."),
                hr(),
                fluidRow(column(3, verbatimTextOutput("value3"))),
                hr(),
                fluidRow(column(3, verbatimTextOutput("value4")))


),
#mainPanel()\
dashboardBody(

tags$head(tags$style(HTML('
.main-header {
  position: fixed;
  width:100%;
}

.sidebar {
                      height: 90vh; overflow-y: auto;
                    }

'))),

  visNetworkOutput("network"))
)
#)


server <- function(input, output, session) {
  #output$value3 <- renderPrint({ input$text3 })
  ###########
  output$network <- renderVisNetwork({
    # minimal example
    nodes <- data.frame(id = 1:3)
    edges <- data.frame(from = c(1,2), to = c(1,3))

    visNetwork(nodes, edges)
  })


  #########
  output$txt <- renderText({
    #terms <- paste(input$terms, collapse = ", ")
    terms1 <- paste(input$terms, collapse = ", ")
    paste("Selected IDs:", terms1)
  })
  output$char <- renderText({"CHAR1: Shape of whatever"})
  output$stat1 <- renderText({"STAT1: Shape of whatever"})
  ### write csv
  observe({
    input$save_btn
    write.csv(file="shiny_test.csv", unlist(input$checkbox1))

  })

  ####
  makeRadioButton=function(n=1){fluidRow(column(10,
                                                h2(paste(shiny_in$c1[n], shiny_in$c2[n], sep=" ")),
                                                verbatimTextOutput(paste0("ids_selec", n)),
                                                textInput(paste0("ids_in", n), label = "Your IDs", value = ""),
                                                checkboxGroupInput(paste0("checkbox",n),label=NA, choices=shiny_in$c5[[shiny_in$c1[n]]]),
                                                hr()
                                                #shiny_in$c5

  ))}


  output$WidgetVectorDisplay <-renderUI({lapply(X = 1:nchar, FUN = makeRadioButton)})
  ####
  values <- reactiveValues(
    lastUpdated = NULL
  )
  observe({
    #lapply(names(input)[1:3], function(x) {
    lapply(map_f, function(x) {
      observe({
        input[[x]]
        #values$lastUpdated <- x

        #output$show_last <- renderPrint({
        output[[names(map_f)[which(map_f==x)]]] <- renderText({

          paste("Selected IDs:", paste(input[[x]], collapse = ", "))

          # paste(input[[x]], sep=", ")
          #names(input)[1:3]
        })

      })
    })
  })

}

shinyApp(ui, server)
