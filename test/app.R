dropdownMenu(type = "task",
             messageItem(
               from = "Download",
               message = "Reports",
               icon = icon("gear")
             )

)


require(shiny)
require(visNetwork)

server <- function(input, output) {
  output$network_proxy_nodes <- renderVisNetwork({
    # minimal example
    nodes <- data.frame(id = 1:3)
    edges <- data.frame(from = c(1,2), to = c(1,3))

    visNetwork(nodes, edges) %>% visNodes(color = "blue")
  })


  observe({
    visNetworkProxy("network_proxy_nodes") %>%
      visFocus(id = input$Focus, scale = 2)
  })

  observe({
    visNetworkProxy("network_proxy_nodes") %>%
      visNodes(color = input$color)
  })

}

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      selectInput("color", "Color :",
                  c("blue", "red", "green")),
      selectInput("Focus", "Focus on node :",
                  c(1:3))
    ),
    column(
      width = 8,
      visNetworkOutput("network_proxy_nodes", height = "400px")
    )
  )
)

shinyApp(ui = ui, server = server)

########################################################################################


nodes <- data.frame(id = 1:3, label = c("A", "B", "A"))
edges <- data.frame(from = c(1,2), to = c(1,3))

require(visNetwork)
require(shiny)
require(shinydashboard)
ui <- dashboardPage(skin = "black",
                    dashboardHeader(),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Network", tabName = "network", icon = icon("dashboard")),
                        sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search...")
                      )
                    ),
                    dashboardBody(
                      box(
                        title = "Network",  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                        visNetworkOutput("network_proxy", height = 700)
                      )
                    )
)


server <- function(input, output, session) {
  output$network_proxy <- renderVisNetwork({
    visNetwork(nodes, edges, height = "100%")
  })

  observe({
    if(input$searchButton > 0){
      isolate({
        print(input$searchText)
        current_node <- nodes[grep(input$searchText, nodes$label), "id"]
        print(current_node)
        visNetworkProxy("network_proxy") %>% visSelectNodes(id  = current_node)
      })
    }
  })

} #end server

shiny::shinyApp(uit, servert)








uit <- fluidPage(
  p("The first checkbox group controls the second"),
  checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                     c("Item A", "Item B", "Item C")),
  checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
                     c("Item A", "Item B", "Item C"))
)

servert <- function(input, output, session) {
  observe({
    x <- input$inCheckboxGroup

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

    # Can also set the label and select items
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = x,
                             selected = x
    )
  })
}




