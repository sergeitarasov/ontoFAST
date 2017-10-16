ui <- dashboardPage(

  dashboardHeader(title = "OntoFAST",
                  tags$li(a(actionButton("savefile_btn", label="Save file", icon=icon("save")),
                            style = "padding-top:5px; padding-bottom:0px;"),
                          class = "dropdown")
  ),

  dashboardSidebar(width="400px",
                   h2("Character Statements:", style='padding-left: 12px;'),

                   hr(),

                   uiOutput("WidgetVectorDisplay")



  ),




  dashboardBody(

    tags$head(tags$style(HTML('
                              .main-header {
                              # position: fixed;
                              width:100%;
                              }

                              .sidebar {
                              # height: 90vh; overflow-y: auto;
                              height: 95vh; overflow-y: auto; overflow-x: auto;
                              }

                              .content {
                              height: 95vh; overflow-y: hidden; overflow-x: scroll;
                              }

                              .hyphenate {
                              -ms-word-break: break-all;
                              word-break: break-all;


                              word-break: break-word;

                              -webkit-hyphens: auto;
                              -moz-hyphens: auto;
                              hyphens: auto;
                              }

                              .box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none; border-style: none;}



                              '))),




    fluidRow(width = "auto",
             box(width = 5, height = 190, title = NULL,

                 fluidRow(box(style="padding:0px; -webkit-border-style: none; -moz-border-style: none; -ms-border-style: none;
                              border-style: none; -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none; border-style: none;",
                              radioButtons("des_chk", label="Show upon expansion",  inline = T,
                                           choices =list("descendants", "ancestors", "both"),
                                           selected =list("descendants")
                              )),
                          box(style='padding:0px;',
                              radioButtons("links_chk", label="Links to show",  inline = T,
                                           choices =list("part_of", "is_a", "both"),
                                           selected =list("both")
                              )
                          )),

                 selectizeInput("selectize", label = NULL, choices=NULL, selected = FALSE, multiple = FALSE, width = "auto",
                                options = list(openOnFocus=F, maxOptions=100, placeholder="Enter term or ID"
                                )),
                 actionButton("select_descen", label = "Expand", icon = icon("glyphicon glyphicon-fullscreen", lib="glyphicon"))
             ),


             box(width = 3, height = 190, title = NULL,
                 h6("ID:"),
                 verbatimTextOutput("id_txt", placeholder = T),

                 h6("Synonyms:"),
                 verbatimTextOutput("syn_txt", placeholder = T),
                 tags$head(tags$style("#syn_txt{overflow-y:scroll; height: 60px;}"))

                 # tags$head(tags$style("#def_txt{color:red; font-size:12px; font-style:italic;
                 #  overflow-y:scroll; max-height: 50px; background: ghostwhite;}"))

             ),

             box(width = 4, height = 190, title = NULL,
                 h6("Definition:"),
                 verbatimTextOutput("def_txt", placeholder = T),
                 tags$head(tags$style("#def_txt{overflow-y:scroll; height: 130px; hyphens: auto; word-break: break-word; -webkit-hyphens: manual;}"))

             )







    ),

    fluidRow(width = "100%",

             box(width = "100%", height=NULL, title = NULL,

                 visNetworkOutput("network", width = "100%", height = "65vh")
             )
    )
    )
    )

