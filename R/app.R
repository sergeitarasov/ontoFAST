#' @title Run ontoFAST interactively
#' @description This function runs ontoFAST in interactive mode. Interactive mode allows character annotation and navigation through ontology network.
#' @param is_a term for is_a relationships
#' @param part_of term for part_of relationships
#' @param nchar number of characters to show
#' @param show.chars shows character statements
#' @param shiny_in a name of ontology (shiny_in) object in "ontofast" enviroment
#' @param file2save a name of the file to which shiny_in object is saved in interactive mode
#' @param ... other arguments
#' @return runs ontoFAST interactively using Shiny.
#' @examples
#' data(Sharkey_2011)
#' data(HAO)
#' hao_obo<-onto_process(HAO, Sharkey_2011[,1], do.annot = FALSE)
#' ontofast <- new.env(parent = emptyenv())
#' ontofast$shiny_in <- make_shiny_in(hao_obo)
#' runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )
#' @export

runOntoFast <- function(is_a=c("is_a"), part_of=c("BFO:0000050"), nchar="all", show.chars=TRUE, shiny_in="shiny_in", file2save="OntoFAST_shiny_in.RData",  ...){


  # nchar to display
  if (nchar=="all"){
    nchar=length(ontofast[[shiny_in]]$id_characters)
  }



  ##### necessary variables
  #create mapping for reative objects
  map_btn_check<-map_obj("add_btn", nchar)
  map_checkbox<-map_obj("checkbox", nchar)
 # map_ids<-map_obj("ids_in", nchar)

  # map for links_chk; first element in the map part of; 2nd is_a
  links_chk_map<-list(part_of=c(part_of, ""), is_a=c("", is_a), both=c(part_of, is_a))
  links_chk_map$part_of[2]



##############
    ui <- dashboardPage(

  dashboardHeader(title = "ontoFAST",
                  tags$li(a(actionButton("savefile_btn", label="Save file", icon=icon("save")),
                            style = "padding-top:5px; padding-bottom:0px;"),
                          class = "dropdown")
  ),

  dashboardSidebar(width="20vw",
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
                              height: 95vh; overflow-y: hidden; overflow-x: scroll; background-color: white;
                              }

                              .hyphenate {
                              -ms-word-break: break-all;
                              word-break: break-all;


                              word-break: break-word;

                              -webkit-hyphens: auto;
                              -moz-hyphens: auto;
                              hyphens: auto;
                              }

                              .box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none; border-style: none; }




                              '))),


#fluidRow(column(width = 12,

    fluidRow(width = "auto", style='padding:0px;', #I dont know if padding is applicable to fluid row


             box(width = 5, height = "auto", title = NULL, style="padding:0px;",

                 fluidRow(box(style="padding:0px;",

                              selectInput("des_chk", "Show upon rendering",
                                          choices =list("descendants", "ancestors", "both"),
                                          selected =list("descendants")
                              )

                              # radioButtons("des_chk", label="Show upon rendering",  inline = TRUE,
                              #              choices =list("descendants", "ancestors", "both"),
                              #              selected =list("descendants")
                              # )

                              ),
                          box(style='padding:0px;',

                              selectInput("links_chk", label="Links to show",
                                          choices =list("part_of", "is_a", "both"),
                                          selected =list("both")
                              )

                              # radioButtons("links_chk", label="Links to show",  inline = TRUE,
                              #              choices =list("part_of", "is_a", "both"),
                              #              selected =list("both")
                              # )
                          )),



                 selectizeInput("selectize", label = NULL, choices=NULL, selected = FALSE, multiple = FALSE, width = "auto",
                                options = list(openOnFocus=FALSE, maxOptions=100, placeholder="Enter term or ID"
                                )),
                 actionButton("select_descen", label = "Visualize", icon = icon("glyphicon glyphicon-fullscreen", lib="glyphicon"))




                 ), #box


             box(width = 3, height = "auto", title = NULL, style='padding:0px;',
                 h5("ID:", style='padding:0px;'),
                 verbatimTextOutput("id_txt", placeholder = TRUE),
                 tags$head(tags$style("#id_txt{font-family: Arial; padding:0px;}")),

                 h5("Synonyms:", style='padding:0px;'),
                 verbatimTextOutput("syn_txt", placeholder = TRUE),
                 tags$head(tags$style("#syn_txt{overflow-y:scroll; height: 5vh; font-family: Arial; padding:0px;
                                      hyphens: auto; word-break: break-word;-webkit-hyphens: manual;}"))



             ),

             box(width = 4, height = "auto", title = NULL, style='padding:0px;',
                 h5("Definition:", style='padding:0px;'),
                 verbatimTextOutput("def_txt", placeholder = TRUE),
                 tags$head(tags$style("#def_txt{overflow-y:scroll; height: 11vh; white-space: pre-wrap; word-break: keep-all; font-family: Arial; padding:0px;}"))
                 # tags$head(tags$style("#def_txt{overflow-y:scroll; overflow-x:visible; height: 11vh; word-break: break-all;
                 #                      font-family: Arial; hyphens: auto;  -webkit-hyphens: manual; padding:0px;}"))

                 # ui <- fluidPage(
                 #   tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                 #   textOutput("text")
                 # )

             )







    ),
hr(),

    fluidRow(width = "100%",

             box(width = "100%", height="auto", title = NULL, style='padding:0px;',

                 visNetworkOutput("network", width = "100%", height = "65vh")
             )
    )

 #))#end col, fluid row
    ) # end body
    ) # end ui

########################################################################################################################
server <- function(input, output, session) {

  session$onSessionEnded(stopApp)

  ########### Network
  output$network <- renderVisNetwork({

    withProgress(message = "Creating Network", value = 0.9, {
      # minimal example
      nodes <- data.frame(id = 1:3)
      edges <- data.frame(from = c(1,2,3), to = c(2,3,1))

      visNetwork(nodes, edges, width = "100%", height = "100%", main = "Ontology Network",
                 submain = "Select terms or IDs above to begin visualization") %>%
        visNodes( size=30, shadow =TRUE)
    })
  })


  ##### Selectize

  updateSelectizeInput(session, "selectize", label = NULL, choices=ontofast[[shiny_in]]$srch_items, selected = FALSE,
                       options = list(openOnFocus=FALSE, maxOptions=100, placeholder="Search Ontology term"
                       ),
                       server = TRUE
  )

  ####
  ####### Save file
  observeEvent(input$savefile_btn, {

#print(session$userData$ontofast[[shiny_in]])
    #save(ontofast[[shiny_in]], file= "OntoFAST_annotation_shiny_in.RData"  ) #"OntoFAST_annotation_shiny_in.RData" file2save
    saveRDS(ontofast[[shiny_in]], file= file2save)
  }
  )


  ############
  makeRadioButton=function(n=1){fluidRow(column(10,
                                                h3(paste(ontofast[[shiny_in]]$id_characters[n], ontofast[[shiny_in]]$name_characters[n], sep=" "), style='padding-left: 12px;'),
                                                #verbatimTextOutput(paste0("ids_selec", n)),
                                                textInput(paste0("ids_in", n), label = "", value = "", placeholder="Enter your ID"),
                                                actionButton(paste0("add_btn", n), label = "Add",
                                                             icon = icon("glyphicon glyphicon-download", lib="glyphicon")),




                                                checkboxGroupInput(paste0("checkbox",n),label=NULL, #label=NA,
                                                                   choices=ontofast[[shiny_in]]$auto_annot_characters_id_name[[ontofast[[shiny_in]]$id_characters[n]]],
                                                                   selected=ontofast[[shiny_in]]$terms_selected[[ontofast[[shiny_in]]$id_characters[n]]]),



                                                hr()


  ))}


  output$WidgetVectorDisplay <-renderUI(
    if (show.chars==TRUE){
    withProgress(message = "Creating character statements", value = 0.1, {
      incProgress(0.3)
      incProgress(0.9)
      {lapply(X = 1:nchar, FUN = makeRadioButton)}
    })
    } else { h3("The characters are disabled", style='padding-left: 12px;') }

  )

  ### Show descnedants upon button
  observeEvent(
    input$select_descen,
    {
      term2show<-input$selectize

      output$network <- renderVisNetwork({
        withProgress(message = "Creating Network", value = 0.1, {

          #dt=get_part_descen(ontofast[[shiny_in]], "HAO:0000013", is_a=c("is_a"), part_of=c("BFO:0000050"))

          if (input$des_chk=="descendants"){
            dt=get_part_descen(ontofast[[shiny_in]], term2show, is_a=links_chk_map[[input$links_chk]][2], #### HAO.obo to ontology index!!!!
                               part_of=links_chk_map[[input$links_chk]][1],
                               all_links=FALSE, incl.top.anc=TRUE, highliht_focus=TRUE)

          }

          if (input$des_chk=="ancestors"){

            dt=get_part_anc(ontofast[[shiny_in]], term2show, is_a=links_chk_map[[input$links_chk]][2],
                            part_of=links_chk_map[[input$links_chk]][1],
                            all_links=FALSE, incl.top.anc=TRUE, highliht_focus=TRUE)

          }

          if (input$des_chk=="both"){ #WORK on duplicated terms
            dt1=get_part_descen(ontofast[[shiny_in]], term2show, is_a=links_chk_map[[input$links_chk]][2], #### HAO.obo to ontology index!!!!
                                part_of=links_chk_map[[input$links_chk]][1],
                                all_links=FALSE, incl.top.anc=TRUE, highliht_focus=TRUE)

            dt2=get_part_anc(ontofast[[shiny_in]], term2show, is_a=links_chk_map[[input$links_chk]][2],
                             part_of=links_chk_map[[input$links_chk]][1],
                             all_links=FALSE, incl.top.anc=TRUE, highliht_focus=TRUE)
            dt<-list(nodes=rbind(dt1$nodes, dt2$nodes), edges=rbind(dt1$edges, dt2$edges))
            dt$nodes<-dt$nodes[!duplicated(as.character(dt$nodes$id)),] #select unique nodes


          }

          incProgress(0.3)

          net_title<-NULL
          net_submain<-NULL


          if (is.null(dt$nodes)){ # if dt is empty show Warnings
            # minimal example
            dt<-list(nodes=data.frame(id = 1:2), edges=data.frame(from = c(1,2), to = c(2,1)))
            net_title<-"Warning: the selected term or link have no relationships to display"
            net_submain<-"Select another term and/or links"
          }




          ## Legend data

          lnodes <- data.frame(label = c("Selected term"),
                               color = c("orange"),
                               id = term2show)

          ledges <- data.frame(color = c("blue", "red"),
                               label = c("part_of", "is_a"), arrows =c("from", "from"))

          ####

          visNetwork(dt$nodes, dt$edges, height = "65vh",  width ="100%", main =net_title, submain =net_submain) %>%
            visNodes(borderWidthSelected=4)%>%
            visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
            visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, position = "right", width=0.09) %>%

            #visLayout(randomSeed = 12) %>%
            visLayout(randomSeed = 12, hierarchical=FALSE) -> visNt # HIERRACHICAL TRUE can be an option!!!
          incProgress(0.9)

          visIgraphLayout(visNt, layout="layout_with_gem")





        })


      })



      # visNetworkProxy("network") %>%
      #    visFocus(id = input$selectize, scale = 2)



    }
  )
  #######

########
  ####### Add button actions
  observe({
    lapply(map_btn_check, function(x) {
      observeEvent(
        input[[x]],
        {
          term_id<-input[[paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)])]]

          #print(term_id)


          ## Input from Selectize

          if ((term_id=="") & (input$selectize!="")){ #if term field is empty then use Selectize input that should not be empty too
            ############## update checkboxes and ontology index



            term_id<-input$selectize

            term_name<-ontofast[[shiny_in]]$name[which(names(ontofast[[shiny_in]]$name)==term_id)]

            if (length(term_name)==0){ #check if term is found in ontology
              term_name="TERM NOT FOUND!!!"
            }

            term_id_name<-paste(term_name, term_id, sep=", ")
            CHAR_id<-paste0("CHAR:", names(map_btn_check)[which(map_btn_check==x)])


            if (term_id_name %in% ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]]){#check for duplication
              updateTextInput(session, paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)]),
                              label = "You are trying to add the same term twice")
            }

            if (!term_id_name %in% ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]]){#check for duplication


              ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
              ### Former global assign. <<-                                               ####
              #update terms selected
              ontofast[[shiny_in]]$terms_selected[[CHAR_id]] <- c(ontofast[[shiny_in]]$terms_selected[[CHAR_id]], term_id_name) # <<-

              #update terms selected id

              #ontofast[[shiny_in]]$terms_selected_id[[CHAR_id]] <<- c(ontofast[[shiny_in]]$terms_selected_id[[CHAR_id]], term_id)

              #print(ontofast[[shiny_in]]$terms_selected_id[[CHAR_id]])

              ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
              ### Former global assign. <<-                                               ####
              #update terms all
              ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]] <- c(ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]], term_id_name) # <<-

              #update checkbox
              updateCheckboxGroupInput(session, paste0("checkbox", names(map_btn_check)[which(map_btn_check==x)]),
                                       label=NULL, choices=ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]],
                                       selected=ontofast[[shiny_in]]$terms_selected[[CHAR_id]]
              ) #label=NA

              withProgress(message = "Added", value = 1, { Sys.sleep(.1) })

              updateTextInput(session, paste0("ids_in", isolate(names(map_btn_check)[which(map_btn_check==x)])), value = "",  label = "")




              term_id<-c("")
            }


            ###########
          }


          ### Manual input
          ############## update checkboxes and ontology index

          if (term_id!=""){ # term fiels must be non-empty
            term_id<-gsub(" ", "", term_id) # remove white spaces

            term_name<-ontofast[[shiny_in]]$name[which(names(ontofast[[shiny_in]]$name)==term_id)]

            if (length(term_name)==0){ #check if term is found in ontology
              term_name="TERM NOT FOUND!!!"
            }

            term_id_name<-paste(term_name, term_id, sep=", ")
            CHAR_id<-paste0("CHAR:", names(map_btn_check)[which(map_btn_check==x)])


            if (term_id_name %in% ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]]){#check for duplication
              updateTextInput(session, paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)]),
                              label = "You are trying to add the same term twice")
            }

            if (!term_id_name %in% ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]]){#check for duplication

              ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
              ### Former global assign. <<-                                               ####
              #update terms selected
              ontofast[[shiny_in]]$terms_selected[[CHAR_id]] <- c(ontofast[[shiny_in]]$terms_selected[[CHAR_id]], term_id_name) # <<-

              #update terms selected id
             # ontofast[[shiny_in]]$terms_selected_id[[CHAR_id]] <<- c(ontofast[[shiny_in]]$terms_selected_id[[CHAR_id]], term_id)

              ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
              ### Former global assign. <<-                                               ####
              #update terms all
              ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]] <- c(ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]], term_id_name) # <<-

              #update checkbox
              updateCheckboxGroupInput(session, paste0("checkbox", names(map_btn_check)[which(map_btn_check==x)]),
                                       label=NA, choices=ontofast[[shiny_in]]$auto_annot_characters_id_name[[CHAR_id]],
                                       selected=ontofast[[shiny_in]]$terms_selected[[CHAR_id]]
              )

              withProgress(message = "Added", value = 1, { Sys.sleep(.1) })

              updateTextInput(session, paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)]), value = "",  label = "")
            }

          }
          ###########


        }
      )
    })
  })
  ####
  ###### Add checkbox actions
  observe({
    lapply(map_checkbox, function(x) {
      observeEvent(
        input[[x]], ignoreNULL = FALSE, # rhe input is the vector of selected terms
        {
          #print("observed")
          CHAR_id<-paste0("CHAR:", names(map_checkbox)[which(map_checkbox==x)])

          ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
          ### Former global assign. <<-                                               ####
          #update terms selected
          ontofast[[shiny_in]]$terms_selected[[CHAR_id]] <- input[[x]] # <<-
          #print(input[[x]])

          #update terms selected id
          #print(names(ontofast[[shiny_in]]$terms_map[ontofast[[shiny_in]]$terms_map %in%input[[x]]]))

          ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
          ### Former global assign. <<-                                               ####
          # TERMS NOT FOUND are not included here
          ontofast[[shiny_in]]$terms_selected_id[[CHAR_id]] <- names(ontofast[[shiny_in]]$terms_map[ontofast[[shiny_in]]$terms_map %in%input[[x]]]) # <<-




        })
    })
  })
  ########
  ################ Node selection
  observeEvent(
    input$network_selected, {
      #print(input$network_selected)

      updateSelectizeInput(session, "selectize", label = NULL, choices=ontofast[[shiny_in]]$srch_items, selected = input$network_selected,
                           options = list(openOnFocus=FALSE, maxOptions=100, placeholder="Enter term or ID"
                           ),
                           server = TRUE
      )

    })

  ########### Selectize change
  observeEvent(
    input$selectize,
    { #print(input$selectize)
      output$id_txt<-renderText({input$selectize})

      output$def_txt<-renderText({
        ontofast[[shiny_in]]$def[which(names(ontofast[[shiny_in]]$def)==input$selectize)]})

      output$syn_txt<-renderText({
        paste(
          ontofast[[shiny_in]]$parsed_synonyms[which(names(ontofast[[shiny_in]]$parsed_synonyms)==input$selectize)],
          collapse = ", ")
        })
    })

  #####

}

###############################################################################################################################
 # end of app list

#######
  if(interactive()){
    runApp(shinyApp(ui = ui, server = server))
    return(ontofast[[shiny_in]])
    #shinyApp(ui = ui, server = server)
    #shiny::runApp(sh)
  } else {
    warning("Run R interactively!")
  }

}
