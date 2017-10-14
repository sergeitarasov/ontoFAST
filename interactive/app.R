##### necessary variables
# define terms for the links
is_a=c("is_a")
part_of=c("BFO:0000050")

# map for links_chk; first element in the map part of; 2nd is_a
links_chk_map<-list(part_of=c(part_of, ""), is_a=c("", is_a), both=c(part_of, is_a))
links_chk_map$part_of[2]
nchar=300 # number of chars to show

#create mapping for reative objects
map_btn_check<-map_obj("add_btn", nchar)
map_checkbox<-map_obj("checkbox", nchar)
#create shiny_in objetc
shiny_in=list(c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=ontology$name, terms_selected=list())

# cretae serch terms object; it has to beleted later
srch_items<-names(ontology$name)
names(srch_items)<-unname(ontology$name)
####


####

shinyApp(ui, server)
# str(shiny_in)
# summary(ontology)
#colnames(dt_out)<-c("id", "statemet", "selected_annot", "grep_id", "grep_id_name")



###########################################################################
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



#shinyApp(ui, server)
############################################################################################################################

server <- function(input, output, session) {

########### Network
  output$network <- renderVisNetwork({

    withProgress(message = "Creating Network", value = 0.9, {
    # minimal example
    nodes <- data.frame(id = 1:3)
    edges <- data.frame(from = c(1,2,3), to = c(2,3,1))

    visNetwork(nodes, edges, width = "100%", height = "100%", main = "Ontology Network",
               submain = "Select terms or IDs above to begin visualization") %>%
    visNodes( size=30, shadow =T)
  })
  })


##### Selectize

  updateSelectizeInput(session, "selectize", label = NULL, choices=srch_items, selected = FALSE,
                 options = list(openOnFocus=F, maxOptions=100, placeholder="Search Ontology term or ID and click Expand to visualize"
                 ),
                                server = TRUE
                 )

####
####### Safe file
  observeEvent(input$savefile_btn, {
    print("sdfds")

      save(shiny_in, file="OntoFAST_annotation_shiny_in.RData")
    }
  )


############
  makeRadioButton=function(n=1){fluidRow(column(10,
                                                h3(paste(shiny_in$c1[n], shiny_in$c2[n], sep=" "), style='padding-left: 12px;'),
                                                #verbatimTextOutput(paste0("ids_selec", n)),
                                                textInput(paste0("ids_in", n), label = "", value = "", placeholder="Enter your ID"),
                                                actionButton(paste0("add_btn", n), label = "Add",
                                                             icon = icon("glyphicon glyphicon-download", lib="glyphicon")),
                                                checkboxGroupInput(paste0("checkbox",n),label=NA, choices=shiny_in$c5[[shiny_in$c1[n]]],
                                                                   selected=shiny_in$terms_selected[[shiny_in$c1[n]]]),



                                                hr()


  ))}


  output$WidgetVectorDisplay <-renderUI(
    withProgress(message = "Creating character statements", value = 0.1, {
      incProgress(0.3)
      incProgress(0.9)
      {lapply(X = 1:nchar, FUN = makeRadioButton)}
    })

                                        )

  ### Show descnedants upon button
  observeEvent(
    input$select_descen,
    {
      term2show<-input$selectize

      output$network <- renderVisNetwork({
        withProgress(message = "Creating Network", value = 0.1, {

          #dt=get_part_descen(hao.obo, term2show, is_a=c("is_a"), part_of=c("BFO:0000050"))

          if (input$des_chk=="descendants"){
            dt=get_part_descen(hao.obo, term2show, is_a=links_chk_map[[input$links_chk]][2], #### HAO.obo to ontology index!!!!
                               part_of=links_chk_map[[input$links_chk]][1],
                               all_links=F, incl.top.anc=T, highliht_focus=T)

          }

          if (input$des_chk=="ancestors"){

            dt=get_part_anc(hao.obo, term2show, is_a=links_chk_map[[input$links_chk]][2],
                            part_of=links_chk_map[[input$links_chk]][1],
                            all_links=F, incl.top.anc=T, highliht_focus=T)

          }

          if (input$des_chk=="both"){ #WORK on duplicated terms
            dt1=get_part_descen(hao.obo, term2show, is_a=links_chk_map[[input$links_chk]][2], #### HAO.obo to ontology index!!!!
                               part_of=links_chk_map[[input$links_chk]][1],
                               all_links=F, incl.top.anc=T, highliht_focus=T)

            dt2=get_part_anc(hao.obo, term2show, is_a=links_chk_map[[input$links_chk]][2],
                            part_of=links_chk_map[[input$links_chk]][1],
                            all_links=F, incl.top.anc=T, highliht_focus=T)
            dt<-list(nodes=rbind(dt1$nodes, dt2$nodes), edges=rbind(dt1$edges, dt2$edges))
            dt$nodes<-dt$nodes[!duplicated(as.character(dt$nodes$id)),] #select unique nodes


          }

          incProgress(0.3)

          ## Legend data

           lnodes <- data.frame(label = c("Selected term"),
                                color = c("orange"),
                                id = term2show)

          ledges <- data.frame(color = c("blue", "red"),
                             label = c("part_of", "is_a"), arrows =c("from", "from"))

          ####

          visNetwork(dt$nodes, dt$edges, height = "65vh",  width ="100%", main = NULL, submain =NULL) %>%
          visNodes(borderWidthSelected=4)%>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = T)%>%
          visLegend(addEdges = ledges, addNodes = lnodes, useGroups = F, position = "right", width=0.09) %>%

          #visLayout(randomSeed = 12) %>%
          visLayout(randomSeed = 12, hierarchical=F) -> visNt # HIERRACHICAL TRUE can be an option!!!
          incProgress(0.9)

          visIgraphLayout(visNt, layout="layout_with_gem")


      })


      })



      # visNetworkProxy("network") %>%
      #    visFocus(id = input$selectize, scale = 2)



    }
    )
  #######

  ####### Add button actions
  observe({
    lapply(map_btn_check, function(x) {
      observeEvent(
        input[[x]],
        {
          term_id<-input[[paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)])]]

          print(term_id)


          ## Input from Selectize

          if ((term_id=="") & (input$selectize!="")){ #if term field is empty then use Selectize input that should not be empty too
           ############## update checkboxes and ontology index



              term_id<-input$selectize

             term_name<-shiny_in$c6[which(names(shiny_in$c6)==term_id)]

             if (length(term_name)==0){ #check if term is found in ontology
               term_name="TERM NOT FOUND!!!"
             }

             term_id_name<-paste(term_name, term_id, sep=", ")
             CHAR_id<-paste0("CHAR:", names(map_btn_check)[which(map_btn_check==x)])


             if (term_id_name %in% shiny_in$c5[[CHAR_id]]){#check for duplication
               updateTextInput(session, paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)]),
                               label = "You are trying to add the same term twice")
             }

             if (!term_id_name %in% shiny_in$c5[[CHAR_id]]){#check for duplication

               #update terms selected
               shiny_in$terms_selected[[CHAR_id]] <<- c(shiny_in$terms_selected[[CHAR_id]], term_id_name)

               #update terms all
               shiny_in$c5[[CHAR_id]] <<- c(shiny_in$c5[[CHAR_id]], term_id_name)

               #update checkbox
               updateCheckboxGroupInput(session, paste0("checkbox", names(map_btn_check)[which(map_btn_check==x)]),
                                        label=NA, choices=shiny_in$c5[[CHAR_id]],
                                        selected=shiny_in$terms_selected[[CHAR_id]]
               )

               updateTextInput(session, paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)]), value = "",  label = "")
               term_id<-c("")
             }


           ###########
         }


      ### Manual input
      ############## update checkboxes and ontology index

          if (term_id!=""){ # term fiels must be non-empty
            term_id<-gsub(" ", "", term_id) # remove white spaces

          term_name<-shiny_in$c6[which(names(shiny_in$c6)==term_id)]

          if (length(term_name)==0){ #check if term is found in ontology
            term_name="TERM NOT FOUND!!!"
          }

          term_id_name<-paste(term_name, term_id, sep=", ")
          CHAR_id<-paste0("CHAR:", names(map_btn_check)[which(map_btn_check==x)])


          if (term_id_name %in% shiny_in$c5[[CHAR_id]]){#check for duplication
            updateTextInput(session, paste0("ids_in", names(map_btn_check)[which(map_btn_check==x)]),
                            label = "You are trying to add the same term twice")
          }

            if (!term_id_name %in% shiny_in$c5[[CHAR_id]]){#check for duplication

          #update terms selected
          shiny_in$terms_selected[[CHAR_id]] <<- c(shiny_in$terms_selected[[CHAR_id]], term_id_name)

          #update terms all
          shiny_in$c5[[CHAR_id]] <<- c(shiny_in$c5[[CHAR_id]], term_id_name)

          #update checkbox
          updateCheckboxGroupInput(session, paste0("checkbox", names(map_btn_check)[which(map_btn_check==x)]),
                                   label=NA, choices=shiny_in$c5[[CHAR_id]],
                                   selected=shiny_in$terms_selected[[CHAR_id]]
          )

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
        input[[x]],
        {
          CHAR_id<-paste0("CHAR:", names(map_checkbox)[which(map_checkbox==x)])

          #update terms selected
          shiny_in$terms_selected[[CHAR_id]] <<- input[[x]]



        })
    })
  })
########
################ Node selection
  observeEvent(
    input$network_selected, {
      #print(input$network_selected)

      updateSelectizeInput(session, "selectize", label = NULL, choices=srch_items, selected = input$network_selected,
                           options = list(openOnFocus=F, maxOptions=100, placeholder="Enter term or ID"
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
          ontology$def[which(names(ontology$def)==input$selectize)]})

        output$syn_txt<-renderText({
          ontology$parsed_synonyms[which(names(ontology$parsed_synonyms)==input$selectize)]})
      })

#####

}

###############################################################################################################################

##########################

shinyApp(ui, server)



