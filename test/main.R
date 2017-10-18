#save ontology
#save(ontology, file="Ontology_HAO-save_before_expereminet.RData")
require("ontologyIndex")

setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/data")
setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/")
# creating OntologyIndex object
hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
                extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

# creating character ids for all 392 characters
id_characters<-paste("CHAR:",c(1:392), sep="")
hao_obo$id_characters<-id_characters

# reading characters and states
#char_et_states<-read.csv("Sharkey_chars.csv", header=T,  stringsAsFactors = F, na.strings = "")
char_et_states<-Sharkey_2011

# creating character name vector
name_characters<-char_et_states[,1]
names(name_characters)<-id_characters
hao_obo$name_characters<-name_characters

#including manual annotations !!! work on it to do better exmaple from the beginning of file reading
#hao_obo$annot_characters<-ontology$annot_characters

# pase synonyms for automatic annotation
hao_obo$parsed_synonyms<-syn_extract(hao_obo)

# automatic annotation
hao_obo$auto_annot_characters<-annot_all_chars(hao_obo, use.synonyms=TRUE, min_set=TRUE)

#shiny_in<<-c()
#make a global object
shiny_in<<-make_shiny_in(hao_obo)
#run OntoFast
runOntoFast(nchar=5, show.chars=T)


#selected terms
shiny_in$terms_selected
shiny_in$terms_selected_id
names(shiny_in$terms_map[shiny_in$terms_map %in%sa])

sa<<-c()
shiny_in$auto_annot_characters[["CHAR:381"]][a]

system.file("data_onto", "HAO.obo", package = "ontoFAST")

#roxygen2::roxygenize()

save(char_et_states, file="Sharkey_chars.RData")
HAO<-hao_obo1
devtools::use_data(HAO)

devtools::build()
devtools::use_vignette("my-vignette")
