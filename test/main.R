#save ontology
#save(ontology, file="Ontology_HAO-save_before_expereminet.RData")
require("ontologyIndex")

setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/data")
# creating OntologyIndex object
hao_obo=get_OBO("hao_new.obo", extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

# creating character ids for all 392 characters
id_characters<-paste("CHAR:",c(1:392), sep="")
hao_obo$id_characters<-id_characters

# reading characters and states
char_et_states<-read.csv("Sharkey-chars-and-states.csv", header=F,  stringsAsFactors = F, na.strings = "")

# creating character name vector
name_characters<-char_et_states[,1]
names(name_characters)<-id_characters
hao_obo$name_characters<-name_characters

#including manual annotations !!! work on it to do better exmaple from the beginning of file reading
hao_obo$annot_characters<-ontology$annot_characters

# pase synonyms for automatic annotation
hao_obo$parsed_synonyms<-syn_extract(hao_obo)

# automatic annotation
hao_obo$auto_annot_characters<-annot_all_chars(hao_obo, use.synonyms=TRUE, min_set=TRUE)

#make a global object
shiny_in<<-make_shiny_in(hao_obo)
#run OntoFast
runOntoFast(nchar=15)



