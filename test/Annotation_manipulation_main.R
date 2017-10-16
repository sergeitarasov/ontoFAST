setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/data")
library("plyr", lib.loc="~/.local/R/site-library")

# creating OntologyIndex object
hao_obo=get_OBO("hao_new.obo", extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

# creating character ids for all 392 characters
id_characters<-paste("CHAR:",c(1:392), sep="")

# reading characters and states
char_et_states<-read.csv("Sharkey-chars-and-states.csv", header=F,  stringsAsFactors = F, na.strings = "")

# creating character name vector
name_characters<-char_et_states[,1]
names(name_characters)<-id_characters

# creating list of char state  names
name_character_states<-table2list(char_et_states)
names(name_character_states)<-id_characters # assigning character ids to list

# creating list of char states ids
id_character_states<-lapply(name_character_states, function(x) {x<-paste0("STATE:", c(1:length(x)))} )

# assigning states ids to state names: name_character_states
for (x in seq_along(name_character_states)) names(name_character_states[[x]])<-
  paste0("STATE:", c(1:length(name_character_states[[x]])))

# now I have
id_characters
name_characters
id_character_states
name_character_states

#######################

###################################
# using pipline

# creating character ids for all 392 characters
paste("CHAR:",c(1:392), sep="")->id_characters

# reading characters and states
char_et_states<-read.csv("Sharkey-chars-and-states.csv", header=F,  stringsAsFactors = F, na.strings = "")

#___________
# creating character name vector
char_et_states[,1] %>% setNames(id_characters)-> name_characters
###

char_et_states %>% table2list(.) %>% setNames(id_characters) -> name_character_states

name_character_states %>%
  #setNames(id_characters) %>%
  lapply(function(x) {x<-paste0("STATE:", c(1:length(x)))}) -> id_character_states

# assigning states ids to state names: name_character_states
for (x in seq_along(name_character_states)) paste0("STATE:",
                    c(1:length(name_character_states[[x]]))) -> names(name_character_states[[x]])


#_____________________

char_matrix<-read.csv("Sharkey-matrix.csv", header=F, row.names=1,  na.strings = "")
names(char_matrix)<-(id_characters) # names to data frame

apply(char_matrix, 2,  function(x) any(unique(x)=="-")) #if char contains symbol

names(char_matrix)[apply(char_matrix, 2,  function(x) any(unique(x)=="-"))] ->contains_inapplicable #chrs with "-"
names(char_matrix)[apply(char_matrix, 2,  function(x) any(unique(x)=="?"))] ->contains_missing #chrs with "?"
contains_missing%>%length

apply(char_matrix, 2,  function(x) (any(unique(x)=="-")*any(unique(x)=="?"))==T) #if char contains two symbols

names(char_matrix)[apply(char_matrix, 2,  function(x) any(grepl("/", unique(x))))]->contains_polymorph #contains polymorphic states

## check if enumertion of states is sequential e.g. 0, 1, 2, ...
# returns chars that are not
enumeration_not_seq<-function(char_matrix){
chars_only_numbers<-apply(char_matrix, 2,  function(x) { unique(x)[!grepl("\\D", unique(x))==T ] %>% as.numeric }) # retrive chars encoded with integers (excl. - and ?)
diff=lapply(chars_only_numbers, function(x) identical(x[order(x)], as.numeric(c(0:max(x)))) )%>%unlist #chaeck if enumeration in matrix is sequential
return(names(which(diff==F))) #which chars are not sequential
}
not_seq=enumeration_not_seq(char_matrix)
#[1] "CHAR:24"  "CHAR:36"  "CHAR:380"
lapply(not_seq, function(x) levels(char_matrix[[x]])) %>% setNames(not_seq) # get char info for not sequential characters
#________________

########### unused char states; function compares which states are present in report but absent in matirx; returns list of two lists

unused_states<-function(coding_states_matrix, name_character_states){

names_mt=lapply(coding_states_matrix, names)

names_report=lapply(name_character_states, names)


unused_states<-list(unused_matrix=list(), unused_chrs_report=list())


for (i in seq_along(names_mt)){
  x=setdiff(names_mt[[i]], names_report[[i]])
  y=setdiff(names_report[[i]], names_mt[[i]])
  if (length(x)>0) unused_states$unused_chrs_report[[names(names_mt[i])]]=x
  #unused_states$unused_chrs_report[[names(names_mt[i])]]=setdiff(names_mt[[i]], names_report[[i]])
  if (length(y)>0) unused_states$unused_matrix[[names(names_mt[i])]]=y
  #unused_states$unused_matrix[[names(names_mt[i])]]=setdiff(names_report[[i]], names_mt[[i]])

}
return(unused_states)
}

unused_states(coding_states_matrix, name_character_states)->unused_chr_states
#____________________________

## make list of states and their coding from matrix
coding_states_mt<-function(char_matrix){
chars_only_numbers<-apply(char_matrix, 2,  function(x) { unique(x)[!grepl("\\D", unique(x))==T ] %>% as.numeric }) # retrive chars encoded with integers (excl. - and ?)
chars_only_numbers=lapply(chars_only_numbers, function(x) x[order(x)])
for (x in seq_along(chars_only_numbers)){
  paste0("STATE:", chars_only_numbers[[x]]+1)->names(chars_only_numbers[[x]])
}
return(chars_only_numbers)
}

coding_states_mt(char_matrix)->coding_states_matrix
#___________

########## Number of the same patterns
same_patterns<-function(char_matrix){

char_matrix_nofac=apply(char_matrix, 2,  function(x) as.character(x))
recode_mt=c()

for (i in 1:ncol(char_matrix_nofac)){
  recode_mt=cbind(recode_mt, mapvalues(char_matrix_nofac[,i],   from = unique(char_matrix_nofac[,i]), to = c(1:length(unique(char_matrix_nofac[,i])))))
}

pattern_str=apply(recode_mt, 2, function(x) paste(x, collapse=""))

pattern_str[duplicated(pattern_str)] %>% unique ->pattern_uniq

return(lapply(c(1:length(pattern_uniq)), function(x) names(char_matrix[,which(pattern_str==pattern_uniq[x])])))

}
same_patterns(char_matrix)->same_patterns

#############
taxa_missing_states<-function(char_matrix){
miss=apply(char_matrix, 1,  function(x) {which(x=="?") %>% length})
return(miss[order(miss, decreasing=T)])
}
taxa_missing_states(char_matrix)->taxa_missing_chrs_states








