setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/data")
library("plyr", lib.loc="~/.local/R/site-library")



# Operations over matrices and chars reports give the follwoing subobjects
id_characters
name_characters
id_character_states
name_character_states #coding_states_report
contains_inapplicable
contains_missing
contains_polymorph
unused_chr_states
coding_states_matrix
same_chrs_patterns
taxa_missing_states
#######################

# Using pipline

########################
# Incorporating Character Report
#
#######################

# creating character ids for all 392 characters
paste("CHAR:",c(1:392), sep="")->id_characters

# reading characters and states
char_et_states<-read.csv("Sharkey-chars-and-states.csv", header=F,  stringsAsFactors = F, na.strings = "")

# creating character name vector
char_et_states[,1] %>% setNames(id_characters)-> name_characters
###

char_et_states %>% table2list(.) %>% setNames(id_characters) -> coding_states_report

coding_states_report %>%
  #setNames(id_characters) %>%
  lapply(function(x) {x<-paste0("STATE:", c(1:length(x)))}) -> id_character_states

# assigning states ids to state names: coding_states_report
for (x in seq_along(coding_states_report)) paste0("STATE:",
                                                   c(1:length(coding_states_report[[x]]))) -> names(coding_states_report[[x]])

########################
# Working on Matrix
#
#######################

char_matrix<-read.csv("Sharkey-matrix.csv", header=F, row.names=1,  na.strings = "")
names(char_matrix)<-(id_characters) # names to data frame


names(char_matrix)[apply(char_matrix, 2,  function(x) any(unique(x)=="-"))] ->contains_inapplicable #chrs with "-"
names(char_matrix)[apply(char_matrix, 2,  function(x) any(unique(x)=="?"))] ->contains_missing #chrs with "?"
names(char_matrix)[apply(char_matrix, 2,  function(x) any(grepl("/", unique(x))))]->contains_polymorph #contains polymorphic states

#############################
# Miscelaneous operations
#
############################


apply(char_matrix, 2,  function(x) any(unique(x)=="-")) #if char contains symbol
contains_missing%>%length
apply(char_matrix, 2,  function(x) (any(unique(x)=="-")*any(unique(x)=="?"))==T) #if char contains two symbols


#################################
# FUNCTIONS
#
#################################


#' @title Gives full report and all subobjects for charactert matrix
#' @description Takes dataframe (character matrix) with taxa as rows and ids as column names and returns a list of various objects
#' @param char_matrix Character matrix (dataframe)
#' @param coding_states_report list of chrs and states from character report
#' @return List.
#' @examples
#' parse_matrix(char_matrix, coding_states_report)

parse_matrix<-function(char_matrix, coding_states_report){
  names(char_matrix)[apply(char_matrix, 2,  function(x) any(unique(x)=="-"))] ->contains_inapplicable #chrs with "-"
  names(char_matrix)[apply(char_matrix, 2,  function(x) any(unique(x)=="?"))] ->contains_missing #chrs with "?"
  names(char_matrix)[apply(char_matrix, 2,  function(x) any(grepl("/", unique(x))))]->contains_polymorph #contains polymorphic states
  coding_states_mt(char_matrix)->coding_states_matrix
  same_patterns(char_matrix)->same_chrs_patterns
  unused_states(coding_states_matrix, coding_states_report)->unused_chr_states
  taxa_missing_states(char_matrix)->taxa_missing_chrs_states

  matrix=list(
    contains_inapplicable=contains_inapplicable,
    contains_missing=contains_missing,
    contains_polymorph=contains_polymorph,
    unused_chr_states=unused_chr_states,
    coding_states_matrix=coding_states_matrix,
    same_chrs_patterns=same_chrs_patterns,
    taxa_missing_states=taxa_missing_chrs_states
  )
  return(matrix)
}

cbind(paste0("CHAR:", c(1:392)),(paste0("CHAR:", c(1:392)) %in% mt_data$contains_inapplicable)) %>% write.csv(., file="contains_inaplic.csv")
getwd()


#' @title Check if enumertion of states in matrix is sequential e.g. 0, 1, 2, ... per each character
#' @description Returns chars that are not sequential
#' Each character is assigned its own ID CHAR:XXXX
#' @param char_matrix Character matrix (dataframe)
#' @return characters.
#' @examples
#' not_seq=enumeration_not_seq(char_matrix)
#' lapply(not_seq, function(x) levels(char_matrix[[x]])) %>% setNames(not_seq) # get char info for not sequential characters

enumeration_not_seq<-function(char_matrix){
  chars_only_numbers<-apply(char_matrix, 2,  function(x) { unique(x)[!grepl("\\D", unique(x))==T ] %>% as.numeric }) # retrive chars encoded with integers (excl. - and ?)
  diff=lapply(chars_only_numbers, function(x) identical(x[order(x)], as.numeric(c(0:max(x)))) )%>%unlist #chaeck if enumeration in matrix is sequential
  return(names(which(diff==F))) #which chars are not sequential
}





#' @title Unused char states; function compares which states are different between report and character matrix
#' @description Returns a list unused_states with two components unused_matrix (states present in report but absent in matrix)
#' and  unused_chrs_report (states present in matrix but absent in report)
#' Each character is assigned its own ID CHAR:XXXX
#' @param coding_states_matrix list of states and chars from matrix
#' @param coding_states_report list of state and chars from report
#' @return The list.
#' @examples
#' unused_states(coding_states_matrix, coding_states_report)->unused_chr_states

unused_states<-function(coding_states_matrix, coding_states_report){

  names_mt=lapply(coding_states_matrix, names)
  names_report=lapply(coding_states_report, names)
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





#' @title List of ids: states and chars (including coding symbols) from matrix
#' @description Make list of states and their coding from matrix
#' @param char_matrix character matrix
#' @return The list.
#' @examples
#' coding_states_mt(char_matrix)->coding_states_matrix

coding_states_mt<-function(char_matrix){
  chars_only_numbers<-apply(char_matrix, 2,  function(x) { unique(x)[!grepl("\\D", unique(x))==T ] %>% as.numeric }) # retrive chars encoded with integers (excl. - and ?)
  chars_only_numbers=lapply(chars_only_numbers, function(x) x[order(x)])
  for (x in seq_along(chars_only_numbers)){
    paste0("STATE:", chars_only_numbers[[x]]+1)->names(chars_only_numbers[[x]])
  }
  return(chars_only_numbers)
}




#' @title Characters with the same patterns are returned
#' @description Same chrs pattern
#' @param char_matrix character matrix
#' @return The list.
#' @examples
#' same_patterns(char_matrix)->same_chrs_patterns
##########
str(dt_rates)
char_matrix<-dt_rates

unlist(same_patterns(dt_rates))
unique(pattern_str)

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




#' @title Number of missing characters per each taxon
#' @description Number of missing characters per each taxon
#' @param char_matrix character matrix
#' @return Vector.
#' @examples
#' taxa_missing_states(char_matrix)->taxa_missing_chrs_states
#'
taxa_missing_states<-function(char_matrix){
  miss=apply(char_matrix, 1,  function(x) {which(x=="?") %>% length})
  return(miss[order(miss, decreasing=T)])
}


