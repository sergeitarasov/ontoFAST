setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/data")

# read dependency file
mt_depend<-read.csv("dependencies_proper_format.csv", header=T,  stringsAsFactors = F, na.strings = "")



#' @title Gives list of dependencies between characters
#' @description List describing dependent characters in chrs matrix
#' @param mt_depend table summarizing dependecies between chrs. Format (char_id	char_state,	depends_upon_char,	depends_upon_state)
#' @return List.
#' @examples
#' dependency_list(mt_depend)->depends_upon

dependency_list<-function(mt_depend){
mt_depend[,1] %>% unique->unique_ids
depend_list=list()
for (i in seq_along(unique_ids)){
  cont_chr<-paste0("CHAR:", unique_ids[i])
  mt_depend[,2][which(mt_depend[,1]==unique_ids[i])]->cont_state
  names(cont_state)=paste0("STATE:", (cont_state+1))

  mt_depend[,3][which(mt_depend[,1]==unique_ids[i])] %>% unique ->dep_on_chrs
  dep_on_chrs=paste0("CHAR:", dep_on_chrs)

  mt_depend[,4][which(mt_depend[,1]==unique_ids[i])] %>% unique ->dep_on_states
  names(dep_on_states)=paste0("STATE:", (dep_on_states+1))

  depend_list[[cont_chr]]=list(
    states=cont_state,
    depends_upon_char=dep_on_chrs,
    depends_upon_states=dep_on_states
  )
}

return(depend_list)
}

#dependency_list(mt_depend)->depends_upon




#' @title Gives a list of characters that control other characters
#' @description List describing which characters controls which
#' @param depends_upon list from dependency_list()
#' @return List.
#' @examples
#' controllers_list(dependency_list(mt_depend))->controls_characters

controllers_list<-function(depends_upon){
conrol_list=list()
for (i in seq_along(depends_upon)){
  controller=depends_upon[[i]]$depends_upon_char
  conrol_list[[controller]]=c(conrol_list[[controller]], names(depends_upon[i]))
}
return(conrol_list)
}













