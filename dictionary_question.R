#Create dictionary
question_value<-read.xlsx("C:/Users/habe/Documents/NKI_clustering/2019_Variable_Info.xlsx",1,header = T)

quest_dict<-dict()


for (i in 1:NROW(question_value)){
  quest_dict[[as.character(question_value[i,1])]]<-as.character(question_value[i,3])
}

quest_dict[["v5"]]
quest_dict[["KÃ¤nndeom_scale"]]
