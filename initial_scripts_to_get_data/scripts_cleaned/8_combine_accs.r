require(plyr)
earlier_acc_file<-read.csv(file="/xxxx/MASTER_accession_list_FAILS.ADDED.csv",stringsAsFactors=FALSE)
additions<-read.csv(file="working_dir/MASTER_accession_list.csv")
master<-rbind.fill(earlier_acc_file,additions)
target.list<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$CladeBlast=='Trebouxia' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.a<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeA' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.i<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeI' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.g<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeG' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.s<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeS' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
write(target.list$Organism_Acc_No,file="working_dir/target.taxa.combined.txt")
write(target.list.clade.a$Organism_Acc_No,file="working_dir/target.taxa.A.combined.txt")
write(target.list.clade.i$Organism_Acc_No,file="working_dir/target.taxa.I.combined.txt")
write(target.list.clade.g$Organism_Acc_No,file="working_dir/target.taxa.G.combined.txt")
write(target.list.clade.s$Organism_Acc_No,file="working_dir/target.taxa.S.combined.txt")
write.csv(master,file="working_dir/MASTER_accession_list.csv",row.names=FALSE)