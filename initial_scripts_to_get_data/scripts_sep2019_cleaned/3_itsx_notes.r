date<-"16_Sep_2019"
bl<-read.table(file="working_dir_sep2019/passed_itsx.txt",sep="\t",header=FALSE,fill=TRUE,na.strings = "", quote="\"")
colnames(bl)<-"Taxon"
master<-read.csv(file="working_dir_sep2019/MASTER_accession_list.csv")
master$Organism_Acc_No<-as.character(master$Organism_Acc_No)
master$ITSx_PASS<-"FAIL"
master$ITSx_Date<-date
for(z in 1:nrow(bl)){
	master$ITSx_PASS[master$Organism_Acc_No==bl$Taxon[z]]<-"PASS"
}
write.csv(master,file="working_dir_sep2019/MASTER_accession_list.csv",row.names=FALSE)