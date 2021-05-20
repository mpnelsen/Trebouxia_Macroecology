date<-"16_Sep_2019"
bl<-read.table(file="working_dir_sep2019/passed_mothur.txt",sep="\t",header=FALSE,fill=TRUE,na.strings = "", quote="\"")
colnames(bl)<-"Taxon"
master<-read.csv(file="working_dir_sep2019/MASTER_accession_list.csv")
#head(bl)
#nrow(bl)
#head(master)
#nrow(master)
master$Organism_Acc_No<-as.character(master$Organism_Acc_No)

for(z in 1:nrow(bl)){
	master$Quality_PASS[master$Organism_Acc_No==bl$Taxon[z]]<-"PASS"
	master$Quality_Date[master$Organism_Acc_No==bl$Taxon[z]]<-date
	if(master$ITSx_PASS[z]=='PASS' & is.na(master$Quality_PASS[z])){
		master$Quality_PASS[z]<-"FAIL"
		master$Quality_Date[z]<-date
	}
}

target.list<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS'),]
#nrow(target.list)
write(target.list$Organism_Acc_No,file="working_dir_sep2019/target.taxa.passitsx.passmothur.txt")
write.csv(master,file="working_dir_sep2019/MASTER_accession_list.csv",row.names=FALSE)