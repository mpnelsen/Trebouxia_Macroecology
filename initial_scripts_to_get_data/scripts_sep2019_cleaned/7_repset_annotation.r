date<-"16_Sep_2019"
master<-read.csv(file="working_dir_sep2019/MASTER_accession_list.csv")
master$Organism_Acc_No<-as.character(master$Organism_Acc_No)
thresh_extract<-list.files("working_dir_sep2019", pattern="^individual_[0-9,.]*._clusters", full.names=TRUE)
for(q in 1:length(thresh_extract)){
	working_thresh<-strsplit(thresh_extract[q],split="_")[[1]][4]
	repfile<-list.files(as.character(paste("working_dir_sep2019/individual_",working_thresh,"_clusters",sep="")), pattern="^[0-9,.]*_rep_set_accessions.txt", full.names=TRUE)
	for(i in 1:length(repfile)){
		bl<-read.table(file=repfile[i],sep="\t",header=FALSE,fill=TRUE,na.strings = "", quote="\"")
		colnames(bl)<-c("RepSeq")
		oturep<-paste("RepSeq",working_thresh,"Raw",date,sep="_")
		master[[oturep]]<-NA
		master[[oturep]][!is.na(master$Blast_Date)]<-"No"
		for(z in 1:nrow(bl)){
			master[[oturep]][master$Organism_Acc_No==bl$RepSeq[z]]<-"Yes"
		}
	}	
}

write.csv(master,file="working_dir_sep2019/MASTER_accession_list.csv",row.names=FALSE)
