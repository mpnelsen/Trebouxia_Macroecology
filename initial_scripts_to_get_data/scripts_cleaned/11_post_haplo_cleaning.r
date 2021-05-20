date<-"24_June_2018"
master<-read.csv(file="working_dir/MASTER_accession_list.csv")
master$Organism_Acc_No<-as.character(master$Organism_Acc_No)

otufiles<-list.files("working_dir/", pattern=c("blastclust_H_otu_assignment.txt"), full.names=TRUE)

for(i in 1:length(otufiles)){
	bl<-read.table(file=otufiles[i],sep="\t",header=FALSE,fill=TRUE,na.strings = "", quote="\"")
	colnames(bl)<-c("OTUraw","Taxon")
	newotu<-paste("OTU_Haplotype_Raw",date,sep="_")
		for(z in 1:nrow(bl)){
			master[[newotu]][master$Organism_Acc_No==bl$Taxon[z]]<-as.character(bl$OTUraw[z])
		}	
}


library(seqinr)
seqs<-read.fasta("working_dir/MASTER_sequence_file_TARGET.fasta")
master$SeqLength<-NA
for(x in 1:length(seqs)){
	master$SeqLength[master$Organism_Acc_No %in% names(seqs[x])]<-length(seqs[[x]])
}

master$RepSeq_Haplotype_Raw_24_June_2018 <-NA
master$RepSeq_Haplotype_Raw_24_June_2018[!is.na(master$OTU_Haplotype_Raw_24_June_2018)]<-"No"
u.haps<-unique(master$OTU_Haplotype_Raw_24_June_2018)
u.haps<-u.haps[!is.na(u.haps)]
ord.mast<-master[order(master$SeqLength,decreasing=TRUE),]
ord.mast<-ord.mast[!is.na(ord.mast$SeqLength),]

#identifies longest sequence of each haplotype and designates as representative
for(x in 1:length(u.haps)){
	hap.rep<-ord.mast$Organism_Acc_No[ord.mast$OTU_Haplotype_Raw_24_June_2018 %in% u.haps[x]][1]
	master$RepSeq_Haplotype_Raw_24_June_2018[master$Organism_Acc_No %in% hap.rep]<-"Yes"
}
write.csv(master,file="working_dir/MASTER_accession_list_haps_added.csv",row.names=FALSE)

hap.reps<-master$Organism_Acc_No[master$RepSeq_Haplotype_Raw_24_June_2018 %in% "Yes"]
haps.sequences<-seqs[names(seqs) %in% hap.reps]
#hap.reps[!hap.reps %in% names(haps.sequences)]
write.fasta(sequences=haps.sequences,names=names(haps.sequences),file="working_dir/H_sequence_file.fasta")

