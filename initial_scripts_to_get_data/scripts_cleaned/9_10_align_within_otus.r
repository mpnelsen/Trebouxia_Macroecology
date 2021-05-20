#this will align repseqs from each clade at each threshold
thresh<-c("97.5","98","97","95","99","100")
clades<-c("A","G","I","S")
date<-"24_June_2018"
folderpath<-"/xxxx/working_dir_for_aligns/"
tax.acc.list<-read.csv(file="/xxxx/working_dir_for_aligns/MASTER_accession_list.csv",stringsAsFactors=FALSE)
threads<-5
path.to.mafft<-"/xxxx/mafft-7.402-without-extensions/core/mafft"
path.to.gblocks<-"/xxxx/Gblocks_0.91b/Gblocks"

#this will extract the type repseqs for each clade and then align
for(t in thresh){
	dir.create(as.character(paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade",sep="")))
	require(seqinr)
	all.reps<-read.fasta(file=paste(folderpath,"individual_",t,"_clusters/",t,"_rep_seqs.fasta",sep=""),seqtype="DNA",forceDNAtolower=FALSE)
	for(r in 1:nrow(tax.acc.list)){
		tax.acc.list$Clade[r]<-strsplit(tax.acc.list[,paste("OTU_",t,"_Raw_",date,sep="")],"_")[[r]][1]
	}
	for(c in clades){
		clade.rep.names<-tax.acc.list$Organism_Acc_No[tax.acc.list[,paste("RepSeq",t,"Raw",date,sep="_")] %in% "Yes" & tax.acc.list[,"Clade"] %in% c]
		print(clade.rep.names)
		clade.reps.align<-all.reps[names(all.reps) %in% clade.rep.names]
		write.fasta(sequences=clade.reps.align,names=names(clade.reps.align),file=paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade/",c,"_rep_seqs_unaligned.fasta",sep=""),nbchar=1000000)
		if(length(names(clade.reps.align))==1){
			write.fasta(sequences=clade.reps.align,names=names(clade.reps.align),file=paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade/",c,"_rep_seqs_aligned.fasta",sep=""),nbchar=1000000)
		}
		if(length(names(clade.reps.align))>1){
			cat(paste("\t...Aligning", c, "sequences...\n",sep=" "))
			input.file<-as.character(paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade/",c,"_rep_seqs_unaligned.fasta",sep=""))
			output.file<-as.character(paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade/",c,"_rep_seqs_aligned.fasta",sep=""))	
			#do G-INS-i
			mafft.commands<-paste(path.to.mafft,"--globalpair --maxiterate 1000 --thread", threads, "--preservecase --unalignlevel 0.8 --leavegappyregion", input.file, ">", output.file, sep=" ")	
			print(mafft.commands)
			system(mafft.commands,intern=TRUE)
			Sys.sleep(0.25)
			cat(paste("\t...Aligning", c, "sequences...\n",sep=" "))
			input.file<-as.character(paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade/",c,"_rep_seqs_unaligned.fasta",sep=""))
			output.file<-as.character(paste(folderpath,"individual_",t,"_clusters/repseq_alignments_by_clade/",c,"_rep_seqs_aligned_regular_ginsi.fasta",sep=""))	
			#do G-INS-i
			mafft.commands<-paste(path.to.mafft,"--globalpair --maxiterate 1000 --thread", threads, "--preservecase", input.file, ">", output.file, sep=" ")	
			print(mafft.commands)
			system(mafft.commands,intern=TRUE)
			Sys.sleep(0.25)
		}
	}
}
