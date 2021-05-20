import re
import os
import csv
import time
import shutil
import subprocess
from Bio import SeqIO
from Bio.Seq import Seq
from cogent import LoadSeqs, DNA
from Bio.SeqRecord import SeqRecord
from Bio.SeqFeature import SeqFeature, FeatureLocation, Reference
#01mar2018 fasta file was actually acquired on 24june2018 and just labeled incorrectly in thegrabber script.
input_acc_file ="/Users/matthewnelsen/Documents/lichen_symbiotic_algae_review/24june2018/Trebouxia_24june2018_new_accession_list_updates_ready_to_go.csv" 
input_seq_file ="/Users/matthewnelsen/Documents/lichen_symbiotic_algae_review/24june2018/Trebouxia_01mar2018_out_parsed_removed_quotes_around_simplexclade1.fasta"
earlier_seq_file = "/Users/matthewnelsen/Documents/lichen_symbiotic_algae_review/23sep2016_starting_over/MASTER_sequence_file_TARGET_from_16sep2016c_manfailpulled.fasta"
sim_thresh = ['95', '97', '97.5', '98', '99', '100']
subclades = ['A', 'I', 'G', 'S']
blastn="/Applications/ncbi-blast-2.2.29+/bin/blastn"
blastdb="/Applications/blast_23_september_2016/blast_23_september_2016.db"
itsx="/Applications/ITSx_1.0.9_mod/ITSx"
mothur="/Applications/mothur_1.33.3/mothur"
blastclust="/Applications/blast-2.2.26/bin/blastclust"
megablast="/Applications/blast-2.2.26/bin/megablast"
windomasker="/Applications/ncbi-blast-2.2.29+/bin/windowmasker"
makeblastdb="/Applications/ncbi-blast-2.2.29+/bin/makeblastdb"

os.makedirs ("working_dir_sep2019")
shutil.copy(input_acc_file,"working_dir_sep2019/MASTER_accession_list.csv");
shutil.copy(input_seq_file,"working_dir_sep2019/MASTER_sequence_file.fasta");

#use subprocess to call ITSx...then generate list of those that are of target and PASS ITSx
subprocess.call(args="{0} -i working_dir_sep2019/MASTER_sequence_file.fasta -t Chlorophyta --save regions ITS1,5.8S,ITS2 --summary T --preserve T --partial 125 --not_found T --reset T -E 1 --truncate T -o working_dir_sep2019/itsx.parsed".format(itsx), shell=True);

passed_itsx=open("working_dir_sep2019/passed_itsx.txt","w")
for new_record in SeqIO.parse("working_dir_sep2019/itsx.parsed.full_and_partial.fasta", "fasta"):
	passed_itsx.write('%s\n' % new_record.id);

passed_itsx.close()

#use R to modify master list and record who passed itsx
subprocess.call(args="Rscript scripts_sep2019/itsx_notes.r",shell=True);

#use subprocess to call mothur and remove those w/sequence issues and low quality issues, and again use R to modify list and generate list of those that pass target, itsx and mothur
subprocess.call(["{0}".format(mothur),"#screen.seqs(fasta=working_dir_sep2019/itsx.parsed.full_and_partial.fasta, maxambig=3, minlength=450)"])
passed_mothur=open("working_dir_sep2019/passed_mothur.txt","w")
for new_record in SeqIO.parse("working_dir_sep2019/itsx.parsed.full_and_partial.good.fasta", "fasta"):
	passed_mothur.write('%s\n' % new_record.id);

passed_mothur.close()

#use R to modify master list and record who passed this stage
subprocess.call(args="Rscript scripts_sep2019/mothur_quality_filter.r",shell=True);

###############BLAST AND GROUP INTO 4 CLADES...ADD INFO TO SPREADSHEET AND SEPARATE INTO CLUSTERS#########
#perform local blast to check if seqs actually from organisms of interest
subprocess.call(args="{0} -query working_dir_sep2019/itsx.parsed.full_and_partial.good.fasta -out working_dir_sep2019/blast.txt -task blastn -outfmt 7 -max_target_seqs 1 -db {1}".format(blastn,blastdb), shell=True);
shutil.copy("working_dir_sep2019/blast.txt","working_dir_sep2019/blast_for_edit.txt");

#use some perl to edit file
subprocess.call(args="perl -i -pe 's/^[#].*\n.*/\1/g' working_dir_sep2019/blast_for_edit.txt", shell=True);
subprocess.call(args="perl -i -pe 's/^(\S+\t\S+).*/$1/g' working_dir_sep2019/blast_for_edit.txt", shell=True);
#don't need the \001 line here if running perl natively (ie, not from w/in python)
subprocess.call(args="perl -pi -e 's/\001//g' working_dir_sep2019/blast_for_edit.txt", shell=True);
subprocess.call(args="uniq working_dir_sep2019/blast_for_edit.txt > working_dir_sep2019/blast_for_edit_clean.txt", shell=True);

#then call R to execute a script that adds blast results and generates a list of those to keep
subprocess.call(args="Rscript scripts_sep2019/blast_filter.r",shell=True);

#this then pulls out those that blast correctly...
aln = LoadSeqs('working_dir_sep2019/itsx.parsed.full_and_partial.good.fasta', moltype=DNA, aligned=False)
keepers_first_round=open("working_dir_sep2019/target.taxa.txt")
have=keepers_first_round.read().split()
have_align=aln.takeSeqs(have)
have_align.writeToFile('working_dir_sep2019/MASTER_sequence_file_TARGET.fasta')

for clade in subclades:
	keepers_first_round=open("working_dir_sep2019/target.taxa.{0}.txt".format(clade))
	have=keepers_first_round.read().split()
	have_align=aln.takeSeqs(have)
	try:
		have_align.writeToFile('working_dir_sep2019/{0}.fasta'.format(clade))
	except AttributeError:
		continue

###########################################################################################################
#combine files now
subprocess.call(args="cat {0} working_dir_sep2019/MASTER_sequence_file_TARGET.fasta > working_dir_sep2019/TEST.fasta".format(earlier_seq_file), shell=True)
subprocess.call(args="mv working_dir_sep2019/TEST.fasta working_dir_sep2019/MASTER_sequence_file_TARGET.fasta", shell=True)

#then call R to add new accession info to old
subprocess.call(args="Rscript scripts_sep2019/combine_accs.r",shell=True);


aln = LoadSeqs('working_dir_sep2019/MASTER_sequence_file_TARGET.fasta', moltype=DNA, aligned=False)
for clade in subclades:
	keepers_first_round=open("working_dir_sep2019/target.taxa.{0}.combined.txt".format(clade))
	have=keepers_first_round.read().split()
	have_align=aln.takeSeqs(have)
	if have_align:
		have_align.writeToFile('working_dir_sep2019/{0}.fasta'.format(clade));

###########################################################################################################

#blastclust for individual clusters at different similarity thresholds...
for sim in sim_thresh:
	for clade in subclades:
		subprocess.call(args="{0} -i working_dir_sep2019/{1}.fasta -S {2} -L 0.5 -a 8 -e F -o working_dir_sep2019/{1}_{2}_clusters -p F".format(blastclust,clade,sim), shell=True);

#this creates a list for each similarity threshold
for thresh in sim_thresh:
	new_list=open("working_dir_sep2019/blastclust_{0}_otu_assignment.txt".format(thresh),"w")
	for clade in subclades:
		cluster = [line.strip() for line in open("working_dir_sep2019/{0}_{1}_clusters".format(clade,thresh))]
		numb = 0
		for line in cluster:
			numb = numb + 1
			line = line.split()
			for tax in line:
				new_list.write("%s_%s_%s\t%s\n" % (clade, thresh, numb, tax))

new_list.close();

#adds raw otu info to master file at differing similarity thresholds
subprocess.call(args="Rscript scripts_sep2019/post_otu_cleaning.r",shell=True);

#create separate directories for each similarity threshold
for thresh in sim_thresh:
	os.mkdir("working_dir_sep2019/individual_{0}_clusters".format(thresh));

#create separate sequence files for every otu, make consensus sequence for each otu,
#make individual blastdb's for each otu, blast consensus against individual db,
#pull real seq that is best match w/consensus, make list and make repset 
for thresh in sim_thresh:
	total_group=csv.reader(open("working_dir_sep2019/blastclust_{0}_otu_assignment.txt".format(thresh)),delimiter="\t")
	otus_set=[]
	for row in total_group:
		otus_set.append(row[0])
	otus_set=set(otus_set)
	aln = LoadSeqs('working_dir_sep2019/MASTER_sequence_file_TARGET.fasta', moltype=DNA, aligned=False)
	for otu in otus_set:
		seq_list=[]
		total_group=csv.reader(open("working_dir_sep2019/blastclust_{0}_otu_assignment.txt".format(thresh)),delimiter="\t")
		for row in total_group:
			if row[0]=="{0}".format(otu):
				seq_list.append(row[1])
		otuseqs=aln.takeSeqs(seq_list)
		otuseqs.writeToFile("working_dir_sep2019/individual_{0}_clusters/{1}_seqs.fasta".format(thresh,otu))
		subprocess.call(args="usearch -cluster_fast working_dir_sep2019/individual_{0}_clusters/{1}_seqs.fasta -consout working_dir_sep2019/individual_{0}_clusters/{1}_consensus.fasta -id 0.80".format(thresh,otu),shell=True)
		subprocess.call(args="{0} -in working_dir_sep2019/individual_{1}_clusters/{2}_seqs.fasta -infmt fasta -mk_counts -parse_seqids -out working_dir_sep2019/individual_{1}_clusters/{2}.mask.counts".format(windomasker,thresh,otu),shell=True)
		subprocess.call(args="{0} -in working_dir_sep2019/individual_{1}_clusters/{2}_seqs.fasta -infmt fasta -ustat working_dir_sep2019/individual_{1}_clusters/{2}.mask.counts -outfmt maskinfo_asn1_bin -parse_seqids -out working_dir_sep2019/individual_{1}_clusters/{2}_mask.asnb".format(windomasker,thresh,otu),shell=True)
		subprocess.call(args="{0} -in working_dir_sep2019/individual_{1}_clusters/{2}_seqs.fasta -input_type fasta -dbtype nucl -parse_seqids -mask_data working_dir_sep2019/individual_{1}_clusters/{2}_mask.asnb -out working_dir_sep2019/individual_{1}_clusters/{2}.db -title {2}_database".format(makeblastdb,thresh,otu),shell=True)
		subprocess.call(args="{0} -W 8 -r 2 -q -3 -G 5 -E 2 -v 1 -b 1 -m 8 -i working_dir_sep2019/individual_{1}_clusters/{2}_consensus.fasta -d working_dir_sep2019/individual_{1}_clusters/{2}.db -o working_dir_sep2019/individual_{1}_clusters/{2}_consensus_megablast_result".format(megablast,thresh,otu),shell = True)
		#use some perl to edit file
		shutil.copy("working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result".format(thresh,otu),"working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_pre_edit.txt".format(thresh,otu))
		#added this to save only first line in the event that there are multiple best hits
		subprocess.call(args="head -n 1 working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_pre_edit.txt > working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_edit.txt".format(thresh,otu), shell=True)
		subprocess.call(args="perl -i -pe 's/^[#].*\n.*/\1/g' working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_edit.txt".format(thresh,otu), shell=True)
		subprocess.call(args="perl -i -pe 's/^(\S+\t\S+).*/$1/g' working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_edit.txt".format(thresh,otu), shell=True)
		#don't need the \001 line here if running perl natively (ie, not from w/in python)
		subprocess.call(args="perl -pi -e 's/\001//g' working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_edit.txt".format(thresh,otu), shell=True)
		subprocess.call(args="perl -i -pe 's/^(\S+\t)(\S+).*/$2/g' working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_edit.txt".format(thresh,otu), shell=True)
		subprocess.call(args="uniq working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_for_edit.txt > working_dir_sep2019/individual_{0}_clusters/{1}_consensus_megablast_result_clean.txt".format(thresh,otu), shell=True)
	for clade in subclades:
		subprocess.call(args="cat working_dir_sep2019/individual_{0}_clusters/{1}_{0}_*_consensus_megablast_result_clean.txt > working_dir_sep2019/individual_{0}_clusters/{1}_{0}_rep_set_accessions.txt".format(thresh, clade), shell=True)
		second_aln = LoadSeqs('working_dir_sep2019/MASTER_sequence_file_TARGET.fasta', moltype=DNA, aligned=False)
		rep_accs=open("working_dir_sep2019/individual_{0}_clusters/{1}_{0}_rep_set_accessions.txt".format(thresh, clade))
		rep_accs=rep_accs.read().split()
		repseqs=second_aln.takeSeqs(rep_accs)
		repseqs.writeToFile("working_dir_sep2019/individual_{0}_clusters/{1}_{0}_rep_seqs.fasta".format(thresh,clade))
	subprocess.call(args="cat working_dir_sep2019/individual_{0}_clusters/*_{0}_*_consensus_megablast_result_clean.txt > working_dir_sep2019/individual_{0}_clusters/{0}_rep_set_accessions.txt".format(thresh), shell=True)
	second_aln = LoadSeqs('working_dir_sep2019/MASTER_sequence_file_TARGET.fasta', moltype=DNA, aligned=False)
	rep_accs=open("working_dir_sep2019/individual_{0}_clusters/{0}_rep_set_accessions.txt".format(thresh))
	rep_accs=rep_accs.read().split()
	repseqs=second_aln.takeSeqs(rep_accs)
	repseqs.writeToFile("working_dir_sep2019/individual_{0}_clusters/{0}_rep_seqs.fasta".format(thresh))

#adds rep set info to master file at differing similarity thresholds
subprocess.call(args="Rscript scripts_sep2019/repset_annotation.r",shell=True);
