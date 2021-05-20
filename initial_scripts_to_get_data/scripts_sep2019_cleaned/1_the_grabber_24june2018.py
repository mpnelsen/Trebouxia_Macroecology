#This can be used to check, retrieve and parse NCBI sequences not already in a list of GI's.
#Portions this code were derived from: http://www.molecularecologist.com/2011/04/get-your-protein/

from Bio import Entrez
from Bio import SeqIO
from Bio.SeqFeature import SeqFeature, FeatureLocation, Reference
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord
import numpy as np

taxa="Trebouxia","Asterochloris"
higherclade="Chlorophyta"
searchterm="internal transcribed spacer OR 5.8S"
folderpath="/Users/xxx/"
filenamewoldgi="TrebAcc.txt"
querysize=100
date="01mar2018"
email="xxx"

Entrez.email = "{0}".format(email)
if not Entrez.email:
    print "need to provide email address to ncbi"
    sys.exit(2);

alreadyhave=open("{0}{1}".format(folderpath,filenamewoldgi))
have=alreadyhave.read().split("\r")
for taxon in taxa:
	search=Entrez.esearch(db="nucleotide", term="{0} AND {1}[ORGN] AND {2}[ORGN]".format(searchterm,taxon,higherclade), retmax=100000, idtype="acc",usehistory="y")
	search.results = Entrez.read(search)
	print "\n{0}".format(taxon)
	if search.results["Count"]=='1':
		print "%s sequence matching query" % search.results["Count"]
	else:
		print "%s sequences matching query" % search.results["Count"];
	allgenusseqs=search.results['IdList']
	#drop decimal
	allgenusseqs=[x.split('.')[0] for x in allgenusseqs]
	toget=list(set(allgenusseqs)-set(have))
	numnew=len(toget)
	print(numnew)
	if numnew==0:
		pass
		print "Nothing new!"
	else:
		if numnew=='1':
			print "%s new sequence" % numnew
		else:
			print "%s new sequences" % numnew
		newprunedlist=open("{0}{1}_{2}_new_accession_list.txt".format(folderpath,taxon,date),"w")
		for newacc in toget:
			newprunedlist.write("%s\n" % newacc)
		newprunedlist.close()
		out = open("{0}{1}_{2}_out.gbk".format(folderpath,taxon,date),"wa")
		noqueries=int(np.ceil(float(numnew)/querysize))
		for query in range(noqueries):
			fetch_handle = Entrez.efetch(db="nucleotide", id=toget[((query)*querysize):(query+1)*querysize], rettype="gb", retmode="text")
			data = fetch_handle.read()
			fetch_handle.close()
	    		out.write(data)
	    		#print data
	    	out.close()
	    	search.close()

		if "{0}{1}_{2}_out.gbk".format(folderpath,taxon,date)>0:
			records = SeqIO.parse(open("{0}{1}_{2}_out.gbk".format(folderpath,taxon,date),"r"),"genbank")
			genefile = open("{0}{1}_{2}_out_parsed.fasta".format(folderpath,taxon,date),"w")
			for record in records:
				organism=record.annotations['organism']
				organism2=organism.replace(" ","_")
				organism3=organism2.replace(".","")
				accession=record.name
				seq=record.seq
				#print '>%s_%s\n%s' % (organism3, accession, seq)
				genefile.write('>%s_%s\n%s\n' % (organism3, accession, seq))
			genefile.close();


			
def index_genbank_features(record, feature_type, qualifier):
    	for (index, feature) in enumerate(record.features):
        	if feature.type==feature_type:
            		if qualifier in feature.qualifiers:						
						for value in feature.qualifiers[qualifier]:
							return value;


for taxon in taxa:
	try:
		if "{0}{1}_{2}_out.gbk".format(folderpath,taxon,date):
			records = SeqIO.parse(open("{0}{1}_{2}_out.gbk".format(folderpath,taxon,date),"r"), "genbank")
			list=open("{0}{1}_{2}_new_accession_list.txt".format(folderpath,taxon,date),"w")
			for record in records:
				organism=record.annotations['organism']
				accession=record.name
				strain = index_genbank_features(record,"source","strain")
				isolation_source_gb = index_genbank_features(record,"source","isolation_source")
				isolation_source_gbstr=str(isolation_source_gb)
				isolation_source_cut=isolation_source_gbstr.split(";")[0].split(" ")
				isolation_source_genus=isolation_source_cut[0]
				isolation_source_species=" ".join(isolation_source_cut[1:])			
				host_gb = index_genbank_features(record,"source","host")
				host_gbstr=str(host_gb)
				hostcut=host_gbstr.split(";")[0].split(" ")
				genus=hostcut[0]
				species=" ".join(hostcut[1:])
				country_gb = index_genbank_features(record,"source","country")
				country_gbstr=str(country_gb)
				countrycut=country_gbstr.split(":")[0].split(" ")
				country=" ".join(countrycut[0:])
				note = index_genbank_features(record,"source","note")
				voucher = index_genbank_features(record,"source","specimen_voucher")
				print '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' % (organism, accession, strain, isolation_source_gb, isolation_source_genus, isolation_source_species, host_gb, genus, species, country_gb, country, note, voucher)
				list.write('%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' % (organism, accession, strain, isolation_source_gb, isolation_source_genus, isolation_source_species, host_gb, genus, species, country_gb, country, note, voucher))
	except IOError:
        	print "\n{0} not present\n".format(taxon)
	list.close();