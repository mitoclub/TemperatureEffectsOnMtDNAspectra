if (!requireNamespace("seqinr", quietly = TRUE)) {
  install.packages("seqinr")
}
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("systemPipeR", quietly = TRUE)) {
  BiocManager::install("systemPipeR")
}
if (!requireNamespace("Biostrings", quietly = TRUE)) {
  BiocManager::install("Biostrings")
}
if (!requireNamespace("coRdon", quietly = TRUE)) {
  BiocManager::install("coRdon")
}

library(seqinr)
library(ORFik)
library(Biostrings) 
library(ShortRead)
library(coRdon)
library(here)
library(tidyverse)

# Set paths
src_dir <- here("code")
data_dir <- here("data")
output_dir <- here("output")
plots_dir <- here(output_dir, "figures")
tables_dir <- here(output_dir, "tables")


CytBsequenses <- readDNAStringSet(here(data_dir, "MIDORI2_LONGEST_NUC_GB265_Cytb_BLAST.fasta"))
CytBsequenses <- CytBsequenses[grepl("Chordata_7711", names(CytBsequenses))]

start_codons <- subseq(CytBsequenses, 1, 3)
valid_starts <- start_codons == "ATG"
CytBsequensesonlyATG <- CytBsequenses[valid_starts]
CytBsequensesonlyATG <- CytBsequensesonlyATG[width(CytBsequensesonlyATG) %% 3 == 0]

codon_table <- codonTable(CytBsequensesonlyATG)
CodonsCytB <- codonCounts(codon_table)
dim(CodonsCytB)
CodonsCytB <- data.frame(CodonsCytB)
CodonsCytB$IDs <- getID(codon_table)
CodonsCytB$Sequence <- as.character(CytBsequensesonlyATG)
## count the number of A T G C in neutral positions of each gene (8 synon fourlfold codons
CodonsCytB$NeutralA = as.numeric(CodonsCytB$CTA) + as.numeric(CodonsCytB$GTA) + as.numeric(CodonsCytB$TCA) + as.numeric(CodonsCytB$CCA)  + as.numeric(CodonsCytB$ACA)  + as.numeric(CodonsCytB$GCA)  + as.numeric(CodonsCytB$CGA)  + as.numeric(CodonsCytB$GGA)
CodonsCytB$NeutralT = as.numeric(CodonsCytB$CTT) + as.numeric(CodonsCytB$GTT) + as.numeric(CodonsCytB$TCT) + as.numeric(CodonsCytB$CCT)  + as.numeric(CodonsCytB$ACT)  + as.numeric(CodonsCytB$GCT)  + as.numeric(CodonsCytB$CGT)  + as.numeric(CodonsCytB$GGT)
CodonsCytB$NeutralG = as.numeric(CodonsCytB$CTG) + as.numeric(CodonsCytB$GTG) + as.numeric(CodonsCytB$TCG) + as.numeric(CodonsCytB$CCG)  + as.numeric(CodonsCytB$ACG)  + as.numeric(CodonsCytB$GCG)  + as.numeric(CodonsCytB$CGG)  + as.numeric(CodonsCytB$GGG)
CodonsCytB$NeutralC = as.numeric(CodonsCytB$CTC) + as.numeric(CodonsCytB$GTC) + as.numeric(CodonsCytB$TCC) + as.numeric(CodonsCytB$CCC)  + as.numeric(CodonsCytB$ACC)  + as.numeric(CodonsCytB$GCC)  + as.numeric(CodonsCytB$CGC)  + as.numeric(CodonsCytB$GGC)

CodonsCytB$Taxonomy <- strsplit(CodonsCytB$ID, ";")
for (i in 1:nrow(CodonsCytB)) {
  CodonsCytB$Type[i] <- CodonsCytB$Taxonomy[[i]][3]
  CodonsCytB$Class[i] <- CodonsCytB$Taxonomy[[i]][4]
  CodonsCytB$Species[i] <- CodonsCytB$Taxonomy[[i]][length(CodonsCytB$Taxonomy[[i]])]
}

CodonsCytB$Species <- gsub("_[0-9]+", "", CodonsCytB$Species)
CodonsCytB$Class <- gsub("_[0-9]+", "", CodonsCytB$Class)
CodonsCytB$Type <- gsub("_[0-9]+", "", CodonsCytB$Type)
CodonsCytB$Taxonomy <- NULL


#write.table(CodonsCytB, file = here(data_dir, "Codons_of_CytB_gene_Chordata.txt"))

