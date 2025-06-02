library(here)
library(tidyverse)

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

library(seqinr)
library(ORFik)
library(Biostrings) 
library(systemPipeR)

# Set paths
src_dir <- here("code")
data_dir <- here("data")
output_dir <- here("output")
plots_dir <- here(output_dir, "figures")
tables_dir <- here(output_dir, "tables")




  
seqs <- readDNAStringSet(here(data_dir, "MIDORI2_LONGEST_NUC_GB265_Cytb_BLAST.fasta"))
#problem_seqs <- vapply(seqs, function(x) any(uniqueLetters(x) %in% c("-", "N", "Y", "R")), logical(1))
#names(seqs)[problem_seqs]
seqs <- replaceAmbiguities(seqs)
# Predict ORFs with ATG start codons
orfs <- predORF(seqs,
                n = 1,
                mode = "orf",
                type = "df",
                startcodon = "ATG",
                strand = "sense")


extracted_seqs <- data.frame()

for (i in seq_along(orfs$seqnames)) {
  seqname <- as.character(orfs$seqnames[i])
  subseq <- subseq(seqs[[seqname]], orfs$start[i], orfs$end[i])
  df <- data.frame("Taxonomy" = seqname, "Sequence" = as.character(subseq), "Length" = orfs$width[i])
  extracted_seqs <- rbind(extracted_seqs, df)
}


Less <- extracted_seqs[extracted_seqs$Length > 99,]


OnlyChordata <- orfs %>% dplyr::filter(grepl("Chordata_7711", seqnames))
OnlyFishes <- OnlyChordata %>% dplyr::filter(grepl("Actinopteri_186623", Name)) 
OnlySharks <- OnlyChordata %>% dplyr::filter(grepl("Chondrichthyes_7777", Name))
FishesCytB <- rbind(OnlySharks, OnlyFishes)

write.csv(FishesCytB, here(data_dir, "MIDORI_WG_CYTB_FISHES.csv"))
