library(DECIPHER)
sessionInfo() # verify DECIPHER v3.0.0+

options(timeout=3600) # might be needed for downloads

other_dna <- readDNAStringSet("ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/002/035/GCF_000002035.6_GRCz11/GCF_000002035.6_GRCz11_cds_from_genomic.fna.gz")
human_dna <- readDNAStringSet("ftp://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/009/914/755/GCF_009914755.1_T2T-CHM13v2.0/GCF_009914755.1_T2T-CHM13v2.0_cds_from_genomic.fna.gz")

# parameters
processors <- NULL # NULL for all available processors

# order the sequences by descending length
other_dna <- other_dna[order(width(other_dna), decreasing=TRUE)]
human_dna <- human_dna[order(width(human_dna), decreasing=TRUE)]

# translate coding sequences to proteins
other_aa <- suppressWarnings(translate(other_dna, if.fuzzy.codon="solve"))
human_aa <- suppressWarnings(translate(human_dna, if.fuzzy.codon="solve"))

# dereplicate sequences using protein sequences
distinct <- !duplicated(other_aa)
other_aa <- other_aa[distinct]
other_dna <- other_dna[distinct]
distinct <- !duplicated(human_aa)
human_aa <- human_aa[distinct]
human_dna <- human_dna[distinct]

# dereplicate isoforms and sort by protein name
prot_name <- gsub("^.*\\[protein=(.*?)].*$", "\\1", names(other_dna))
o <- order(prot_name)
distinct <- o[!duplicated(prot_name[o])]
other_aa <- other_aa[distinct]
other_dna <- other_dna[distinct]
prot_name <- gsub("^.*\\[protein=(.*?)].*$", "\\1", names(human_dna))
o <- order(prot_name)
distinct <- o[!duplicated(prot_name[o])]
human_aa <- human_aa[distinct]
human_dna <- human_dna[distinct]

# dereplicate isoforms by gene name
gene_name <- gsub("^.*\\[gene=(.*?)].*$", "\\1", names(other_dna))
distinct <- !duplicated(gene_name)
other_aa <- other_aa[distinct]
other_dna <- other_dna[distinct]
gene_name <- gsub("^.*\\[gene=(.*?)].*$", "\\1", names(human_dna))
distinct <- !duplicated(gene_name)
human_aa <- human_aa[distinct]
human_dna <- human_dna[distinct]

ReciprocalSearch <- function(seqs1, seqs2, k, s, CPUs=NULL) {
	# perform bidirectional searching
	index1 <- IndexSeqs(seqs2, K=k, step=s, verbose=FALSE, processors=CPUs)
	hits1 <- SearchIndex(seqs1, index1, perPatternLimit=1, verbose=FALSE, processors=CPUs)
	index2 <- IndexSeqs(seqs1, K=k, step=s, verbose=FALSE, processors=CPUs)
	hits2 <- SearchIndex(seqs2, index2, perPatternLimit=1, verbose=FALSE, processors=CPUs)
	# intersect the best hits
	m <- match(hits1$Pattern, hits2$Subject)
	w <- which(!is.na(m))
	w <- w[hits1$Subject[w] == hits2$Pattern[m[w]]]
	hits1[w,]
}

results <- setNames(vector("list", 2L), c("DNA", "AA"))
for (i in 1:2) { # nucleotide and proteins
	if (i == 1L) { # nucleotides
		cat("Nuceotides:")
		human <- human_dna
		other <- other_dna
		Ks <- 9:11
		steps <- seq(1, 9, 4)
	} else { # proteins
		cat("\n\nProteins:")
		human <- human_aa
		other <- other_aa
		Ks <- 5:7
		steps <- seq(1, 5, 2)
	}
	human_protein_name <- gsub("^.*\\[protein=(.+?)].*$", "\\1", names(human))
	other_protein_name <- gsub("^.*\\[protein=(.+?)].*$", "\\1", names(other))
	times <- matrix(NA_real_, nrow=length(Ks), ncol=length(steps), dimnames=list(Ks, steps))
	annotations <- matches <- possibles <- matrix(NA_integer_, nrow=length(Ks), ncol=length(steps), dimnames=list(Ks, steps))
	for (j in seq_along(steps)) {
		cat("\n\ns =", steps[j])
		for (k in seq_along(Ks)) {
			cat("\nk =", Ks[k])
			times[k, j] <- system.time(res <- ReciprocalSearch(human, other, Ks[k], steps[j], processors))[3L]
			matches[k, j] <- nrow(res)
			annotations[k, j] <- sum(human_protein_name[res$Pattern] == other_protein_name[res$Subject])
			possibles[k, j] <- sum(human_protein_name[res$Pattern] %in% other_protein_name[res$Subject])
		}
	}
	results[[i]] <- list(times=times, matches=matches, annotations=annotations, possibles=possibles)
}
print(sum(unique(other_protein_name) %in% unique(human_protein_name))) # shared distinct names

# plot the results
dev.new(height=4, width=7)
layout(matrix(1:2, nrow=1))
TOT <- max(results[[1L]][[2L]], results[[2L]][[2L]], na.rm=TRUE)
SUB <- max(results[[1L]][[3L]], results[[2L]][[3L]], na.rm=TRUE)
for (i in seq_along(results)) {
	if (i == 1L) {
		par(mar=c(4, 5, 4, 2))
	} else {
		par(mar=c(4, 0, 4, 7))
	}
	tot <- t(results[[i]][[2L]]) # total matches
	dims <- dim(results[[i]][[2L]])
	dimnames <- dimnames(results[[i]][[2L]])
	sub <- t(results[[i]][[3L]]) # matching subset
	b <- barplot(matrix(c(sub, tot - sub), 2, byrow=TRUE),
		col=c("darkgreen", "lightgreen"),
		ylim=c(0, TOT*1.2),
		border=NA,
		space=rep(c(0.8, rep(0.2, dims[2L] - 1)), dims[1L]),
		ylab=ifelse(i == 1L, "Predicted orthologs", ""),
		yaxt=ifelse(i == 1L, "s", "n"),
		main=ifelse(i == 1L, "Nucleotide predictions", "Protein predictions"))
	u <- par("usr")
	Y <- -0.15*u[4]
	text(x=b, y=Y, dimnames[[2L]], xpd=TRUE)
	if (i == 1L) {
		text(u[1] - strwidth("Step size:")/2, Y, "Step size:", xpd=TRUE)
	} else {
		axis(2L, labels=FALSE)
	}
	Y <- Y - strheight("")
	X1 <- b[(seq_len(dims[1L]) - 1L)*dims[2L] + 1L]
	X2 <- b[seq_len(dims[1L])*dims[2L]]
	segments(X1, Y, X2, Y, xpd=TRUE)
	Y <- Y - strheight("")
	text(x=(X1 + X2)/2, y=Y, dimnames[[1L]], xpd=TRUE)
	pval <- -dbinom(results[[i]]$annotations,
		results[[i]]$possibles,
		1/results[[i]]$matches,
		log=TRUE)*log10(exp(1)) # convert to -log10(p)
	for (j in seq_along(pval))
		text(x=b[j] - 0.5,
			y=t(results[[i]]$matches)[j] + 100,
			bquote("p="*10^-.(round(t(pval)[j]))),
			srt=90,
			pos=4,
			cex=0.7,
			xpd=TRUE)
	if (i == 1L) {
		text(u[1] - strwidth("K-mer size:")/2, Y, "K-mer size:", xpd=TRUE)
	} else {
		legend(u[2L],
			u[4L]*2/3,
			c("Unmatched", "Matched"),
			fill=c("lightgreen", "darkgreen"),
			xpd=TRUE,
			title="Annotations",
			bty="n")
	}
}
