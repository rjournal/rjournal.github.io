setwd("./scripts/") # set working directory to scripts folder

library(DECIPHER)
sessionInfo() # verify DECIPHER v3.0.0+

mmseqs_path <- "mmseqs"
blastdb_path <- "makeblastdb"
blastp_path <- "blastp"

PROCs <- DECIPHER:::.detectCores() # number of processors
tf <- tempfile() # temporary file
td <- tempdir() # temporary directory

seqs <- readAAStringSet("../data/astral-scopedom-seqres-gd-sel-gs-bib-100-2.08.fa.gz")
seqs <- unique(seqs) # remove duplicate sequences

family <- gsub("^.+? (.+?\\..+?\\..+?\\..+?) .+$", "\\1", names(seqs))
groups <- tapply(seq_along(family), family, c)
select <- sapply(groups[lengths(groups) > 1L], `[`, 1L)
ls <- lengths(groups)[family[select]] - 1L # number of possible TPs per group

PID <- rep(NA_real_, length(seqs))
pBar <- txtProgressBar(style=3L)
for (i in seq_along(groups)) {
	group <- groups[[i]]
	one <- DECIPHER:::.subset(seqs, group[1L])
	j <- 2L
	while (j < length(group)) {
		two <- DECIPHER:::.subset(seqs, group[j])
		ali <- AlignProfiles(one, two)
		PID[group[j]] <- 100*(1 - DistanceMatrix(ali, type="dist", includeTerminalGaps=TRUE, verbose=FALSE)[1L])
		j <- j + 1L
	}
	setTxtProgressBar(pBar, i/length(groups))
}
close(pBar)
bins <- seq(0, 100, 5) # PID bins for plotting
TOT <- tabulate(.bincode(PID, bins, include.lowest=TRUE))

names(seqs) <- seq_along(seqs)
test <- c(seqs[select], reverse(seqs[select])) # combine test and decoy sets
unselected <- seq_along(seqs)[-select]
train <- seqs[unselected]

getAUCs <- function(hits) {
	AUC <- numeric(length(select))
	TPs <- logical(length(seqs))
	for (j in seq_along(select)) {
		TP <- which(hits$Pattern == j)
		FP <- which(hits$Pattern == j + length(select))
		if (length(FP) > 0L)
			TP <- TP[hits$Score[TP] > max(hits$Score[FP])]
		TP <- TP[family[unselected[hits$Subject[TP]]] == family[select[[j]]]]
		AUC[j] <- length(TP)/ls[j]
		TPs[unselected[hits$Subject[TP]]] <- TRUE
	}
	list(AUC, TPs)
}

### Run DECIPHER

Ks <- 4:6
steps <- c(1, 4)
times_DECIPHER_without <- AUCs_DECIPHER_without <- times_DECIPHER_with <- AUCs_DECIPHER_with <- matrix(NA_real_, length(Ks), length(steps), dimnames=list(Ks, steps))
TPRs_DECIPHER_without <- TPRs_DECIPHER_with <- array(NA_real_, dim=c(length(TOT), length(Ks), length(steps)))
for (i in seq_along(steps)) {
	cat("\nstep =", steps[i])
	for (k in seq_along(Ks)) {
		cat("\nk =", Ks[k], "\n")
		gc() # for accurate timing
		index <- IndexSeqs(train, K=Ks[k], step=steps[i], processors=PROCs)
		times_DECIPHER_with[k, i] <- system.time({
				hits <- SearchIndex(test, index, train, scoreOnly=TRUE, processors=PROCs)
			})[3L]
		res <- getAUCs(hits)
		AUCs_DECIPHER_with[k, i] <- mean(res[[1L]])
		TPRs_DECIPHER_with[, k, i] <- tabulate(.bincode(PID[res[[2L]]], bins, include.lowest=TRUE))/TOT
		times_DECIPHER_without[k, i] <- system.time({
				hits <- SearchIndex(test, index, scoreOnly=TRUE, processors=PROCs) # omit subject sequences
			})[3L]
		res <- getAUCs(hits)
		AUCs_DECIPHER_without[k, i] <- mean(res[[1L]])
		TPRs_DECIPHER_without[, k, i] <- tabulate(.bincode(PID[res[[2L]]], bins, include.lowest=TRUE))/TOT
	}
}

### Run MMseqs2

writeXStringSet(setNames(train, seq_along(train)), tf)
res <- system(paste(mmseqs_path,
		"createdb",
		tf,
		"",
		paste(td, "targetDB", sep="/")),
	intern=TRUE)
res <- system(paste(mmseqs_path,
		"createindex",
		paste(td, "targetDB", sep="/"),
		paste(td, "tmp", sep="/")),
	intern=TRUE)

writeXStringSet(setNames(test, seq_along(test)), tf)

Ss <- sort(c(5.7, seq(1, 10, 1)))
times_MMseqs2 <- AUCs_MMseqs2 <- rep(NA_real_, length(Ss))
TPRs_MMseqs2 <- matrix(NA_real_, length(TOT), length(Ss))
for (i in seq_along(Ss)) {
	cat("\ns =", Ss[i])
	gc() # for accurate timing
	times_MMseqs2[i] <- system.time({
			res <- system(paste(mmseqs_path,
				"easy-search",
				tf,
				"--threads",
				PROCs,
				"-s",
				Ss[i],
				paste(td, "targetDB", sep="/"),
				paste(td, "output.txt", sep="/"),
				paste(td, "tmp", sep="/")),
			intern=TRUE)
		})[3L]
	res <- readLines(paste(td, "output.txt", sep="/"))
	
	hits <- strsplit(res, "\t", fixed=TRUE)
	hits <- data.frame(Pattern=as.integer(sapply(hits, `[`, 1L)),
		Subject=as.integer(sapply(hits, `[`, 2L)),
		Score=-log(as.numeric(sapply(hits, `[`, 11L))))
	hits <- hits[order(hits$Score, decreasing=TRUE),]
	hits <- hits[!duplicated(paste(hits$Pattern, hits$Subject)),]
	
	res <- getAUCs(hits)
	AUCs_MMseqs2[i] <- mean(res[[1L]])
	TPRs_MMseqs2[, i] <- tabulate(.bincode(PID[res[[2L]]], bins, include.lowest=TRUE))/TOT
}

### Run BLAST

# make database
writeXStringSet(setNames(train, seq_along(train)), tf)
res <- system(paste(blastdb_path,
		"-in",
		tf,
		"-dbtype prot -title temp -out",
		td),
	intern=TRUE)

writeXStringSet(setNames(test, seq_along(test)), tf)

gc() # for accurate timing
time_BLAST <- system.time({
		res <- system(paste(blastp_path,
				"-query",
				tf,
				"-num_threads ",
				PROCs,
				"-outfmt 6 -db",
				td),
			intern=TRUE)
	})[3L]

hits <- strsplit(res, "\t", fixed=TRUE)
hits <- data.frame(Pattern=as.integer(sapply(hits, `[`, 1L)),
	Subject=as.integer(sapply(hits, `[`, 2L)),
	Score=-log(as.numeric(sapply(hits, `[`, 11L))))
hits <- hits[order(hits$Score, decreasing=TRUE),]
hits <- hits[!duplicated(paste(hits$Pattern, hits$Subject)),]

res <- getAUCs(hits)
AUC_BLAST <- mean(res[[1L]])
TPR_BLAST <- tabulate(.bincode(PID[res[[2L]]], bins, include.lowest=TRUE))/TOT

### Plot the results

dev.new(height=5, width=10)
layout(matrix(1:2, ncol=2))
matplot(NA,
	log="y",
	xlab="Average AUC1 up to the first false positive",
	ylab=paste0("Speedup relative to BLAST (", PROCs, " threads)"),
	yaxt="n",
	xlim=range(AUCs_DECIPHER_with, AUCs_DECIPHER_without, AUC_BLAST, AUCs_MMseqs2),
	ylim=range(time_BLAST/c(times_DECIPHER_with, times_DECIPHER_without, times_MMseqs2)))
abline(h=1, lty=3, col="gray")
matlines(AUCs_DECIPHER_with,
	time_BLAST/times_DECIPHER_with,
	type="b",
	pch=NA,
	lwd=2,
	col=seq_along(steps))
matlines(AUCs_DECIPHER_without,
	time_BLAST/times_DECIPHER_without,
	type="b",
	pch=NA,
	lwd=2,
	col=seq_along(steps) + length(steps))
text(AUCs_DECIPHER_with, time_BLAST/times_DECIPHER_with, rep(Ks, length(steps)), col=rep(seq_along(steps), each=length(Ks)))
matlines(AUCs_MMseqs2,
	time_BLAST/times_MMseqs2,
	lwd=2,
	lty=3,
	col=7)
points(AUCs_MMseqs2[Ss == 5.7],
	time_BLAST/times_MMseqs2[Ss == 5.7],
	col=7,
	lwd=2)
text(AUCs_DECIPHER_without, time_BLAST/times_DECIPHER_without, rep(Ks, length(steps)), col=rep(seq_along(steps) + length(steps), each=length(Ks)))
for (i in -1:3)
	axis(2, 10^i, label=bquote(10^.(i)), las=2)
axis(2, sapply(1:9, function(x) x*10^(-1:3)), labels=FALSE, tcl=-0.2)
points(AUC_BLAST, 1, pch=8, col=6)
mtext("(A)", 2, padj=-19, line=2.9, las=1, font=2)
xy <- par("usr")
offset <- c(0.05, 6.5, 1.2)
l <- legend(xy[1] + offset[1]*(xy[2] - xy[1]),
	offset[2]*10^xy[4],
	legend=rep("", 2*length(steps)),
	lwd=2,
	lty=rep(seq_along(steps), 2),
	col=seq_len(2*length(steps)),
	ncol=2,
	bty="n",
	xpd=TRUE,
	title="DECIPHER",
	title.adj=0.4)
text(l$text$x[1], 10^l$text$y[length(l$text$y)], substitute(paste("with      w/out providing ", italic("subject"))), adj=c(0.2, 1.6), xpd=TRUE)
text(l$rect$left, 10^l$text$y[seq_along(steps)], steps, xpd=TRUE)
text(l$rect$left, 10^mean(l$text$y[seq_along(steps)]), "Step size:", pos=2, xpd=TRUE)
l <- legend(l$rect$left + offset[3]*l$rect$w,
	offset[2]*10^xy[4],
	legend="",
	pch=1,
	lwd=2,
	lty=3,
	col=7,
	bty="n",
	xpd=TRUE,
	title="MMseqs2")
l <- legend(l$rect$left + offset[3]*l$rect$w,
	offset[2]*10^xy[4],
	legend="",
	pch=8,
	col=6,
	bty="n",
	xpd=TRUE,
	title="BLAST")

# plot sensitivity versus PID

m <- matrix(c(TPRs_DECIPHER_with[,, which(steps == 1)],
		TPRs_MMseqs2[, which(Ss == 5.7)],
		TPR_BLAST),
	ncol=ncol(TPRs_DECIPHER_with) + 2)
b <- barplot(t(m[bins[-1] <= 70,]),
	beside=TRUE,
	col=c(rep(1, length(Ks)), 7, 6),
	density=c(seq(100, 20, length.out=length(Ks)), -1, -1),
	ylab="Probability of detection")
labels <- paste0(c("[", rep("(", length(bins) - 2)), bins[-length(bins)], ", ", bins[-1], "]")
for (i in seq_len(ncol(b))) {
	axis(1, b[ceiling(nrow(b)/2), i], labels[i], las=2, col.ticks=1)
	axis(1, b[c(1, nrow(b)), i] + c(-0.5, 0.5), labels=FALSE, tck=0)
}
mtext("Percent identity (%)", 1, line=4)
mtext("(B)", 2, padj=-19, line=2.9, las=1, font=2)
xy <- par("usr")
offset <- c(0.18, 1.27)
l <- legend(xy[1] + offset[1]*(xy[2] - xy[1]),
	offset[2]*xy[4],
	legend=Ks,
	fill=1,
	density=seq(100, 20, length.out=length(Ks))/1.1,
	ncol=3,
	bty="n",
	xpd=TRUE,
	title="DECIPHER")
text((l$rect$left + l$text$x[1])/2, l$text$y[1], "k-mer length:", xpd=TRUE, pos=2)
l <- legend(l$rect$left + l$rect$w,
	offset[2]*xy[4],
	"",
	fill=7,
	bty="n",
	xpd=TRUE,
	title="MMseqs2")
l <- legend(l$rect$left + l$rect$w,
	offset[2]*xy[4],
	"",
	fill=6,
	bty="n",
	xpd=TRUE,
	title="BLAST")
