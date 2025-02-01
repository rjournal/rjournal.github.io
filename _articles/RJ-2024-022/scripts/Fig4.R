setwd("./scripts/") # set working directory to scripts folder

library(DECIPHER)
sessionInfo() # verify DECIPHER v3.0.0+

reads <- readDNAStringSet("../data/SRR5098782.fas.gz")
reference <- readDNAStringSet("../data/BioProjectPRJNA177353.fas.gz")

index <- IndexSeqs(reference, K=8L, processors=NULL)
hits <- SearchIndex(reads, index, perPatternLimit=100, processors=NULL)
aligned <- AlignPairs(reads, reference, hits, processors=NULL)
PID <- 100*aligned$Matches/aligned$AlignmentLength
coverage <- 100*(aligned$Matches + aligned$Mismatches)/width(reads)[aligned$Pattern]

# plot the results
dev.new(height=3, width=7)
layout(matrix(1:2, nrow=1))
colors <- colorRampPalette(c("#FF000001", "#00000001"), alpha=TRUE)(101 + 1)
par(mar=c(4, 4, 1, 1.5))
plot(hits$Score, PID, pch=46, ylim=c(0, 100), ylab="", xlab="", col=colors[coverage + 1])
mtext("Alignment percent identity", 2, 2.3)
mtext(substitute(paste(italic("SearchIndex"), " score")), 1, 2.3)
par(mar=c(4, 1.5, 1, 4))
plot(aligned$Score, PID, pch=46, ylim=c(0, 100), ylab="", xlab="", col=colors[coverage + 1])
mtext(substitute(paste(italic("AlignPairs"), " score")), 1, 2.3)
xy <- par("usr")
offset <- c(0.05, 0.1, 0.1, 0.9, 0.12, 0.18, 0.5)
x1 <- xy[2] + offset[1]*(xy[2] - xy[1])
x2 <- xy[2] + offset[2]*(xy[2] - xy[1])
y1 <- xy[3] + offset[3]*(xy[4] - xy[3])
y2 <- xy[3] + offset[4]*(xy[4] - xy[3])
rect(x1,
	seq(y1, y2, length.out=length(colors) + 1)[-(length(colors) + 1)],
	x2,
	seq(y1, y2, length.out=length(colors) + 1)[-1],
	col=substring(colors, 1, 7),
	border=NA,
	xpd=TRUE)
rect(x1, y1, x2, y2, xpd=TRUE)
x3 <- xy[2] + offset[5]*(xy[2] - xy[1])
segments(x2, c(y1, y2), x3, c(y1, y2), xpd=TRUE)
text(x2, y1, "0", pos=4, xpd=TRUE)
text(x2, y2, "100", pos=4, xpd=TRUE)
x4 <- xy[2] + offset[6]*(xy[2] - xy[1])
y3 <- xy[3] + offset[7]*(xy[4] - xy[3])
text(x4, y3, "Coverage (%)", srt=90, xpd=TRUE)
