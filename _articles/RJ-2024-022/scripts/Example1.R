setwd("./scripts/") # set working directory to scripts folder

library(DECIPHER)
sessionInfo() # verify DECIPHER v3.0.0+

PID <- 80 # goal percent identity

promoters <- readDNAStringSet("../data/Eco_promoters_v1.fas.gz")
genome <- readDNAStringSet("../data/Eco_genome.fas.gz")
genome <- c(genome, reverseComplement(genome)) # search forward and reverse stands

l <- unique(width(promoters))
if (length(l) != 1L)
	stop("Example depends on having a single sequence length.")
max_mismatch <- l - l*PID/100
print(max_mismatch)
max_mismatch <- ceiling(max_mismatch)

t1 <- system.time(index <- IndexSeqs(genome, sensitivity=0.9, percentId=PID, patternLength=l, processors=NULL))[3L]
index
t2 <- system.time(hits <- SearchIndex(promoters, index, perSubjectLimit=0, processors=NULL))[3L]
t3 <- system.time(hits <- SearchIndex(promoters, index, perSubjectLimit=0, processors=1))[3L]
nrow(hits)

times <- rep(NA_real_, length(promoters))
starts <- vector("list", width(genome)[1L])
pBar <- txtProgressBar(style=3L)
for (i in seq_along(promoters)) {
	times[i] <- system.time({
		v1 <- matchPattern(promoters[[i]], genome[[1L]], max.mismatch=max_mismatch, with.indels=TRUE)
		v2 <- matchPattern(promoters[[i]], genome[[2L]], max.mismatch=max_mismatch, with.indels=TRUE)
		})[3L]
	s1 <- start(v1)
	s2 <- start(v2)
	for (j in seq_along(s1))
		starts[[s1[j]]] <- c(starts[[s1[j]]], i)
	for (j in seq_along(s2))
		starts[[s2[j]]] <- c(starts[[s2[j]]], -i)
	setTxtProgressBar(pBar, i/length(promoters))
}

t4 <- system.time({
	p <- PDict(promoters, max.mismatch=max_mismatch)
	v3 <- matchPDict(p, genome[[1L]], max.mismatch=max_mismatch)
	v4 <- matchPDict(p, genome[[2L]], max.mismatch=max_mismatch)
	})[3L]
sum(lengths(v3) + lengths(v4))

mean(times) # average time taken via matchPattern per query
sum(lengths(starts)) # number of matches identified

sum(times)/t2
sum(times)/t3
sum(times)/t4

# determine fraction of SearchIndex hits overlapping matchPattern
found <- logical(nrow(hits))
for (i in seq_along(found)) {
	region <- hits$Position[[i]][3, 1]
	region <- seq(region - l + 1, region + l - 1)
	region <- region[region > 0 & region < width(genome)[1]]
	if (hits$Subject[i] == 1L) { # forward strand
		found[i] <- hits$Pattern[i] %in% unlist(starts[region])
	} else { # reverse strand
		found[i] <- (-hits$Pattern[i]) %in% unlist(starts[region])
	}
}
mean(found) # fraction of hits in starts

# determine the index of patterns present in matchPattern matches but missing from SearchIndex hits
w <- which(lengths(starts) > 0)
for (i in seq_along(w)) {
	I <- starts[[w[i]]]
	region <- seq(w[i] - l + 1, w[i] + l - 1)
	for (j in seq_along(I)) {
		J <- I[[j]]
		if (J > 0) {
			m <- which(J == hits$Pattern & hits$Subject == 1L)
		} else {
			m <- which(-J == hits$Pattern & hits$Subject == 2L)
		}
		m <- sapply(hits$Position[m], `[`, 3L)
		if (sum(m %in% region) == 0L)
			cat("\nmissed:", J)
	}
}

# determine the fraction of matchPDict matches missing from matchPattern matches
found <- integer(length(v3))
for (i in seq_along(found)) {
	s <- start(v3[[i]])
	for (j in seq_along(s)) {
		region <- s[j]
		region <- seq(region - l + 1, region + l - 1)
		region <- region[region > 0 & region < width(genome)[1]]
		if (i %in% unlist(starts[region]))
			found[i] <- found[i] + 1L
	}
	s <- start(v4[[i]])
	for (j in seq_along(s)) {
		region <- s[j]
		region <- seq(region - l + 1, region + l - 1)
		region <- region[region > 0 & region < width(genome)[1]]
		if ((-i) %in% unlist(starts[region]))
			found[i] <- found[i] + 1L
	}
}
sum(found)/sum(lengths(v3), lengths(v4)) # fraction of matchPDict matches in matchPattern
