---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

Editor   
The R Journal  
\bigskip

Dear Professor Urbánek,
\bigskip

Thank you again for editing our manuscript "Analysis of the Results of Metadynamics Simulations
by metadynminer and metadynminer3D". Below are our answers to your questions (questions are in bold).

**The code for replicating results in the paper should be included as supplementary material.
The original submission has some such in the figures directory, the the revision did not. Also
the code did not match the paper: Fig 2 does not match the output listed in the paper, the
listing in the paper would produce 1d-hill, whereas the the shown figure uses 2d hill from
acealanme and not the HILLS file. Further Fig-2 should be improved to be more meaningful, such as
plot(acealanme, xlab="phi", ylab="psi", pch=19, cex=0.5, col=gray(0, 0.1))**

We apologize for the omission of this supplementary material. In the revised version we included it
in a similar form as in the first submission.

We also double checked that the code in the manuscript matches the figures.

We decided to include all input files (*HILLS*, *HILLS3d* and *COLVAR*) into supplementary material
rather than using identical in-package data (*acealanme*, *acealanme3d*). Our motivation is to
attract the metadynamics and molecular simulation community to use R. Therefore we wanted to show
how to load and then analyze the output of their simulations. We added a sentence indicating that
*hillsfile* and *acealanme* are identical.

We made a correction to the script generating a video sequence (*tfes* was accidentally replaced
by *acealanme*).

We changed the command to generate Figure 2 as suggested by you.

**The code listed in the paper should be cleaned up: it should not use deprecated constructs like
T/F variables where TRUE/FALSE values were meant. The formatting should be consistent: spaces around
assignment operators and remove superfluous book-keeping lines (such as opening or closing of
devices).**

We fixed all T/F. We fixed formatting.

Regarding opening and closing of devices, in the manuscript we use it just once in the script
generating a video sequence. We would prefer to keep it there because, as already mentioned, our
motivation is to attract the metadynamics and molecular simulation community to use R. Opening and
closing devices and use of % operator in file name is familiar to experienced R users. For
inexperienced ones this example may be useful and inspiring.

**The reweighting code should be re-written in proper R using sapply() instead of vector
concatenation and the second loop is unnecessary as ix, iy can be computed directly on the whole
dataset. Ideally, the use of the file COLVAR should be avoided if it can be made part of the package
since the dataset is internal to the package and thus it's unclear why another special file is needed
and would require additional explanation.**

We rewritten the code as:
```
bf <- 15
kT <- 8.314*300/1000
npoints <- 50
maxfes <- 75
outfes <- 0*fes(hillsfile, npoints=npoints)
step <- 1:50*length(hillsfile$time)/50
s1 <- sapply(step, FUN=function(x) {
          sum(exp(-fes(hillsfile, imax=x)$fes/kT))
})
s2 <- sapply(step, FUN=function(x) {
          sum(exp(-fes(hillsfile, imax=x)$fes/kT/bf))
})
ebetac <- s1/s2
cvs <- read.table("COLVAR")
nsamples <- nrow(cvs)
xlim <- c(-pi,pi)
ylim <- c(-pi,pi)
step <- (1:nsamples-1)*50/nsamples+1
ix <- npoints*(cvs[,2]-xlim[1])/(xlim[2]-xlim[1])+1
iy <- npoints*(cvs[,3]-ylim[1])/(ylim[2]-ylim[1])+1
for(i in 1:nsamples) {
  outfes$fes[ix[i],iy[i]] <- outfes$fes[ix[i],iy[i]] + exp(cvs[i,4]/kT)/ebetac[step[i]]
}
outfes$fes <- -kT*log(outfes$fes)
outfes <- outfes - min(outfes)
outfes$fes[outfes$fes>maxfes] <- maxfes
plot(outfes, xlab="phi", ylab="psi")
```

We replaced the first loop by a simple vector operation and a pair of *sapply* functions.
We replaced three of four lines in the second loop by vector operations. Unfortunately we
were not able to entirely remove the loop and we kept it with one line. This step is
reasonably fast and all alternatives were more complicated and counterintuitive, with
similar speed. As already mentioned above, we prefer to present the opening of data
files, so we kept the *COLVAR* file in supporting material. Another reason was to keep
the package small. Addition of *COLVAR* would double the size of the package. To better
introduce *COLVAR* we added a sentence:

A file containing values of collective variables and the bias potential at different snapshots
of the simulation (default filename COLVAR) is required.

Beside these changes we made minor changes in the manuscript mostly in connection to
the revisions. We also added a new grant ID for a funding grant (ELIXIR project), because
the project acknowledged in the original manuscript has finished. Since the mission of
ELIXIR is to prolong the life of data and code, rather than produce them, we believe
this change in funding statement is adequate.

The manuscript was generated and checked by rjtools. We ignored the error of using lower-case
letters in package names in the title (same issue as
https://github.com/rjournal/rjtools/issues/37) and we copied the generated metadynminer.pdf file
to RJwrapper.pdf (same issue as https://github.com/rjournal/rjtools/issues/36).

Many thanks again for your editorial work and help with the manuscript.

\bigskip
\bigskip

Yours sincerely,
    
    
    
    
Vojtěch Spiwok  
University of Chemistry and Technology, Prague  
spiwokv@vscht.cz

\bigskip

