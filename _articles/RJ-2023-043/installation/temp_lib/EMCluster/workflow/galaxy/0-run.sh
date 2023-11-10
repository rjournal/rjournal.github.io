#!/bin/sh

rm -rf data plot
mkdir data
mkdir plot

Rscript 2-em.Rnd.r
Rscript 3-test.r
Rscript 4-plot.r
Rscript 6-qmap.r

