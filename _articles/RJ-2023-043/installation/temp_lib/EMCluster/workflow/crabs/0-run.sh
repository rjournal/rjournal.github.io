#!/bin/sh

rm -rf data plot
mkdir data
mkdir plot

Rscript 2-em.Rnd.r
Rscript 3-adjR.r
Rscript 3-test.r
Rscript 4-plot_pp.r
Rscript 5-plot_contour.r
Rscript 6-qmap.r

