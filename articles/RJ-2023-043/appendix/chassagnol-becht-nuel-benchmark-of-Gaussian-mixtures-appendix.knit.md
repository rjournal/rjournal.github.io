---
title: "Supplementary Notes on Gaussian Mixture Models in R"
date: "2023-11-01"
draft: true
author:  
  - name: Bastien Chassagnol
    affiliation: Laboratoire de Probabilités, Statistiques et Modélisation (LPSM), UMR CNRS 8001
    address:
    - 4 Place Jussieu Sorbonne Université
    - 75005, Paris, France
    orcid: 0000-0002-8955-2391
    email:  bastien_chassagnol@laposte.net
  - name: Antoine Bichat
    affiliation: Les Laboratoires Servier
    address:
    - 50 Rue Carnot
    - 92150, Suresnes, France
    url: https://rdrr.io/github/abichat/abutils/
    orcid: 0000-0001-6599-7081
    email: antoine.bichat@servier.com
  - name: Cheïma Boudjeniba
    affiliation: Systems Biology Group, Dept. of Computational Biology, Institut Pasteur
    address:
    - 25 Rue du Dr Roux
    - 75015 Paris
    email: cheima.boudjeniba@servier.com
  - name: Pierre-Henri Wuillemin
    affiliation: Laboratoire d'Informatique de Paris 6 (LIP6), UMR 7606
    address:
    - 4 Place Jussieu Sorbonne Université
    - 75005, Paris, France
    url: http://www-desir.lip6.fr/~phw/
    orcid: 0000-0003-3691-4886
    email:  pierre-henri.wuillemin@lip6.fr
  - name: Mickaël Guedj
    affiliation: Les Laboratoires Servier
    address:
    - 50 Rue Carnot
    - 92150, Suresnes, France
    url: https://michaelguedj.github.io/
    orcid: 0000-0001-6694-0554
    email: mickael.guedj@gmail.com
  - name: David Gohel
    affiliation: ArData
    address:
    - 59 rue Voltaire
    - 92800, PUTEAUX, France
    url: https://www.ardata.fr/expertise-r/
    orcid: 0000-0003-2837-8884
    email: david.gohel@ardata.fr
  - name: Gregory Nuel
    affiliation: Laboratoire de Probabilités, Statistiques et Modélisation (LPSM), UMR CNRS 8001
    address:
    - 4 Place Jussieu Sorbonne Université
    - 75005, Paris, France
    url: http://nuel.perso.math.cnrs.fr/
    orcid: 0000-0001-9910-2354
    email:  Gregory.Nuel@math.cnrs.fr
  - name: Etienne Becht
    affiliation: Les Laboratoires Servier
    address:
    - 50 Rue Carnot
    - 92150, Suresnes, France
    orcid: 0000-0003-1859-9202
    email: etienne.becht@servier.com
type: package
header-includes:
  - \usepackage[most]{tcolorbox}
  - \usepackage{booktabs}
  - \usepackage{bm, amsmath}
  - \usepackage{array}
  - \usepackage{multirow}
  - \usepackage{wrapfig}
  - \usepackage{float}
  - \usepackage{colortbl}
  - \usepackage{hhline}
  - \usepackage{xcolor}
  - \usepackage{longtable}
  - \usepackage{tabu}
  - \usepackage{adjustbox}
  - \usepackage{threeparttable}
  - \usepackage{makecell}
  - \def\greentick{\includegraphics[scale=0.05]{figures/green_tick.png}}
  - \def\redcross{\includegraphics[scale=0.05]{figures/red_cross.png}}
  - \newtcolorbox{blackbox}[1]{colback=white, colframe=black, coltext=black, boxsep=1.5pt, arc=4pt, before=\centering, title=#1}
  - \newenvironment{cols}[1][]{}{} \newenvironment{col}[1]{\begin{minipage}{#1}\ignorespaces}{\end{minipage} \ifhmode\unskip\fi
  - \aftergroup\useignorespacesandallpars} \def\useignorespacesandallpars#1\ignorespaces\fi{#1\fi\ignorespacesandallpars} \makeatletter  
  - \def\ignorespacesandallpars{\@ifnextchar\par {\expandafter\ignorespacesandallpars\@gobble} {} } \makeatother
output: 
  rjtools::rjournal_article:
    self_contained: yes
    toc: no
    number_sections: false
bibliography: chassagnol-becht-nuel-benchmark-of-Gaussian-mixtures-appendix.bib
---














































































































































































