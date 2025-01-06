---
title: Remembering Friedrich "Fritz" Leisch
date: 2024-09-18
draft: yes
author:
  - name: Bettina Grün
    affiliation: WU Wirtschaftsuniversität Wien
    address: Austria
    orcid: 0000-0001-7265-4773
    email: Bettina.Gruen@wu.ac.at
  - name: Kurt Hornik
    affiliation: WU Wirtschaftsuniversität Wien
    address: Austria
    orcid: 0000-0003-4198-9911
    email: Kurt.Hornik@R-project.org
  - name: Torsten Hothorn
    affiliation: Universität Zürich
    address: Switzerland
    orcid: 0000-0001-8301-0471
    email: Torsten.Hothorn@R-project.org
  - name: Theresa Scharl
    affiliation: BOKU University
    address: Austria
    orcid: 0000-0001-8850-3312
    email: Theresa.Scharl@boku.ac.at
  - name: Achim Zeileis
    affiliation: Universität Innsbruck
    address: Austria
    url: https://www.zeileis.org/
    orcid: 0000-0003-0918-3766
    email:  Achim.Zeileis@R-project.org
abstract: >
  This article remembers our friend and colleague Fritz Leisch (1968--2024) who
  sadly died earlier this year. Many of the readers of The R Journal will know
  Fritz as a member of the R Core Team and for many of his contributions to the
  R community. For us, the co-authors of this article, he was an important
  companion on our journey with the R project and other scientific endeavours
  over the years. In the following, we provide a brief synopsis of his career,
  present his key contributions to the R project and to the scientific community
  more generally, acknowledge his academic service, and highlight his teaching
  and mentoring achievements.
preamble: |
  \newcommand{\doi}[1]{\href{https://doi.org/#1}{\normalfont\texttt{doi:\discretionary{}{}{}{#1}}}}
bibliography: fritz.bib
output: 
  rjtools::rjournal_article:
    self_contained: yes
---

# Career

Friedrich Leisch (see Figure&nbsp;\@ref(fig:leisch)) was born 1968 in Vienna (Austria) and
died after serious illness in 2024 in Vienna. Everyone called him Fritz.

\begin{figure}[h!]

{\centering \includegraphics[width=0.55\linewidth]{fritz_files/figure-latex/leisch-1} 

}

\caption{Fritz Leisch at his inaugural lecture at BOKU in 2011. Source: BOKU.}(\#fig:leisch)
\end{figure}

Starting in 1987, Fritz studied Applied Mathematics at Technische Universität Wien (TU Wien),
earning his master's degree (Dipl.-Ing.) in 1993. Subsequently, he joined the
Department of Statistics and Probability Theory at TU Wien as an
assistant professor which he continued to be, with short intermissions, until 2006.
During this time he also defended his doctoral thesis in Applied Mathematics (Dr.techn.)
in 1999 and earned his habilitation (venia docendi) in Statistics in 2005.

In 1995, he visited the Knowledge-Based Engineering Systems Group at the University of
South-Australia in Adelaide on a Kurt Gödel scholarship for postgraduate
studies. From 1997 to 2004 he was a member of the SFB project
"Adaptive Information Systems and Modeling in Economics and Management Science", coordinated
at Wirtschaftsuniversität Wien (WU Wien). From 2002 to 2003 he was assistant professor
at the Department of Statistics and Decision Support Systems, Universität Wien.

In 2006 Fritz moved to Munich, Germany, to become a professor for computational
statistics at the Department of Statistics, Ludwig-Maximilians-Universität München (LMU), see Figure&nbsp;\@ref(fig:lmu).
He returned to Vienna in 2011 to join the BOKU University as head of the Institute of Statistics, see Figure&nbsp;\@ref(fig:boku).

\begin{figure}[t!]

{\centering \includegraphics[width=0.83\linewidth]{fritz_files/figure-latex/lmu-1} 

}

\caption{Computational statistics group at LMU in 2007 (left to right): Sebastian Kaiser, Adrian Duffner, Manuel Eugster, Fritz Leisch. Source: Carolin Strobl.}(\#fig:lmu)
\end{figure}

\begin{figure}[t!]

{\centering \includegraphics[width=0.83\linewidth]{fritz_files/figure-latex/boku-1} 

}

\caption{Institute of Statistics at BOKU in 2022 (left to right, back to front): Johannes Laimighofer, Nur Banu Özcelik, Ursula Laa, Fritz Leisch, Bernhard Spangl, Gregor Laaha, Matthias Medl. Robert Wiedermann, Lena Ortega Menjivar, Theresa Scharl, Melati Avedis. Source: BOKU.}(\#fig:boku)
\end{figure}


# Key contributions

Fritz' scientific contributions span an impressive range including 
theoretical and methodological work (especially in the field of clustering 
and finite mixture models) over software (mostly related to the R 
programming language) to applied work and cooperations (notably in 
marketing, biotechnology, and genomics, among many others). In the 
following sections we try to highlight his key contributions and 
scientific legacy.

## R Core & CRAN

During his stay in Australia, Fritz had learned about the existence of
R. Back in Austria, he and Kurt started to explore this potentially
good news more systematically. They soon stopped further work on a
statistics toolbox they had developed for Octave [@Eaton+Bateman+Hauberg:2024],
and switched to R for their applied work, finding lots of room for
further improvement, and thus sending polite emails with patches and
more suggestions to Ross Ihaka and Robert Gentleman. Clearly these were
acceptable in quality but too high in quantity, and it did not take very
long that Ross and Robert gave Fritz and Kurt write access to the R
sources (initially in CVS, then moved to SVN), and in 1997, they both
officially became very early members of the R Core Team.

One of the main challenges then was that the functionality provided by R
was rather limited. Contributed extensions for S were available from the
Carnegie Mellon University Statlib S Archive^[Unfortunately, the Statlib
S Archive is currently not available anymore. A snapshot, including many
of the actual source code files, is available on the Internet Archive at
<https://web.archive.org/web/20000815063825/http://lib.stat.cmu.edu/S/>.], and could typically be
ported to R rather easily, but there was no mechanism for conveniently
distributing or actually using these extensions. This fundamentally
changed, when in 1997 Fritz and Kurt implemented the R package
management system, using ideas from Debian's APT (advanced package tool,
<https://wiki.debian.org/AptCLI>) they had successfully employed for
managing their computer systems. They also set up the Comprehensive R
Archive Network [CRAN, <https://CRAN.R-project.org/>, see also
@Hornik:2012] as a means for redistributing R and its contributed
extensions, and infrastructure for quality assurance of these
extensions.  These two contributions paved the way for the amazing
growth and success of R through its wealth of high-quality contributed
extensions.
See <https://stat.ethz.ch/pipermail/r-announce/1997/000001.html> for
the first announcement of CRAN, starting with 12 extension packages.
Currently, there are more than 21,000. See Figure&nbsp;\@ref(fig:cran)
for a screenshot^[This is from the earliest capture, from 1998-01-10,
available on the Internet Archive at
<https://web.archive.org/web/19980110082558/http://www.ci.tuwien.ac.at/R/contents.html>.]
of the landing page of the CRAN master site at TU Wien, as last modified
by Fritz on 1997-12-09.

\begin{figure}[t!]

{\centering \includegraphics[width=1\linewidth]{fritz_files/figure-latex/cran-1} 

}

\caption{Screenshot of the landing page of the CRAN master site at TU Wien on 1998-01-10, as last modified by Fritz on 1997-12-09. Source: Internet Archive.}(\#fig:cran)
\end{figure}

The first SVN commit by Fritz is from 1997-10-02, the last from
2013-10-04. Overall, there are 651 commits by Fritz, mostly from the
early years of R Core, and related to the R package management and CRAN
mirror system, and the addition of the `Sweave` system
(see Section&nbsp;[2.3](#sec:sweave-reproducibility) for more details).

## DSC & useR! conferences

With establishing CRAN in Vienna at TU Wien, Fritz and Kurt
laid the foundation for a special relationship between Vienna and R that they
characterized as a story of "love and marriage" [@Hornik+Leisch:2002]. In the decade
after the creation of CRAN a number of seminal R-related meetings took place in Vienna,
co-organized by Fritz as well as several of the co-authors of this paper.

The first workshop on "Distributed Statistical Computing" (DSC) took place from
March 19-23, 1999, at TU Wien. The main motivations were bringing together the R Core Team
for its first face-to-face meeting, discussing the roadmap for the release of R 1.0.0,
as well as exploring potential synergies with other environments for statistical computing.
There were around 30 participants and about 20 presentations, many of which were
relatively short, leaving ample time for discussions (see Figure&nbsp;\@ref(fig:dsc1999)).

\begin{figure}[p!]

{\centering \includegraphics[width=0.83\linewidth]{fritz_files/figure-latex/dsc1999-1} \includegraphics[width=0.83\linewidth]{fritz_files/figure-latex/dsc1999-2} \includegraphics[width=0.83\linewidth]{fritz_files/figure-latex/dsc1999-3} 

}

\caption{Discussions at DSC 1999 (top to bottom, left to right): Thomas Lumley, Fritz Leisch, Luke Tierney. Peter Dalgaard, Ross Ihaka, Paul Murrell. Brian Ripley, Martin Mächler, Robert Gentleman, Kurt Hornik. Source: Douglas Bates (DSC 1999 homepage).}(\#fig:dsc1999)
\end{figure}

Two more DSC workshops were organized at TU Wien
in 2001 and 2003. While meetings focusing on R development issues (with the
R Core Team and everyone else interested) were still an important part of
these conferences, they also saw an increasing number of regular conference
presentations on R packages and their different fields of application
(e.g., establishing infrastructure for spatial data). In 2001 there were
around 60 participants and about 30 presentations, most with corresponding
papers in the online proceedings [@Hornik+Leisch:2001]. In 2003 this
increased to more than 150 participants and about 60 presentations, again
with the majority in the online proceedings [@Hornik+Leisch+Zeileis:2003].

The high demand for a platform, where R users from different fields could
exchange ideas, prompted the creation of a new conference series called
useR!. The first two installments again took place in Vienna in 2004
at TU Wien and in 2006 at WU Wien.
Torsten Hothorn, David Meyer, and Achim Zeileis took the lead in the
organization with support and advice from Fritz and Kurt in the background.
An important contribution from the R Core Team at the useR! conferences
were keynote lectures highlighting important developments, e.g., a keynote
given by Fritz at useR! 2004 on S4 classes and methods. Both conferences
continued the success of the earlier DSC workshops with the number of
participants rising to more than 200 in 2004 and close to 350 in 2006.
Similarly, the number of presentations grew to about 100 in 2004 and more
than 150 in 2006.

In addition to the efforts initiated by Fritz and Kurt, another key factor
to the success of these meetings was the city of Vienna with its culture,
cafes, wine and beer pubs, etc. [see @Hornik+Leisch:2002 and also
Figure&nbsp;\@ref(fig:user2006)].

\begin{figure}[t!]

{\centering \includegraphics[width=0.83\linewidth]{fritz_files/figure-latex/user2006-1} 

}

\caption{Conference dinner at useR! 2006 (left to right): Fritz Leisch, Torsten Hothorn, Tim Hesterberg. Source: Carolin Strobl (useR! 2006 homepage).}(\#fig:user2006)
\end{figure}


## Sweave & reproducibility

With `Sweave` [@Leisch:2002], Fritz pioneered what we now can understand as
the technical foundation of reproducible research. `Sweave` was the main
inspiration for \CRANpkg{knitr} [@Xie:2015] which in turn led to
\CRANpkg{rmarkdown} [@Xie+Allaire+Grolemund:2018] and \CRANpkg{quarto}
[@Scheidegger+Teague+Dervieux:2024]. All these systems are used today to
generate countless scientific articles, package vignettes, webpages, books, blogs,
and much more in a dynamic and reproducible way.

Of course, Fritz was not the first one going in this direction. The concept
of "literate programming" had been introduced by @Knuth:1984, allowing to
combine the source code for software and the corresponding documentation
in the same file. The concepts of "tangling", that is, extracting the code
for compilation, and "weaving", the process of generating a nicely looking
document containing code next to prosa and formulae, have their roots in the
`WEB` and `CWEB` systems [@Knuth+Levy:1993]. As these packages were specific
to code in Pascal (`WEB`) and C (`CWEB`), respectively, and documentation in
LaTeX, @Ramsey:1994 introduced his `noweb` system as a literate programming
tool that is agnostic to the programming language used and also supports HTML
in addition to LaTeX and a few other backends for documentation. The `noweb`
syntax for code chunks is:

\pagebreak

```
<<code>>=
1 + 2
@
```

This will look familiar to users of `Sweave`. From this history, the naming
decisions for the software and its file format can be understood: `Sweave`
is the function that weaves code in S (or R - both languages still existed
side by side at the time) with its output and documentation. And `Rnw` stands for files
mixing R code with `noweb` syntax.

Starting in the mid-1990s to the early 2000s, interests shifted from just
"literate programming" to "literate data analysis" [@Leisch:2002; @Leisch+Rossini:2003]
as a core ingredient for reproducible research [@Buckheit+Donoho:1995].
The seminal new idea was to have dynamic documents so _outputs_ of code
such as figures and tables could be updated automatically when the underlying
data changed, which was pioneered by the late Günter Sawitzki in his
`Voyager` system [@Sawitzki:1996].

Fritz amalgamated all of this into `Sweave` which was the first time that the
power of dynamic reporting became easily available in a widely-used programming
language for statistics in combination with the standard textprocessing system
LaTeX. This turned out to be a "killer feature" of R at the time and the basis
for further work towards reproducible research [@Hothorn+Leisch_2011; @Stodden:2014].

`Sweave` was also the basis for R package vignettes  [@Leisch:2003] as an
addition to the previously available technical manual pages.
The first R package vignette published on CRAN in May 2002 was in the
\CRANpkg{strucchange} package, providing methods for testing, monitoring,
and dating structural changes. The vignette was the `Sweave` adaptation
of an introduction to the package that had been co-authored by Fritz and
published a couple of months earlier in the _Journal of Statistical Software_
[@Zeileis+Leisch+Hornik:2002]. See Figure&nbsp;\@ref(fig:sweave) for how
Fritz used it to illustrate the idea of package vignettes in @Leisch:2003
and that the R code from vignettes can be easily extracted (also interactively),
explored, and re-run.

\begin{figure}[t!]

{\centering \includegraphics[width=1\linewidth]{fritz_files/figure-latex/sweave-1} 

}

\caption{Screenshot of the strucchange package vignette, shown in a PDF viewer (right), along with the vExplorer from Bioconductor for interactive code execution (top left) with output in the active R graphics window (bottom left). Source: Leisch (2003, Figure 2).}(\#fig:sweave)
\end{figure}


## Clustering & mixture models

Fritz' theoretical and methodological work focused in particular on
clustering and finite mixture models. Centroid-based partitioning
methods as well as finite mixture models allow that their fitting
algorithm is embedded in a common estimation framework. In this
framework, each of the steps is adapted in a modular way depending on
the specific setup, e.g., the distance and centroid determining method
or the component distribution used. Fritz exploited this for the
implementation of the packages \CRANpkg{flexclust} [@Leisch:2006] and
\CRANpkg{flexmix} [@Leisch:2004; @Gruen+Leisch:2008], contributing to
the clustering tools available for R (see the CRAN Task View
\ctv{Cluster}). Both packages provide general infrastructure for
(model-based) clustering and enable rapid prototyping and the simple
extension to new variants taking into account complicated data
structures or challenging model specifications [see, for example, 
\pkg{psychomix}, @Frick+Strobl+Leisch:2012].


## Applied work

For many years, Fritz and Kurt actively participated in the Biological
Psychiatry working group at Medizinische Universität Wien. The first
paper co-authored by Fritz dates from 2000
[@Bailer+Leisch+Meszaros:2000], the last from 2023
[@Solmi+Thompson:2023]. The joint research was mostly
focused on linking genetic traits to psychiatric disorders and
treatment success. This prompted many enhancements in the classical test
infrastructure in base R - in surprising ways to some reviewers, who
could not believe that Fisher's test really worked for tables with more
than two rows or columns. It also established a strong need for
conveniently reporting the results of the statistical analyses to the
medical doctors in the group that went beyond providing annotated
transcripts, which Fritz eventually managed to satisfy by inventing the
`Sweave` system (see Section&nbsp;[2.3](#sec:sweave-reproducibility)).

Fritz also intensively collaborated with Sara Dolnicar to advance data
analytic methods for data-driven market segmentation analysis. They
received the Charles R. Goeldner Article of Excellence Award for their
work on extracting stable Winter tourist segments in Austria with
bagged clustering [@Dolnicar+Leisch:2003]. They focused on the
evaluation of data structure and the selection of suitable segments
based on segment stability as a key criterion [@Dolnicar+Leisch:2010;
@Dolnicar+Leisch:2017]. Finally, this joint work resulted in
@Dolnicar+Gruen+Leisch:2018 which provides practical guidance for
users of market segmentation solutions and for data analysts with
respect to the technical and statistical aspects of market
segmentation analysis.

As head of the Institute of Statistics, Fritz was involved 
in various interdisciplinary research projects covering almost the whole 
range of core areas of research at BOKU. He was key researcher at the 
Austrian Centre of Industrial Biotechnology (acib)
[@Scharl+Voglhuber+Leisch:2009; @Melcher+Scharl+Leisch:2017] and 
faculty member of the doctoral schools on agricultural genomics and 
bioprocess engineering. Among others he contributed to the fields of 
zoology [@Cech:2022], forestry, transportation and tourism 
[@Taczanowska:2023] as well as chemistry, genomics and wildlife 
biology [@Steiner:2014].


# Academic service

In addition to the services for the various conferences and proceedings
already described above, he served the scientific community in various ways.
In January 2001, he co-created _R News_ which evolved into
_The R Journal_ eight years later. For the journal _Computational Statistics_
he was an associate editor from 2005 to 2006 before he became editor-in-chief
from 2007 to 2011 [see @Symanzik+Mori+Vieu:2024 for more details].
Other notable contributions include being 
editor for the _Journal of Statistical Software_, core member of the
_Bioconductor_ project for statistical software in bioinformatics, and
first secretary general of the _R Foundation for Statistical Computing_ when
it was formed in 2002. 



# Teaching & mentoring

Fritz taught generations of students at bachelor, master, and PhD level and
introduced hundreds of useRs to proper R development in his "Introduction to
R Programming" short course.  At TU Wien, LMU, and BOKU, he taught courses in applied
statistics, statistical computing and computational statistics.  He had the 
ability to explain even difficult content in a simple way and to inspire students 
with statistics and programming with R. He
co-founded the "Munich R Courses" lecture series and was part of a group
aiming to initiate a formal PhD program in statistics at LMU. 

Fritz supervised
Bettina Grün, Theresa Scharl, 
Sebastian Kaiser, Manuel Eugster, 
Christina Yassouridis, Rainer Dangl,
Weksi Budiaji, Muhammad Atif and
Simona Jokubauskaite as his PhD students.
Based on his research, Fritz often discussed the state of and the need for reproducible
research and taught his many students how to avoid the many small and
innocent errors that have a tendency to pile up and invalidate reported
statistical results, with potentially devastating consequences, as we all know.

# Odds & ends

Fritz loved cooking, music, motorbike riding, playing cards with his
friends, skiing and hiking. A late afternoon call to his office
asking him to go along for a beer in Munich's English Garden almost never went
unanswered, positively. Back in Vienna at BOKU, colleagues got to know Fritz as a very 
structured, thoughtful, calm person who involved everyone, listened to 
everyone and always endeavored to balance interests and ensure fairness.
He strengthened cooperation and cohesion with his leadership style. 
Fritz was a friendly, always modest person who was free of airs and graces or 
vanity, despite or perhaps because of his great scientific successes.
The R Core Team and the R community at large miss a contributor,
collaborator, teacher, colleague, and friend.

# References

