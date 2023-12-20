---
title: "Changes on CRAN"
subtitle: "2023-05-01 to 2023-09-30"
date: "2023-11-26"
draft: true
author:  
  - name: Kurt Hornik
    affiliation: WU Wirtschaftsuniversität Wien
    address: Austria
    orcid: 0000-0003-4198-9911
    email: Kurt.Hornik@R-project.org
  - name: Uwe Ligges
    affiliation: TU Dortmund
    address: Germany
    orcid: 0000-0001-5875-6167
    email: Uwe.Ligges@R-project.org
  - name: Achim Zeileis
    affiliation: Universität Innsbruck
    address: Austria
    orcid: 0000-0003-0918-3766
    email: Achim.Zeileis@R-project.org
output: 
  rjtools::rjournal_article:
    self_contained: yes
    toc: no
preamble: 
  \usepackage{float, longtable}
---



# CRAN growth



In the past 5 months, 866 new packages were
added to the CRAN package repository. 332 packages
were unarchived, 701 were archived and 
5 had to be removed. The following shows the
growth of the number of active packages in the CRAN package repository:


\begin{center}\includegraphics[width=1\linewidth]{cran_files/figure-latex/cran_growth-1} \end{center}

\noindent On 2023-09-30, the number of active packages was around 19932.


# Changes in the CRAN Repository Policy

The [Policy](https://CRAN.R-project.org/web/packages/policies.html) now links to an
accompanying document on [Using Rust in CRAN packages](https://CRAN.R-project.org/web/packages/using_rust.html).

<!--

# Changes in the CRAN Repository Policy

Start out from

```
cd ~/src/org/R-project/R-dev-web/CRAN/Policy
svn diff -r{2023-05-01}:{2023-09-30}
```

-->


# CRAN package submissions



From May 2023 to September 2023
CRAN received 12237 package submissions.
For these, 20527 actions took place of which
14006 (68%) were auto processed actions and
6521 (32%) manual actions.

Minus some special cases, a summary of the auto-processed and manually
triggered actions follows:


|       | archive| inspect| newbies| pending| pretest| publish| recheck| waiting|
|:------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
|auto   |    3677|    1889|    2460|       0|       0|    3734|    1282|     964|
|manual |    2378|     186|     431|     317|      52|    2423|     599|     135|

These include the final decisions for the submissions which were


|       |      archive|      publish|
|:------|------------:|------------:|
|auto   | 3573 (29.7%)|   3246 (27%)|
|manual | 2337 (19.4%)| 2883 (23.9%)|

\noindent where we only count those as _auto_ processed whose publication or
rejection happened automatically in all steps.






# CRAN mirror security

Currently, there are 94 official CRAN mirrors,
76 of which provide both
secure downloads via '`https`' _and_ use secure mirroring from the CRAN master
(via rsync through ssh tunnels). Since the R 3.4.0 release, `chooseCRANmirror()`
offers these mirrors in preference to the others which are not fully secured (yet).


# CRAN Task View Initiative



There is one new task view:

- [Actuarial Science](https://CRAN.R-project.org/view=ActuarialScience): Maintained by Christophe Dutang, Vincent Goulet.

Currently there are 44 task views (see <https://CRAN.R-project.org/web/views/>),
with median and mean numbers of CRAN packages covered
104 and 122, respectively.
Overall, these task views cover 4496 CRAN packages,
which is about 22% of all active CRAN packages.
