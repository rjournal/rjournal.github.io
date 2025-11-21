---
title: Changes on CRAN
subtitle: 2025-07-01 to 2025-09-30
date: '2025-09-01'
draft: no
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
preamble: \usepackage{float, longtable}
volume: 17
issue: 3
slug: RJ-2025-3-cran

---




# CRAN growth



In the past 3 months, 562 new packages were
added to the CRAN package repository. 165 packages
were unarchived, 287 were archived and 
1 had to be removed. The following shows the
growth of the number of active packages in the CRAN package repository:


\begin{center}\includegraphics[width=1\linewidth,alt={CRAN growth: Number of CRAN packages over time in levels (left) and in logs (right).}]{RJ-2025-3-cran_files/figure-latex/cran_growth-1} \end{center}

\noindent On 2025-09-30, the number of active packages was around 22791.


<!--

# Changes in the CRAN Repository Policy

Start out from

```
cd ~/src/org/R-project/R-dev-web/CRAN/Policy
svn diff -r{2025-07-01}:{2025-09-30}
```

-->


# CRAN package submissions



From July 2025 to September 2025
CRAN received 7923 package submissions.
For these, 12326 actions took place of which
9289 (75%) were auto processed actions and
3037 (25%) manual actions.

Minus some special cases, a summary of the auto-processed and manually
triggered actions follows:


|       | archive| inspect| newbies| pending| pretest| publish| recheck| waiting|
|:------|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
|auto   |    2422|     673|    1657|     107|       0|    2856|     981|     392|
|manual |    1212|       0|      20|       9|      19|    1406|     289|      80|

These include the final decisions for the submissions which were


|       |      archive|      publish|
|:------|------------:|------------:|
|auto   | 2348 (30.1%)| 2607 (33.4%)|
|manual | 1204 (15.4%)| 1652 (21.1%)|

\noindent where we only count those as _auto_ processed whose publication or
rejection happened automatically in all steps.





# CRAN mirror security

Currently, there are 93 official CRAN mirrors,
77 of which provide both
secure downloads via '`https`' _and_ use secure mirroring from the CRAN master
(via rsync through ssh tunnels). Since the R 3.4.0 release, `chooseCRANmirror()`
offers these mirrors in preference to the others which are not fully secured (yet).


# CRAN Task View Initiative



There is one new task view:

- [Anomaly Detection](https://CRAN.R-project.org/view=AnomalyDetection): Maintained by Priyanga Dilini Talagala, Rob J. Hyndman, Gaetano Romano.

Currently, there are 49 task views (see <https://CRAN.R-project.org/web/views/>),
with median and mean numbers of CRAN packages covered
112 and 124, respectively.
Overall, these task views cover 5051 CRAN packages,
which is about 22% of all active CRAN packages.
