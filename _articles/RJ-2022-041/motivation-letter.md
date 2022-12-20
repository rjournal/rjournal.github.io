---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

Editor   
The R Journal  
\bigskip

Dear Catherine,
\bigskip

This is a small revision of my last submission. I switched to the new rjtools template and included the script files of corEvol required to run the benchmark. I believe it still makes sense to explain how these files can be acquired from github. But I also added the following sentence to the benchmark section:

"Alternatively, you can download the required files from the supplementary material of this article."

Let me know, if this works or you had something different in mind.

There are a couple of issues with the new template / new instructions:

- The benchmark is run twice (for html and pdf), which obviously does not give identical timings. I had to manually cache the output in a separate chunk.

- The rjtools documentation mentions that `\CRANpkg` should be used for all package-references, which I did. But the old instructions required to use it only the first time. What is correct?

- The Rmd template does not handle paragraphs correctly. There is always a new paragraph after code, even if the code belongs to a paragraph (similar to math environments). In Latex I can simply put % after the code, which in Rmd does not seem to be possible.

- The title in the Rmd does not accept the `\pkg` command. The old instructions required to set the package name inside `\pkg{}` for the tex-file.

- Citations are not ordered alphabetically in html. For instance, [@borenstein2021; @sedlmeier2018] results in (Sedlmeier and Renkewitz 2018; Borenstein et al. 2021). Is this desired behavior? If so, this should be consistent with the pdf, where the output is Borenstein first, then Sedlmeier (what I would expect).

- ORCID is displayed as ORCiD in the pdf when using rjtools.

Otherwise the pdf and html look fine to me. Overall, these are rather small problems, but they can probably be resolved quickly. Let me know how you want to proceed.

\bigskip
\bigskip

Regards,
    
    
    
    
Johannes

\bigskip