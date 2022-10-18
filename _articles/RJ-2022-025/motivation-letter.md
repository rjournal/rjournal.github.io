---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

Editor   
The R Journal  
\bigskip

Dear Professor Cook,
\bigskip

It is delightful every time using R tools to cope with the manuscript. We are pleased that our manuscript is considered for potential publication in R Journal. Thank you for the advice on our manuscript which improve our paper as well as the package, and we would like to resubmit our revised manuscript entitled "akc: A Tidy Framework for Automatic Knowledge Classification in R" (Manuscript NO: 2021-71) for further consideration. We have revised the issues brought up by the editor and reviewer, the point-by-point response can be found below.  

We hope you will find our revision is satisfactory and our manuscript is acceptable for publication in R Journal. Thank you. 

\bigskip
\bigskip

Best Regards,
    
\bigskip
    
Tian-Yuan Huang  
National Science Library    
Chinese Academy of Sciences    
Beijing, China     
huangtianyuan@mail.las.ac.cn    

\bigskip


### Response to the comments

- Captions need to be extended to have three components: (1) what is the plot/table about, (2) specific details of plot/table, like what type of display and how variables are mapped, (3) the most important thing that the reader should learn.  
  **Reply**: Done as suggested. Thank you for the comments.
  
  
- Figure 3 should be a table. And there is no need for it to be an image, you should make it a typeset table.   
  **Reply**: Done as suggested. Thank you for the advice.
  
  
- In the pdf version of paper figures 4 and 5 extend outside the text area.  
  **Reply**: Thank you for the comment. We have restricted the width of figure using *out.width* parameter in **rmarkdown** package. Hopefully, this could be a default setting in **rjtools** package when dealing with the figures.
  
  
- Files should be named after the author, or even the software, not as generically as "article.XXX".  
  **Reply**: Done as suggested. In the revision, we used the package name ("akc") instead of "article". Thanks.
  
  
- No need to reference the rjtools package.  
  **Reply**: Done as suggested.   
  
  
- The example is really interesting. Can you extend the text to explain what is learned about the bibliographic data? Is it that most of the articles are about R packages? What do groups 1, 2, 3 represent? Is optimisation the most common subject?  
  **Reply**: Done as suggested. We did not try to make further explanation in the first place because the example is supposed to provide how to use the functions to yield results in simple ways, but this process could not be considered as a serious analysis, and the conclusions could be misleading if we are not cautious. In the revision, we have added a last paragraph in the end of *Example*. 
  
  
- The way that I have learned text processing is by Text Mining with R by Silge and Robinson. It seems to be a glaring omission from the citations in your paper.  
  **Reply**: Done as suggested. This is a very important book in NLP area in R. I myself started my text mining journey with R using this book (online version) when its Chinese translation is far from reach. We are sorry that we only cite the **tidytext** package but miss this important work in our paper, we have added this citation in the revision. Thank you for the advice. 
