


We thank both reviewers for their reviews.  Nearly all comments we questions for
clarification. In most cases these have been resolved by extending the text
and/or rewriting parts of the text. Their comments have really helped
making the text more clear we think.



First review
===============================================================================

> * Overview: Is the article important to R the community? Is the approach
>   sensible? Are technology and methods up-to-date?
> 
> The article is relevant to the R community since it covers the area of data
> integration where relatively few R packages are available. Methods and
> implementation seem in line with the literature on record linkage
> 
> 
> * Article: Is anything unclear? Is anything missing? Are the examples helpful?
>   Is there sufficient background, including alternative approaches in R?
> 
> 
> The article provides relevant references on record linkage and mentions also
> the other available R packages for performing it. There are a couple of
> concepts that need to be clarified:
> 
> 1) The introduction does not explicitly define the concept of “exact linkage”
> and what is the difference with respect to “record linkage” (exact linkage can
> be done by calling the “merge” function even if it may not be the most
> efficient solution  unless it is applied to data.table objects).

Added the following text to the introduction: "In these cases exact linkage can
be used. In exact linkage records are linked when they agree exactly on the
linkage keys used. Exact linkage can be performed in R..."

> 
> 2) Please briefly explain what happens when “cluster_pair_blocking” is used
> instead of “pair_blocking”

The explanation below the example containing the call to `cluster_pair_blocking`
is extended. 

> 
> 3) Page 4:  It would be better to please briefly explain what is the “weight”
> and the corresponding range as the decision on weights’ threshold determines
> the linked dataset

I don't want to focus too much on the theory and the methods themselves.
However, I did extend the explanation of the weights somewhat. The other
reviewer also had questions on this. Hopefully it is a bit more clear how these
weights can be used. 

> 
> Other minor edits/typos
> 
> 4) end of page 2: please mention explicitly that the example uses blocking on
> the variable “postcode” (even if it appears on the output)

Some additional explanation has been added to this example (and others). This
also explains that postcode is used as a blocking variable.s 

> 
> 5) end of page 5: “The different in computation” should be “The difference in
> computation”

This has been fixed. 


Second review
===============================================================================

> This paper describes a new package for probabilistic record linkage, called reclin2.
> 
> The package succeeds the earlier reclin. Reclin2 is designed with core design
> principles in mind that yield an easy-to-understand interface and a consistent
> user experience. I also find the choice to adhere to well-known existing
> interfaces a good design choince (e.g.  formula-data interface, model-predict
> functions).
> 
> I think that the package is an important contribution to the R community as it
> combines flexibility, modularity,  performance, and an intuitive user interface
> for those who understand what the main steps in a Record Linkage problem are.
> 
> I do feel that the paper would benefit from textual improvements which I have
> listed below. There is one question, also below, regarding the measurement of
> memory usage in the benchmark.
> 
> 
> ## Introduction
> 
> Please provide a short introduction to the basic steps of record linkage so
> readers have a mental map of what the process entails (pairing, estimating
> likelyhood of being a match, deciding which pairs to match) before reading
> further.

Added a paragraph to the introduction with the steps. 

> Please provide a little more information on why reclin was succeeded by
> reclin2. Something along the lines of: "it was not possible to retain backward
> compatability while improving support for the design principles."

Thanks. The following lines have been added: "Unfortunately this was not possible while keeping
backward compatibility, therefore it was decided to continue with a new package. The core design
considerations are:"

> 
> Please refer to the RJournal paper for the RecordLinkage package:
> https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Sariyar+Borg.pdf
> (unfortunately, these authors have not added this paper to their CITATION
> file).

Thanks. Has been updated.

> 
> ## Design Principles
> p2.l24: there is also the option of using one of the cluster_ variants
>     --> please introduce the functionality, before using the technical implementation in a sentence. e.g:
>     --> there is also the option of creating a pairs object that is distributed over a cluster.
>     --> Special functions with the prefix cluster_ are offered... and so on.

The order of this text in this paragraph has been changed and some lines have
been added (there are now two paragraphs). The more technical parts are now
more to the back of the paragraph. 


> 
> ## The Record linkage process
> Please provide a short conceptual overview of the steps that together compose a
> record linkage process.

An overview of the steps have been added to the introduction of the paper to which we now refer
here. 


> 
> ## Comparing pairs
> 
> p3. Please explain the full code. What is the 0.9 in jaro_winkler(0.9)?

Extended the explanation below the example. 

> p3. Please provide a small example of the compare_vars function. Its purpose
>     is somewhat hard to parse from the text alone.

An example has been added. 

> p4. Please provide some interpretation of the weights. E.g. can they be translated to a probability?
>     how would one choose a limit weight to decide whether a pair is a match or not? Or, refer
>     to the next section explicitly for this.

Text has be added explaining the weights in more detail and also mentioning how
the threshold is often determined in practice. 


> p4. Please explain the full code where compare_vars() is used. What happens with the 'pairs'
>     data set? Something like: "in the first line, a column called 'true' is added to pairs...",
>     in the second line ...

Done. 

> ## Creating the linked dataset
> p4. Please explain what the result of select_n_to_m is: is it a one-to-many join? Do you
>     get the actual linked dataset, or is the pairs object updated? Also: what are the 
>     restrictions on maximizing the weights? Does it enforce a 1-1 linkage.

Text has been added that, hopefully, makes more clear what the function does. 


> p5. Please explain the full code creating 'cpairs', and so on.

An explanation had been added.

> 
> ## Helper functions for cluster_pair objects
> p5.l2-3: already mentiond cluster_collect function. The function was not
> mentioned before. (see previous remark).

I think this is not covered by the extended explanation in the previous example. 

> p5. Please provide a small example fot cluster_call (e.g. adding a column to
> 'pairs') and explain the code

I assume the reviewer means the `cluster_modify_pairs' function as there is an
example for `cluster_call'. An example for `cluster_modify_pairs' has been added
to the section.

> 
> ## Benchmark
> 
> p5.l2 General readers and readers outside of the European Statistical System 
>       may not know what an ESSnet is. Please reformulate

Added definition of ESSnet and made clear that it is a Eurostat financed
project. 

> p5l15: can be found on GitHub is mentioned twice with reference only second time.
>        Please reformulate.

Removed second reference and moved first one to the back of the paragraph. 

> p5l28: You could refer to Ahmdal's law here.

Thank you for the suggestion. Has been added. 

> p5/p6, figure 2. the memory use of fastLink is so low that I wonder if it was measured correctly.
>                  I am no expert on this, but is it possible that fastLink spawns mutliple
>                  processes of which the memory consumption was not measured? It seems to me 
>                  that at least the comparison vectors need to be stored. In the figure, memory
>                  usage seems constant with the number of possible pairs.

It is low, but I believe the measurement is correct. I also ran the examples while
monitoring using a system monitor and the memory use reported by time is in line
with what I saw there. In principle it is not necessary to store the comparison
vectors. The EM algorithm, for example only needs a table with observed patterns
and the frequencies of those patterns. Also see the appendix of 
https://oar.princeton.edu/bitstream/88435/pr1r180/1/%5BAPSR%5Dlinkage.pdf for 
more information on this.  I have considered doing something like this in
reclin2, but decided against it, as it would be very difficult to do while
keeping the flexibility (at least I couldn't think of a way). 


> 
> ## Conclusion
> p7. Perhaps give a few pointers of which future developments can be expected for the package.

One of the extensions that is being worked on has been added as an example of
possible future extensions.

> 
> 
> ## Typos/language notes

> 
> p1.Introduction l5: In these case --> in these cases.

I do believe this is a separate sentence so I kept the original text. 

> p1.Introduction l17: double use of 'principle'
> p1.Introduction l21: are often of order --> are of order O(N2N1). 
> p1.Introduction l22: one date set is often the population register --> is the population register
> p1.Introduction l32: no [...] project is the same --> no two [...] projects are the same
> p1.Introduction l34: generic record linkage --> record linkage
> p1.Introduction l38: flexipble -> flexible
> p1.Introduction l34: generic record linkage --> record linkage
> p2.The RecordLinkage Process l1: in the reclin2 --> in reclin2
> p2.Generating pairs l3. of pairs, often --> of pairs. Often, 
> p2.Generating pairs l3: attributes x and x --> x and y?
> p3.Comparing pairs l1: to compare the pair --> to compare the pairs of records?
> p3.Comparing pairs l9: We see [...], 'sex' --> We see [...], while 'sex'
> p4.Comparing pairs l5: it is also relatively easy to also estimate --> ... to estimate
> p4.Comparing pairs l8: match status is know --> is known
> p5.Benchmark l25: The different in computation --> The difference in computation...
> p5.Benchmark l29: steps table place --> take place
> p7.Conclusiot l10: --> the package is still being developed.

We appreciate that the reviewer has taken time to write these suggestion down.
They have been corrected. 


