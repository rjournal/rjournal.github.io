We would like to thank the reviewer for the comments and issues raised. Both
the manuscript and package are more complete thanks to the feedback.
Below each of the issues are addressed directly. Please let us know
if there any other comments or suggestions.

# Article Issues

1. Consider adding the package name to the title of the article. This will help readers quickly identify scope of the paper.

This has been done.

2. A few times in the text of the manuscript, the authors refer to R objects as variables, which I found to be unclear. For example in the Communicating results section, “First, the computational components, stored in the `trial_summary` variable are written to the disk.” When first read, it was unclear the authors were referring to the previously defined list. Another example, “The `md_header` variable is a yml object making it easy to control how the document is rendered.” I recommend referring to these as objects or lists.

Thanks for pointing this out. We now distinguish between variables, which
are sets of mesurements in a data.frame, for example, from an object which
is an instance of a data structure.

3. “Our simple example will contain four sections: Outcome Information, Adverse Events, and Biomarkers.” There are three sections listed here.

Thanks for pointing this out it has been corrected.

4. In the last chunk of code provided, it would be helpful to have the `ld_write_file()` function argument names included, e.g. `ld_write_file(rmd_header = as.character(md_header))`.

Agreed. The argument names have been added.

# Package Issues

1. The article example begins by defining the `trial_summary` list. Before this the readers are briefly introduced to a custom function, `class_and_tag()`, in the text. I found this function’s role in the listdown workflow difficult to understand. After more reading and reviewing, I came to understand the role further: export a data object along with instructions for analysis/summary as class and attributes. I presume the point is to have a separation from the data object and summary instructions, and the resulting summary object...

The reviewer is correct that the example provided is simpler than
the one shown in the manuscript and it is sufficient for the described 
task. However, it should be pointed out that the decorators can be used
to separate the content of the list from how the list content is displayed. 
We have added text to make this more clear, pointing out that neither
`class_and_tag()` nor a decorator is needed if the default rendering
of the object is sufficient.

The reviewer is also correct that this is is a nice convenience function 
and it has been added to the package. We did leaving the function in the 
paper to show its implementation so that it is clear what the function
is doing and how it works with the decorator.

Does this make sense? I think you understood the what was being done but I 
needed to be clearer about why I did it this way.

2. It is much easier to explore a new package when there is a website. I recommend adding one with pkgdown.

Agree. The pkgdown page is up. I'll update it with this and another paper
when they are available.

3. Please add more detail to the help file of listdown(). Some of the arguments role are not clear. An example would be helpful utilizing the various arguments would be helpful.


Examples have been added and documentation has been filled out. If this is
not sufficient please feel free to identify areas where the documentation is
ambiguous or pull requests are always appreciated.

4. The unit tests are failing locally and on Travis CI. Failing unit tests often give users pause when trying a new package. 

Thanks for pointing this out. I've switched to GitHub actions. Version 0.4.5
appears to be building correctly.

5. The NEWS.md does not seem to be up to date. The current CRAN version is 0.4.1, but there is no corresponding entry in NEWS.

This has been updated. Please note that we've had a few internal version bumps
and we are now at 0.4.5.
