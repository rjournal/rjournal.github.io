# R Journal templates & stylesheets

## Changes

### Feb 1, 2013

Moved to a single-column format. Figures and tables are now properly floating. The style file now uses the placeins latex package to stop badly placed floats from appearing above the article title or, worse, drifting off to the end of the issue. NB that it will be useful to add the command `\FloatBarrier` (from the `placeins` package) _before_ the bibliography of each article, to stop floats appearing after the bibliography and author affiliations.
