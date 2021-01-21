# R Journal website

The R journal website is published using [github pages](http://pages.github.com/). This makes it easier to manage the articles in git, and the website automatically updated when the git repo is pushed to github.

## Templates

The overall layout for the site can be found in `layouts/defaults.html`.  The template is written using [liquid](https://github.com/Shopify/liquid/wiki/Liquid-for-Designers), a simple templating system.

## Building the site locally

You'll need to install the jekyll ruby gem:

```
gem install jekyll
```

Then run the following command in the web site directory to preview the website http://localhost:4000

```
jekyll serve --watch
```

## Publishing new articles

Modify `_config.yml`

__Note__: `_config.yml` is only parsed once when you start jekyll, it's not automatically updated like when you modify html files. This means that whenever you add new articles you'll need to stop and restart jekyll to see the changes.

## Publishing a new issue

* In `_config.yml` move current `accepted` issue to correct name and add additional metadata. Delete all pdfs in `archive/accepted`

* Create `archive/issue`, copy in `index.html` and `references.bib`

* In `_layouts/default.html`, change the path to the current issue
