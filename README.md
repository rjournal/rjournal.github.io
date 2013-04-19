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
jekyll --auto --server
```