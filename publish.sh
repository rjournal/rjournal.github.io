#!/bin/bash
set -e

# Assumes there is a web-svn directory, which is a git svn checkout
# of https://svn.r-project.org/Rjournal/trunk

jekyll build

# from http://nathangrigg.net/2012/04/rsyncing-jekyll/
# rsync -i --recursive --delete --checksum _site/ ../web-svn/html/

#rsync -i --recursive --checksum _site/ ../RJ_trunk/html/
rsync -i --recursive --checksum _site/ ../Rjournal/trunk/html/

git add -A .
git commit -m "$1"
git push

#cd ../RJ_trunk
cd ../Rjournal
svn add --force .
svn ci -m "$1"
