#!/bin/bash
set -e

# Assumes there is a web-svn directory, which is a git svn checkout
# of https://svn.r-project.org/Rjournal/trunk

jekyll build

# from http://nathangrigg.net/2012/04/rsyncing-jekyll/
# rsync -i --recursive --delete --checksum _site/ ../web-svn/html/

rsync -i --recursive --checksum build/ ../web-svn/html/

## Option 1: Using git-svn

cd ../web-svn
git add -A .
git commit -m"Update site"

git svn rebase
git svn dcommit

## Option 2: Using just SVN

cd ../web-svn
svn status

# Check-in added files and svn commit as usual
