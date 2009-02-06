#!/bin/bash

# fetch newest srfis from repository and just copy to Mosh.
cd /tmp
bzr checkout --lightweight lp:~ikarus-libraries-team/ikarus-libraries/srfi
find srfi -name ".bzr" | xargs rm -rf
cp -r srfi ~/mosh/lib/
cd ~/mosh
svn st lib/srfi

