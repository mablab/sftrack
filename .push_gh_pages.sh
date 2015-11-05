#!/bin/bash

rm -rf out || exit 0;
mkdir out;

GH_REPO="@github.com/stephlocke/isc-proposal.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

cd out
git init
git config user.name "stephs-travis"
git config user.email "travis"

cd ../
R CMD BATCH 'ghgenerate.R'

git add .
git commit -m "deployed to github pages"
git push --force --quiet $FULL_REPO master:gh-pages