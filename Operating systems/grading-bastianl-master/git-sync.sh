#!/bin/sh


echo "Sync init"
cd grading-bastianl
git fetch upstream
git merge -m "Merge upstream changes" upstream/master
git push
echo "Sync done"
