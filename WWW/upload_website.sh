#! /bin/bash

if [ $# == 0 ]; then
    user=`whoami`
else
    user=$1
fi

target=$user@cardinal.stanford.edu:/afs/ir/class/psych253/WWW

echo "Uploading to $target"

cp datasets/* _build/html/data/
cp tutorial_files/* _build/html/tutorials/
cp section_files/* _build/html/section/
cp slides_files/* _build/html/slides/
cp plot_files/* _build/html/plots/
cp cheatsheet_files/* _build/html/cheatsheets/
rsync -azP _build/html/ $target
