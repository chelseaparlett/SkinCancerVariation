#!/bin/bash
for f in $(ls | grep 'IMD[0-9][0-9][0-9]');
do
  cd "$f"
  for k in $(ls | grep 'IMD[0-9][0-9][0-9]_lesion');
  do
    cd "$k"
    for i in $(ls | grep 'IMD[0-9][0-9][0-9]_lesion.bmp')
    do
      p2=$(ls "$i" | sed -e s/\.bmp//)
      echo $p2
      convert "$i" $p2.jpg
    done
    cd ../
  done
  cd ../
done
