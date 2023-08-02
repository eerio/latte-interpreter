#!/bin/bash

RT=/home/pawel/jpp-2023/latte/app

for filename in "$RT"/good/*.lt; do
  [ -e "$filename" ] || continue
  if $RT/Latte/Test "$filename" >/dev/null ; then
    echo "$filename: correct"
  else
    echo "$filename failed!"
    break
  fi
done

for filename in "$RT"/bad/*.lt; do
  [ -e "$filename" ] || continue
  if $RT/Latte/Test "$filename" >/dev/null ; then
    echo "$filename failed!"
  else
    echo "$filename: correct"
    break
  fi
done
