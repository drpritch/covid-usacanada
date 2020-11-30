#!/bin/bash

# Anh Nguyen <anh.ng8@gmail.com>
# 2016-04-30
# MIT License

# This script takes in images from a folder and make a crossfade video from the images using ffmpeg.
# Make sure you have ffmpeg installed before running.

# The output command looks something like the below, but for as many images as you have in the folder.
# See the answer by LordNeckbeard at:
# http://superuser.com/questions/833232/create-video-with-5-images-with-fadein-out-effect-in-ffmpeg/1071748#1071748
#
#
# ffmpeg \
# -loop 1 -t 1 -i 001.png \
# -loop 1 -t 1 -i 002.png \
# -loop 1 -t 1 -i 003.png \
# -loop 1 -t 1 -i 004.png \
# -loop 1 -t 1 -i 005.png \
# -filter_complex \
# "[1:v][0:v]blend=all_expr='A*(if(gte(T,0.5),1,T/0.5))+B*(1-(if(gte(T,0.5),1,T/0.5)))'[b1v]; \
# [2:v][1:v]blend=all_expr='A*(if(gte(T,0.5),1,T/0.5))+B*(1-(if(gte(T,0.5),1,T/0.5)))'[b2v]; \
# [3:v][2:v]blend=all_expr='A*(if(gte(T,0.5),1,T/0.5))+B*(1-(if(gte(T,0.5),1,T/0.5)))'[b3v]; \
# [4:v][3:v]blend=all_expr='A*(if(gte(T,0.5),1,T/0.5))+B*(1-(if(gte(T,0.5),1,T/0.5)))'[b4v]; \
# [0:v][b1v][1:v][b2v][2:v][b3v][3:v][b4v][4:v]concat=n=9:v=1:a=0,format=yuv420p[v]" -map "[v]" out.mp4

#----------------------------------------------------------------
# SETTINGS
input_dir="."  # Replace this by a path to your folder /path/to/your/folder
n_files=29
files=`ls -1 ${input_dir}/$1_*.png | head -${n_files}`  # Change the file type to the correct type of your images
output_file="$1.mp4"           # Name of output video
crossfade=0.2                     # Crossfade duration between two images
#----------------------------------------------------------------

# Making an ffmpeg script...
input=""
filters=""
output="[0:v]"

i=0

for f in ${files}; do
  input+=" -loop 1 -t 0.3 -i $f"

  next=$((i+1))
  if [ "${i}" -ne "$((n_files-1))" ]; then
    filters+=" [${next}:v][${i}:v]blend=all_expr='A*(if(gte(T,${crossfade}),1,T/${crossfade}))+B*(1-(if(gte(T,${crossfade}),1,T/${crossfade})))'[b${next}v];"
  fi

  if [ "${i}" -gt "0" ]; then
    output+="[b${i}v][${i}:v]"
  fi

  i=$((i+1))
done

output+="concat=n=$((i * 2 - 1)):v=1:a=0,format=yuv420p[v]\" -map \"[v]\" ${output_file}"

script="ffmpeg ${input} -filter_complex \"${filters} ${output}"

echo ${script}

# Run it
eval "${script}"
