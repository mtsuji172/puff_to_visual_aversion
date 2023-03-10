#!/bin/bash
# bash multimeasure.sh ../20200818_GCaMP6s/
target=$1

echo "target is: "$target
find $target -maxdepth 1 ! -path $target -type d -name '*ID*' -exec basename \{} \; | while read dirname; do
    fileNo=$(ls -l $target$dirname/zprojection/aligned/*aligned.tif | wc -l)
    echo "${fileNo} ${target}${dirname}/ zprojection/aligned/"
    /home/masato/bin/fiji/ImageJ-linux64 -macro multimeasure.ijm "${fileNo} ${target}${dirname}/ zprojection/aligned/"
done;
