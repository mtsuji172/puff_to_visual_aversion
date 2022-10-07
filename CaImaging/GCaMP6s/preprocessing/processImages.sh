#!/usr/bin/bash
# bash processImages_batch.sh ../Tk-GCaMP6s/

function processImages () { #accept child dir such as "ID0"
    target=$1/
    echo $target
    
    # preprocess: remove "ChanA_Preview.tif" images
    find $target -name "Chan*_Preview.tif" -delete

    # execute GmunusR.ijm
    echo "executing GminusR.ijm..."
    mkdir $target\chanA/
    mkdir $target\chanB/
    mv $target\ChanA*.tif $target\chanA/
    mv $target\ChanB*.tif $target\chanB/
    fileNo=$(ls -l $target\chanA/ | wc -l)
    fileNo=$(($fileNo-1))
    ~/bin/fiji/ImageJ-linux64 -macro GminusR.ijm "$target $fileNo"
    mkdir $target\GminusR
    mv $target*.tif  $target\GminusR/
    echo "GminusR.ijm done"

    # maxproject along z axis for each time point
    myarray=()
    myzarray=()
    echo "maxprojecting along z..."
    mkdir $target\zprojection/
    for fullfile in $target\GminusR/*.tif
    do
        filename="${fullfile##*/}"
        substring="${filename##*_}"
        myframe="${substring/.tif/}"
        myarray+=($myframe)
        z=$(tr "_" "\n" <<< $filename | tail -n2 | head -n1)
        myzarray+=($z)
    done

    sorted_unique_ids=($(echo "${myarray[@]}" | tr " " "\n" | sort -u | tr "\n" " "))
    sorted_unique_z=($(echo "${myzarray[@]}" | tr " " "\n" | sort -u | tr "\n" " "))

    for t in ${sorted_unique_ids[@]}
    do
        convert $target\GminusR/ChanA_0001_0001_0001_${t}.tif $target\GminusR/ChanA_0001_0001_0002_${t}.tif -compose lighten -composite $target\zprojection/ChanA_${t}.tif 2> /dev/null
        for i in ${sorted_unique_z[@]}
        do
            convert $target\GminusR/ChanA_0001_0001_${i}_${t}.tif $target\zprojection/ChanA_${t}.tif -compose lighten -composite $target\zprojection/ChanA_${t}.tif 2> /dev/null
        done
    done
    echo "zprojection done"
}
export -f processImages

# process images in given dir
mydirs=$(find $1 -maxdepth 1 ! -path $1 ! -path $1/zprojection -type d) #$1 is e.g. "../Tk-GCaMP6s/"
for mydir in $mydirs; do
    processImages $mydir
done
