// usage fiji -macro multimeasure.ijm '10 ../20200901/20200901_ID0/ zprojection/aligned/'
args = split(getArgument(), " ");
mynum = args[0];
targetParent = args[1];
targetChild = args[2];
print('loading images...');
run("Image Sequence...", "open="+targetParent+targetChild+" number="+mynum+" starting=1 increment=1 scale=100 file=tif sort");
//run("Despeckle", "stack");
print('reading ROI named:'+targetParent+'RoiSet.zip');
roiManager("Open", targetParent+"RoiSet.zip");
roiManager("Multi Measure");
saveAs("Results", targetParent+targetChild+'Results.csv');
print('results saved to '+targetParent+targetChild+'Results.csv');
run("Quit");
