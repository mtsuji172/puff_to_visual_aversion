//usage fiji --headless -macro GminusR.ijm '../test/ 225'

args = split(getArgument(), " ");
target = args[0];
mynum = args[1];
print("importing image seqs...");
run("Image Sequence...", "open=["+target+"chanA/] number="+mynum+" starting=1 increment=1 scale=100 file=tif");
run("Image Sequence...", "open=["+target+"chanB/] number="+mynum+" starting=1 increment=1 scale=100 file=tif");
print("executing chanA - chanB...");
imageCalculator("Subtract create stack", "chanA","chanB");
print("saving the results...");
selectWindow("Result of chanA");
run("Image Sequence... ", "format=TIFF use save="+target);
selectWindow("Result of chanA");
close();
selectWindow("chanA");
close();
selectWindow("chanB");
close();
run("Quit");
