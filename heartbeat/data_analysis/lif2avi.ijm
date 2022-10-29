/* usage: fiji -macro lif2avi.ijm path/to/dir/containing/lif/files/ (fiji may need to be open in another thread for this program to work)
 */

///// PARAMETERS
 
extension = "lif";

// initialize Bio-Formats Macro Extensions
run("Bio-Formats Macro Extensions");

////// MAIN

dir = getArgument();
print(dir);
files = getFileList(dir);

setBatchMode(true);
k=0;
n=0;

for(f=0; f<files.length; f++) {
	if(endsWith(files[f], "."+extension)) {
		k++;
		id = dir+files[f];
		Ext.setId(id);
		Ext.getSeriesCount(seriesCount);
		print(seriesCount+" series in "+id);
		n+=seriesCount;
		for (i=0; i<seriesCount; i++) {
			run("Bio-Formats Importer", "open=["+id+"] color_mode=Default view=Hyperstack stack_order=XYCZT series_"+(i+1));
			fullName	= getTitle();
			dirName 	= substring(fullName, 0,lastIndexOf(fullName, "."+extension));
			fileName 	= substring(fullName, lastIndexOf(fullName, " - ")+3, lengthOf(fullName));
//			File.makeDirectory(dir+File.separator+dirName+File.separator);

			print("Saving "+fileName+" under "+dir+File.separator+dirName);
			
			getDimensions(x,y,c,z,t);
			
            run("AVI... ", "compression=None frame=24.04615 save=["+dir+File.separator+fullName+".avi]"); // frame=23.28413 or 42.33077 or 43.66
			run("Close All");
		}
	}
}
Ext.close();
setBatchMode(false);
run("Quit");
