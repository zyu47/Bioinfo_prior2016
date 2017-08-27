//this program works on Zhixian's login on lab PC
//To activate program delete (/* and */) flanking code
//To deactivate, add (/* and */) flanking code

//Get input directory
input = getDirectory("Input Directory");
earlist = getFileList(input);
for(earnum = 0; earnum < earlist.length; earnum++)
{
	if (endsWith(earlist[earnum],"/"))
	{
		daylist = getFileList(input + earlist[earnum]);
		for(daynum = 0; daynum < daylist.length; daynum++)
		{
			if(endsWith(daylist[daynum],"/"))
			{
				for(i=1; i!= 10; ++i)
				{
					
					//save original file
					open(input + earlist[earnum] + daylist[daynum] + "0"+i+".oib");
					rename("0"+i+".oib");
					Stack.getDimensions(width, height, channels, slices, frames);
					run("Z Project...", "start =1 stop="+nSlices/channels+" projection=[Max Intensity]");
					selectWindow("0"+i+".oib");
					close();
					selectWindow("MAX_0"+i+".oib");
					run("Split Channels");
					run("Merge Channels...", "c2=C2-MAX_0"+i+".oib c6=C1-MAX_0"+i+".oib ignore");
					run("Save", "save=[" + input + earlist[earnum] + daylist[daynum] + "0" + i + ".tif]");
					close();
					//selectWindow("RGB");
					//close();
					//save original file
					
					
					/*
					//save mod file
					if(File.exists(input + earlist[earnum] + daylist[daynum] + "0"+i+"_mod.tif"))
					{
						open(input + earlist[earnum] + daylist[daynum] + "0"+i+"_mod.tif");
						rename("0"+i+".oib");
						Stack.getDimensions(width, height, channels, slices, frames);
						run("Z Project...", "start =1 stop="+nSlices/channels+" projection=[Max Intensity]");
						selectWindow("0"+i+".oib");
						close();
						selectWindow("MAX_0"+i+".oib");
						run("Split Channels");
						run("Merge Channels...", "c1=C2-MAX_0"+i+".oib c2=C1-MAX_0"+i+".oib ignore");
						run("Save", "save=[" + input + earlist[earnum] + daylist[daynum] + "0" + i + "_mod2.tif]");
						close();
					//	selectWindow("RGB");
					//	close();
					}
					//save mod file
					*/
				}
				
				 //stiching the original file 
				 run("Grid/Collection stitching", "type=[Grid: row-by-row] order=[Right & Down                ] grid_size_x=3 grid_size_y=3 tile_overlap=10 first_file_index_i=1 directory=["+ input + earlist[earnum] + daylist[daynum] +  "] file_names={ii}.tif output_textfile_name=TileConfiguration.txt fusion_method=[Linear Blending] regression_threshold=0.30 max/avg_displacement_threshold=2.50 absolute_displacement_threshold=3.50 compute_overlap subpixel_accuracy computation_parameters=[Save memory (but be slower)] image_output=[Fuse and display]");
				run("Stack to RGB");
				filename = substring(daylist[daynum],0,lengthOf(daylist[daynum])-1) + ".tif";
				saveAs("Tiff", input + earlist[earnum] + daylist[daynum] + filename);
				close();
				//close();
				//stiching the original file 
				
				/*
				//stich and save mod file
				if(File.exists(input + earlist[earnum] + daylist[daynum] + "01_mod2.tif"))
				{
					run("Grid/Collection stitching", "type=[Grid: row-by-row] order=[Right & Down                ] grid_size_x=3 grid_size_y=3 tile_overlap=10 first_file_index_i=1 directory=["+ input + earlist[earnum] + daylist[daynum] +  "] file_names={ii}_mod2.tif output_textfile_name=TileConfiguration.txt fusion_method=[Linear Blending] regression_threshold=0.30 max/avg_displacement_threshold=2.50 absolute_displacement_threshold=3.50 compute_overlap subpixel_accuracy computation_parameters=[Save memory (but be slower)] image_output=[Fuse and display]");
					run("Stack to RGB");
					filename = substring(daylist[daynum],0,lengthOf(daylist[daynum])-1) + "_mod.tif";
					saveAs("Tiff", input + earlist[earnum] + daylist[daynum] + filename);
					close();
					close();
				}
				//stich and save mod file
				*/
			}
		}
	}
}