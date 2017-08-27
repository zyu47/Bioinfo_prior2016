//use this to delete points from cell counter outside of wound area
//input center coordinates for each image
//input destination
a = 475.0; //radius on x-axis, ie. width/2, make sure add .0
b = 550.0; 
time_point = getNumber("How many slices?", 17);
x_coord = newArray(time_point);
y_coord = newArray(time_point);
centers = File.openAsString("");
center_array = split(centers);
for (j = 0; j!=time_point; ++j)
{
	x_coord[j] = center_array[2*j];//getNumber("X coordinate for center in slice " + j + 1, 0);
	y_coord[j] = center_array[2*j+1];//getNumber("Y coordinate for center in slice " + j + 1, 0);
}

//delete useless points by calculating 
xml_file = File.openAsString("");
file_length = lengthOf(xml_file);
starti = indexOf(xml_file, "<Marker>");
output = substring(xml_file, 0, starti);
for(i = starti; i < file_length && starti != -1; )
{
	endi = indexOf(xml_file, "</Marker>",i);
	test = substring(xml_file, starti, endi + 9);
	//extract coordinate
	x_i_start = indexOf(test, "X");
	x_i_end = indexOf(test, "</MarkerX>");
	y_i_start = indexOf(test, "Y");
	y_i_end = indexOf(test, "</MarkerY>");
	z_i_start = indexOf(test, "Z");
	z_i_end = indexOf(test, "</MarkerZ>");
	x = parseInt(substring(test, x_i_start+2, x_i_end));
	y = parseInt(substring(test, y_i_start+2, y_i_end));
	z = parseInt(substring(test, z_i_start+2, z_i_end));
	//print (x+"\t"+y+"\t"+z+"\t"+starti+"\t"+endi + "\n");
	//determine if deleting
	determinant = pow(((x-x_coord[z-1])/a),2) + pow(((y-y_coord[z-1])/b),2);
	//print(determinant);
	if(determinant <= 1)
	{
		output = output + test;
	}
	starti = indexOf(xml_file, "<Marker>", endi);
	if(starti != -1)
	{
		output = output + substring(xml_file, endi + 9, starti);
	}
	else
	{
		output = output + substring(xml_file, endi + 9, file_length);
	}
	i = starti;
}
File.saveString(output,"G:\\wounding\\Expt.208_Flk1-EGFP_wounding_WT_data\\2013-07-21_Expt.208_flk1EGFP_wounding_17812\\right_ear\\CellCounter_Stack_17812_right_ear_wound_only.xml");
