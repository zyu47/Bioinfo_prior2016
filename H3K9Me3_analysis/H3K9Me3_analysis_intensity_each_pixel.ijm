//for(i = 0; i != 11; ++i)
{	
//	open("\\\\BioArk.bio.unc.edu\\Bautchlab\\Zhixian\\Results\\Maitaining centrosome\\Images\\H3K9Me3 Tet+DOX quant DRAQ\\012916 Day4\\"+i+".oif");
	name = getTitle();
	run("Z Project...", "projection=[Sum Slices]");
	selectWindow(name);
	close();
	selectWindow("SUM_" + name);
	run("Split Channels");
	//selectWindow("C2-SUM_" + name);
	//close();
	selectWindow("C1-SUM_" + name);
	run("8-bit");
	//run("Duplicate...", "title=new_image");
	//selectWindow("new_image");
	selectWindow("C2-SUM_" + name);
	run("8-bit");
	run("Gaussian Blur...", "sigma=2");
	waitForUser("threshold");
	run("Convert to Mask");
	run("Watershed");
	
	width = getWidth();
	height = getHeight();
	pmatrix = newArray(width*height);
	for(i=0; i<height; i++)
	{
		for(j=0; j<width; j++)
		{
			pmatrix[j + i*height] = 0;
		}
	}
	count = 1;
	//newImage("temp", "RGB", width, height, 1);
	selectWindow("C2-SUM_" + name);
	for(i=0; i<height; i++)
	{
		for(j=0; j<width; j++)
		{
			if(pmatrix[j + i*height] == 0 && getPixel(j,i) !=0)
			{
				doWand(j,i);
				for(k = i; k<height; ++k)
				{
					for(l = 0; l<width; ++l)
					{
						if(Roi.contains(l, k))
						{
							pmatrix[l + k*height] = count;
						}
					}
				}
	//			getSelectionCoordinates(x,y);
	//			newImage("test","8-bit",width,height,1);
	//			selectImage("test");
	//			for(k=0; k<y.length; ++k)
	//			{
	//				pmatrix[x[k] + y[k]*height] = count;
	//				print (x[k] + "\t" + y[k]);
	//				setPixel(x[k],y[k],0);
	//			}
	//			getSelectionBounds(a1, a2, a3, a4);
	//			selectImage("temp");
	//			makeRectangle(a1, a2, a3, a4);	
	//			getSelectionCoordinates(x,y);
	//			for(t=0; t<y.length; ++t)
	//			{
	//				pmatrix[x[k] + y[k]*height] = count;
	//				print (x[k] + "\t" + y[k]);
	//				setPixel(x[t],y[t],0);
	//			}
				setColor(255,0,0);
				setFont("SansSerif" , 28, "antialiased");
				drawString("#" + count, j, i);
				count = count + 1;
	//			cont = getNumber("continue", 1);
	//			selectImage("C2-SUM_1.oif");
			}
		}
	}
	selectImage("C1-SUM_" + name);
	for(i=0; i<height; i++)
	{
		for(j=0; j<width; j++)
		{
			tmp = pmatrix[j + i*height];
			if(tmp != 0)
				print(tmp + "\t" + getPixel(j,i));
		}
	}
	selectWindow("C1-SUM_" + name);
	close();
	waitForUser("Analyze");
	//selectWindow("new_image");
	//close();
	selectWindow("C2-SUM_" + name);
	close();
}