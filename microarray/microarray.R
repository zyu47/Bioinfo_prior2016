# load the script from the internet that is used in install bioconductor
source("http://bioconductor.org/biocLite.R")

# Each of these commands tells Bioconductor to download and install each package
biocLite()
biocLite("affy")
biocLite("oligo")
biocLite("limma")

# load the oligo library
library(oligo)

# Set directory
setwd('/Users/Zhixian/Desktop/Liang')

# Read in the CEL files in the directory
celFiles <- list.celfiles()
affyRaw <- read.celfiles(celFiles)

# You might need to install and load a package for the specific array you are using (this example is mouse gene 2.0 ST)
# It may try to load it automatically, but may fail.  Install & load the library manually if this happens.
library(pd.mogene.2.0.st) 
eset <- rma(affyRaw)

# Finally, save the data to an output file to be used by other programs, etc (Data will be log2 transformed and normalized)
write.exprs(eset,file="data.txt")


###########Adding Gene Annotation to Normalized Expression Output#############
biocLite("mogene20sttranscriptcluster.db")

# This assumes you already normalized the data, and the object "eset" has the data in it (from above)
# Load annotation library
library(mogene20sttranscriptcluster.db)
library(mogene20stprobeset.db)

# Strategy is to create data frame objects and merge them together - put expression info into a data frame
my_frame <- data.frame(exprs(eset))

# Put annotation information in a data frame.  To get specific fields, use packageNameSYMBOL, where the caps part names the type of data you're after
# To get a list of available annotation information, run the packagename with () at the end, i.e. mogene20sttranscriptcluster()
ls("package:mogene20stprobeset.db")
Annot <- data.frame(ACCNUM=sapply(contents(mogene20sttranscriptclusterACCNUM), paste, collapse=", "), SYMBOL=sapply(contents(mogene20sttranscriptclusterSYMBOL), paste, collapse=", "), DESC=sapply(contents(mogene20sttranscriptclusterGENENAME), paste, collapse=", "))
Annot <- data.frame(SYMBOL=sapply(contents(mogene20sttranscriptclusterSYMBOL), paste, collapse=", "), GENENAME=sapply(contents(mogene20sttranscriptclusterGENENAME), paste, collapse=", "))

# Merge data frames together (like a database table join)
all <- merge(Annot, my_frame, by.x=0, by.y=0, all=T)

# Write out to a file:
write.table(all,file="data.ann.txt",sep="\t")

y <- c(2,1,1,2,1,2,2,2,1)
samfit <- SAM(my_frame, y,resp.type = "Two class unpaired")
tmp <- samfit$samr.obj$x

##SAM excel: copy rscproxy.dll from C:\Users\Zhixian\Documents\R\win-library\3.0\rscproxy\libs\i386 to C:\Program Files\R\R-3.0.2\bin\i386