# Author = Sean Hendryx

# Script matches faces from landmarks points

##Load packages:
packages = c('ggplot2', 'data.table', 'dplyr', 'tools', 'plotly', 'phia', 'xtable', 'Morpho')
lapply(packages, library, character.only = TRUE)

# Function Declarations:

#improved print function:
printer <- function(string, var){
	print(paste0(string, " ", var))
}


euc.dist <- function(x1, x2){
	sqrt(sum((x1 - x2) ^ 2))
}	


setwd("/Users/seanhendryx/IVILab/Sean_Test_Data/300W/OpenFace_Output")

#Read .pts file in:
list <- scan("indoor_001_det_0.pts", skip = 3, nmax = 68*2)
mat <- matrix(list, nrow = 68, ncol = 2, byrow = TRUE)
DT = as.data.table(mat)
DT[,name := "indoor_001_det_0.pts"]
DT[,image := "indoor_001"]
file = "indoor_001_det_0.pts"
DT[,face := file_path_sans_ext(read.table(text = file, sep = "_", as.is = TRUE)$V4)]

pointID <- rownames(DT)
DT <- cbind(pointID=pointID, DT)

#Add unique id for each point in all images
DT[,UID := paste0(substr("indoor_001", 1,10), "_", pointID)]


files <- list.files(pattern = "\\.pts$")

# Start at position 2, since first image already loaded into datatable
files <- files[2:length(files)]

#Make datatable of all points:
#files <- files[1:which(files == "indoor_055_det_0.pts")]
for(file in files){
	list <- scan(file, skip = 3, nmax = 68*2)
	mat <- matrix(list, nrow = 68, ncol = 2, byrow = TRUE)
	DT_i = as.data.table(mat)
	DT_i[,name := file]
	DT_i[,image := substr(file, 1,10)]

	#Add face indicator column:
	DT_i[,face := file_path_sans_ext(read.table(text = file, sep = "_", as.is = TRUE)$V4)]
    
	#Add point id:
	pointID <- rownames(DT_i)
    
    DT_i <- cbind(pointID=pointID, DT_i)
    
    #Add unique id for each point in all images
    DT_i[,UID := paste0(substr(file, 1,10), "_", pointID)]
    
	DT <- rbind(DT, DT_i)
}

#Now make same DT of ground truths (annotated dataset):
setwd("/Users/seanhendryx/IVILab/Sean_Test_Data/300W/01_Indoor")
files <- list.files(pattern = "\\.pts$")

#Read .pts file in:
list <- scan("indoor_001.pts", skip = 3, nmax = 68*2)
mat <- matrix(list, nrow = 68, ncol = 2, byrow = TRUE)
annoDT = as.data.table(mat)
annoDT[,name := "indoor_001.pts"]
annoDT[,image := "indoor_001"]


pointID <- rownames(annoDT)
annoDT <- cbind(pointID=pointID, annoDT)

#Add unique id for each point in all images
annoDT[,UID := paste0(substr("indoor_001", 1,10), "_", pointID)]

# Start at position 2, since first image already loaded into datatable, annoDT
files <- files[2:length(files)]
for(file in files){
	list <- scan(file, skip = 3, nmax = 68*2)
	mat <- matrix(list, nrow = 68, ncol = 2, byrow = TRUE)
	DT_i = as.data.table(mat)
	DT_i[,name := file]
	DT_i[,image := substr(file, 1,10)]

    #Add point id:
	pointID <- rownames(DT_i)
    
    DT_i <- cbind(pointID=pointID, DT_i)
    
    #Add unique id for each point in all images
    DT_i[,UID := paste0(substr(file, 1,10), "_", pointID)]

	annoDT <- rbind(annoDT, DT_i)
}

######################################################################################################################################################################################################################################################################################################################################################################################
# Face matching algorithm

# Find matching faces base on euclidean distance of landmark centroids.  Closest centroids assumed to be the same face.
# Instantiate empty matched-faces datatable (includes all images that have matches in annotated dataset based on closest centroid and images that only include one face):
matchedDT <- data.table()

# Make list of unique images in OpenFace output:
images <- unique(DT[,image])


#loop through images from OpenFace output:
for(imagei in images){
	# Make list of faces found by OpenFace,
	# to loop through each face found by OpenFace:
	faces <- unique(DT[image == imagei, face])
	#instantiate distance as infinite, to be used in finding "closest" face centroid within each image
	distance <- Inf
	#first test to see if there are multiple faces:
	if(length(faces) == 1){
		matchedDT <- rbind(matchedDT, DT[image == imagei])
	} else {
		annoPoints <- annoDT[image == imagei, .(V1, V2)]
		annoCentr <- colMeans(annoPoints)
		printer("annoCentr",annoCentr)
		for(facei in faces){
			predPoints <- DT[image == imagei & face == facei, .(V1, V2)]
			predCentr <- colMeans(predPoints)
			distancei <- euc.dist(predCentr, annoCentr)
			if(distancei < distance){
				distance <- distancei
				closestMatch <- facei
			}# end if
		}# end faces for loop
		matchedDT <- rbind(matchedDT, DT[image == imagei & face == closestMatch,])
	}
}

######################################################################################################################################################################################################################################################################################################################################################################################

# Test to make sure only one face matched per image:
for(imagei in images){
	faces <- unique(matchedDT[image == imagei, face])
	if(length(faces) > 1){
		print(paste0("ERROR: multiple faces for same image concatenated into data table from image: \n", imagei))
	} else{
		#print(paste0("Test passed: only one face concatenated for image: \n", imagei))
	}
}



