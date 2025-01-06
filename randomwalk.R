# Brownian random walk simulation
# www.overfitting.net
# https://www.overfitting.net/2025/01/random-walk-browniano-con-r.html

# In mathematics, a random walk is a stochastic process that describes
# a path that consists of a succession of random steps on some mathematical
# space. When this motion is continuous, i.e. when all locations in the space
# can be reached, we can talk about Brownian motion which is studied in Physics
# to model the behaviour of particles suspended in a medium (a liquid or a gas).
# This motion pattern typically consists of random fluctuations in a particle's
# position inside a fluid sub-domain, followed by a relocation to another
# sub-domain.

library(png)


# Simulation parameters
DIMX=1920  # Full HD resolution
DIMY=1080
Gamma=2.2  # gamma lift
fps=24
audioduration=220.325
NFRAMES=ceiling(audioduration*fps)  # 220.325s audio at 24fps -> 5288 frames
NSTEPS=10000000  # number of steps of random walk (excl. starting position)


# Calculate Random walk XY coords
set.seed(100)  # reproducible results
walk=matrix(rnorm(2*(NSTEPS+1)), ncol=2)  # normal distribution
#walk=matrix(runif(2*(NSTEPS+1))-0.5, ncol=2)  # uniform distribution
walk[1,]=c(0,0)  # starting from (0,0)
walk=apply(walk, 2, cumsum)  # accumulate steps per column
# (Did I ever tell you I love the cumsum() function?)


# Fit random walk to output dimensions and round coords

# Check for aspect ratio
DIMXwalk=max(walk[,1])-min(walk[,1])
DIMYwalk=max(walk[,2])-min(walk[,2])
if (DIMYwalk>DIMXwalk) {
    print("X and Y coords were switched")
    walk=walk[,2:1]  # switch X and Y
    DIMXwalk=DIMYwalk
    DIMYwalk=max(walk[,2])-min(walk[,2])    
}
# Plot continuous random walk
plot(walk, type='l', asp=1)
abline(h=0, v=0, col='red')

# Round position values to output matrix range
walkint=walk*0
if (DIMXwalk/DIMYwalk > DIMX/DIMY) {
    # Random walk is more panoramic than output
    # 1. Fit X dimensions to 1..DIMX -> fscale
    # 2. Centre Y dimensions applying same fscale
    MINIMO=min(walk[,1])
    MAXIMO=max(walk[,1])
    MEDIO=(max(walk[,2])+min(walk[,2]))/2
    fscale=(DIMX-1)/(MAXIMO-MINIMO)

    walkint[,1]=round(fscale*(walk[,1]-MINIMO)+1)
    walkint[,2]=round(fscale*(walk[,2]-MEDIO)+DIMY/2)
    
    x0=round(fscale*(0-MINIMO)+1)
    y0=round(fscale*(0-MEDIO)+DIMY/2)
} else {
    # Random walk is less panoramic than output
    # 1. Fit Y dimensions to 1..DIMY -> fscale
    # 2. Centre X dimensions applying same fscale
    MINIMO=min(walk[,2])
    MAXIMO=max(walk[,2])
    MEDIO=(max(walk[,1])+min(walk[,1]))/2
    fscale=(DIMY-1)/(MAXIMO-MINIMO)
    
    walkint[,2]=round(fscale*(walk[,2]-MINIMO)+1)
    walkint[,1]=round(fscale*(walk[,1]-MEDIO)+DIMX/2)
    
    y0=round(fscale*(0-MINIMO)+1)
    x0=round(fscale*(0-MEDIO)+DIMX/2)
}
rm(walk)
# Plot discretized random walk
plot(walkint, type='l', asp=1)
abline(h=y0, v=x0, col='red')


# Build animation
imgbgd=readPNG("randomwalkbgdnorm.png")
#imgbgd=readPNG("randomwalkbgdunif.png")
INC=floor(NSTEPS/NFRAMES)  # INC*NFRAMES <= NSTEPS some steps may be dropped

# Precalculate absolute max density to normalize entire animation
img=matrix(0, nrow=DIMY, ncol=DIMX)
for (i in 1:(INC*NFRAMES))  # very fast loop
    img[walkint[i,2], walkint[i,1]]=img[walkint[i,2], walkint[i,1]]+1
MAXDENSITY=max(img)

# Generate frames    
img=img*0
INI=1
for (f in 0:(NFRAMES-1)) {
    FIN=INI+INC-1
    for (i in INI:FIN)  # very fast loop
        img[walkint[i,2], walkint[i,1]]=img[walkint[i,2], walkint[i,1]]+1
    y1=walkint[FIN,2]  # last position reached
    x1=walkint[FIN,1]
    INI=INI+INC
    
    imgout=(img/MAXDENSITY)^(1/Gamma)  # normalize output grayscale
    imgout[y1,]=(1-imgout[y1,])*0.5  # draw current walk position
    imgout[,x1]=(1-imgout[,x1])*0.5    
    imgout=imgout[nrow(imgout):1,]  # flip rows
    imgout=replicate(3, imgout)  # add colour
    imgout[,,1:2]=(sin(pi*(imgout[,,1:2]-1)+pi/2)+1)/2  # add R and G contrast
    imgout=imgout+imgbgd  # add background
    imgout[imgout>1]=1  # clip sat values

    # Save frame
    name=paste0("randomwalk", ifelse(f<10, "000", ifelse(f<100, "00",
                              ifelse(f<1000, "0", ""))), f, ".png")
    print(paste0(f+1, "/", NFRAMES, ": Writing '", name, "'..."))
    writePNG(imgout, name)
}


# FFmpeg commands:
# 24 fps, 220.325 s, 5287.8 frames -> 5288 total frames
# MP4 Video (MPEG-4 AVC/H.264):

# ffmpeg -loop 1 -framerate 24 -i randomwalk%04d.png -i popcorn.wav
# -t 220.325 -c:v libx264 -crf 23 -pix_fmt yuv420p randomwalknorm.mp4

# ffmpeg -loop 1 -framerate 24 -i randomwalk%04d.png -i popcorn.wav
# -t 220.325 -c:v libx264 -crf 23 -pix_fmt yuv420p randomwalkunif.mp4



##############################################
# High resolution 1000 million steps random walk

library(tiff)


# Simulation parameters
Gamma=2.2  # gamma lift
NSTEPS=1000000000  # number of steps of random walk (excl. starting position)

# Calculate Random walk XY coords
set.seed(100)  # reproducible results
walk=matrix(rnorm(2*(NSTEPS+1)), ncol=2)  # normal distribution
walk[1,]=c(0,0)  # starting from (0,0)
walk=apply(walk, 2, cumsum)  # accumulate steps per column

walk[,1]=walk[,1]-min(walk[,1])
walk[,2]=walk[,2]-min(walk[,2])
walk=round(walk*0.2)+1  # max(walk)=70360.42
DIMX=max(walk[,1])
DIMY=max(walk[,2])

# Build matrix (192Mpx)
img=matrix(0, nrow=DIMY, ncol=DIMX)
for (i in 1:nrow(walk))  # very fast loop
    img[walk[i,2], walk[i,1]]=img[walk[i,2], walk[i,1]]+1
img=img/max(img)

writeTIFF(img, "ramdonwalk_1000million_g1.0.tif", bits.per.sample=16)
writeTIFF(img^(1/Gamma), "ramdonwalk_1000million_g2.2.tif", bits.per.sample=16)
