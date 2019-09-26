# generate 10000 X coordinates
xVal = runif(10000,0,1)

# generate 10000 Y coordinates
yVal = runif(10000,0,1)

# calculate distance between center of circle and all points
# center of circle at (0.5,0.5)
distVal = sqrt((xVal-0.5)^2 + (yVal-0.5)^2)

# Find the total number of points within circle
circlePoints = length(which(distVal <= 0.5))
print(circlePoints)

# Find the total number of points within square
squarePoints = length(distVal)
print(squarePoints)

# calculate ratio of points within circle to within square
result = circlePoints/squarePoints
print(result)

# calculate value of pi from relation
piVal = result * 4
print(piVal)





