source('Util.R')

getNormalizedLargestForest = function(forest) {
  return (getLargestForestSize(forest) / getForestDim(forest))
}

getLargestForestSize = function(forest) {
  return(max(calculateForestSize(forest)))
}

siteIsOccupied = function(forest, point) {
  return (forest[point@x, point@y]) 
}

getForestDim = function(forest) {
  return (dim(forest)[[1]])
}

calculateForestSize = function(forest) {
  forest_sizes = 1*forest
  size = getForestDim(forest)

  for (y in 1:size) {
    for (x in 1:size) {
      if (forest_sizes[x, y] == 1) {
        start_point = Point(x, y)
        forest_sizes = calculateForestSizesFromLocation(forest_sizes, start_point)
      }
    }
  }
  return (forest_sizes)
}

calculateForestSizesFromLocation = function(forest, point) {
  if (forest[point@x, point@y] == 0) {
    return (forest)
  }

  forest_size = getForestDim(forest)
  forest_size = 0
  frontier = c(point)
  explored = c()
  refill_locations = c()

  # Determine the forest size at the current position
  while (length(frontier) > 0) {
    to_explore = frontier[[1]]
    frontier = tail(frontier, -1)
    explored = c(explored, to_explore)

    if (forest[to_explore@x, to_explore@y] == 1) {
      forest_size = forest_size + 1
      refill_locations = c(refill_locations, to_explore)

      # Add Neighbors
      neighbors = getPointNeighbors(to_explore, size)

      valid_neighbors = c()
      for (neighbor in neighbors) {
        if (!pointInVector(neighbor, frontier) &&
            !pointInVector(neighbor, explored)) {
          valid_neighbors = c(valid_neighbors, neighbor)
        }
      }
      frontier = c(frontier, valid_neighbors)
    }
  }

  # Update the forest sizes for each of the locations
  updated_forest = forest
  for (pos in refill_locations) {
    updated_forest[pos@x, pos@y] = forest_size
  }

  return (updated_forest)
}