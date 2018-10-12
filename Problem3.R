source("Util.R")


main = function() {
  probability = 0.25 
  size = 5
  n_tests = 100

  createSheepFarm(probability, size)
}

createSheepFarm = function(probability, size) {
  v_chance = Vectorize(chance)
  farm_vector = v_chance(rep(probability, size*size))
  farm_matrix = matrix(farm_vector, nrow=size, ncol=size)

  return (farm_matrix)
}

calculateForestSize = function(forest) {
  forest_sizes = 1*forest
  size = getForestSize(forest)

  forest_sizes

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
      neighbors = getPointNeighbors(forest, to_explore)

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

main()