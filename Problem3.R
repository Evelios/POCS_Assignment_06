# Problem 3
# Coding up 2-d percolation model

main = function() {
  size = 5
  design_ammount = 5

  forest = getBlankForest(size)

  running = TRUE
  while (running) {
    new_loc = findBestTreePlacement(forest, design_ammount)
    if (is.null(new_loc)) {
      running = FALSE
    }
    else {
      forest[new_loc@x, new_loc@y] = TRUE
      getForestDamageFromStrikes(forest)
    }
  }

}



getLightningStrikeProbablilities = function(size) {
  lightning_probabilities = matrix(1 / (size * size), nrow = size, ncol = size)

  return (lightning_probabilities)
}

# ---- Forest Specific Code ----

getBlankForest = function(size) {
  return (matrix(data=FALSE, nrow=size, ncol=size))
}

findBestTreePlacement = function(forest, attempts) {

  empty_locations = getRandomEmptyLocations(forest, attempts)
  lowest_score = Inf
  best_location = NULL

  for (location in empty_locations) {
    forest_sizes = calculateForestSize(forest)
    avg_trees_damaged =  getForestDamageFromStrikes(forest_sizes)

    if (avg_trees_damaged < lowest_score) {
      lowest_score = avg_trees_damaged
      best_location = location
    }
  }

  return (best_location)
}

getForestDamageFromStrikes = function(forest_sizes) {
  size = getForestSize(forest_sizes)
  probabilities = getLightningStrikeProbablilities(size)
  damage_distribution = probabilities * forest_sizes
  average_damage_done = sum(rowSums(damage_distribution))
  return (average_damage_done)
}
getRandomEmptyLocations = function(forest, num_items) {
  size = getForestSize(forest) 
  open_locations = c()
  for (y in 1:size) {
    for (x in 1:size) {
      if (!forest[x, y]) {
        open_locations = c(open_locations, Point(x, y))
      }
    }
  }

  if (length(open_locations) > num_items) {
    return(sample(open_locations, num_items))
  }
  else {
    return (open_locations)
  }
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

# ---- Helper Functions ----

setClass('Point', representation(x = 'numeric', y = 'numeric'))

Point = function(x, y) {
  return (new ('Point', x=x, y=y))
}

getPointNeighbors = function(forest, point) {
  point1 = Point(point@x + 1, point@y)
  point2 = Point(point@x - 1, point@y)
  point3 = Point(point@x, point@y + 1)
  point4 = Point(point@x, point@y - 1)

  neighbors = c(point1, point2, point3, point4)

  valid_neighbors = c()
  for (neighbor in neighbors) {
    if (pointIsInBounds(forest, neighbor)) {
      valid_neighbors = c(valid_neighbors, neighbor)
    }
  }

  return (valid_neighbors)
}

pointInVector = function(test, vector) {
  for (point in vector) {
    if (test@x == point@x && test@y == point@y) {
      return (TRUE)
    }
  }
  return (FALSE)
}

pointIsInBounds = function(forest, point) {
  size = getForestSize(forest)
  x = point@x
  y = point@y
  return (x > 0 && x <= size && y > 0 && y <= size)
}

getForestSize = function(forest) {
  return (dim(forest)[1])
}

getRandomInt = function(min, max) {
  return (floor(runif(1, min, max + 1)))
}

main()