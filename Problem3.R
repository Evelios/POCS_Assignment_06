source('Util.R')
source('Forest.R')


main = function() {
  probability = 0.25 
  n_tests = 100

  grid_sizes = c(20, 50, 100, 200)
  avg_farm_sizes = c()

  for (size in grid_sizes) {
    avg_farm_size = getAverageSheepFarmSize(probability, size, n_tests)
    avg_farm_sizes = c(avg_farm_sizes, avg_farm_size)
  }

  print(avg_farm_sizes)
}

getAverageSheepFarmSize = function(probability, size, n_tests) {
  sizes = c()

  for (i in 1:n_tests) {
    sheep_farm = createSheepFarm(probability, size)
    norm_largest_forest = getNormalizedLargestForest(sheep_farm)
    sizes = c(sizes, norm_largest_forest)
  }

  return (mean(sizes))
}

createSheepFarm = function(probability, size) {
  tree_farm = createTreeFarm(probability, size)
  sheep_farm = addSheepToTreeFarm(tree_farm)

  return (sheep_farm)
}

createTreeFarm = function(probability, size) {
  v_chance = Vectorize(chance)
  farm_vector = v_chance(rep(probability, size*size))
  farm_matrix = matrix(farm_vector, nrow=size, ncol=size)

  return (farm_matrix)
}

addSheepToTreeFarm = function(farm) {
  sheep_farm = farm
  size = dim(farm)[[1]]

  for (y in 1:size) {
    for (x in 1:size) {
      location = Point(x, y)
      if (!siteIsOccupied(farm, location)) {

        # Check to see if a sheep goes there
        neighbors = getPointNeighbors(location, size)
        for (neighbor in neighbors) {
          if (siteIsOccupied(farm, neighbor)) {
            sheep_farm[location@x, location@y] = TRUE
          }
        }
      }
    }
  }

  return(sheep_farm) 
}

main()