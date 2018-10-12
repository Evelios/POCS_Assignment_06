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

main()