# Randomness Functions

chance = function(probability) {
  return (runif(1, 0, 1) < probability)
}

getRandomInt = function(min, max) {
  return (floor(runif(1, min, max + 1)))
}

# Point Functions

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

pointIsInBounds = function(point, size) {
  x = point@x
  y = point@y
  return (x > 0 && x <= size && y > 0 && y <= size)
}

