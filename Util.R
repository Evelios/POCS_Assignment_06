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

getPointNeighbors = function(point, size) {
  point1 = Point(point@x + 1, point@y)
  point2 = Point(point@x - 1, point@y)
  point3 = Point(point@x, point@y + 1)
  point4 = Point(point@x, point@y - 1)

  neighbors = c(point1, point2, point3, point4)

  valid_neighbors = c()
  for (neighbor in neighbors) {
    if (pointIsInBounds(neighbor, size)) {
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

pointInVector = function(test, vector) {
  for (point in vector) {
    if (test@x == point@x && test@y == point@y) {
      return (TRUE)
    }
  }
  return (FALSE)
}