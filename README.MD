# This is a travel agent problem that has been written in lisp. The goal is to find the shortest path using A* going from start city to the goal city.

## We have our open and closed list in a 2 element type of list. Such will look like this for example (10 43) (423 21) First element is our open list and second is closed list.

### COST of FLYING equals 400 plus the distance between the cities that are availible.

### COST to take the TRAIN will be equal to 200 plus the distance between cities IF AND ONLY IF distance is
* less than 800 and will be equal to 200 plus 1.5 times the distance between cities otherwise.

### COST of taking a BUS will be equal to 100 plus the distance between the cities if the distance is less than 400
 * and 100 plus twice the distance between the cities otherwise.

### The goal of this program is to go from a start city and make it to the goal city using the A Star Algorithm. The path that is taken is printed and the cost of the path is also printed.

## Output Can be Viewed Here:
[Travel Agent Output](TravelAgent-output)

