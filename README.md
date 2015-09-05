# scala-simplex-dsl
A domain Specific Language for simplex algorithm in scala.

## Usage
To solve the following linear programming problem:

```
maximize:
  x[0] + x[1] + x[2]
subject to:
  5x[0] +  x[1] + 2x[2] <= 20
  2x[0] + 2x[1] + 6x[2] <= 30
  2x[0] + 6x[1] + 4x[2] <= 40
  x[0], x[1], x[2] >= 0
```

evaluate the following expression in SBT console.

```
$ sbt console
[info] Set current project to scala-simplex-dsl (in build file:path/to/scala-simplex-dsl/)
[info] Starting scala interpreter...
[info] 
import simplex._
Welcome to Scala version 2.11.7 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_31).
Type in expressions to have them evaluated.
Type :help for more information.

scala> maximize (x(0) + x(1) + x(2)) subjectTo {
     |   5*x(0) +   x(1) + 2*x(2) <= 20
     |   2*x(0) + 2*x(1) + 6*x(2) <= 30
     |   2*x(0) + 6*x(1) + 4*x(2) <= 40
     | }
maximum value of objective function: 9.000000000000002
value of variables:
x0 = 1.9999999999999996
x1 = 4.0
x2 = 3.0
```
