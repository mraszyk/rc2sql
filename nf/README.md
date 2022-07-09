This directory contains benchmarks supporting our observations
in Example 2.18 (SRNF vs ENF) and Section 4.2.6 (LEFT JOIN vs EXCEPT).

To run the benchmarks, execute the script `run.sh` as shown below
in the directory whose name is shown next to the respective query.

The output lists the queries and execution times (up to one minute).

enf1: P(x, y) AND NOT (Q(x) AND R(y))
=====================================

$ ./run.sh 400 200 0
NOT IN too slow

$ ./run.sh 4000 2000 1
LEFT JOIN faster than EXCEPT on equal attributes
Query 1, 2 are in ENF
Query 3, 4 are in SRNF (much faster)

$ ./run.sh 40000 20000 3
LEFT JOIN faster than EXCEPT on distinct attributes

enf2: P(x, y) AND NOT (Q(x, y) AND R(y))
========================================

$ ./run.sh 5000 4746 0
Query 0
Filter: (NOT (hashed SubPlan 1))

$ ./run.sh 5000 4747 0
Query 0 uses nested loop after this threshold
Filter: (NOT (SubPlan 1))

$ ./run.sh 200000 175000 1
LEFT JOIN faster than EXCEPT on equal attributes
LEFT JOIN faster than EXCEPT on distinct attributes
Query 1, 2 are in ENF
Query 3, 4, 5 are in SRNF (slightly faster)

enf3: P(x, y) AND (Q(x) OR NOT R(y))
====================================

$ ./run.sh 100000 75000 0
- Query 0 (single SELECT): fastest
- Query 1 (SRNF), Query 2 (ENF): similar performance

left1: P(x, y) AND NOT Q(x, y)
==============================

$ ./run.sh 20 130854 0
Query 0
Filter: (NOT (hashed SubPlan 1))

$ ./run.sh 20 130855 0
Query 0 uses nested loop after this threshold
Filter: (NOT (SubPlan 1))

$ ./run.sh 400000 300000 1
LEFT JOIN faster than EXCEPT on equal attributes

left2: P(x, y) AND NOT Q(x)
==============================

$ ./run.sh 20 116164 0
Query 0
Filter: (NOT (hashed SubPlan 1))

$ ./run.sh 20 116165 0
Query 0 uses nested loop after this threshold
Filter: (NOT (SubPlan 1))

$ ./run.sh 400000 300000 1
LEFT JOIN faster than EXCEPT on distinct attributes
