EXPLAIN ANALYZE
SELECT P.x, P.y
FROM P
WHERE P.x NOT IN (SELECT Q.x FROM Q)