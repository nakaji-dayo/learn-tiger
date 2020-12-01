 data A = A Int A | N

x = A 9 N
x' = A 9 (A 8 N)

-- 末端からtraverseしtcできていれば
-- 根本ではNameでの比較で良い
