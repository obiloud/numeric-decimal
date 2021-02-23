# Decimal

An implementation of a decimal point data type. It is safe, because things like integer overflows, underflows, division by zero etc. are checked for during the runtime and are prevented in pure code.

At the time only subset of rounding algorythms is implemented:

- RoundDown
- RoundTowardsZero
- HalfToEven

The plan is to add more in the future.

