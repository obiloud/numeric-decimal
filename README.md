# Decimal

An implementation of a fixed point decimal number. Floating point types make sense when numerical approximation is acceptable and you care primarily about performance rather than correctness. It is a known fact that using Double or Float for monetary values is unacceptable.
Values like `NaN`, `+/-Infinity` and `+/-0` have no meaning in handling money. In addition, the inability to represent most decimal values exactly should be enough reason to avoid floating point.

It is safe, because things like integer overflows, underflows, division by zero etc. are checked for during the runtime.

At the time only subset of rounding algorythms is implemented:

- RoundDown
- RoundTowardsZero
- HalfToEven

Plan is to add more in the future.

