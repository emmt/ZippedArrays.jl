# User visible changes in `ZippedArrays`

## Version 0.1.3

- Extend methods `append!`, `copy`, `deepcopy`, `push!`, `resize!`, `similar`,
  and `sizehint!` for zipped arrays. As a result, out-of-place sorting of a
  zipped array yields a zipped array.

- Export aliases `ZippedVector` and `ZippedMatrix`.

## Version 0.1.2

- `ZippedArray{Tuple{T...}}(undef, dims)` builds an uninitialized zipped array
  of size `dims` whose elements are tuples of type `Tuple{T...}`.
