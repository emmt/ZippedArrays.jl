# User visible changes in `ZippedArrays`

## Unreleased

### Added

- Pass all [`Aqua.jl`](https://github.com/JuliaTesting/Aqua.jl) tests.

## Version 0.2.0

- Element type of a zipped array can be a tuple type (as before) or a structure
  type. For example, `ZippedArray{Complex{Float32}}(undef, dims)` creates an
  array of `Complex{Float32}` elements, of size `dims`, and such that the real
  and imaginary parts are stored into two separate arrays. As another example,
  `Z = ZippedArray{Complex{Float32}}(A, B))` wraps array `A` and `B` into an
  abstract arrays of `Complex{Float32}` elements, of same size as `A` and `B`
  and such that `Z[i]` yields `Complex{Float32}(A[i],B[i])`.

- A zipped array, say `A::ZippedArray{T,N}`, enforces conversion to its element
  type `T` when indexed. This guarantees the type of the returned elements with
  no speed penalties when conversion is a no-op. This can be exploited to
  perform lazy conversion. Method `ZippedArrays.build` may be specialized for
  specific types `T`.

## Version 0.1.4

- Fix signature for base abstract array methods (`getindex`, `setindex!`, etc.)
  to correctly dispatch upon fast (linear) and slow (Cartesian) indexing.

## Version 0.1.3

- Extend methods `append!`, `copy`, `deepcopy`, `push!`, `resize!`, `similar`,
  and `sizehint!` for zipped arrays. As a result, out-of-place sorting of a
  zipped array yields a zipped array.

- Export aliases `ZippedVector` and `ZippedMatrix`.

## Version 0.1.2

- `ZippedArray{Tuple{T...}}(undef, dims)` builds an uninitialized zipped array
  of size `dims` whose elements are tuples of type `Tuple{T...}`.
