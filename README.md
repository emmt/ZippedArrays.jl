# Zipping Julia arrays together

[![License](http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](./LICENSE.md)
[![Build Status](https://github.com/emmt/ZippedArrays.jl/actions/workflows/CI.yml/badge.svg?branch=master)](https://github.com/emmt/ZippedArrays.jl/actions/workflows/CI.yml?query=branch%3Amaster)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/emmt/ZippedArrays.jl?branch=master)](https://ci.appveyor.com/project/emmt/ZippedArrays-jl/branch/master)
[![Coverage](http://codecov.io/github/emmt/ZippedArrays.jl/coverage.svg?branch=master)](http://codecov.io/github/emmt/ZippedArrays.jl?branch=master)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

`ZippedArrays` is a [Julia](https://julialang.org/)] package to zip several (abstract)
arrays together for accessing their elements simultaneously. For instance, assuming that
`A`, `B` and `C` are 3 Julia arrays, then:

```julia
using ZippedArrays
Z = ZippedArray(A,B,C)
```

builds a zipped array instance `Z` such that the syntax `Z[i]` yields the 3-tuple
`(A[i],B[i],C[i])` while the syntax `Z[i] = (a,b,c)` is equivalent to `(A[i],B[i],C[i]) =
(a,b,c)`.

Any number of arrays can be zipped together, they must however have the same indices (as
returned by the `axes` method).

To build an uninitialized zipped array of size `dims` whose elements are tuples of items
of types `T1`, `T2`, etc., call:

```julia
Z = ZippedArray{Tuple{T1,T2,...}}(undef, dims)
```

For example:

```julia
Z = ZippedArray{Tuple{Int,Float64}}(undef, 2, 3, 4)
```

builds a 3-dimensional array of size `(2,3,4)` and whose elements are 2-tuples of type
`Tuple{Int,Float64}`.

Element type of a zipped array can also be a structure type. For example:

```julia
Z = ZippedArray{Complex{Float32}}(undef, dims)
```

creates an uninitialized array of `Complex{Float32}` elements, of size `dims`, and such
that the real and imaginary parts are stored into two separate arrays. As another example:

```julia
Z = ZippedArray{Complex{Float32}}(A, B))
```

wraps arrays `A` and `B` into an abstract array of `Complex{Float32}` elements, of same
size as `A` and `B` and such that `Z[i]` yields `Complex{Float32}(A[i],B[i])`.

A zipped array, say `A::ZippedArray{T,N}`, enforces the type of the returned elements by
calling `convert(T, x)` with `x` the tuple of values and if `T` is a tuple type, or by
calling the constructor `T(x...)` if `T` is not a tuple type. This guarantees the type of
the returned elements with no speed penalties when `x` needs no conversion. This can be
also exploited to perform lazy conversion (in the above example `A` and `B` may have other
element type than `Float32`). If the type `T` has no constructor matching the syntax
`T(x...)`, the method `ZippedArrays.build(::Type{T}, x)` can be specialized to yield an
object of type `T` whose fields are given by the tuple of values `x`.

Compared to the `zip` function which only provides means to iterate through its arguments,
a zipped array can be accessed in random order and for reading and writing. This makes
zipped arrays useful for in-place multi-key sorting. For instance:

```julia
sort!(ZippedArray(A,B);
      lt = (x,y) -> ifelse(x[1] == y[1], x[2] < y[2], x[1] < y[1]))
```

will sort in-place vectors `A` and `B` such that the values in `A` are in increasing order
and, in case of equality, the values in `B` are in increasing order.

A zipped array is a simple immutable structure wrapped around the arguments of
`ZippedArray` so zipped arrays are almost costless to build. Below is an example of how to
build an array `C` whose elements are pairs of values from `A` and `B` and a zipped array
`Z` also built from `A` and `B`:

```julia
using ZippedArrays
n = 10_000
A = rand(Float64, n)
B = rand(Int64, n)
C = [(A[i],B[i]) for i in 1:n]
Z = ZippedArray(A,B)
C == Z # yields true
```

The comparison `C == Z` shows that the two arrays are virtually the same (although not the
same object, that is `C !== Z`). Building `Z` however requires no copy of array elements
and hardly requires additional memory, the sizes of `Z` and `C` are indeed quite
different:

```julia
julia> sizeof(Z)
16

julia> sizeof(C)
160000
```

These numbers may depend on the architecture (here a 64-bit processor).

Thanks to the in-lining of functions and optimizations, a zipped array may also be faster.
For instance, with the arrays `C` and `Z` defined above:

```julia
using BenchmarkTools
function sum_first(A::AbstractArray{<:Tuple})
    s = 0.0
    @inbounds @simd for i in eachindex(A)
        s += first(A[i])
    end
    return s
end
@btime sum_first($C) # 1.615 μs (0 allocations: 0 bytes)
@btime sum_first($Z) # 643.983 ns (0 allocations: 0 bytes)
```
