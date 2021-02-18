# Zipping Julia arrays together

| **License**                     | **Build Status**                                                | **Code Coverage**                                                   |
|:--------------------------------|:----------------------------------------------------------------|:--------------------------------------------------------------------|
| [![][license-img]][license-url] | [![][travis-img]][travis-url] [![][appveyor-img]][appveyor-url] | [![][coveralls-img]][coveralls-url] [![][codecov-img]][codecov-url] |

`ZippedArrays` is a [Julia][julia-url] package to zip several (abstract) arrays
together for accessing their elements simultaneously.  For instance, assuming
that `A`, `B` and `C` are 3 Julia arrays, then:

```julia
using ZippedArrays
Z = ZippedArray(A,B,C)
```

builds a zipped array instance `Z` such that the syntax `Z[i]` yields the
3-tuple `(A[i],B[i],C[i])` while the syntax `Z[i] = (a,b,c)` is equivalent to
`(A[i],B[i],C[i]) = (a,b,c)`.

Any number of arrays can be zipped together, they must however have the same
indices (as returned by the `axes` method).

Compared to the `zip` function which only provides means to iterate through its
arguments, a zipped array can be accessed in random order and for reading and
writing.  This makes zipped arrays useful for multi-key sorting.  For instance:

```julia
sort!(ZippedArray(A,B);
      lt = (x,y) -> ifelse(x[1] == y[1], x[2] < y[2], x[1] < y[1]))
```

will sort in-place vectors `A` and `B` such that the values in `A` are in
increasing order and, in case of equality, the values in `B` are in increasing
order.

A zipped array is a simple immutable structure wrapped around the arguments of
`ZippedArray` so zipped arrays are almost costless to build.  Below is an
example of how to build an array `C` whose elements are pairs of values from
`A` and `B` and a zipped array `Z` also built from `A` and `B`:

```julia
using ZippedArrays
n = 10_000
A = rand(Float64, n)
B = rand(Int64, n)
C = [(A[i],B[i]) for i in 1:n]
Z = ZippedArray(A,B)
C == Z # yields true
```

The comparison `C == Z` shows that the two arrays are virtually the same
(although not the same object, that is `C !== Z`).  Building `Z` however
requires no copy of array elements and hardly requires additional memory, the
sizes of `Z` and `C` are indeed quite different:

```julia
julia> sizeof(Z)
16

julia> sizeof(C)
160000
```

These numbers may depend on the architecture (here a 64-bit processor).

Thanks to the in-lining of functions and optimizations, a zipped array may also
be faster.  For instance, with the arrays `C` and `Z` defined above:

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

[doc-dev-img]: https://img.shields.io/badge/docs-dev-blue.svg
[doc-dev-url]: https://emmt.github.io/ZippedArrays.jl/dev

[license-url]: ./LICENSE.md
[license-img]: http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat

[travis-img]: https://travis-ci.org/emmt/ZippedArrays.jl.svg?branch=master
[travis-url]: https://travis-ci.org/emmt/ZippedArrays.jl

[appveyor-img]: https://ci.appveyor.com/api/projects/status/github/emmt/ZippedArrays.jl?branch=master
[appveyor-url]: https://ci.appveyor.com/project/emmt/ZippedArrays-jl/branch/master

[coveralls-img]: https://coveralls.io/repos/emmt/ZippedArrays.jl/badge.svg?branch=master&service=github
[coveralls-url]: https://coveralls.io/github/emmt/ZippedArrays.jl?branch=master

[codecov-img]: http://codecov.io/github/emmt/ZippedArrays.jl/coverage.svg?branch=master
[codecov-url]: http://codecov.io/github/emmt/ZippedArrays.jl?branch=master

[julia-url]: https://julialang.org/
[julia-pkgs-url]: https://pkg.julialang.org/
