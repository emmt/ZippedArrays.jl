module ZippedArrays

export
    ZippedArray,
    ZippedVector,
    ZippedMatrix

using Base: Fix1, Fix2, IteratorSize, HasLength, HasShape, to_shape

# Alias for a tuple of arrays.
const ArrayTuple{L,N} = NTuple{L,AbstractArray{<:Any,N}}

"""
    Z = ZippedArray(A,B,C,...)

builds a zipped array `Z` based on arrays `A`, `B`, `C`, etc. such that the
syntax `Z[i]` yields a tuple of values `(A[i],B[i],C[i],...)` while the syntax
`Z[i] = (a,b,c,...)` is equivalent to `(A[i],B[i],C[i],...) = (a,b,c,...)`.

Any number of arrays can be zipped together, they must however have the same
indices (as given by calling the `axes` method).

Use the syntax `Z.args` to retrieve the arrays `A`, `B`, `C`, etc.

""" ZippedArray

# Parameters:
#     T = array element type;
#     N = number of dimensions;
#     L = number of zipped arrays
#     I = indexing style (true for linear, false otherwise)
#     S = type of L-tuple of zipped arrays
struct ZippedArray{T,N,L,I,S<:ArrayTuple{L,N}} <: AbstractArray{T,N}
    args::S
end

ZippedArray(args::AbstractArray{<:Any,N}...) where {N} = ZippedArray(args)
ZippedArray(args::S) where {L,N,S<:ArrayTuple{L,N}} =
    ZippedArray{Tuple{map(eltype,args)...}, N, L,
                get_index_style(args...) === IndexLinear(), S}(args)

ZippedArray() = error("at least one array argument must be provided")

for (f, n) in ((:ZippedVector, 1), (:ZippedMatrix, 2))
    @eval begin
        const $f{T,L,I,S<:ArrayTuple{L,$n}} = ZippedArray{T,$n,L,I,S}
        $f(args::AbstractVector{<:Any}...) = ZippedVector(args)
        $f(args::S) where {L,S<:ArrayTuple{L,$n}} =
            ZippedArray{Tuple{map(eltype,args)...}, $n, L,
                        get_index_style(args...) === IndexLinear(), S}(args)
    end
end

"""
    Z = ZippedArray{Tuple{T1,T2,...}}(undef, dims...)

builds an uninitialized zipped array `Z` of size `dims...` whose elements are
tuples whose entries have types `T1`, `T2`, ...

"""
ZippedArray{T}(::UndefInitializer, dims::Integer...) where {T<:Tuple} =
    ZippedArray{T}(undef, dims)
ZippedArray{T}(::UndefInitializer, dims::Tuple{Vararg{Integer}}) where {T<:Tuple} =
    ZippedArray{T}(undef, map(Int, dims))
ZippedArray{T}(::UndefInitializer, dims::Dims{N}) where {T<:Tuple,N} =
    ZippedArray{T,N,length(T.parameters)}(undef, dims)

ZippedArray{T,N}(::UndefInitializer, dims::Integer...) where {T<:Tuple,N} =
    ZippedArray{T,N}(undef, dims)
ZippedArray{T,N}(::UndefInitializer, dims::Tuple{Vararg{Integer}}) where {T<:Tuple,N} =
    ZippedArray{T,N}(undef, map(Int, dims))
ZippedArray{T,N}(::UndefInitializer, dims::Dims{N}) where {T<:Tuple,N} =
    ZippedArray{T,N,length(T.parameters)}(undef, dims)

function ZippedArray{T,N,L}(::UndefInitializer, dims::Dims{N}) where {T<:Tuple,N,L}
    L == length(T.parameters) || throw(ArgumentError(
        "incompatible type parameter L, gor $L, should be $(length(T.parameters))"))
    args = ntuple(i -> Array{T.parameters[i],N}(undef, dims), Val(L))
    S = Tuple{ntuple(i -> Array{T.parameters[i],N}, Val(L))...}
    return ZippedArray{T,N,L,true,S}(args)
end

Base.length(A::ZippedArray) = length(A.args[1])

Base.size(A::ZippedArray) = size(A.args[1])
Base.size(A::ZippedArray, i) = size(A.args[1], i)

Base.axes1(A::ZippedArray) = Base.axes1(A.args[1])
Base.axes(A::ZippedArray) = axes(A.args[1])
Base.axes(A::ZippedArray, i::Integer) = axes(A.args[1], i)

# Aliases for zipped arrays with fast (linear) and slow (Cartesian) indexing.
const FastZippedArray{T,N,L,S} = ZippedArray{T,N,L,true,S}
const SlowZippedArray{T,N,L,S} = ZippedArray{T,N,L,false,S}

Base.IndexStyle(::Type{<:FastZippedArray}) = IndexLinear()
Base.IndexStyle(::Type{<:SlowZippedArray}) = IndexCartesian()

@generated function Base.getindex(A::FastZippedArray{T,N,L},
                                  i::Int) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i)
        @inbounds r = $lhs
        r
    end
end

@generated function Base.setindex!(A::FastZippedArray{T,N,L},
                                   val, i::Int) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i)
        @inbounds $lhs = val
        A
    end
end

@inline function Base.checkbounds(::Type{Bool},
                                  A::FastZippedArray{T,N,L},
                                  i::Int) where {T,N,L}
    checkbounds(Bool, A.args[1], i)
end

@generated function Base.getindex(A::SlowZippedArray{T,N,L},
                                  i::Vararg{Int,N}) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i...]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i...)
        @inbounds r = $lhs
        return r
    end
end

@generated function Base.setindex!(A::SlowZippedArray{T,N,L},
                                   val, i::Vararg{Int,N}) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i...]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i...)
        @inbounds $lhs = val
        return A
    end
end

@inline function Base.checkbounds(::Type{Bool},
                                  A::SlowZippedArray{T,N,L},
                                  i::Vararg{Int,N}) where {T,N,L}
    checkbounds(Bool, A.args[1], i...)
end

Base.copy(A::ZippedArray) = ZippedArray(map(copy, A.args))

Base.deepcopy(A::ZippedArray) = ZippedArray(map(deepcopy, A.args))

Base.similar(A::ZippedArray) = ZippedArray(map(similar, A.args))

Base.similar(A::ZippedArray, dims::Integer...) =
    similar(A, to_shape(dims))

Base.similar(A::ZippedArray, ::Type{T}, dims::Integer...) where {T} =
    similar(A, T, to_shape(dims))

Base.similar(A::ZippedArray{<:Any,<:Any,L}, dims::Dims{N}) where {N,L} =
    ZippedArray(ntuple(i -> similar(A.args[i], dims), Val(L)))

Base.similar(A::ZippedArray, ::Type{T}) where {T<:Tuple} =
    ZippedArray{T}(undef, size(A))

Base.similar(A::ZippedArray, ::Type{T}, dims::Dims) where {T<:Tuple} =
    ZippedArray{T}(undef, dims)

function Base.resize!(A::ZippedVector{<:Any,L}, n::Integer) where {L}
    newlen = Int(n)
    oldlen = length(A)
    if newlen != oldlen
        try
            for i in 1:L
                resize!(A.args[i], newlen)
            end
        catch err
            # Restore previous length.
            for i in 1:L
                length(A.args[i]) == oldlen || resize!(A.args[i], oldlen)
            end
            rethrow(err)
        end
    end
    return A
end

function Base.sizehint!(A::ZippedVector, n::Integer)
    map(Fix2(sizehint!, Int(n)), A.args)
    return A
end

function Base.push!(A::ZippedVector, x)
    resize!(A, length(A) + 1)
    @inbounds A[end] = x
    return A
end

function Base.append!(A::ZippedVector, iter)
    if IteratorSize(iter) isa Union{HasLength,HasShape}
        n = length(iter)
        if n > 0
            i = lastindex(A)
            resize!(A, length(A) + n)
            @inbounds for x in iter
                A[i += 1] = x
            end
        end
    else
        n = length(A)
        i = lastindex(A)
        @inbounds for x in iter
            resize!(A, n += 1)
            A[i += 1] = x
        end
    end
    return A
end

"""
    all_match(val, f, args...) -> bool

yields whether `f(arg) == val` for all `arg ∈ args`.

"""
all_match(val, f::Function) = true
all_match(val, f::Function, A) = f(A) == val
@inline all_match(val, f::Function, A, B...) =
    all_match(val, f, A) && all_match(val, f, B...)

"""
    get_index_style(A...) -> IndexLinear() or IndexCartesian()

yields the most efficient indexing style for accessing arrays `A...` with the
same index. An exception is thrown if arguments do not have the same axes.

"""
get_index_style() = IndexLinear()
get_index_style(A::AbstractArray) = IndexStyle(A)
@inline function get_index_style(A::AbstractArray, B::AbstractArray...)
    all_match(axes(A), axes, B...) || throw_indices_mismatch(A, B...)
    return IndexStyle(A, B...)
end

@noinline throw_indices_mismatch(A::AbstractArray...) =
    throw_indices_mismatch(Base.has_offset_axes(A...), A...)

@noinline function throw_indices_mismatch(show_axes::Bool, A::AbstractArray...)
    io = IOBuffer()
    write(io, "all arguments of `ZippedArray` must have the same ")
    write(io, show_axes ? "indices, got axes " : "shape, got shapes ")
    m = length(A)
    for i in 1:m
        write(io, (i == 1 ? "(" : i == m ? " and (" : ", ("))
        if show_axes
            inds = axes(A[i])
            n = length(inds)
            for j in 1:n
                j > 1 && write(io, ",")
                print(io, first(inds[j]), ":", last(inds[j]))
            end
        else
            dims = size(A[i])
            n = length(dims)
            for j in 1:n
                j > 1 && write(io, ",")
                print(io, dims[j])
            end
        end
        write(io, (n == 1 ? ",)" : ")"))
    end
    throw(DimensionMismatch(String(take!(io))))
end

end # module
