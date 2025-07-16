module ZippedArrays

export
    ZippedArray,
    ZippedVector,
    ZippedMatrix

using Base: Fix1, Fix2, IteratorSize, HasLength, HasShape, to_shape

"""
    ZippedArrays.destruct(x) -> tuple

yields a tuple of the fields of `x` if `x` is a structured object, or just returns `x` if
`x` is a tuple.

`x` may also be a structure or tuple type.

See also [`ZippedArrays.build`](@ref).

"""
destruct(x::Tuple) = x
@generated function destruct(x::T) where {T}
    expr = Expr(:tuple, ntuple(i -> :(getfield(x, $i)), Val(fieldcount(T)))...)
    quote
        $(Expr(:meta, :inline))
        return $expr
    end
end
destruct(::Type{T}) where {T<:Tuple} = T
destruct(::Type{T}) where {T} = Tuple{ntuple(i -> fieldtype(T, i), Val(fieldcount(T)))...}

"""
    ZippedArrays.destruct(x, i)

yields `i`-th field of `x` if `x` is a structured object, or just `i`-th entry of `x` if
`x` is a tuple.

`x` may also be a structure or tuple type.

"""
destruct(x::Tuple, i::Int) = getindex(x, i)
destruct(x, i::Int) = getfield(x, i)
destruct(::Type{T}, i::Int) where {T<:Tuple} = T.parameters[i]
destruct(::Type{T}, i::Int) where {T} = fieldtype(T, i)

"""
    ZippedArrays.destruct_count(T)

yields the number of fields of `T` if `T` is a structure type, or the number of entries in
`T` if `T` is a tuple type.

"""
destruct_count(::Type{T}) where {T<:Tuple} = length(T.parameters)
destruct_count(::Type{T}) where {T} = fieldcount(T)

"""
    ZippedArrays.build(T, x)

builds an object of type `T` whose fields are the entries of the tuple `x`.

This method reverses the effects of [`ZippedArrays.destruct`](@ref).

The method may be extended by callers to implement a different behavior than the default
implementation which is:

    convert(T, x)   # if `T` is a tuple type
    T(x...)         # otherwise (i.e., call constructor)

"""
@inline build(::Type{T}, args...) where {T} = build(T, args)
@inline build(::Type{T}, x::T) where {T<:Tuple} = x
@inline build(::Type{T}, x::Tuple) where {T<:Tuple} = convert(T, x)
@inline build(::Type{T}, x::T) where {T} = x
@inline build(::Type{T}, x::Tuple) where {T} = T(x...)

"""
    Z = ZippedArray(A,B,C,...)

builds a zipped array `Z` based on arrays `A`, `B`, `C`, etc. such that the syntax `Z[i]`
yields a tuple of values `(A[i],B[i],C[i],...)` while the syntax `Z[i] = (a,b,c,...)` is
equivalent to `(A[i],B[i],C[i],...) = (a,b,c,...)`.

Any number of arrays can be zipped together, they must however have the same indices (as
given by calling the `axes` method).

Use the syntax `Z.args` to retrieve the arrays `A`, `B`, `C`, etc.

""" ZippedArray

# Parameters:
#     T = array element type;
#     N = number of dimensions;
#     L = number of zipped arrays
#     I = indexing style (true for linear, false otherwise)
#     S = type of L-tuple of zipped arrays
struct ZippedArray{T,N,L,I,S<:NTuple{L,AbstractArray{<:Any,N}}} <: AbstractArray{T,N}
    args::S
end

const at_least_one_array_to_zip = ArgumentError("there must be at least one array to zip")
const not_same_ndims = DimensionMismatch("arrays to zip must have the same number of dimensions")


ZippedArray(args::AbstractArray...) = ZippedArray(args)
function ZippedArray(args::S) where {L,S<:NTuple{L,AbstractArray}}
    L ≥ 1 || throw(at_least_one_array_to_zip)
    T = Tuple{map(eltype, args)...}
    N = length(same_axes(args...)) # clash if arguments do not have the same axes
    I = IndexStyle(args...) === IndexLinear()
    return ZippedArray{T,N,L,I,S}(args)
end

ZippedArray{T}(args::AbstractArray...) where {T} = ZippedArray{T}(args)
function ZippedArray{T}(args::S) where {T,L,S<:NTuple{L,AbstractArray}}
    L ≥ 1 || throw(at_least_one_array_to_zip)
    # We do not enforce that the number of arrays matches the number of fields if `T` is a
    # structure type to let the caller provides its own builder.
    !(T <: Tuple) || destruct_count(T) == length(args) || throw(ArgumentError(
        "number of entries in element type `$T` must match number of arguments"))
    N = length(same_axes(args...)) # clash if arguments do not have the same axes
    I = IndexStyle(args...) === IndexLinear()
    return ZippedArray{T,N,L,I,S}(args)
end

ZippedArray{T,N}(args::AbstractArray...) where {T,N} = ZippedArray{T,N}(args)
ZippedArray{T,N}(args::Tuple{Vararg{AbstractArray}}) where {T,N} =
    throw(DimensionMismatch("arrays to zip must all have $N dimensions"))
ZippedArray{T,N}(args::Tuple{Vararg{AbstractArray{<:Any,N}}}) where {T,N} =
    ZippedArray{T}(args)

for (f, n) in ((:ZippedVector, 1), (:ZippedMatrix, 2))
    @eval begin
        const $f{T,L,I,S<:NTuple{L,AbstractArray{<:Any,$n}}} = ZippedArray{T,$n,L,I,S}
        $f(args::AbstractArray...) = $f(args)
        $f(args::Tuple{Vararg{AbstractArray}}) = ZippedArray{Tuple{map(eltype, args)...},$n}(args)
    end
end

"""
    Z = ZippedArray{Tuple{T1,T2,...}}(undef, dims...)

builds an uninitialized zipped array `Z` of size `dims...` whose elements are tuples whose
entries have types `T1`, `T2`, ...

"""
ZippedArray{T}(::UndefInitializer, dims::Integer...) where {T} =
    ZippedArray{T}(undef, dims)
ZippedArray{T}(::UndefInitializer, dims::NTuple{N,Integer}) where {T,N} =
    ZippedArray{T,N}(undef, dims)

ZippedArray{T,N}(::UndefInitializer, dims::Integer...) where {T,N} =
    ZippedArray{T,N}(undef, dims)
ZippedArray{T,N}(::UndefInitializer, dims::NTuple{N,Integer}) where {T,N} =
    ZippedArray{T,N}(undef, Dims(dims))

function ZippedArray{T,N}(::UndefInitializer, dims::Dims{N}) where {T,N}
    #isconcretetype(T) || throw(ArgumentError("element type `$T` is not a concrete type"))
    L = destruct_count(T)
    L > zero(L) || throw(ArgumentError(
        "there must be at least one entry/field in element type `$T`"))
    args = ntuple(i -> Array{destruct(T,i),N}(undef, dims), Val(L))
    S = Tuple{ntuple(i -> Array{destruct(T,i),N}, Val(L))...}
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

@generated Base.getindex(A::FastZippedArray{T,N,L}, i::Int) where {T,N,L} =
    _encode_getindex(:T, :A, :i, L)
@generated Base.getindex(A::SlowZippedArray{T,N,L}, i::Vararg{Int,N}) where {T,N,L} =
    _encode_getindex(:T, :A, :(i...), L)

@generated Base.setindex!(A::FastZippedArray{T,N,L}, x, i::Int) where {T,N,L} =
    _encode_setindex(:A, :x, :i, L)
@generated Base.setindex!(A::SlowZippedArray{T,N,L}, x, i::Vararg{Int,N}) where {T,N,L} =
    _encode_setindex(:A, :x, :(i...), L)

Base.checkbounds(::Type{Bool}, A::FastZippedArray{T,N,L}, i::Int) where {T,N,L} =
    checkbounds(Bool, A.args[1], i)
Base.checkbounds(::Type{Bool}, A::SlowZippedArray{T,N,L}, i::Vararg{Int,N}) where {T,N,L} =
    checkbounds(Bool, A.args[1], i...)

# Generate expression corresponding to the list of entries of a zipped array whose name is
# `A` and at index expression `i`.
_encode_list_of_entries(A::Symbol, i::Union{Integer,Symbol,Expr}, n::Int) =
    Expr(:tuple, ntuple(j -> :($A.args[$j][$i]), Val(n))...)

# Generate code for method `Base.getindex` applied to a zipped array whose name is `A` and
# at index expression `i`.
function _encode_getindex(T::Symbol, A::Symbol, i::Union{Integer,Symbol,Expr}, n::Int)
    vals = _encode_list_of_entries(A, i, n)
    return quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds($A, $i)
        return @inbounds build($T, $vals)
    end
end

# Generate code for method `Base.setindex!` applied to a zipped array whose name is `A`
# and at index expression `i`.
function _encode_setindex(A::Symbol, x::Symbol, i::Union{Integer,Symbol,Expr}, n::Int)
    vals = _encode_list_of_entries(A, i, n)
    return quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds($A, $i)
        @inbounds $vals = destruct($x)
        return $A
    end
end

Base.copy(A::ZippedArray) = ZippedArray{eltype(A)}(map(copy, A.args))

Base.deepcopy(A::ZippedArray) = ZippedArray{eltype(A)}(map(deepcopy, A.args))

Base.similar(A::ZippedArray, ::Type{T}, dims::Dims{N}) where {T,N} =
    destruct_count(T) > 0 ? ZippedArray{T}(undef, dims) : Array{T}(undef, dims)

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

# `all_isequal(f,x,args...)` yields whether `isequal(f(a),x)` holds for all `a ∈ args`.
all_isequal(f::Function, x) = true
all_isequal(f::Function, x, A) = isequal(f(A), x)
@inline all_isequal(f::Function, x, A, B...) = all_isequal(f, x, A) && all_isequal(f, x, B...)

same_axes() = ()
same_axes(A::AbstractArray) = axes(A)
@inline function same_axes(A::AbstractArray, B::AbstractArray...)
    inds = axes(A)
    all_isequal(axes, inds, B...) || throw(DimensionMismatch(indices_mismatch(A, B...)))
    return inds
end

@noinline indices_mismatch(A::AbstractArray...) =
    indices_mismatch(Base.has_offset_axes(A...), A...)

@noinline function indices_mismatch(show_axes::Bool, A::AbstractArray...)
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
    return String(take!(io))
end

end # module
