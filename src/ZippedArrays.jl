module ZippedArrays

export ZippedArray

const ZTuple{L,N} = NTuple{L,AbstractArray{<:Any,N}}

struct ZippedArray{T,N,L,I,S<:ZTuple{L,N}} <: AbstractArray{T,N}
    args::S

    # Inner constructor just to avoid outer constructor.
    ZippedArray{T,N,L,I,S}(args::S) where {T,N,L,I,S} = new{T,N,L,I,S}(args)
end

"""
    ZippedArray(A,B,C,...) -> Z

yields a zipped array instance `Z` based on arrays `A`, `B`, `C`, etc.  such
that the syntax `Z[i]` yields a tuple of values `(A[i],B[i],C[i],...)` while
the syntax `Z[i] = (a,b,c,...)` is equivalent to `(A[i],B[i],C[i],...) =
(a,b,c,...)`.

Any number of arrays can be zipped together, they must however have the same
indices (as given by calling the `axes` method).

"""
ZippedArray(args::AbstractArray{<:Any,N}...) where {N} =
    ZippedArray{Tuple{map(eltype,args)...},N,length(args),
                typeof(checkindexing(args...)),typeof(args)}(args)

ZippedArray() = error("at least one array argument must be provided")

Base.length(A::ZippedArray) = length(A.args[1])

Base.size(A::ZippedArray) = size(A.args[1])
Base.size(A::ZippedArray, i) = size(A.args[1], i)

Base.axes1(A::ZippedArray) = Base.axes1(A.args[1])
Base.axes(A::ZippedArray) = axes(A.args[1])
Base.axes(A::ZippedArray, i::Integer) = axes(A.args[1], i)

Base.IndexStyle(::Type{<:ZippedArray{T,N,L,I}}) where {T,N,L,I} = I()

# FIXME: improve error message in bound checking.

@generated function Base.getindex(A::ZippedArray{T,N,L,IndexLinear},
                                  i::Int) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i]), Val(L))...)
    quote
        @boundscheck checkbounds(A.args[1], i)
        @inbounds $lhs
    end
end

@generated function Base.setindex!(A::ZippedArray{T,N,L,IndexLinear},
                                   val, i::Int) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i]), Val(L))...)
    quote
        @boundscheck checkbounds(A.args[1], i)
        @inbounds $lhs = val
    end
end

@generated function Base.getindex(A::ZippedArray{T,N,L,IndexCartesian},
                                  i::Vararg{Int,N}) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i...]), Val(L))...)
    quote
        @boundscheck checkbounds(A.args[1], i...)
        @inbounds $lhs
    end
end

@generated function Base.setindex!(A::ZippedArray{T,N,L,IndexCartesian},
                                   val, i::Vararg{Int,N}) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i...]), Val(L))...)
    quote
        @boundscheck checkbounds(A.args[1], i...)
        @inbounds $lhs = val
    end
end

all_match(val, f::Function) = true
all_match(val, f::Function, A) = f(A) == val
@inline all_match(val, f::Function, A, B...) =
    all_match(val, f, A) && all_match(val, f::Function, B...)

checkindexing(A::AbstractArray) = IndexStyle(A)

@inline checkindexing(A::AbstractArray, B::AbstractArray) =
    checkindexing(IndexStyle(A, B), A, B)
@inline checkindexing(A::AbstractArray, B::AbstractArray...) =
    checkindexing(IndexStyle(A, B...), A, B...)

@inline function checkindexing(I::Union{IndexLinear,IndexCartesian},
                               A::AbstractArray,
                               B::AbstractArray...)
    all_match(axes(A), axes, B...) ||
        throw_indices_mismatch(I, A, B...)
    return I
end

@noinline function throw_indices_mismatch(::IndexLinear,
                                          A::AbstractArray...)
    io = IOBuffer()
    write(io, "all arguments of `ZippedArray` must have the same shape, got ")
    m = length(A)
    for i in 1:m
        write(io, (i == 1 ? "(" : i == m ? " and (" : ", )"))
        dims = size(A[i])
        n = length(dims)
        for j in 1:n
            j > 1 && write(io, ",")
            print(io, dims[j])
        end
        write(io, (n == 1 ? ",)" : ")"))
    end
    throw(DimensionMismatch(String(take!(io))))
end

@noinline function throw_indices_mismatch(::IndexCartesian,
                                          A::AbstractArray...)
    io = IOBuffer()
    write(io, "all arguments of `ZippedArray` must have the same axes, got ")
    m = length(A)
    for i in 1:m
        write(io, (i == 1 ? "(" : i == m ? " and (" : ", )"))
        inds = axes(A[i])
        n = length(inds)
        for j in 1:n
            j > 1 && write(io, ",")
            print(io, first(inds[j]), ":", last(inds[j]))
        end
        write(io, (n == 1 ? ",)" : ")"))
    end
    throw(DimensionMismatch(String(take!(io))))
end

end # module
