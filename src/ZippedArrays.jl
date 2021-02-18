module ZippedArrays

export ZippedArray

# Alias for a tuple of arrays.
const ArrayTuple{L,N} = NTuple{L,AbstractArray{<:Any,N}}

"""
    ZippedArray(A,B,C,...) -> Z

yields a zipped array instance `Z` based on arrays `A`, `B`, `C`, etc.  such
that the syntax `Z[i]` yields a tuple of values `(A[i],B[i],C[i],...)` while
the syntax `Z[i] = (a,b,c,...)` is equivalent to `(A[i],B[i],C[i],...) =
(a,b,c,...)`.

Any number of arrays can be zipped together, they must however have the same
indices (as given by calling the `axes` method).

""" ZippedArray

struct ZippedArray{T,N,L,I,S<:ArrayTuple{L,N}} <: AbstractArray{T,N}
    args::S
end

ZippedArray(args::AbstractArray{<:Any,N}...) where {N} =
    ZippedArray{Tuple{map(eltype,args)...},N,length(args),
                typeof(get_index_style(args...)),typeof(args)}(args)

ZippedArray() = error("at least one array argument must be provided")

Base.length(A::ZippedArray) = length(A.args[1])

Base.size(A::ZippedArray) = size(A.args[1])
Base.size(A::ZippedArray, i) = size(A.args[1], i)

Base.axes1(A::ZippedArray) = Base.axes1(A.args[1])
Base.axes(A::ZippedArray) = axes(A.args[1])
Base.axes(A::ZippedArray, i::Integer) = axes(A.args[1], i)

Base.IndexStyle(::Type{<:ZippedArray{T,N,L,I}}) where {T,N,L,I} = I()

@generated function Base.getindex(A::ZippedArray{T,N,L,IndexLinear},
                                  i::Int) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i)
        @inbounds r = $lhs
        r
    end
end

@generated function Base.setindex!(A::ZippedArray{T,N,L,IndexLinear},
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
                                  A::ZippedArray{T,N,L,IndexLinear},
                                  i::Int) where {T,N,L}
    checkbounds(Bool, A.args[1], i)
end

@generated function Base.getindex(A::ZippedArray{T,N,L,IndexCartesian},
                                  i::Vararg{Int,N}) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i...]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i...)
        @inbounds r = $lhs
        r
    end
end

@generated function Base.setindex!(A::ZippedArray{T,N,L,IndexCartesian},
                                   val, i::Vararg{Int,N}) where {T,N,L}
    lhs = Expr(:tuple, ntuple(j -> :(A.args[$j][i...]), Val(L))...)
    quote
        $(Expr(:meta, :inline))
        @boundscheck checkbounds(A, i...)
        @inbounds $lhs = val
        A
    end
end

@inline function Base.checkbounds(::Type{Bool},
                                  A::ZippedArray{T,N,L,IndexCartesian},
                                  i::Vararg{Int,N}) where {T,N,L}
    checkbounds(Bool, A.args[1], i...)
end

"""
    all_match(val, f, args...) -> bool

yields whether `f(arg) == val` for all `arg ∈ args`.

"""
all_match(val, f::Function) = true
all_match(val, f::Function, A) = f(A) == val
@inline all_match(val, f::Function, A, B...) =
    all_match(val, f, A) && all_match(val, f::Function, B...)

"""
    get_index_style(A...) -> IndexLinear() or IndexCartesian()

yields the most efficient indexing style for accessing arrays `A...` with the
same index.  An exception is thrown if arguments do not have the same axes.

"""
get_index_style(A::AbstractArray) = IndexStyle(A)

@inline get_index_style(A::AbstractArray, B::AbstractArray) =
    get_index_style(IndexStyle(A, B), A, B)
@inline get_index_style(A::AbstractArray, B::AbstractArray...) =
    get_index_style(IndexStyle(A, B...), A, B...)

@inline function get_index_style(I::Union{IndexLinear,IndexCartesian},
                                 A::AbstractArray,
                                 B::AbstractArray...)
    all_match(axes(A), axes, B...) || throw_indices_mismatch(A, B...)
    return I
end

@noinline throw_indices_mismatch(A::AbstractArray...) =
    throw_indices_mismatch(Base.has_offset_axes(A...), A...)

@noinline function throw_indices_mismatch(show_axes::Bool, A::AbstractArray...)
    io = IOBuffer()
    write(io, "all arguments of `ZippedArray` must have the same ",
          (show_axes ? "axes" : "shape"), ", got ")
    m = length(A)
    for i in 1:m
        write(io, (i == 1 ? "(" : i == m ? " and (" : ", )"))
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
