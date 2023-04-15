module TestingZippedArrays

using Test
using ZippedArrays
using ZippedArrays: throw_indices_mismatch, all_match

function generate_array(::Type{T}, dims::Dims{N}) where {T,N}
    A = Array{T,N}(undef, dims)
    for i in 1:length(A)
        A[i] = convert(T, i)
    end
    return A
end

@testset "zipped arrays" begin
    dims = (2,3,4)
    A = generate_array(Int32, dims)
    B = generate_array(Float64, dims)
    C = generate_array(Char, dims)
    D = generate_array(Int16, (1, dims...))
    E = generate_array(Float32, (dims .+ 1))
    V = view(E, map(d -> 2:d+1, dims)...)

    for bool in (true, false)
        @test_throws DimensionMismatch throw_indices_mismatch(bool, A, B, E)
    end
    @test_throws ErrorException ZippedArray()
    @test_throws MethodError ZippedArray(A,D)
    @test_throws DimensionMismatch ZippedArray(A,E)
    @test all_match(nothing, cos)

    # Create uninitialized zipped arrays.
    types = (Int16,)
    Z = @inferred ZippedArray{Tuple{types...}}(undef, map(Int16, dims)...)
    @test IndexStyle(Z) === IndexLinear()
    @test eltype(Z) === Tuple{types...}
    @test ndims(Z) == length(dims)
    @test size(Z) == dims
    @test [x isa Array{types[i],length(dims)} for (i,x) in enumerate(Z.args)] == trues(length(types))
    @test [size(x) == dims for x in Z.args] == trues(length(types))
    types = (Int16,Float32,Char)
    Z = @inferred ZippedArray{Tuple{types...}}(undef, dims)
    @test IndexStyle(Z) === IndexLinear()
    @test eltype(Z) === Tuple{types...}
    @test ndims(Z) == length(dims)
    @test size(Z) == dims
    @test [x isa Array{types[i],length(dims)} for (i,x) in enumerate(Z.args)] == trues(length(types))
    @test [size(x) == dims for x in Z.args] == trues(length(types))

    # Zip 1 array.
    Z = @inferred ZippedArray(D)
    @test Z.args === (D,)
    @test IndexStyle(Z) === IndexStyle(D)
    @test eltype(Z) === Tuple{eltype(D)}
    @test ndims(Z) == ndims(D)
    @test size(Z) == size(D)
    @test size(Z,1) == size(D,1)
    @test axes(Z) == axes(D)
    @test axes(Z,2) == axes(D,2)
    @test setindex!(Z, (11,), 1,2,3,1) === Z
    for i in (12, CartesianIndex(1,1,2,3))
        x = Z[i] # save values
        @test Z[i] === (D[i],)
        Z[i] = (D[i]+1,)
        @test D[i] == x[1]+1
    end
    @test map(x -> x[1], Z) == D
    @test_throws BoundsError Z[-1]

    # Zip 2 regular arrays.
    Z = @inferred ZippedArray(A,B)
    @test Z.args === (A,B,)
    @test IndexStyle(Z) === IndexLinear()
    @test eltype(Z) === Tuple{eltype(A),eltype(B)}
    @test ndims(Z) == ndims(A)
    @test size(Z) == size(A)
    @test size(Z,1) == size(A,1)
    @test axes(Z) == axes(A)
    @test axes(Z,2) == axes(A,2)
    @test setindex!(Z, (7, 2.3), 2,3,1) === Z
    for i in (12, CartesianIndex(1,2,3))
        x = Z[i] # save values
        @test Z[i] === (A[i], B[i])
        Z[i] = (A[i]+1, B[i]+2)
        @test A[i] == x[1]+1
        @test B[i] == x[2]+2
    end
    @test map(x -> x[1], Z) == A
    @test map(x -> x[2], Z) == B
    @test_throws BoundsError Z[-1]

    # Zip 2 regular arrays and a view.
    Z = @inferred ZippedArray(A,V,C)
    @test Z.args === (A,V,C,)
    @test IndexStyle(Z) === IndexCartesian()
    @test eltype(Z) === Tuple{eltype(A),eltype(V),eltype(C)}
    @test ndims(Z) == ndims(A)
    @test size(Z) == size(A)
    @test size(Z,1) == size(A,1)
    @test axes(Z) == axes(A)
    @test axes(Z,2) == axes(A,2)
    @test setindex!(Z, (-1, 0.3, 'x'), 1,2,3) === Z
    for i in (12, CartesianIndex(1,2,3))
        x = Z[i] # save values
        @test Z[i] === (A[i], V[i], C[i])
        Z[i] = (A[i]+1, V[i]+2, C[i]+3)
        @test A[i] == x[1]+1
        @test V[i] == x[2]+2
        @test C[i] == x[3]+3
    end
    @test map(x -> x[1], Z) == A
    @test map(x -> x[2], Z) == V
    @test map(x -> x[3], Z) == C
    @test_throws BoundsError Z[1,2,-1]

    # This works for vectors.
    n = 23
    a = rand(Bool, n)
    b = rand(Float32, n)
    c = rand(Int16, n)
    @test [(a[i],b[i],c[i]) for i in 1:n] == ZippedArray(a,b,c)

    sort!(ZippedArray(a,b);
          lt = (x,y) -> ifelse(x[1] == y[1], x[2] < y[2], x[1] < y[1]))
    flag = true
    for i in 2:n
        flag &= ((a[i-1] < a[i])|((a[i-1] == a[i])&(b[i-1] <= b[i])))
    end
    @test flag
end

end
