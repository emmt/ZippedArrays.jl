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

struct MyIterator{T1,T2,T3} end
Base.IteratorSize(::Type{<:MyIterator}) = Base.SizeUnknown()
Base.iterate(iter::MyIterator{T1,T2,T3}, state = 0) where {T1,T2,T3} =
    state < 5 ? ((rand(T1),rand(T2),rand(T3)), state+1) : nothing

struct MyType{A,B,C}
    a::A
    b::B
    c::C
end

@testset "zipped arrays" begin
    @testset "helper methods" begin
        let destruct = ZippedArrays.destruct,
            destruct_count = ZippedArrays.destruct_count,
            build = ZippedArrays.build
            let x = (1,2.0,'x'), T = typeof(x), y = (Int16(x[1]), Float64(x[2]), x[3])
                @test @inferred(destruct(x)) === x
                @test destruct(x,1) === x[1]
                @test destruct(x,2) === x[2]
                @test destruct(x,3) === x[3]
                @test @inferred(destruct(T)) === T
                @test @inferred(destruct_count(T)) === 3
                @test destruct(T,1) === typeof(x[1])
                @test destruct(T,2) === typeof(x[2])
                @test destruct(T,3) === typeof(x[3])
                @test @inferred(build(T, x)) === x
                @test @inferred(build(T, y)) === x
                @test @inferred(build(T, x...)) === x
                @test @inferred(build(T, y...)) === x
            end
            let x = Complex{Float32}(-1, 2), T = typeof(x), y = (x.re, x.im)
                @test @inferred(destruct(x)) === y
                @test @inferred(destruct(x,1)) === y[1]
                @test @inferred(destruct(x,2)) === y[2]
                @test @inferred(destruct(T)) === typeof(y)
                @test @inferred(destruct_count(T)) === 2
                @test @inferred(destruct(T,1)) === typeof(y[1])
                @test @inferred(destruct(T,2)) === typeof(y[2])
                @test @inferred(build(T, x)) === x
                @test @inferred(build(T, y)) === x
                @test @inferred(build(T, x...)) === x
                @test @inferred(build(T, y...)) === x
            end
        end
        let get_index_style = ZippedArrays.get_index_style,
            A = rand(Float32, 4, 3), B = rand(Int16, 4, 3), C = rand(Int8, 2, 3)
            @test @inferred(get_index_style()) === IndexLinear()
            @test @inferred(get_index_style(A, B)) === IndexLinear()
            @test @inferred(get_index_style(view(A, 2:3, :), C)) === IndexCartesian()
            @test_throws DimensionMismatch get_index_style(A, B, C)
        end
    end
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
    @test_throws DimensionMismatch ZippedArray(A,D)
    @test_throws DimensionMismatch ZippedArray(A,E)
    @test all_match(nothing, cos)

    # Create uninitialized zipped arrays.
    let types = (Int16,), Z = @inferred ZippedArray{Tuple{types...}}(undef, map(Int16, dims)...)
        @test IndexStyle(Z) === IndexLinear()
        @test eltype(Z) === Tuple{types...}
        @test ndims(Z) == length(dims)
        @test size(Z) == dims
        @test [x isa Array{types[i],length(dims)} for (i,x) in enumerate(Z.args)] == trues(length(types))
        @test [size(x) == dims for x in Z.args] == trues(length(types))
    end
    let types = (Int16,Float32,Char), Z = @inferred ZippedArray{Tuple{types...}}(undef, dims)
        @test IndexStyle(Z) === IndexLinear()
        @test eltype(Z) === Tuple{types...}
        @test ndims(Z) == length(dims)
        @test size(Z) == dims
        @test [x isa Array{types[i],length(dims)} for (i,x) in enumerate(Z.args)] == trues(length(types))
        @test [size(x) == dims for x in Z.args] == trues(length(types))
    end
    let types = (Int16,Char), dims = (Int16(5),), Z = @inferred ZippedVector{Tuple{types...}}(undef, dims...)
        @test IndexStyle(Z) === IndexLinear()
        @test eltype(Z) === Tuple{types...}
        @test ndims(Z) == length(dims)
        @test size(Z) == dims
    end
    let types = (Char,UInt8), dims = (Int16(2),Int8(3)), Z = @inferred ZippedMatrix{Tuple{types...}}(undef, dims...)
        @test IndexStyle(Z) === IndexLinear()
        @test eltype(Z) === Tuple{types...}
        @test ndims(Z) == length(dims)
        @test size(Z) == dims
    end

    # Zip 0 arrays.
    @test_throws ArgumentError ZippedArray()
    @test_throws ArgumentError ZippedArray{Tuple{Int16,Float32}}()
    @test_throws ArgumentError ZippedArray{Tuple{Int16,Float32},2}()
    @test_throws ArgumentError ZippedVector()
    @test_throws ArgumentError ZippedVector{Tuple{Int16,Float32}}()
    @test_throws ArgumentError ZippedMatrix()
    @test_throws ArgumentError ZippedMatrix{Tuple{Int16,Float32}}()

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
    @test @inferred(ZippedArray((A,B))) === Z
    @test @inferred(ZippedArray{Tuple{eltype(A),eltype(B)}}((A,B))) === Z
    @test @inferred(ZippedArray{Tuple{eltype(A),eltype(B)}}(A,B)) === Z
    @test @inferred(ZippedArray{Tuple{eltype(A),eltype(B)},ndims(A)}((A,B))) === Z
    @test_throws DimensionMismatch ZippedArray(A,view(A,:,:,2))
    @test_throws DimensionMismatch ZippedArray{Tuple{eltype(A),eltype(A)}}(A,view(A,:,:,2))
    @test_throws DimensionMismatch ZippedArray{Tuple{eltype(A),eltype(A)},ndims(A)}(A,view(A,:,:,2))
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

# Zip 2 named arrays.
    Z = @inferred ZippedArray(A=A,B=B)
    @test @inferred(ZippedArray((A=A,B=B))) === Z
    @test @inferred(ZippedArray{Tuple{eltype(A),eltype(B)}}((A=A,B=B))) === Z
    @test @inferred(ZippedArray{Tuple{eltype(A),eltype(B)}}(A=A,B=B)) === Z
    @test @inferred(ZippedArray{Tuple{eltype(A),eltype(B)},ndims(A)}((A=A,B=B))) === Z
    @test_throws DimensionMismatch ZippedArray(A,view(A,:,:,2))
    @test_throws DimensionMismatch ZippedArray{Tuple{eltype(A),eltype(A)}}(A,view(A,:,:,2))
    @test_throws DimensionMismatch ZippedArray{Tuple{eltype(A),eltype(A)},ndims(A)}(A,view(A,:,:,2))
    @test Z.args === (A=A,B=B,)
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
        @test Z[i] === (A=A[i], B=B[i])
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

    # Zip 3 arrays in a structured type (with no conversion and with
    # conversion).
    T1 = MyType{eltype(A),eltype(B),eltype(C)}
    T2 = MyType{eltype(A),Float32,eltype(C)}
    Z1 = @inferred(ZippedArray{T1}(undef, dims))
    Z2 = @inferred(ZippedArray{T2}(undef, dims))
    @test eltype(Z1) == T1
    @test eltype(Z2) == T2
    @test size(Z1) == dims
    @test size(Z2) == dims
    for i in eachindex(Z1, Z2, A, B, C)
        Z1[i] = T1(A[i], B[i], C[i])
        Z2[i] = T2(A[i], B[i], C[i])
    end
    @test Z1[3] === T1(A[3], B[3], C[3])
    @test Z2[3] === T2(A[3], B[3], C[3])
    let x = (-A[3], -B[2], C[2])
        Z1[3] = T1(x...); @test Z1[3] === T1(x...)
        Z2[3] = T2(x...); @test Z2[3] === T2(x...)
    end
    let x = (A[5], B[1], C[4])
        # Setting value as a tuple is supported.
        Z1[3] = x; @test Z1[3] === T1(x...)
        Z2[3] = x; @test Z2[3] === T2(x...)
    end

    # Test `copy`.
    let X = @inferred copy(Z)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z)
        @test axes(X) === axes(Z)
        @test X == Z
    end
    let X = @inferred copy(Z1)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z1)
        @test axes(X) === axes(Z1)
        @test X == Z1
    end
    let X = @inferred copy(Z2)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z2)
        @test axes(X) === axes(Z2)
        @test X == Z2
    end

    # Test `deepcopy`.
    let X = @inferred deepcopy(Z)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z)
        @test axes(X) === axes(Z)
        @test X == Z
    end
    let X = @inferred deepcopy(Z1)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z1)
        @test axes(X) === axes(Z1)
        @test X == Z1
    end
    let X = @inferred deepcopy(Z2)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z2)
        @test axes(X) === axes(Z2)
        @test X == Z2
    end

    # Test `similar`.
    let X = @inferred similar(Z)
        @test isa(X, ZippedArray)
        @test eltype(X) === eltype(Z)
        @test axes(X) === axes(Z)
    end
    let X = @inferred similar(Z, T1)
        @test isa(X, ZippedArray{T1})
        @test axes(X) === axes(Z)
    end
    let X = @inferred similar(Z2)
        @test isa(X, ZippedArray{T2})
        @test axes(X) === axes(Z2)
    end
    let new_dims = (Int16(3), Int8(4))
        let X = @inferred similar(Z, new_dims)
            @test isa(X, ZippedArray)
            @test eltype(X) === eltype(Z)
            @test size(X) == new_dims
        end
        # FIXME: @inferred is broken below for Julia < 1.7
        let X = VERSION < v"1.7" ? similar(Z, new_dims...) : @inferred similar(Z, new_dims...)
            @test isa(X, ZippedArray)
            @test eltype(X) === eltype(Z)
            @test size(X) == new_dims
        end
    end
    let new_types = Tuple{Int16,Float32}, X = @inferred similar(Z, new_types)
        @test isa(X, ZippedArray)
        @test eltype(X) === new_types
        @test size(X) == size(Z)
    end
    let new_dims = (Int16(3), Int8(4)), new_types = Tuple{Float32,Char}
        let X = @inferred similar(Z, new_types, new_dims)
            @test isa(X, ZippedArray)
            @test eltype(X) === new_types
            @test size(X) == new_dims
        end
        let X = @inferred similar(Z, new_types, new_dims...)
            @test isa(X, ZippedArray)
            @test eltype(X) === new_types
            @test size(X) == new_dims
        end
    end

    # Tests on zipped vectors.
    n = 17
    a = rand(Bool, n)
    b = rand(Float32, n)
    c = rand(Int16, n)
    @test_throws Exception sizehint!(ZippedVector(a,b,c,1:n), n + 50)
    let Z = ZippedVector(a,b,c,1:n)
        @test_throws Exception resize!(Z, n + 50)
        @test map(length, Z.args) == (n,n,n,n)
    end
    Z = @inferred sizehint!(ZippedVector(a,b,c), n + 50)
    @test [(a[i],b[i],c[i]) for i in 1:n] == Z
    x = (rand(eltype(a)), rand(eltype(b)), rand(eltype(c)))
    @test push!(Z, x) === Z
    @test length(Z) == n + 1
    @test Z[end] == x
    x = [(rand(eltype(a)), rand(eltype(b)), rand(eltype(c))) for i in 1:4]
    n = length(Z)
    @test append!(Z, x) === Z
    @test length(Z) == n + length(x)
    @test Z[end-length(x)+1:end] == x
    let iter = MyIterator{eltype(a),eltype(b),eltype(c)}()
        n = length(Z)
        @inferred append!(Z, iter)
        @test length(Z) == n + 5
    end

    # Test sorting (works for vectors).
    # Neither a nor b shall change for out-of-place sort.
    aref = copy(a);
    bref = copy(b);
    Z = @inferred sort(ZippedVector(a, b));
    @test Z isa ZippedVector
    @test aref == a
    @test bref == b
    let a = Z.args[1], b = Z.args[2]
        flag = true
        for i in 2:n
            flag &= ((a[i-1] < a[i])|((a[i-1] == a[i])&(b[i-1] <= b[i])))
        end
        @test flag
    end
    # In-place sort shall result in a and b being sorted.
    sort!(ZippedArray(a,b);
          lt = (x,y) -> ifelse(x[1] == y[1], x[2] < y[2], x[1] < y[1]))
    flag = true
    for i in 2:n
        flag &= ((a[i-1] < a[i])|((a[i-1] == a[i])&(b[i-1] <= b[i])))
    end
    @test flag
end

end
