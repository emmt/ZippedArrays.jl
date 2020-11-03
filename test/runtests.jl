module TestingZippedArrays

using Test
using ZippedArrays

dims = (2,3,4)
Ta = Int
Tb = Float64
Tc = Bool
A = rand(Ta, dims)
B = rand(Tb, dims)
C = rand(Tc, dims)
D = rand(Bool, 1, dims...)
@testset "zipped arrays" begin
    Z = ZippedArray(A,B)
    @test eltype(Z) === Tuple{eltype(A),eltype(B)}
    @test ndims(Z) == ndims(A)
    @test size(Z) == size(A)
    @test size(Z,1) == size(A,1)
    @test axes(Z) == axes(A)
    @test axes(Z,2) == axes(A,2)
    @test IndexStyle(Z) == IndexLinear()
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
    @test_throws ErrorException ZippedArray()

    # This works for vectors.
    n = 23
    a = rand(eltype(A), n)
    b = rand(eltype(B), n)
    c = rand(eltype(C), n)
    @test [(a[i],b[i],c[i]) for i in 1:n] == ZippedArray(a,b,c)
end

end
