module BenchmarkingZippedArrays

using ZippedArrays
using BenchmarkTools

n = 10_000;
A = rand(Float64, n);
B = rand(Int64, n);
C = [(A[i],B[i]) for i in 1:n];
Z = ZippedArray(A,B);
C == Z || println("unexpected result!!!")

N = ZippedArray(A=A,B=B);

function sum_first(A::AbstractArray{<:Tuple})
    s = 0.0
    @inbounds @simd for i in eachindex(A)
        s += first(A[i])
    end
    return s
end

@btime sum_first($C) # 1.615 μs (0 allocations: 0 bytes)
@btime sum_first($Z) # 643.983 ns (0 allocations: 0 bytes)
@btime sum_first($N) # 643.983 ns (0 allocations: 0 bytes)

end # module
