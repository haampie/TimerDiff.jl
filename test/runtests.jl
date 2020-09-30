using Test
import TimerDiff: backtrack, KEEP, ADD, DELETE

@testset "Diff" begin
    @test all(backtrack("abc", "abc") .== KEEP)

    # test a minimal example which has two correct solutions
    result = backtrack("hallo", "hello!")
    @test result == [KEEP, DELETE, ADD, KEEP, KEEP, KEEP, ADD] || result == [KEEP, ADD, DELETE, KEEP, KEEP, KEEP, ADD]
end