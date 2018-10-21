
package math

object FpxxTesterSupport {

    def printAll(opA: Float, opB: Float, expected: Float, actual: Float) {
        printf("op A:     ")
        Fp32.print(opA)
        printf("\n")

        printf("op B:     ")
        Fp32.print(opB)
        printf("\n")
        printf("\n")

        printf("Expected: ")
        Fp32.print(expected)
        printf("\n")

        printf("Actual  : ")
        Fp32.print(actual)
        printf("\n")
    }

    def printAll(op: Float, expected: Float, actual: Float) {
        printf("op:       ")
        Fp32.print(op)
        printf("\n")

        printf("Expected: ")
        Fp32.print(expected)
        printf("\n")

        printf("Actual  : ")
        Fp32.print(actual)
        printf("\n")
    }

    def directedStimuli = Array[(Float, Float)](
                                (0,0), (0,1), (1,0),
                                (0,-0), (0,-1), (-1,0),
                                (1,1), (1, -1), (-1, 1), (-1, -1),
                                (100, 1), (-100, 1), (100, -1), (1, -100), (-1, 100), (-100, -1), (-1, -100),
                                (100000000, 1), (1, 100000000),
                                (100, 0.001f), (100, -0.001f),
                                (100, -99.9999f),
                                (Float.NaN, Float.NaN), (Float.NaN, 1), (Float.NaN, Float.PositiveInfinity), (Float.NaN, Float.NegativeInfinity),
                                (Float.PositiveInfinity, 1), (Float.NegativeInfinity, 1), (1, Float.PositiveInfinity), (1, Float.NegativeInfinity),
                                (Float.PositiveInfinity, Float.PositiveInfinity), (Float.PositiveInfinity, Float.NegativeInfinity), (Float.NegativeInfinity, Float.NegativeInfinity),
                                (Float.MaxValue, 1), (Float.MaxValue, Float.MaxValue), (1, Float.MaxValue),
                                (-Float.MaxValue, 1), (-Float.MaxValue, -Float.MaxValue), (-1, Float.MaxValue)
                            )

}
