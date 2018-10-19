
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

}
