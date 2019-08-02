package boomerang.example;

import java.util.Random;

public class Primtest {

    public static void main(String[] args) {

        Square square = new Square();
        ModuloPower moduloPower = new ModuloPower();
        moduloPower.square = square;
        IsTrivialComposite isTrivialComposite = new IsTrivialComposite();
        IsFermatPrime isFermatPrime = new IsFermatPrime();
        isFermatPrime.moduloPower = moduloPower;
        GenerateFermatPrime generateFermatPrime = new GenerateFermatPrime();
        generateFermatPrime.isFermatPrime = isFermatPrime;
        generateFermatPrime.isTrivialComposite = isTrivialComposite;

        Integer iterations = new Integer(10);
        Integer byteSize = new Integer(15);
        Integer result = generateFermatPrime.apply(byteSize, iterations);

        System.out.println(result);

        queryFor(square);
        queryFor(moduloPower);
        queryFor(isTrivialComposite);
        queryFor(isFermatPrime);
        queryFor(generateFermatPrime);
        queryFor(iterations);
        queryFor(byteSize);
        queryFor(result);
    }

    private static class Square {

        public Integer apply(Integer x) {
            return new Integer(x * x);
        }

    }

    private static class ModuloPower {

        Square square;

        public Integer apply(Integer base, Integer exp, Integer n) {
            if (exp == 0) {
                return new Integer(1);
            }
            if (exp % 2 != 0) {
                Integer result = new Integer (n * this.apply(base, exp-1, n)) % n;
                return result;
            }
            else {
                Integer result = new Integer (square.apply(this.apply(base, exp/2, n))) % n;
                return result;
            }
        }

    }

    private static class IsTrivialComposite {

        public Boolean apply(Integer n) {
            Boolean result = (n%2 == 0) || (n%3 == 0) || (n%5 == 0) || (n%7 == 0) ||
                             (n%11 == 0) || (n%13 == 0) || (n%17 == 0) || (n%19 == 0) ||
                             (n%23 == 0) || (n == 1);
            return new Boolean(result);
        }

    }

    private static class IsFermatPrime {

        ModuloPower moduloPower;

        public Boolean apply(Integer n, Integer iterations) {
            Boolean b1 = new Boolean (iterations <= 0);
            Double bSize = Math.pow(2, Math.ceil(Math.log(n)/Math.log(2)));
            Integer byteSize = bSize.intValue();
            Random random = new Random();
            Integer a = random.nextInt(byteSize);
            if (moduloPower.apply(a, n-1, n) == 1) {
                return b1 || new Boolean(this.apply(n, iterations-1));
            }
            else {
                return b1 || new Boolean(false);
            }
        }
    }

    private static class GenerateFermatPrime {

        IsTrivialComposite isTrivialComposite;
        IsFermatPrime isFermatPrime;

        public Integer apply(Integer byteSize, Integer iterations) {
            Random random = new Random();
            Double bSize = Math.pow(2, byteSize);
            Integer realByteSize = bSize.intValue();
            Integer n = random.nextInt(realByteSize);
            if ((!isTrivialComposite.apply(n)) && isFermatPrime.apply(n, iterations)) {
                return n;
            }
            else {
                return this.apply(byteSize, iterations);
            }
        }

    }

    private static <T> void queryFor(T query) {

    }
}
