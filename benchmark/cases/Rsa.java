package boomerang.example;

import java.math.*;
import boomerang.example.TestUtils.*;

public class Rsa {

    public static void main(String[] args) {

        ExtendedGcd extendedGcd = new ExtendedGcd();
        ModuloInverse moduloInverse = new ModuloInverse();
        moduloInverse.extendedGcd = extendedGcd;
        Totient totient = new Totient();
        Square square = new Square();
        ModuloPower moduloPower = new ModuloPower();
        moduloPower.square = square;
        IsLegalPublicExponent isLegalPublicExponent = new IsLegalPublicExponent();
        isLegalPublicExponent.totient = totient;
        PrivateExponent privateExponent = new PrivateExponent();
        privateExponent.moduloInverse = moduloInverse;
        privateExponent.isLegalPublicExponent = isLegalPublicExponent;
        privateExponent.totient = totient;

        Encrypt encrypt = new Encrypt();
        encrypt.moduloPower = moduloPower;
        Decrypt decrypt = new Decrypt();
        decrypt.moduloPower = moduloPower;

        Integer p = new Integer(41);
        Integer q = new Integer(47);
        Integer n = new Integer(p*q);

        Integer e = new Integer(7);
        Integer d = privateExponent.apply(e, p, q);

        Integer plaintext = new Integer(42);
        Integer ciphertext = encrypt.apply(plaintext, e, n);
        Integer decryptedCiphertext = decrypt.apply(ciphertext, d, n);

        queryFor(extendedGcd);
        queryFor(moduloInverse);
        queryFor(totient);
        queryFor(square);
        queryFor(moduloPower);
        queryFor(isLegalPublicExponent);
        queryFor(privateExponent);
        queryFor(encrypt);
        queryFor(decrypt);
        queryFor(p);
        queryFor(q);
        queryFor(n);
        queryFor(e);
        queryFor(d);
        queryFor(plaintext);
        queryFor(ciphertext);
        queryFor(decryptedCiphertext);

        if (!(plaintext == decryptedCiphertext)) {
            throw new RuntimeException("RSA fail!");
        }
        else {
            System.out.println("RSA success!");
        }


    }

    private static class ExtendedGcd {

        public LinkedList<Integer> apply(Integer a, Integer b) {
            if (Math.floorMod(a, b) == 0) {
                LinkedList<Integer> result = new LinkedList<>();
                result.head = new Integer(0);
                result.tail = new LinkedList<>();
                result.tail.head = new Integer(1);
                return result;
            }
            else {
                LinkedList<Integer> resLst = this.apply(b, Math.floorMod(a, b));
                Integer x = resLst.head;
                Integer y = resLst.tail.head;
                LinkedList<Integer> result = new LinkedList<>();
                result.head = y;
                result.tail = new LinkedList<>();
                result.tail.head = x - (y * (a / b));
                return result;
            }
        }
    }

    private static class ModuloInverse {

        ExtendedGcd extendedGcd;

        public Integer apply(Integer a, Integer n) {
            LinkedList<Integer> temp = extendedGcd.apply(a, n);
            Integer resVal = Math.floorMod(temp.head, n);
            return new Integer(resVal);
        }
    }

    private static class Totient {

        public Integer apply(Integer p, Integer q) {
            return new Integer ((p - 1) * (q - 1));
        }

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
            if (Math.floorMod(exp, 2) != 0) {
                Integer result = new Integer (Math.floorMod(n * this.apply(base, exp-1, n),n));
                return result;
            }
            else {
                Integer result = new Integer (Math.floorMod(square.apply(this.apply(base, exp/2, n)), n));
                return result;
            }
        }

    }

    private static class IsLegalPublicExponent {

        Totient totient;

        public Boolean apply(Integer e, Integer p, Integer q) {
            Boolean b1 = 1 < e;
            Boolean b2 = e < totient.apply(p, q);
            Boolean b3 = (BigInteger.valueOf(e).gcd(BigInteger.valueOf(totient.apply(p, q))).intValue()) == 1;
            return new Boolean(b1 && b2 && b3);
        }

    }

    private static class PrivateExponent {

        IsLegalPublicExponent isLegalPublicExponent;
        ModuloInverse moduloInverse;
        Totient totient;

        public Integer apply(Integer e, Integer p, Integer q) {
            if (isLegalPublicExponent.apply(e, p, q)) {
                return moduloInverse.apply(e, totient.apply(p, q));
            }
            else {
                throw new RuntimeException("Illegal public exponent!");
            }
        }

    }

    private static class Encrypt {

        ModuloPower moduloPower;

        public Integer apply(Integer m, Integer e, Integer n){
            if (m > n) {
                throw new RuntimeException("The modulus is too small to encrypt the message.");
            }
            else {
                return moduloPower.apply(m, e, n);
            }
        }

    }

    private static class Decrypt {

        ModuloPower moduloPower;

        public Integer apply(Integer c, Integer d, Integer n) {
            return moduloPower.apply(c, d, n);
        }

    }

    private static <T> void queryFor(T query) {

    }
}
