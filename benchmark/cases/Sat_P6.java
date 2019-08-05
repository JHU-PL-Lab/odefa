package boomerang.example;

import java.util.function.Function;

public class Sat_P6 {
    public static void main(String[] args) {
        Phi phi = new Phi();
        Try tryFun = new Try();
        SatSolve6 satSolve6 = new SatSolve6();
        satSolve6.tryFun = tryFun;
        Boolean result = satSolve6.apply(phi);
        System.out.println(result);
        queryFor(phi);
        queryFor(tryFun);
        queryFor(satSolve6);
        queryFor(result);
    }

    private static class Phi {
        public Boolean apply(Boolean x1, Boolean x2, Boolean x3, Boolean x4, Boolean x5, Boolean x6) {
            boolean val = (x1 || (!x2) || (!x3)) && ((!x2) || (!x3)) && (x4 || x2);
            Boolean result = new Boolean(val);
            return result;
        }
    }

    private static class Try {
        public Boolean apply(Function<Boolean, Boolean> f) {
            Boolean tru = new Boolean(true);
            Boolean fal = new Boolean(false);
            Boolean res = f.apply(tru) || f.apply(fal);
            return res;
        }
    }

    private static class SatSolve6 {
        public static Try tryFun;
        public Boolean apply(Phi phi) {
            Function<Boolean,Boolean> f1 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n1) {
            Function<Boolean,Boolean> f2 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n2) {
            Function<Boolean,Boolean> f3 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n3) {
            Function<Boolean,Boolean> f4 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n4) {
            Function<Boolean,Boolean> f5 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n5) {
            Function<Boolean,Boolean> f6 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n6) {
                boolean resVal = phi.apply(n1, n2, n3, n4, n5, n6);
                 Boolean result = new Boolean(resVal);
                 return result;
             }};
             boolean resVal = tryFun.apply(f6);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f5);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f4);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f3);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f2);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f1);
             Boolean result = new Boolean(resVal);
             return result;
        }
    }
    private static <T> void queryFor(T query) { }
}
