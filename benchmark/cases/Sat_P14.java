package boomerang.example;

import java.util.function.Function;

public class Sat_P14 {
    public static void main(String[] args) {
        Phi phi = new Phi();
        Try tryFun = new Try();
        SatSolve14 satSolve14 = new SatSolve14();
        satSolve14.tryFun = tryFun;
        Boolean result = satSolve14.apply(phi);
        System.out.println(result);
        queryFor(phi);
        queryFor(tryFun);
        queryFor(satSolve14);
        queryFor(result);
    }

    private static class Phi {
        public Boolean apply(Boolean x1, Boolean x2, Boolean x3, Boolean x4, Boolean x5, Boolean x6, Boolean x7, Boolean x8, Boolean x9, Boolean x10, Boolean x11, Boolean x12, Boolean x13, Boolean x14) {
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

    private static class SatSolve14 {
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
            Function<Boolean,Boolean> f7 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n7) {
            Function<Boolean,Boolean> f8 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n8) {
            Function<Boolean,Boolean> f9 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n9) {
            Function<Boolean,Boolean> f10 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n10) {
            Function<Boolean,Boolean> f11 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n11) {
            Function<Boolean,Boolean> f12 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n12) {
            Function<Boolean,Boolean> f13 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n13) {
            Function<Boolean,Boolean> f14 = new Function<Boolean,Boolean>() {
            public Boolean apply(Boolean n14) {
                boolean resVal = phi.apply(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14);
                 Boolean result = new Boolean(resVal);
                 return result;
             }};
             boolean resVal = tryFun.apply(f14);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f13);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f12);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f11);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f10);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f9);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f8);
             Boolean result = new Boolean(resVal);
             return result;
             }};
             boolean resVal = tryFun.apply(f7);
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
