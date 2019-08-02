package boomerang.example;

import java.util.function.Function;

public class Eta {

    public static void main(String[] args) {

        DoSomething doSomething = new DoSomething();

        IdFun idFun = new IdFun();
        idFun.doSomething = doSomething;

        Function<Boolean, Boolean> fun1 = new Function<Boolean, Boolean>() {
            public Boolean apply(Boolean b) {
                return b;
            }
        };

        Function<Boolean, Boolean> fun2 = new Function<Boolean, Boolean>() {
            public Boolean apply(Boolean b) {
                return b;
            }
        };

        Boolean t = new Boolean(true);
        Boolean f = new Boolean(false);
        Boolean res1 = idFun.apply(fun1).apply(t);
        Boolean res2 = idFun.apply(fun2).apply(f);

        System.out.println(res1);
        System.out.println(res2);

        queryFor(doSomething);
        queryFor(idFun);
        queryFor(fun1);
        queryFor(fun2);
        queryFor(t);
        queryFor(f);
        queryFor(res1);
        queryFor(res2);
    }

    private static class DoSomething {
        public Integer apply() {
            return new Integer(10);
        }
    }

    private static class IdFun {

        DoSomething doSomething;

        public <T> T apply(T elm) {
            doSomething.apply();
            return elm;
        }
    }

    private static <T> void queryFor(T query) {

    }
}
