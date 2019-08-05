package boomerang.example;

import boomerang.example.TestUtils.*;

import java.util.function.Function;

public class Mj09 {

    public static void main(String[] args) {

        H h = new H();
        Boolean tru = new Boolean(true);
        Boolean fal = new Boolean(false);
        Object x = h.apply(tru);
        Object y = h.apply(fal);

        System.out.println(x);
        System.out.println(y);

        queryFor(h);
        queryFor(tru);
        queryFor(fal);
        queryFor(x);
        queryFor(y);

    }

    private static class H {

        public Object apply(Boolean b) {
            Function<Object, Object> g = new Function<Object, Object>() {
                public Object apply(Object elm) {
                    return elm;
                }
            };
            Function<Function<Object, Object>, Object> f = new Function<Function<Object, Object>, Object>() {
                public Object apply(Function<Object, Object> k) {
                    if (b) {
                        return k.apply(new Integer(1));
                    }
                    else {
                        return k.apply(new Integer(2));
                    }
                }
            };
            Function<Object, Object> lambda1 = new Function<Object, Object>() {
                public Object apply(Object elm) {
                    return elm;
                }
            };
            Object y = f.apply(lambda1);
            return g.apply(y);
        }

    }


    private static <T> void queryFor(T query) {

    }
}
