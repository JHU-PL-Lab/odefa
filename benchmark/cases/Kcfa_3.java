package boomerang.example;

import java.util.Objects;
import java.util.function.Function;

public class Kcfa_3 {

    public static void main(String... args) {
        Fun1 fun1 = new Fun1();
        X1 x1 = new X1();
        Object result = fun1.apply(x1);
        System.out.println(result);
        queryFor(result);
    }

    private static <T> void queryFor(T query) {

    }

    private static class Fun1 implements Function<Function<Object, Object>, Object> {
        public Object apply(Function<Object, Object> fun) {
            Boolean tru = new Boolean(true);
            Boolean fal = new Boolean(false);
            fun.apply(tru);
            return fun.apply(fal);
        }
    }

    private static class X1 implements Function<Object, Object> {
        public Object apply(Object x1) {
            Fun2 fun2 = new Fun2();
            X2 x2 = new X2(x1);
            Object result = fun2.apply(x2);
            return result;
        }

        private static class Fun2 implements Function<Function<Object, Object>, Object> {
            public Object apply(Function<Object, Object> fun) {
                Boolean tru = new Boolean(true);
                Boolean fal = new Boolean(false);
                fun.apply(tru);
                return fun.apply(fal);
            }
        }

        private static class X2 implements Function<Object, Object> {
            private Object x1;

            public X2(Object x1) { this.x1 = x1; }

            public Object apply(Object x2) {
                Fun3 fun3 = new Fun3();
                X3 x3 = new X3(this.x1, x2);
                Object result = fun3.apply(x3);
                return result;
            }

            private static class Fun3 implements Function<Function<Object, Object>, Object> {
                public Object apply(Function<Object, Object> fun) {
                    Boolean tru = new Boolean(true);
                    Boolean fal = new Boolean(false);
                    fun.apply(tru);
                    return fun.apply(fal);
                }
            }

            private static class X3 implements Function<Object, Object> {
                private Object x1;
                private Object x2;

                public X3(Object x1, Object x2) {
                    this.x1 = x1;
                    this.x2 = x2;
                }

                public Object apply(Object x3) {
                    Z z_fun = new Z(this.x1, this.x2, x3);
                    Y1 y1_fun = new Y1();
                    Object result = z_fun.apply(y1_fun);
                    return result;
                }

                private static class Z implements Function<TriFunction<Object, Object, Object, Object>, Object> {
                    private Object x1;
                    private Object x2;
                    private Object x3;

                    public Z(Object x1, Object x2, Object x3) {
                        this.x1 = x1;
                        this.x2 = x2;
                        this.x3 = x3;
                    }

                    public Object apply(TriFunction<Object, Object, Object, Object> fun) {
                        Object result = fun.apply(this.x1, this.x2, this.x3);
                        return result;
                    }
                }

                public interface TriFunction<A, B, C, R> {
                    public R apply(A a, B b, C c);
                }

                private static class Y1 implements TriFunction<Object, Object, Object, Object> {
                    public Object apply(Object y1, Object y2, Object y3) {
                        return y1;
                    }
                }

            }

        }

    }


}
