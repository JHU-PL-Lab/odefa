package boomerang.example;

import java.util.function.Function;
import java.util.function.BiFunction;


public class Kcfa_2 {

    public static void main(String[] args) {

        Function<Function<Object, Object>, Object> fun1 = new Function<Function<Object, Object>, Object>() {
            public Object apply(Function<Object, Object> f1) {
                Boolean tru = new Boolean(true);
                Boolean fal = new Boolean(false);
                f1.apply(tru);
                return f1.apply(fal);
            }
        };

        Function<Object, Object> fun2 = new Function<Object, Object>() {
            public Object apply(Object x1) {
                Function<Function<Object, Object>, Object> fun3 = new Function<Function<Object, Object>, Object>() {

                    public Object apply(Function<Object, Object> f2) {
                        Boolean tru = new Boolean(true);
                        Boolean fal = new Boolean(false);
                        f2.apply(tru);
                        return f2.apply(fal);
                    }
                };
                Function<Object, Object> fun4 = new Function<Object, Object>() {

                    public Object apply(Object x2) {

                        Function<BiFunction<Object, Object, Object>, Object> fun5 = new Function<BiFunction<Object, Object, Object>, Object>() {

                            public Object apply(BiFunction<Object, Object, Object> z) {
                                return z.apply(x1, x2);
                            }

                        };
                        BiFunction<Object, Object, Object> fun6 = new BiFunction<Object, Object, Object>() {

                            public Object apply(Object y1, Object y2) {
                                return y1;
                            }

                        };
                        return fun5.apply(fun6);

                    }
                };
                return fun3.apply(fun4);
            }
        };

        Object result = fun1.apply(fun2);

        System.out.println(result);

        queryFor(fun1);
        queryFor(fun2);
        queryFor(result);

    }

    private static <T> void queryFor(T query) {

    }
}

/*
(fun f1 -> let dum = (f1 true) in (f1 false)) Function<Function<Object, Object>, Object>
  (fun x1 ->
    (fun f2 ->
      (let dum = (f2 true) in (f2 false))
    )
    (Function<Fuction<Object, Object>, Object>)

    (fun x2 ->
      (fun z -> z x1 x2) (Function<BiFunction<Object, Object, Object>, Object>)
      (fun y1 (Object) -> fun y2 (Object) -> y1 (Object)) (BiFunction<Object, Object, Object>)
    ) (Fuction<Object, Object>)
  )Function<Object, Object>
)
 */