package boomerang.example;

import java.util.function.Function;

public class Recurse_S2 {
    public static void main(String[] args) {
        Pathological pathological = new Pathological();
        Integer answer = pathological.apply(new Integer(5));
        queryFor(pathological);
        queryFor(answer);
    }

    private static class Pathological {
        public Integer apply(Integer x) {
            if (x.intValue() == x.intValue()) { return this.apply(x); } else
            if (x.intValue() == x.intValue()) { return this.apply(x); } else
            return 0;
        }
    }
    private static <T> void queryFor(T query) { }
}
