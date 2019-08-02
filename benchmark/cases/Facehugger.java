package boomerang.example;

public class Facehugger {

    public static void main(String[] args) {

        Integer three = new Integer(3);
        Integer four = new Integer(4);
        IdFun idFun = new IdFun();
        F f = new F();
        G g = new G();
        int resVal = idFun.apply(f).apply(three) + idFun.apply(g).apply(four);
        Integer result = new Integer(resVal);

        System.out.println(result);

        queryFor(three);
        queryFor(four);
        queryFor(idFun);
        queryFor(f);
        queryFor(g);
        queryFor(resVal);
        queryFor(result);
    }

    private static class IdFun {
        public <T> T apply(T elm){
            return elm;
        }
    }

    private static class F {
        public Integer apply(Integer n) {
            if (n <= 1) {
                return new Integer(1);
            }
            else {
                Integer res = new Integer(n * (this.apply(n-1)));
                return res;
            }
        }
    }

    private static class G {
        public Integer apply(Integer n) {
            if (n <= 1) {
                return new Integer(1);
            }
            else {
                Integer res = new Integer(n * (this.apply(n-1)));
                return res;
            }
        }
    }


    private static <T> void queryFor(T query) {

    }
}
