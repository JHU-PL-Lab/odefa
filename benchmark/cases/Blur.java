package boomerang.example;
import boomerang.example.TestUtils.*;

public class Blur {

    public static void main(String[] args) {

        IdFun idFun = new IdFun();
        BlurFun blurFun = new BlurFun();
        Lp lp = new Lp();
        lp.idFun = idFun;
        lp.blurFun = blurFun;
        lp.lp = lp;
        MyBoolean f = new MyBoolean(false){};
        MyBoolean result = lp.apply(f, 2);

        System.out.println(result);

        queryFor(idFun);
        queryFor(blurFun);
        queryFor(lp);
        queryFor(f);
        queryFor(result);

    }

    private static class IdFun {

        public <T> T apply(T elm) {
            return elm;
        }
    }

    private static class BlurFun {

        public <T> T apply(T elm) {
            return elm;
        }
    }

    private static class Lp {

        BlurFun blurFun;
        IdFun idFun;
        Lp lp;

        public MyBoolean apply(MyBoolean a, Integer n) {
            if (n <= 1) {
                return idFun.apply(a);
            }
            else {
                MyBoolean tru = new MyBoolean(true){};
                MyBoolean fal = new MyBoolean(false){};
                MyBoolean r = blurFun.apply(idFun).apply(tru);
                MyBoolean s = blurFun.apply(idFun).apply(fal);
                MyBoolean res = (blurFun.apply(lp).apply(s, n-1));
                return res;
            }
        }
    }

    private static <T> void queryFor(T query) {

    }
}
