package boomerang.example;

import boomerang.example.TestUtils.*;
import java.util.function.Function;

public class Map {

    public static void main(String[] args) {

        DebugTrace debugTrace = new DebugTrace();
        IdFun idFun = new IdFun();
        idFun.debugTrace = debugTrace;
        MyMap myMap = new MyMap();

        myMap.debugTrace = debugTrace;
        myMap.idFun = idFun;

        Function<Object, Object> f1 = new Function<Object, Object>() {
            public Object apply(Object a) {
                return new Integer((Integer) a+1);
            }
        };
        Function<Object, Object> f2 = new Function<Object, Object>() {
            public Integer apply(Object b) {
                return new Integer((Integer) b+1);
            }
        };

        LinkedList<Object> lst1 = new LinkedList<>();
        lst1.head = new Integer(1);
        lst1.tail = new LinkedList<>();
        lst1.tail.head = new Integer(2);
        lst1.tail.tail = new LinkedList<>();
        lst1.tail.tail.head = new Integer(3);

        LinkedList<Object> lst2 = new LinkedList<>();
        lst2.head = new Integer(7);
        lst2.tail = new LinkedList<>();
        lst2.tail.head = new Integer(8);
        lst2.tail.tail = new LinkedList<>();
        lst2.tail.tail.head = new Integer(9);

        LinkedList<Object> ans1 = myMap.apply(idFun.apply(f1), lst1);
        LinkedList<Object> ans2 = myMap.apply(idFun.apply(f2), lst2);

        queryFor(debugTrace);
        queryFor(idFun);
        queryFor(myMap);
        queryFor(f1);
        queryFor(f2);
        queryFor(lst1);
        queryFor(lst2);
        queryFor(ans1);
        queryFor(ans2);

    }

    public static class DebugTrace {
        public Boolean apply() {
            return new Boolean(false);
        }
    }

    private static class IdFun {

        DebugTrace debugTrace;

        public <T> T apply(T xx) {
            debugTrace.apply();
            return xx;
        }

    }

    private static class MyMap {

        DebugTrace debugTrace;
        IdFun idFun;

        public LinkedList<Object> apply(Function<Object, Object> f, LinkedList<Object> l) {
            debugTrace.apply();
            Function<LinkedList<Object>, LinkedList<Object>> lp = new Function<LinkedList<Object>, LinkedList<Object>>() {

                public LinkedList<Object> apply(LinkedList<Object> lst) {
                    if (lst == null) {
                        return null;
                    }
                    else {
                        LinkedList<Object> result = new LinkedList<>();
                        Object newHd = idFun.apply(f).apply(lst.head);
                        LinkedList<Object> newTl = this.apply(lst.tail);
                        result.head = newHd;
                        result.tail = newTl;
                        return result;
                    }
                }
            };
            return lp.apply(l);
        }

    }

    private static <T> void queryFor(T query) {

    }
}
