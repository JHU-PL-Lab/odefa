package boomerang.example;

import boomerang.example.TestUtils.*;

public class Flatten {

    public static void main(String[] args) {

        FlattenFun flatten = new FlattenFun();
        LinkedList<Integer> lst1 = new LinkedList<>();
        lst1.head = new Integer(1);
        lst1.tail = new LinkedList<>();
        lst1.tail.head = new Integer(2);
        LinkedList<Integer> lst2 = new LinkedList<>();
        lst2.head = new Integer(3);
        lst2.tail = new LinkedList<>();
        lst2.tail.head = new Integer(4);
        lst2.tail.tail = new LinkedList<>();
        lst2.tail.tail.head = new Integer(5);
        LinkedList<Object> lst3 = new LinkedList<>();
        lst3.head = lst2;
        LinkedList<Object> lst4 = new LinkedList<>();
        lst4.head = lst3;
        LinkedList<Object> lst = new LinkedList<>();
        lst.head = lst1;
        lst.tail = lst4;
        LinkedList<Object> result = flatten.apply(lst);

        queryFor(flatten);
        queryFor(lst1);
        queryFor(lst2);
        queryFor(lst3);
        queryFor(lst4);
        queryFor(lst);
        queryFor(result.head);

    }

    private static class FlattenFun {

        private <T> LinkedList<T> append(LinkedList<T> lst1, LinkedList<T> lst2) {
            if (lst1 == null) {
                return lst2;
            }
            LinkedList<T> newLst = new LinkedList<>();
            newLst.head = lst1.head;
            newLst.tail = append(lst1.tail, lst2);
            return newLst;
        }

        public LinkedList<Object> apply(Object x) {
            if (x == null) {
                return null;
            }
            if (x instanceof LinkedList) {
                LinkedList<Object> resultHead = this.apply(((LinkedList) x).head);
                LinkedList<Object> resultTail = this.apply(((LinkedList) x).tail);
                LinkedList<Object> result = append(resultHead, resultTail);

                return result;
            }
            else {
                LinkedList<Object> result = new LinkedList<>();
                result.head = x;
                return result;
            }
        }
    }

    private static <T> void queryFor(T query) {

    }
}
