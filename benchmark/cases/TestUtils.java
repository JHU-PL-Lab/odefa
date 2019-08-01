package boomerang.example;

public class TestUtils {

    public static class LinkedList <Abs_type> {
        Abs_type head;
        LinkedList<Abs_type> tail;
    }

    // function for dynamic list access
    public static <T> T listAccess(LinkedList<T> list_to_search, int index) {
        if (index == 0) {
            T head = list_to_search.head;
            return head;
        }
        {
            int new_index = index - 1;
            LinkedList<T> new_list_to_search = list_to_search.tail;
            T result = listAccess(new_list_to_search, new_index);
            return result;
        }
    }

    // function that makes homogenious lists
    public static <T> LinkedList<T> listMaker (T element, int numTimes) {

        if (numTimes == 0) {
            return null;
        } {
            LinkedList new_list = new LinkedList();
            new_list.head = element;
            LinkedList tail = listMaker(element, (numTimes - 1));
            new_list.tail = tail;
            return new_list;
        }
    }

    // function making MyInts of the current number, decrementing every time.
    public static LinkedList<MyInteger> myIntListMaker (int curr_num) {

        if (curr_num == 0) {
            return null;
        } {
            LinkedList new_list = new LinkedList();
            new_list.head = new MyInteger(curr_num);
            LinkedList tail = myIntListMaker(curr_num - 1);
            new_list.tail = tail;
            return new_list;
        }
    }

    private static abstract class AbstractValue {
    }

    public static class MyBoolean extends AbstractValue {
        private boolean value;

        public MyBoolean (boolean value) {
            this.value = value;
        }

        public boolean getValue() {

            return this.value;
        }

        public MyBoolean and(MyBoolean bool2) {
            boolean val1 = this.getValue();
            boolean val2 = bool2.getValue();
            boolean result_val = val1 & val2;
            MyBoolean result_boolean = new MyBoolean(result_val);
            return result_boolean;
        }

        public MyBoolean or(MyBoolean bool2) {
            boolean val1 = this.getValue();
            boolean val2 = bool2.getValue();
            boolean result_val = val1 | val2;
            MyBoolean result_boolean = new MyBoolean(result_val);
            return result_boolean;
        }

        public MyBoolean not() {
            boolean curr_val = this.getValue();
            boolean result_val = ! curr_val;
            MyBoolean result_boolean = new MyBoolean(result_val);
            return result_boolean;
        }
    }

    public static class MyInteger extends AbstractValue {
        private int value;

        public MyInteger(int value) {
            this.value = value;
        }

        public int getValue() {
            return this.value;
        }

        // method that lets you add the MyInteger with another MyInteger, and returns the result as a new MyInteger.
        public MyInteger add(MyInteger int2) {
            int val1 = this.getValue();
            int val2 = int2.getValue();
            int result_val = val1 + val2;
            MyInteger result_integer = new MyInteger(result_val);
            return result_integer;
        }

        public MyInteger multiply(MyInteger int2) {
            int val1 = this.getValue();
            int val2 = int2.getValue();
            int result_val = val1 * val2;
            MyInteger result_integer = new MyInteger(result_val);
            return result_integer;
        }


    }


    public static class Pair <T, U> {
        private T first;
        private U second;

        public Pair(T first, U second) {
            this.first = first;
            this.second = second;
        }

        public T getFirst() {
            return this.first;
        }

        public U getSecond() {
            return this.second;
        }

    }
}
