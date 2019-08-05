package boomerang.example;

import java.util.Objects;
import java.util.function.Function;

public class Regex {


    public static void main(String... args) {
        // values for integers in alphabet
        int foo_val = 1;
        int bar_val = 2;
        int baz_val = 3;
        int barn_val = 4;
        int f_val = 100;

        // here is our alphabet
        Integer foo = new Integer(foo_val);
        Integer bar = new Integer(bar_val);
        Integer baz = new Integer(baz_val);
        Integer barn = new Integer(barn_val);
        Integer f = new Integer(f_val);

        RegularExpr seq_symbolLeft = new Symbol(foo);

        RegularExpr rep_innerSymbol = new Symbol(bar);
        RegularExpr seq_symbolRight = new Rep(rep_innerSymbol);
        RegularExpr seq_expr = new Seq(seq_symbolLeft, seq_symbolRight);

        Symbol symbolFoo = new Symbol(foo);
        Symbol symbolBar = new Symbol(bar);
        Symbol symbolBarn = new Symbol(barn);

        LinkedList<Symbol> dummy_list_0 = null;
        LinkedList<Symbol> dummy_list_1 = new LinkedList<>();
        dummy_list_1.head = symbolBar;
        dummy_list_1.tail = dummy_list_0;

        LinkedList<Symbol> dummy_list_2 = new LinkedList<>();
        dummy_list_2.head = symbolFoo;
        dummy_list_2.tail = dummy_list_1;

        LinkedList<Symbol> data = dummy_list_2;

        Object regex_match_res = regexMatch(seq_expr, data);

        Boolean fal = new Boolean(false);

        Object result = checkExpect(regex_match_res, fal);

        System.out.println(result);

        queryFor(result);

    }

    // interface for visitors
    public interface RegexVisitor<P, R> {
        public R visitEmptyset(RegularExpr e, P p);

        public R visitEpsilon(RegularExpr e, P p);

        public R visitSymbol(RegularExpr e, P p);

        public R visitAlt(RegularExpr e, P p);

        public R visitSeq(RegularExpr e, P p);

        public R visitRep(RegularExpr e, P p);
    }

    private static RegularExpr matchSeq(RegularExpr re, Function<Pair<RegularExpr, RegularExpr>, RegularExpr> f) {
        // we assume that the regular expression passed in is a Seq... so we can cast?
        Seq expr = (Seq) re;
        RegularExpr expr_left = expr.getLeft();
        RegularExpr expr_right = expr.getRight();
        Pair<RegularExpr, RegularExpr> f_arg = new Pair<>(expr_left, expr_right);
        RegularExpr result = f.apply(f_arg);
        return result;
    }

    private static RegularExpr matchAlt(RegularExpr re, Function<Pair<RegularExpr, RegularExpr>, RegularExpr> f) {
        // we assume that the regular expression passed in is an Alt... so we can cast?
        Alt expr = (Alt) re;
        RegularExpr expr_left = expr.getLeft();
        RegularExpr expr_right = expr.getRight();
        Pair<RegularExpr, RegularExpr> f_arg = new Pair<>(expr_left, expr_right);
        RegularExpr result = f.apply(f_arg);
        return result;
    }

    private static RegularExpr matchRep(RegularExpr re, Function<RegularExpr, RegularExpr> f) {
        // cast since we assume that the regular expression passed in is a Rep
        Rep expr = (Rep) re;
        RegularExpr expr_inner = expr.getInnerExpr();
        RegularExpr result = f.apply(expr_inner);
        return result;
    }

    private static class ConstructSeq implements Function<Pair<RegularExpr, RegularExpr>, RegularExpr> {
        public RegularExpr apply(Pair<RegularExpr, RegularExpr> pat_pair) {
            RegularExpr pat1 = pat_pair.getFirst();
            RegularExpr pat2 = pat_pair.getSecond();
            if (pat1 instanceof Emptyset) {
                RegularExpr empty = new Emptyset();
                return empty;
            } else {
                if (pat2 instanceof Emptyset) {
                    RegularExpr empty = new Emptyset();
                    return empty;
                } else {
                    if (pat1 instanceof Epsilon) {
                        return pat2;
                    } else {
                        if (pat2 instanceof Epsilon) {
                            return pat1;
                        } else {
                            RegularExpr new_seq = new Seq(pat1, pat2);
                            return new_seq;
                        }
                    }
                }
            }
        }
    }


    private static class ConstructAlt implements Function<Pair<RegularExpr, RegularExpr>, RegularExpr> {
        public RegularExpr apply(Pair<RegularExpr, RegularExpr> pat_pair) {
            RegularExpr pat1 = pat_pair.getFirst();
            RegularExpr pat2 = pat_pair.getSecond();
            if (pat1 instanceof Emptyset) {
                return pat2;
            } else {
                if (pat2 instanceof Emptyset) {
                    return pat1;
                } else {
                    RegularExpr new_alt = new Alt(pat1, pat2);
                    return new_alt;
                }
            }
        }
    }

    public static class ConstructRep implements Function<RegularExpr, RegularExpr> {
        public RegularExpr apply(RegularExpr pat) {
            RegexVisitor<Void, RegularExpr> const_rep_visitor = new RegexVisitor<Void, RegularExpr>() {


                public RegularExpr visitEmptyset(RegularExpr e, Void v) {
                    RegularExpr new_rep = new Epsilon();
                    return new_rep;
                }

                public RegularExpr visitEpsilon(RegularExpr e, Void v) {
                    RegularExpr new_rep = new Epsilon();
                    return new_rep;
                }

                public RegularExpr visitSymbol(RegularExpr e, Void v) {
                    RegularExpr new_rep = new Rep(e);
                    return new_rep;
                }

                public RegularExpr visitAlt(RegularExpr e, Void v) {
                    RegularExpr new_rep = new Rep(e);
                    return new_rep;
                }

                public RegularExpr visitSeq(RegularExpr e, Void v) {
                    RegularExpr new_rep = new Rep(e);
                    return new_rep;
                }

                public RegularExpr visitRep(RegularExpr e, Void v) {
                    RegularExpr new_rep = new Rep(e);
                    return new_rep;
                }
            };
            RegularExpr new_rep = pat.visit(null, const_rep_visitor);
            return new_rep;
        }
    }

    // anonymous function classes to avoid Original Boomerang/Boomerang SPDS from skipping functions due to syntax
    public static class RegexEmptyAnonymousFun implements Function<Pair<RegularExpr, RegularExpr>, RegularExpr> {
        public RegularExpr apply(Pair<RegularExpr, RegularExpr> re_pair) {
            RegularExpr re1 = re_pair.getFirst();
            RegularExpr re2 = re_pair.getSecond();
            RegexEmpty regexEmpty = new RegexEmpty();
            RegularExpr arg1 = regexEmpty.apply(re1);
            RegularExpr arg2 = regexEmpty.apply(re2);
            Pair<RegularExpr, RegularExpr> arg_pair = new Pair<>(arg1, arg2);
            ConstructSeq const_seq = new ConstructSeq();
            RegularExpr res = const_seq.apply(arg_pair);
            return res;
        }
    }


    public static class RegexEmpty implements Function<RegularExpr, RegularExpr> {
        public RegularExpr apply(RegularExpr re) {
            RegexVisitor<Void, RegularExpr> regex_empty_visitor = new RegexVisitor<Void, RegularExpr>() {

                public RegularExpr visitEpsilon(RegularExpr re, Void v) {
                    RegularExpr new_epsilon = new Epsilon();
                    return new_epsilon;
                }

                public RegularExpr visitEmptyset(RegularExpr re, Void v) {
                    RegularExpr new_emptyset = new Emptyset();
                    return new_emptyset;
                }

                public RegularExpr visitSymbol(RegularExpr re, Void v) {
                    RegularExpr new_emptyset = new Emptyset();
                    return new_emptyset;
                }

                public RegularExpr visitSeq(RegularExpr re, Void v) {
                    RegexEmptyAnonymousFun anon_fun = new RegexEmptyAnonymousFun();
                    RegularExpr res = matchSeq(re, anon_fun);
                    return res;
                }

                public RegularExpr visitAlt(RegularExpr re, Void v) {
                    RegexEmptyAnonymousFun anon_fun = new RegexEmptyAnonymousFun();
                    RegularExpr res = matchAlt(re, anon_fun);
                    return res;
                }

                public RegularExpr visitRep(RegularExpr re, Void v) {
                    RegularExpr new_epsilon = new Epsilon();
                    return new_epsilon;
                }
            };

            RegularExpr result = re.visit(null, regex_empty_visitor);
            return result;
        }
    }

    // anonymous functions for regex_derivative

    /* translation of
    	  (fun pat1 -> fun pat2 ->
	    (alt (seq (regex_derivative pat1 c) pat2)
		 (seq (regex_empty pat1) (regex_derivative pat2 c)))
     */
    public static class RegexDerivativeSeqFun implements Function<Pair<RegularExpr, RegularExpr>, RegularExpr> {
        RegularExpr c;

        RegexDerivativeSeqFun(RegularExpr arg) {
            this.c = arg;
        }

        public RegularExpr apply(Pair<RegularExpr, RegularExpr> re_pair) {
            RegularExpr pat1 = re_pair.getFirst();
            RegularExpr pat2 = re_pair.getSecond();
            ConstructAlt const_alt = new ConstructAlt();
            ConstructSeq const_seq = new ConstructSeq();
            RegexEmpty regex_empty = new RegexEmpty();
            RegexDerivative regex_deriv = new RegexDerivative();

            Pair<RegularExpr, RegularExpr> regex_deriv_1_arg = new Pair<>(pat1, c);
            RegularExpr seq1_first_arg = regex_deriv.apply(regex_deriv_1_arg);

            Pair<RegularExpr, RegularExpr> seq_1_arg = new Pair<>(seq1_first_arg, pat2);
            RegularExpr alt_arg_first = const_seq.apply(seq_1_arg);

            RegularExpr seq_2_first_arg = regex_empty.apply(pat1);
            Pair<RegularExpr, RegularExpr> regex_deriv_2_arg = new Pair<>(pat2, c);

            RegularExpr seq_2_second_arg = regex_deriv.apply(regex_deriv_2_arg);
            Pair<RegularExpr, RegularExpr> seq_2_arg = new Pair<>(seq_2_first_arg, seq_2_second_arg);

            RegularExpr alt_arg_second = const_seq.apply(seq_2_arg);
            Pair<RegularExpr, RegularExpr> alt_arg = new Pair<>(alt_arg_first, alt_arg_second);
            RegularExpr result = const_alt.apply(alt_arg);
            return result;
        }
    }

    /* translation of
       (fun pat1 -> fun pat2 ->
	      (alt (regex_derivative pat1 c) (regex_derivative pat2 c))

    */

    public static class RegexDerivativeAltFun implements Function<Pair<RegularExpr, RegularExpr>, RegularExpr> {
        RegularExpr c;

        RegexDerivativeAltFun(RegularExpr arg) {
            this.c = arg;
        }

        public RegularExpr apply(Pair<RegularExpr, RegularExpr> re_pair) {
            RegularExpr pat1 = re_pair.getFirst();
            RegularExpr pat2 = re_pair.getSecond();
            ConstructAlt const_alt = new ConstructAlt();
            RegexDerivative regex_deriv = new RegexDerivative();
            Pair<RegularExpr, RegularExpr> regex_deriv_1_arg = new Pair<>(pat1, c);
            RegularExpr alt_arg_first = regex_deriv.apply(regex_deriv_1_arg);
            Pair<RegularExpr, RegularExpr> regex_deriv_2_arg = new Pair<>(pat2, c);
            RegularExpr alt_arg_second = regex_deriv.apply(regex_deriv_2_arg);
            Pair<RegularExpr, RegularExpr> alt_arg = new Pair<>(alt_arg_first, alt_arg_second);
            RegularExpr result = const_alt.apply(alt_arg);
            return result;
        }
    }

    /* translation of
    (fun pat ->
		(seq (regex_derivative pat c) (rep pat))

     */
    public static class RegexDerivativeRepFun implements Function<RegularExpr, RegularExpr> {
        RegularExpr c;

        RegexDerivativeRepFun(RegularExpr arg) { this.c = arg; }

        public RegularExpr apply(RegularExpr pat) {
            ConstructSeq const_seq = new ConstructSeq();
            ConstructRep const_rep = new ConstructRep();
            RegexDerivative regex_deriv = new RegexDerivative();
            Pair<RegularExpr, RegularExpr> regex_deriv_arg = new Pair<>(pat, c);
            RegularExpr const_seq_first_arg = regex_deriv.apply(regex_deriv_arg);
            RegularExpr const_seq_second_arg = const_rep.apply(pat);
            Pair<RegularExpr, RegularExpr> const_seq_arg = new Pair<>(const_seq_first_arg, const_seq_second_arg);
            RegularExpr result = const_seq.apply(const_seq_arg);
            return result;
        }

    }

    public static class RegexDerivative implements Function<Pair<RegularExpr, RegularExpr>, RegularExpr> {

        public RegularExpr apply(Pair<RegularExpr, RegularExpr> re_c_pair) {
            RegularExpr re = re_c_pair.getFirst();
            RegularExpr c = re_c_pair.getSecond();

            if (c.equals(re)) {
                RegularExpr new_epsilon = new Epsilon();
                return new_epsilon;
            }
            else {
                RegexVisitor<Void, RegularExpr> regex_deriv_visitor = new RegexVisitor<Void, RegularExpr>() {
                    @Override
                    public RegularExpr visitEmptyset(RegularExpr e, Void aVoid) {
                        RegularExpr new_emptyset = new Emptyset();
                        return new_emptyset;
                    }

                    @Override
                    public RegularExpr visitEpsilon(RegularExpr e, Void aVoid) {
                        RegularExpr new_emptyset = new Emptyset();
                        return new_emptyset;
                    }

                    @Override
                    public RegularExpr visitSymbol(RegularExpr e, Void aVoid) {
                        RegularExpr new_emptyset = new Emptyset();
                        return new_emptyset;
                    }

                    @Override
                    public RegularExpr visitAlt(RegularExpr e, Void aVoid) {
                        RegexDerivativeAltFun alt_fun = new RegexDerivativeAltFun(c);
                        RegularExpr result = matchAlt(e, alt_fun);
                        return result;
                    }

                    @Override
                    public RegularExpr visitSeq(RegularExpr e, Void aVoid) {
                        RegexDerivativeSeqFun seq_fun = new RegexDerivativeSeqFun(c);
                        RegularExpr result = matchSeq(e, seq_fun);
                        return result;
                    }

                    @Override
                    public RegularExpr visitRep(RegularExpr e, Void aVoid) {
                        RegexDerivativeRepFun rep_fun = new RegexDerivativeRepFun(c);
                        RegularExpr result = matchRep(e, rep_fun);
                        return result;
                    }
                };

                 RegularExpr result = re.visit(null, regex_deriv_visitor);
                 return result;
            }
        }
    }

    public static Object regexMatch(RegularExpr pattern, LinkedList<Symbol> data) {
        if (data == null) {
            RegexEmpty regexEmpty = new RegexEmpty();
            RegularExpr pat_res = regexEmpty.apply(pattern);
            Boolean tru = new Boolean(true);
            Boolean fal = new Boolean(false);
            if (pat_res instanceof Epsilon) {
                return tru;
            }
            else {
                return fal;
            }
        }
        else {
            Symbol data_head = data.head;
            LinkedList<Symbol> data_tail = data.tail;
            RegexDerivative regex_deriv = new RegexDerivative();
            Pair<RegularExpr, RegularExpr> regex_deriv_arg = new Pair<>(pattern, data_head);
            RegularExpr regex_deriv_result = regex_deriv.apply(regex_deriv_arg);
            Object result = regexMatch(regex_deriv_result, data_tail);
            return result;
        }
    }

    public static <T, U> Object checkExpect(T check, U expect) {
        boolean res_bool = check.equals(expect);
        Boolean res = new Boolean(res_bool);
        return res;
    }


    // classes for Regex nodes
    public static abstract class RegularExpr {
        public abstract <P, R> R visit(P p, RegexVisitor<P, R> v);

        public abstract boolean equals(Object o);

        public abstract String toString();

    }

    // classes for each of the subclasses
    public static class Emptyset extends RegularExpr {

        @Override
        public <P, R> R visit(P p, RegexVisitor<P, R> v) {
            return v.visitEmptyset(this, p);
        }

        public boolean equals(Object o) {
            boolean isEmptySet = (o instanceof Emptyset);
            return isEmptySet;
        }

        public String toString() { return "emptyset";}
    }

    public static class Epsilon extends RegularExpr {

        @Override
        public <P, R> R visit(P p, RegexVisitor<P, R> v) {
            return v.visitEpsilon(this, p);
        }

        public boolean equals(Object o) {
            boolean isEpsilon = (o instanceof Epsilon);
            return isEpsilon;
        }

        public String toString() { return "epsilon";}
    }

    public static class Symbol extends RegularExpr {

        private Integer symbol;

        public Integer getSymbol() {
            return symbol;
        }

        public Symbol(Integer symbol) {
            this.symbol = symbol;
        }

        @Override
        public <P, R> R visit(P p, RegexVisitor<P, R> v) {
            return v.visitSymbol(this, p);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Symbol symbol1 = (Symbol) o;
            return Objects.equals(symbol, symbol1.symbol);
        }

        @Override
        public int hashCode() {
            return Objects.hash(symbol);
        }

        public String toString() { return "Symbol with value " + this.symbol.toString();}
    }

    public static class Alt extends RegularExpr {

        private RegularExpr left_expr;
        private RegularExpr right_expr;

        public Alt(RegularExpr r1, RegularExpr r2) {
            this.left_expr = r1;
            this.right_expr = r2;
        }

        public RegularExpr getLeft() {
            return this.left_expr;
        }

        public RegularExpr getRight() {
            return this.right_expr;
        }

        @Override
        public <P, R> R visit(P p, RegexVisitor<P, R> v) {
            return v.visitAlt(this, p);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Alt alt = (Alt) o;
            return Objects.equals(left_expr, alt.left_expr) &&
                    Objects.equals(right_expr, alt.right_expr);
        }

        @Override
        public int hashCode() {
            return Objects.hash(left_expr, right_expr);
        }

        public String toString() {
            String s1 = this.left_expr.toString();
            String s2 = this.right_expr.toString();
            String s = "Alt with subexpr " + s1 + ", " + s2;
            return s;
        }
    }

    public static class Seq extends RegularExpr {

        private RegularExpr left_expr;
        private RegularExpr right_expr;

        public Seq(RegularExpr r1, RegularExpr r2) {
            this.left_expr = r1;
            this.right_expr = r2;
        }

        public RegularExpr getLeft() {
            return this.left_expr;
        }

        public RegularExpr getRight() {
            return this.right_expr;
        }

        @Override
        public <P, R> R visit(P p, RegexVisitor<P, R> v) {
            return v.visitSeq(this, p);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Seq seq = (Seq) o;
            return Objects.equals(left_expr, seq.left_expr) &&
                    Objects.equals(right_expr, seq.right_expr);
        }

        @Override
        public int hashCode() {
            return Objects.hash(left_expr, right_expr);
        }

        public String toString() {
            String s1 = this.left_expr.toString();
            String s2 = this.right_expr.toString();
            String s = "Seq with subexpr " + s1 + ", " + s2;
            return s;
        }
    }

    public static class Rep extends RegularExpr {

        private RegularExpr inner_expr;

        public Rep(RegularExpr r) {
            this.inner_expr = r;
        }

        public RegularExpr getInnerExpr() {
            return this.inner_expr;
        }

        @Override
        public <P, R> R visit(P p, RegexVisitor<P, R> v) {
            return v.visitRep(this, p);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Rep rep = (Rep) o;
            return Objects.equals(inner_expr, rep.inner_expr);
        }

        @Override
        public int hashCode() {
            return Objects.hash(inner_expr);
        }

        public String toString() {
            String s1 = this.inner_expr.toString();
            String s = "Rep with subexpr " + s1 ;
            return s;
        }
    }

    // utilities
    public static class LinkedList <Abs_type> {
        Abs_type head;
        LinkedList<Abs_type> tail;
    }

    public static class Pair<T, U> {
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

    private static <T> void queryFor(T query) {

    }

}
