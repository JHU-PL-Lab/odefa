package boomerang.example;

import boomerang.example.TestUtils.*;
import java.util.function.Function;

public class Deriv {

    public static void main(String[] args) {

        LinkedList<Expr> timesExprLst1 = new LinkedList<>();
        timesExprLst1.head = new IntLiteralExpr(3);
        timesExprLst1.tail = new LinkedList<>();
        timesExprLst1.tail.head = new VarLiteralExpr("x");
        timesExprLst1.tail.tail = new LinkedList<>();
        timesExprLst1.tail.tail.head = new VarLiteralExpr("x");
        TimesExpr timesExpr1 = new TimesExpr(timesExprLst1);
        LinkedList<Expr> timesExprLst2 = new LinkedList<>();
        timesExprLst2.head = new VarLiteralExpr("a");
        timesExprLst2.tail = new LinkedList<>();
        timesExprLst2.tail.head = new VarLiteralExpr("x");
        timesExprLst2.tail.tail = new LinkedList<>();
        timesExprLst2.tail.tail.head = new VarLiteralExpr("x");
        TimesExpr timesExpr2 = new TimesExpr(timesExprLst2);
        LinkedList<Expr> timesExprLst3 = new LinkedList<>();
        timesExprLst3.head = new VarLiteralExpr("b");
        timesExprLst3.tail = new LinkedList<>();
        timesExprLst3.tail.head = new VarLiteralExpr("x");
        TimesExpr timesExpr3 = new TimesExpr(timesExprLst3);
        LinkedList<Expr> plusLinkedList = new LinkedList<>();
        plusLinkedList.head = timesExpr1;
        plusLinkedList.tail = new LinkedList<>();
        plusLinkedList.tail.head = timesExpr2;
        plusLinkedList.tail.tail = new LinkedList<>();
        plusLinkedList.tail.tail.head = timesExpr3;
        plusLinkedList.tail.tail.tail = new LinkedList<>();
        plusLinkedList.tail.tail.tail.head = new IntLiteralExpr(5);
        PlusExpr plusExpr = new PlusExpr(plusLinkedList);

        Derive derive = new Derive();
        ListMap listMap = new ListMap();
        derive.listMap = listMap;

        Expr result = plusExpr.visit(null, derive);
        System.out.println(result);

        queryFor(timesExprLst1);
        queryFor(timesExpr1);
        queryFor(timesExprLst2);
        queryFor(timesExpr2);
        queryFor(timesExprLst3);
        queryFor(timesExpr3);
        queryFor(plusLinkedList);
        queryFor(plusExpr);
        queryFor(derive);
        queryFor(listMap);
        queryFor(result);
    }

    public static class ListMap {

        public <T, U> LinkedList<T> apply(Function<U, T> mapFun, LinkedList<U> lst) {

            if (lst == null) {
                return null;
            }

            U currHead = lst.head;
            T hd = mapFun.apply(currHead);
            LinkedList<T> tl = this.apply(mapFun, lst.tail);
            LinkedList<T> result = new LinkedList<>();
            result.head = hd;
            result.tail = tl;
            return result;
        }

    }

    public interface ExprVisitor<P, R> {
        public R visitIntLiteral(IntLiteralExpr e, P p);
        public R visitVarLiteral(VarLiteralExpr e, P p);
        public R visitPlus(PlusExpr e, P p);
        public R visitMinus(MinusExpr e, P p);
        public R visitTimes(TimesExpr e, P p);
        public R visitDivides(DividesExpr e, P p);
    }

    public static class Derive implements ExprVisitor<Void, Expr> {

        public ListMap listMap;

        public Expr visitIntLiteral(IntLiteralExpr e, Void v) {
            return new IntLiteralExpr(0);
        }

        public Expr visitVarLiteral(VarLiteralExpr e, Void v) {
            if (e.getValue().equals("x")) {
                return new IntLiteralExpr(1);
            }
            return new IntLiteralExpr(0);
        }

        public Expr visitPlus(PlusExpr e, Void v) {
            LinkedList<Expr> exprs = e.getExprs();
            Function<Expr, Expr> visitMapper = new Function<Expr, Expr>() {
                public Expr apply(Expr e) {
                    return e.visit(null, Derive.this);
                }
            };
            LinkedList<Expr> newExprs = listMap.apply(visitMapper, exprs);
            return new PlusExpr(newExprs);
        }

        public Expr visitMinus(MinusExpr e, Void v) {
            LinkedList<Expr> exprs = e.getExprs();
            Function<Expr, Expr> visitMapper = new Function<Expr, Expr>() {
                public Expr apply(Expr e) {
                    return e.visit(null, Derive.this);
                }
            };
            LinkedList<Expr> newExprs = listMap.apply(visitMapper, exprs);
            return new MinusExpr(newExprs);
        }

        public Expr visitTimes(TimesExpr e, Void v) {
            LinkedList<Expr> exprs = e.getExprs();
            Function<Expr, Expr> visitMapper = new Function<Expr, Expr>() {
                public Expr apply(Expr e) {
                    return e.visit(null, Derive.this);
                }
            };
            Function<Expr, Expr> timesMapper = new Function<Expr, Expr>() {
                public Expr apply(Expr e) {
                    Expr derivdExpr = visitMapper.apply(e);
                    LinkedList<Expr> res = new LinkedList<>();
                    res.head = derivdExpr;
                    res.tail = new LinkedList<>();
                    res.tail.head = e;
                    return new DividesExpr(res);
                }
            };
            LinkedList<Expr> newExprs = listMap.apply(timesMapper, exprs);
            PlusExpr newPlusExpr = new PlusExpr(newExprs);
            LinkedList<Expr> res = new LinkedList<>();
            res.head = e;
            res.tail = new LinkedList<>();
            res.tail.head = newPlusExpr;
            return new TimesExpr(res);
        }

        public Expr visitDivides(DividesExpr e, Void v) {
            LinkedList<Expr> exprs = e.getExprs();
            Expr hd1 = exprs.head;
            Expr hd2 = exprs.tail.head;
            LinkedList<Expr> dividesLst = new LinkedList<>();
            dividesLst.head = hd1.visit(null, Derive.this);
            dividesLst.tail = new LinkedList<>();
            dividesLst.tail.head = hd2;
            Expr expr1 = new DividesExpr(dividesLst);
            LinkedList<Expr> timesLst = new LinkedList<>();
            timesLst.head = hd2;
            timesLst.tail = new LinkedList<>();
            timesLst.tail.head = hd2;
            timesLst.tail.tail = new LinkedList<>();
            timesLst.tail.tail.head = hd2.visit(null, Derive.this);
            Expr expr2 = new TimesExpr(timesLst);
            LinkedList<Expr> dividesLst2 = new LinkedList<>();
            dividesLst2.head = hd1;
            dividesLst2.tail = new LinkedList<>();
            dividesLst2.tail.head = expr2;
            Expr expr3 = new DividesExpr(dividesLst2);
            LinkedList<Expr> minusLst = new LinkedList<>();
            minusLst.head = expr1;
            minusLst.tail = new LinkedList<>();
            minusLst.tail.head = expr3;
            return new MinusExpr(minusLst);
        }

    }

    private static abstract class Expr {

        public abstract <P, R> R visit(P p, ExprVisitor<P, R> v);

    }

    public static class IntLiteralExpr extends Expr {

        private Integer value;

        public Integer getValue() {
            return value;
        }

        public IntLiteralExpr(Integer value) {
            this.value = value;
        }

        @Override
        public <P, R> R visit(P p, ExprVisitor<P, R> v) {
            return v.visitIntLiteral(this, p);
        }

    }

    public static class VarLiteralExpr extends Expr {

        private String value;

        public String getValue() {
            return value;
        }

        public VarLiteralExpr(String value) {
            this.value = value;
        }

        @Override
        public <P, R> R visit(P p, ExprVisitor<P, R> v) {
            return v.visitVarLiteral(this, p);
        }

    }

    public static class PlusExpr extends Expr {

        private LinkedList<Expr> exprs;

        public PlusExpr(LinkedList<Expr> es) {
            this.exprs = es;
        }

        public LinkedList<Expr> getExprs() {
            return exprs;
        }

        @Override
        public <P, R> R visit(P p, ExprVisitor<P, R> v) {
            return v.visitPlus(this, p);
        }

    }

    public static class MinusExpr extends Expr {

        private LinkedList<Expr> exprs;

        public MinusExpr(LinkedList<Expr> es) {
            this.exprs = es;
        }

        public LinkedList<Expr> getExprs() {
            return exprs;
        }

        @Override
        public <P, R> R visit(P p, ExprVisitor<P, R> v) {
            return v.visitMinus(this, p);
        }

    }

    public static class TimesExpr extends Expr {

        private LinkedList<Expr> exprs;

        public TimesExpr(LinkedList<Expr> es) {
            this.exprs = es;
        }

        public LinkedList<Expr> getExprs() {
            return exprs;
        }

        @Override
        public <P, R> R visit(P p, ExprVisitor<P, R> v) {
            return v.visitTimes(this, p);
        }

    }

    public static class DividesExpr extends Expr {

        private LinkedList<Expr> exprs;

        public DividesExpr(LinkedList<Expr> es) {
            this.exprs = es;
        }

        public LinkedList<Expr> getExprs() {
            return exprs;
        }


        @Override
        public <P, R> R visit(P p, ExprVisitor<P, R> v) {
            return v.visitDivides(this, p);
        }

    }

    private static <T> void queryFor(T query) {

    }
}
