/*
This file is derived from the BoomerangExampleTarget class from the repository

    https://github.com/secure-software-engineering/SPDS-experiments/tree/popl-aec

As a consequence, this file is licensed according to the EPL 2.0 found at

    https://www.eclipse.org/legal/epl-2.0/
 */
package boomerang.example;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import boomerang.BackwardQuery;
import boomerang.Boomerang;
import boomerang.DefaultBoomerangOptions;
import boomerang.jimple.Statement;
import boomerang.jimple.Val;
import boomerang.preanalysis.BoomerangPretransformer;
import boomerang.results.BackwardBoomerangResults;
import boomerang.seedfactory.SeedFactory;
import soot.G;
import soot.MethodOrMethodContext;
import soot.PackManager;
import soot.Scene;
import soot.SceneTransformer;
import soot.SootClass;
import soot.SootMethod;
import soot.Transform;
import soot.Transformer;
import soot.Unit;
import soot.Value;
import soot.jimple.Stmt;
import soot.jimple.toolkits.callgraph.ReachableMethods;
import soot.jimple.toolkits.ide.icfg.BiDiInterproceduralCFG;
import soot.jimple.toolkits.ide.icfg.JimpleBasedInterproceduralCFG;
import soot.options.Options;
import soot.util.queue.QueueReader;
import wpds.impl.Weight;

/**
 * This class, which is derived from the BoomerangExampleTarget in the Boomerang SPDS POPL AEC artifact
 * (<url>https://github.com/secure-software-engineering/SPDS-experiments/tree/popl-aec</url>), performs an analysis of
 * the class of the provided name using the Boomerang SPDS framework.
 */
public class BoomerangSPDSBenchmarkMain {
    public static void main(String... args) {
        String sootClassPath = getSootClassPath();
        String mainClass = args[0];
        setupSoot(sootClassPath, mainClass);
        analyze();
    }

    private static String getSootClassPath() {
        // Assume target folder to be directly in user dir; this should work in eclipse
        String sootClassPath = System.getProperty("user.dir") + File.separator + "target" + File.separator + "classes";
        File classPathDir = new File(sootClassPath);
        if (!classPathDir.exists()) {
            // We haven't found our target folder
            // Check if if it is in the boomerangPDS in user dir; this should work in IntelliJ
            sootClassPath = System.getProperty("user.dir") + File.separator + "boomerangPDS" + File.separator + "target"
                    + File.separator + "classes";
            classPathDir = new File(sootClassPath);
            if (!classPathDir.exists()) {
                // We haven't found our bytecode anyway, notify now instead of starting analysis anyway
                throw new RuntimeException("Classpath could not be found.");
            }
        }
        return sootClassPath;
    }

    private static void setupSoot(String sootClassPath, String mainClass) {
        G.v().reset();
        Options.v().set_whole_program(true);
        Options.v().setPhaseOption("cg.spark", "on");
        Options.v().set_output_format(Options.output_format_none);
        Options.v().set_no_bodies_for_excluded(true);
        Options.v().set_allow_phantom_refs(true);

        List<String> includeList = new LinkedList<String>();
        includeList.add("java.lang.*");
        includeList.add("java.util.*");
        includeList.add("java.io.*");
        includeList.add("sun.misc.*");
        includeList.add("java.net.*");
        includeList.add("javax.servlet.*");
        includeList.add("javax.crypto.*");

        Options.v().set_include(includeList);
        Options.v().setPhaseOption("jb", "use-original-names:true");

        Options.v().set_soot_classpath(sootClassPath);
        Options.v().set_prepend_classpath(true);
        // Options.v().set_main_class(this.getTargetClass());
        Scene.v().loadNecessaryClasses();
        SootClass c = Scene.v().forceResolve(mainClass, SootClass.BODIES);
        if (c != null) {
            c.setApplicationClass();
        }
    }

    private static void analyze() {
        Transform transform = new Transform("wjtp.ifds", createAnalysisTransformer());
        PackManager.v().getPack("wjtp").add(transform);
        PackManager.v().getPack("cg").apply();
        BoomerangPretransformer.v().apply();
        PackManager.v().getPack("wjtp").apply();
    }

    private static Transformer createAnalysisTransformer() {
        return new SceneTransformer() {
            protected void internalTransform(String phaseName, @SuppressWarnings("rawtypes") Map options) {
                final JimpleBasedInterproceduralCFG icfg = new JimpleBasedInterproceduralCFG(true);

                //1. Create a Boomerang solver.
                Boomerang solver = new Boomerang(new DefaultBoomerangOptions(){
                    public boolean onTheFlyCallGraph() {
                        //Must be turned of if no SeedFactory is specified.
                        return false;
                    }

                    @Override
                    public int analysisTimeoutMS() {
                        return Integer.MAX_VALUE;
                    }
                }) {
                    @Override
                    public BiDiInterproceduralCFG<Unit, SootMethod> icfg() {
                        return icfg;
                    }

                    @Override
                    public SeedFactory<Weight.NoWeight> getSeedFactory() {
                        return null;
                    }
                };

                // 2. Construct the queries
                List<BackwardQuery> queries = createQueries();

                // 3. Get solutions for each query and time the process.
                long startTime = System.currentTimeMillis();
                for (BackwardQuery query : queries) {
                    BackwardBoomerangResults<Weight.NoWeight> backwardQueryResults = solver.solve(query);
                    backwardQueryResults.getAllocationSites();
                }
                long stopTime = System.currentTimeMillis();
                System.out.println("Total analysis time: " + (stopTime - startTime) + " ms");
            }

            private List<BackwardQuery> createQueries() {
                ReachableMethods reachableMethods = Scene.v().getReachableMethods();
                QueueReader<MethodOrMethodContext> l = reachableMethods.listener();
                List<BackwardQuery> results = new ArrayList<>();
                while (l.hasNext()) {
                    MethodOrMethodContext next = l.next();
                    BackwardQuery q = getQuery(next.method());
                    if (q != null) {
                        results.add(q);
                    }
                }
                return results;
            }

            private BackwardQuery getQuery(SootMethod method) {
                if (!method.hasActiveBody()) {
                    return null;
                }
                for (Unit u : method.getActiveBody().getUnits()) {
                    if (!(u instanceof Stmt)) {
                        continue;
                    }
                    Stmt s = (Stmt) u;
                    if (!s.containsInvokeExpr()) {
                        continue;
                    }
                    if (s.toString().contains("queryFor")) {
                        Value arg = s.getInvokeExpr().getArg(0);
                        return new BackwardQuery(new Statement(s, method), new Val(arg, method));
                    }
                }
                return null;
            }
        };
    }
}
