/*
This file is derived from the example.Example class from the repository

    https://github.com/johspaeth/boomerang-artifact

As a consequence, this file is licensed according to the GPL 3.0 found at

    https://www.gnu.org/licenses/gpl-3.0.en.html
 */

package example;

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import boomerang.AliasFinder;
import boomerang.BoomerangContext;
import boomerang.cache.AliasResults;
import boomerang.preanalysis.PreparationTransformer;
import soot.G;
import soot.PackManager;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.Transform;
import soot.jimple.infoflow.solver.cfg.BackwardsInfoflowCFG;
import soot.jimple.infoflow.solver.cfg.InfoflowCFG;
import soot.options.Options;

/**
 * This class, which is derived from example.Example in the original Boomerang
 * reference implementation (
 * <url>https://github.com/johspaeth/boomerang-artifact</url>), performs an
 * analysis of the class of the provided name using the original Boomerang
 * framework.
 */
public class BoomerangOriginalBenchmarkMain {
	public static void main(String[] args) {
		String className = args[0];
		List<String> queryVars = new ArrayList<>(Arrays.asList(args));
		queryVars.remove(0);

		// Set up Boomerang structures
		initializeSoot(className);
		InfoflowCFG icfg = new InfoflowCFG();
		BoomerangContext context = new BoomerangContext(icfg, new BackwardsInfoflowCFG(icfg));
		context.budgetInMilliSeconds = Long.MAX_VALUE;
		AliasFinder af = new AliasFinder(context);
		List<Query> queries = new ArrayList<>();
		for (String queryVar : queryVars) {
			String methodSpec = "<" + className + ": void main(java.lang.String[])>";
			// Determine the number of units to reach the end of main
			int units = Integer.MAX_VALUE;
			try {
				new Query(methodSpec, queryVar, units);
				throw new IllegalStateException("Expected exception for huge unit number!");
			} catch (RuntimeException e) {
				String msg = e.getMessage();
				int endindex = msg.length() - 1;
				while (msg.charAt(endindex) < '0' || msg.charAt(endindex) > '9') {
					endindex--;
				}
				endindex++;
				int startindex = endindex - 1;
				while (msg.charAt(startindex) >= '0' && msg.charAt(startindex) <= '9') {
					startindex--;
				}
				startindex++;
				units = Integer.valueOf(msg.substring(startindex, endindex));
			}
			queries.add(new Query(methodSpec, queryVar, units - 1));
		}

		// Time and perform queries
		try {
			long startTime = System.currentTimeMillis();
			for (Query query : queries) {
				AliasResults aliases = af.findAliasAtStmt(query.getAccessGraph(), query.getStatement());
				aliases.withMethodOfAllocationSite(icfg);
			}
			long stopTime = System.currentTimeMillis();
			System.out.println("Analysis took " + (stopTime - startTime) + " ms");
            long collectionTime = 0;
            for (GarbageCollectorMXBean garbageCollectorMXBean : ManagementFactory.getGarbageCollectorMXBeans()) {
                collectionTime += garbageCollectorMXBean.getCollectionTime();
            }
            System.out.println("GC took " + collectionTime + " ms");
        } catch (RuntimeException e) {
			// Exiting with zero exit code but still logging the exception.
			e.printStackTrace();
		}
	}

	@SuppressWarnings("static-access")
	private static void initializeSoot(String mainClass) {
		G.v().reset();
		Options.v().set_whole_program(true);
		Options.v().setPhaseOption("jb", "use-original-names:true");
		Options.v().setPhaseOption("cg.spark", "on");

		String userdir = System.getProperty("user.dir");
		String sootCp = userdir + "/targets";
		Options.v().set_soot_classpath(sootCp);

		Options.v().set_prepend_classpath(true);
		Options.v().set_no_bodies_for_excluded(true);
		Options.v().set_allow_phantom_refs(true);
		Options.v().set_main_class(mainClass);

		Scene.v().addBasicClass(mainClass, SootClass.BODIES);
		Scene.v().loadNecessaryClasses();
		SootClass c = Scene.v().forceResolve(mainClass, SootClass.BODIES);
		if (c != null) {
			c.setApplicationClass();
		}
		SootMethod methodByName = c.getMethodByName("main");
		List<SootMethod> ePoints = new LinkedList<>();
		ePoints.add(methodByName);
		Scene.v().setEntryPoints(ePoints);
		// Add a transformer
		PackManager.v().getPack("wjtp").add(new Transform("wjtp.preparationTransform", new PreparationTransformer()));
		PackManager.v().getPack("cg").apply();
		PackManager.v().getPack("wjtp").apply();
	}

}
