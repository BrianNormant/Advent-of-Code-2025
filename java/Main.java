import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
	public static final String INPUT_RELATIVE_PATH = "./input.txt";
	public static final String EXAMPLE_RELATIVE_PATH = "./example.txt";
	public static final String SOLUTION_RELATIVE_PATH = "./solution";

	static String input = null;
	static String solution = null;


	/**
	 * Main method
	 * *-v* flag will use the input
	 * *-2* will do the second part of the day
	 */
	public static void main(String[] args) {
		boolean validate = false, p2 = false;

		for (String arg: args) {
			if (arg.equals("-v")) validate = true;
			if (arg.equals("-2")) p2 = true;
		}

		if (validate) {
			System.out.println("Validate mode");
			var path = Paths.get(INPUT_RELATIVE_PATH);
			try {
				input = Files.readString(path);
			} catch (Exception ignored) {
				System.err.println("Could not read ./input.txt");
				System.exit(1);
			}
		} else {
			System.out.println("Example mode");
			var path = Paths.get(EXAMPLE_RELATIVE_PATH);
			var patj = Paths.get(SOLUTION_RELATIVE_PATH + (p2?"2":"") + ".txt" );
			try {
				input = Files.readString(path);
				solution = Files.readString(patj);
			} catch (Exception ignored) {
				System.err.println("Could not read ./example.txt ./solution.txt");
				System.exit(1);
			}
		}

		Solution solver = new Solution();
		String sol;
		if (p2) {
			sol = solver.solve2(input);
		} else {
			sol = solver.solve(input);
		}
		sol += "\n";
		System.out.print(sol);
		if (!validate) {
			if (sol.equals(solution)) {
				System.out.println("Solution is correct");
			} else {
				System.out.println("Solution is incorrect");
				System.out.println("Expected: " + solution);
			}
		}
		System.exit(0);
	}
}
