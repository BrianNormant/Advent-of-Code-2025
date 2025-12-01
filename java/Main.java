import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
	public static final String INPUT_RELATIVE_PATH = "./input.txt";
	public static final String EXAMPLE_RELATIVE_PATH = "./example.txt";
	public static final String SOLUTION_RELATIVE_PATH = "./solution.txt";

	static String input = null;
	static String solution = null;


	public static void main(String[] args) {
		boolean validate = args.length > 0;
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
			var patj = Paths.get(SOLUTION_RELATIVE_PATH);
			try {
				input = Files.readString(path);
				solution = Files.readString(patj);
			} catch (Exception ignored) {
				System.err.println("Could not read ./example.txt ./solution.txt");
				System.exit(1);
			}
		}

		Solution solver = new Solution();
		String sol = solver.solve(input) + "\n";
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
