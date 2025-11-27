package org.example;

import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
	public static final String INPUT_RELATIVE_PATH = "./input.txt";

	static String input = "input.txt";

	public static void main(String[] args) {
		if (args.length > 0) {
			var path = Paths.get(INPUT_RELATIVE_PATH);
			try {
				input = Files.readString(path);
			} catch (Exception e) {
				System.err.println("Could not read input file, check if input.txt was added");
				System.exit(1);
			}
		}

		Solution solver = new Solution();
		String sol = solver.solve(input);
		System.out.print(sol);
		System.exit(0);
	}

    public String getGreeting() {
        return "Hello World!";
    }
}

