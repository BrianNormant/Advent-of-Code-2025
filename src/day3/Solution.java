public class Solution {

	Long asDecimalLong (int[] tab) {
		long n = 0;
		for (int i = tab.length-1; i >= 0; i--) {
			n += Math.pow(10, i) * tab[tab.length - i - 1];
		}
		return n;
	}

	public String solve(String input) {
		var lines = input.split("\\s");


		long total = 0;
		for (String line : lines) {
			int[] banks = new int[] {0, 0};
			for (char i : line.toCharArray()) {
				var n = Integer.parseInt("" + i);
				var c1 = new int[] {banks[1], n};
				var c2 = new int[] {banks[0], n};
				var b0 = asDecimalLong(banks);
				var b1 = asDecimalLong(c1);
				var b2 = asDecimalLong(c2);
				if (b1 > b2) {
					if (b1 > b0) banks = c1;
				} else {
					if (b2 > b0) banks = c2;
				}
			}
			System.out.println(asDecimalLong(banks));
			total += asDecimalLong(banks);
		}

		return "" + total;
	}
	public String solve2(String input) {
		return input;
	}
}
