import java.util.LinkedList;
import java.util.List;

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
	
	class Pair<T,U> {
		T fst;
		U snd;
		public T getFst() {
			return fst;
		}
		public U getSnd() {
			return snd;
		}

		public Pair(T fst, U snd) {
			this.fst = fst;
			this.snd = snd;
		}

		@Override
		public String toString() {
			return String.format("(%s, %s)",
				fst.toString(),
				snd.toString()
			);
		}
	}

	Pair<Integer,Integer> maximum(
		List<Integer> list,
		int minIdx,
		int maxIdx) {
		int max = 0;
		int maxI = -1;
		int idx = 0;
		var it = list.iterator();
		while (it.hasNext() && idx < maxIdx) {
			int n = it.next();
			if (idx >= minIdx && n > max) {
				max = n;
				maxI = idx;
			}
			idx++;
		}
		return new Pair<>(max, maxI);
	}

	long asDecimalLong(List<Integer> list) {
		long n = 0;
		for (int i = list.size()-1; i >= 0; i--) {
			n += Math.pow(10, i)
				* list.get(list.size() - i - 1);
		}
		return n;
	}

	public String solve2(String input) {
		final var banksize = 12;
		var lines = input.split("\\s");
		long total = 0;
		for (String line : lines) {
			LinkedList<Integer> banks = new LinkedList<>();
			LinkedList<Integer> digits = new LinkedList<>();

			for (char c: line.toCharArray()) {
				digits.add(Integer.parseInt("" + c));
			}

			// System.out.println(maximum(
			// 	List.of(12, 9,8,7,6,5),
			// 	0, 5));
			int lastIdx = 0;
			while (banks.size() < 12) {
				System.out.println(
					"max(" + lastIdx + ", " + (digits.size() - 12 + banks.size()) + ")"
				);
				var p = maximum(
					digits,
					lastIdx,
					digits.size() - 11 + banks.size()
				);
				banks.add(p.getFst());
				lastIdx = p.getSnd() + 1;
			}
			var n = asDecimalLong(banks);
			System.out.println(n);
			total += n;

		}
		return total + "";
	}
}
