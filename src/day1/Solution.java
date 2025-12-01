import java.util.LinkedList;

public class Solution {
	public String solve(String input) {
		var list = new LinkedList<Integer>();
		list.add(50);
		var result = input.lines()
		.map(l -> {
			int sign = (l.charAt(0) == 'R')?1:-1;
			int num = Integer.parseInt(
				l.substring(1)
			);
			return num * sign;
		})
		.reduce(list,
		(dials, d) -> {
				var l = new LinkedList<Integer>(dials);
				var n = (dials.getLast() + d) % 100;
				if (n < 0) {n += 100;}
				l.add(n);
				return l;
			},
		(l1, l2) -> {
				var r = new LinkedList<>(l1);
				r.addAll(l2);
				return r;
			}
		).stream()
		.filter(n -> n == 0)
		.count();

		return "" + result;
	}
	public String solve2(String input) {
		return input;
	}
}
