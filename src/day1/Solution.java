import java.util.LinkedList;

public class Solution {
	public String solve(String input) {
		var list = new LinkedList<Integer>();
		list.add(50);
		var r = input.lines()
			.map(l -> {
				int sign = (l.charAt(0) == 'R')?1:-1;
				int num = Integer.parseInt(
					l.substring(1)
				);
				return num * sign;
			})
			.reduce(list,
				(dials, d) -> {
					var l = new LinkedList<Integer>();
					l.add(dials.getLast() + d);
					return l;
				},
				(l1, l2) -> {
					l1.addAll(l2);
					return l1;
				}
			);

		return r.toString();
	}
}
