import java.util.LinkedList;

public class Solution {
	class Tuple<T1, T2> {
		public final T1 _1;
		public final T2 _2;
		public Tuple(T1 _1, T2 _2) {
			this._1 = _1;
			this._2 = _2;
		}
		public String toString() {
			return "(" + _1 + ", " + _2 + ")";
		}
		public T1 get_1() {
			return _1;
		}
		public T2 get_2() {
			return _2;
		}
	}
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
				var n = Math.floorMod((dials.getLast() + d), 100);
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
		// tuple of current dial position and the total passage on zero
		Tuple<Integer, Integer> tuple = new Tuple<>(50, 0);

		var result = input
			.lines()
			.map(l -> {
				int sign = (l.charAt(0) == 'R')?1:-1;
				int num = Integer.parseInt(
					l.substring(1)
				);
				return num * sign;
			})
			.reduce(tuple,
			(t, m) -> {
				var tmp = t.get_1() + m;
				var cnt = Math.abs(Math.floorDiv(tmp, 100));
				var dial = Math.floorMod(tmp, 100);
				if (dial == 0 && m < 0) cnt++;
				if (t.get_1() == 0 && m < 0) cnt = (cnt>0)?cnt-1:0;
				var r = new Tuple<>(dial, t.get_2() + cnt);
				System.out.println(r);
				return r;
			},
			(t1, t2) -> new Tuple<>(t2.get_1(), t1.get_2() + t2.get_2())
			);
		return "" + result.get_2();
	}
}
