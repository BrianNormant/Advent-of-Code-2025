import java.util.regex.Pattern;

public class Solution {
	public String solve(String input) {
		final String r1 = "^(\\d+)\\1$";
		final String r2 = "(\\d+)-(\\d+)";
		var p1 = Pattern.compile( r1 );
		var p2 = Pattern.compile(r2);

		long result = 0;

		while (!input.equals("")) {
			var idx = input.indexOf(",");
			String sub;
			if (idx != -1) {
				sub = input.substring(0, idx);
				input = input.substring(idx+1);
			} else {
				sub = input;
				input = "";
			}

			var m = p2.matcher(sub);
			m.find();
			long n1 = Long.parseLong(m.group(1)),
				 n2 = Long.parseLong(m.group(2));

			for (long i = n1; i <= n2; i++) {
				var m1 = p1.matcher("" + i);
				m1.find();
				if (m1.hasMatch()) {
					result += i;
				}
			}
		}

		return result + "";
	}
	public String solve2(String input) {
		final String r1 = "^(\\d+)\\1+$";
		final String r2 = "(\\d+)-(\\d+)";
		var p1 = Pattern.compile( r1 );
		var p2 = Pattern.compile(r2);

		long result = 0;

		while (!input.equals("")) {
			var idx = input.indexOf(",");
			String sub;
			if (idx != -1) {
				sub = input.substring(0, idx);
				input = input.substring(idx+1);
			} else {
				sub = input;
				input = "";
			}

			var m = p2.matcher(sub);
			m.find();
			long n1 = Long.parseLong(m.group(1)),
				 n2 = Long.parseLong(m.group(2));

			for (long i = n1; i <= n2; i++) {
				var m1 = p1.matcher("" + i);
				m1.find();
				if (m1.hasMatch()) {
					result += i;
				}
			}
		}
		return result + "";
	}
}
