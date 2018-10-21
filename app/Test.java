class Test {
	private static int LENGTH = 50;
	private static String S = "Hello world!";

	public static void main(String[] args) {
		System.out.println(S + LENGTH);
	}

	public int m(boolean b) {
		return b? 1: 0;
	}

	private static void m() {
		String a = "pepe";
	}
}