



fn main() {
	let mut i = 0;
	while i<10
		{
		println!("{}", fib(i));
		i=i+1;
		};
	let mut res = fib(39);
	println!("{}", res);
}
fn fib(mut x: i32) -> i32 {
	if x==0||x==1
		{
		return x;
		};
	return fib(x-1)+fib(x-2);
}
