enum Card {
King;
Queen;
Jack;
}

class Main {

    static function println(str: Any) { Sys.println(str); }
    static function print(str: Any) { Sys.print(str); }

	public static function main() {
var arr  = [1, 3, 3, 5, 23, -4, 0, 3, 1, 10];
for (i in 0...10) {
var e  = arr[i];
println(e);
}
var card  = Jack;
switch (card) {case King: println("It is a king");
case Queen: println("It is a queen");
case Jack: println("It is a jack");
}
}}
