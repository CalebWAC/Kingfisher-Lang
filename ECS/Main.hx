import Entity; import System; import Set;

    enum Option<T> {
        Some(v:T);
        None;
    }

    typedef Vec3 = { x: Float, y: Float, z: Float}
    
    class Standard {
        public static function println(str: Any) { Sys.println(str); }
        public static function print(str: Any) { Sys.print(str); }
    }
    
    class StepIterator {
      var end:Int;
      var step:Int;
      var index:Int;

      public inline function new(start:Int, step:Int, end:Int) {
        this.index = start;
        this.end = end;
        this.step = step;
      }

      public inline function hasNext() return index < end;
      public inline function next() return (index += step) - step;
    }

class Main {
	public static function main() {
var arr  = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
Standard.println(arr);
var evens  = arr.filter((function(num) { return num % 2 == 0;
 }));
Standard.println(evens);
var squared  = evens.map((function(num) { return num * num;
 }));
Standard.println(squared);
for (num in arr) {
for (even in evens) {
if (num == even || num == 5) {
Standard.println(num);
}
}
}
		var systems = new SystemManager();
		systems.run();
}}
