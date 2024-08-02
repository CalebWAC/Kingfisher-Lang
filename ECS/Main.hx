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
var arr  = [for (i in 0...10 + 1) 
i];
Standard.println(arr);
var miniArr  = [-2, 5, 9];
switch (miniArr) {case [1, 2, 3]: Standard.println('Simple counting');
case [2, 4, 6]: Standard.println('Counting by twos');
case [7, 8, 9]: Standard.println('No longer possible');
case [a, b, c] if (a < b && b < c): Standard.println('Its ascending');
case [_, _, _, ]: Standard.println('Out of order');
}
		var systems = new SystemManager();
		systems.run();
}}
