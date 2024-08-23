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
var nums  = [for (i in 0...10 + 1) 
i];
var squared  = nums.map((function(num) { return num * num;
 }));
var largeSquares  = squared.filter((function(num) { return num > 50;
 }));
for (num in largeSquares) {
Standard.print('$num ');
}
		var systems = new SystemManager();
		systems.run();
}}
