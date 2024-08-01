import Entity; import System;

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
var map  = ['H' => 1, 'He' => 2, 'Li' => 3];
		var systems = new SystemManager();
		systems.run();
}}
