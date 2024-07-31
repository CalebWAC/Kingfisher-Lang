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

class Main {
	public static function main() {
var max  = 20;
		var systems = new SystemManager();
		systems.run();
}}
