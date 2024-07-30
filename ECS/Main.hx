import Entity; import System;
typedef Vec3 = { x: Float, y: Float, z: Float}
class Transform extends Component {
	public var data : {position: (Custom "Vec3", ), rotation: (Custom "Vec3", ), scale: (Custom "Vec3", ),};
}

class Player extends Component {
	public var data : {name: (String, ),};
}

class Main {

    static function println(str: Any) { Sys.println(str); }
    static function print(str: Any) { Sys.print(str); }

	public static function main() {
EntityManager.addEntity([new Transform({position: {x: 0,y: 0,z: 0},rotation: {x: 0,y: 0,z: 0},scale: {x: 0,y: 0,z: 0}}),new Player()]);
}}
