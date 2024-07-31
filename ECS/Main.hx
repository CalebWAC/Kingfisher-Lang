import Entity; import System;

    typedef Vec3 = { x: Float, y: Float, z: Float}
    
    class Standard {
        public static function println(str: Any) { Sys.println(str); }
        public static function print(str: Any) { Sys.print(str); }
    }

class Transform extends Component {
	public var data : {position: Vec3, rotation: Vec3, scale: Vec3,};

public function new(d) { data = d; this.name = "Transform"; }
}

class Player extends Component {
	public var data : {name: String,};

public function new(d) { data = d; this.name = "Player"; }
}

class C107104 extends System {
public function new() {
	this.type = Update;
}

public function run() : Void {
	for (entity in EntityManager.entities) {
		var coms = hasAllComponents(entity, ["Transform", "Player", ]);
		if (coms.length == entity.length) {
cast(coms[0], Transform).data.position.x = cast(coms[0], Transform).data.position.x+1;
Standard.println(cast(coms[0], Transform).data.position.x);
}}}}
class Main {
	public static function main() {
EntityManager.addEntity([new Transform({position: {x: 0,y: 0,z: 0},rotation: {x: 0,y: 0,z: 0},scale: {x: 0,y: 0,z: 0}}),new Player({name: "player"})]);
		var systems = new SystemManager();
		systems.addSystem(new C107104());
		systems.run();
}}
