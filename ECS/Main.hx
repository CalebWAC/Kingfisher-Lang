import Entity; import System;
typedef Vec3 = { x: Float, y: Float, z: Float}
class Transform extends Component {
	public var data : {position: Vec3, rotation: Vec3, scale: Vec3,};

public function new(d) { data = d; }
}

class Player extends Component {
	public var data : {name: String,};

public function new(d) { data = d; }
}

class R121117 extends System {
public function new() {
	this.type = Update;
}

public function run() : Void {
	for (entity in EntityManager.entities) {
		var coms = hasAllComponents(entity, ["Transform", "Player", ]);
		if (coms.length == entity.length) {
			var coms = coms.map(function (c) { if (c.name == "Transform") return cast(c, Transform); if (c.name == "Player") return cast(c, Player); return cast(c, Transform); });
coms[0].data.position = coms[0].data.position + {x: 0,y: 1,z: 0};
}}}}
class Main {

    static function println(str: Any) { Sys.println(str); }
    static function print(str: Any) { Sys.print(str); }

	public static function main() {
EntityManager.addEntity([new Transform({position: {x: 0,y: 0,z: 0},rotation: {x: 0,y: 0,z: 0},scale: {x: 0,y: 0,z: 0}}),new Player({name: "player"})]);
}}
