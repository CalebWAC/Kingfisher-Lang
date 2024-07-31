import Entity;
import System;

typedef Vec3 = { x: Float, y: Float, z: Float}

class Transform extends Component { 
    public var data: { position: Vec3, rotation: Vec3, scale: Vec3};

    public function new(d) { name = "Transform"; data = { position: d.position, rotation: d.rotation, scale: d.scale }; }
}

class Move extends System {
    public function new() {
        this.type = Update;
    }

    public function run() : Void {
        for (entity in EntityManager.entities) {
            var coms = hasAllComponents(entity, ["Transform"]);
            if (coms.length == entity.length) {
                var coms = coms.map(function (c) { if (c.name == "Transform") return cast(c, Transform); return cast(c, Transform); });
                coms[0].data.position.x += 1;
                Sys.println(coms[0].data.position.x);
            }
        }
    }
}

class Main {
    static public function main() {
        var systems = new SystemManager();

        EntityManager.addEntity([new Transform({position: {x: 0, y: 0, z: 0},
                                                rotation: {x: 0, y: 0, z: 0}, 
                                                scale: {x: 1, y: 1, z: 1}})]);
        systems.addSystem(new Move());

        systems.run();
    }
}