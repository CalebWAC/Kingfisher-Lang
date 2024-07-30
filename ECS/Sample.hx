/* import Entity;
import System;

class Transform extends Component { 
    public var data: { position: Vec3, rotation: Vec3, scale: Vec3};

    public function new(d) { data = { position: d.position, rotation: d.rotation, scale: d.scale }; }
}

class Move extends System {
    public function new() {
        this.type = Update;
    }

    public function run() : Void {
        for (entity in EntityManager.entities) {
            for (com in entity) {
                if (Std.isOfType(com, Transform)) {
                    cast(com, Transform).data.position.x += 1;
                    Sys.println(cast(com, Transform).data.position.x);
                }
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
} */