import Entity;
import System;

typedef Transform = Component & { position: Vec3, rotation: Vec3, scale: Vec3 }

class Move extends System {
    public function new() {
        this.type = Start;
    }

    public function run() : Void {
        for (entity in EntityManager.entities) {
            for (com in entity) {
                Sys.println(com);
            }
        }
    }
}

class Main {
    static public function main() {
        var systems = new SystemManager();

        EntityManager.addEntity([{ x: 0, y: 0, z: 0}]);
        systems.addSystem(new Move());
    }
}