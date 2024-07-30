typedef Vec3 = { x: Float, y: Float, z: Float}

typedef Component = {}

class EntityManager {
    static public var entities : Array<Array<Component>> = [];

    static public function addEntity(coms : Array<Component>) : Int {
        entities.push(coms);
        return entities.length - 1;
    }
}