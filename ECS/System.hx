enum SystemType {
    Awake;
    Start;
    Update;
    End;
}

abstract class System {
    public var type : SystemType;

    abstract public function run() : Void;
}

class SystemManager {
    var systems: Array<System>;

    public function new() {
        systems = [];
    }

    public function addSystem(system: System) {
        systems.push(system);

        if (system.type == Start) {
            system.run();
        }
    }
    
    public function run() {
        while (true) {
            for (system in systems) {
                if (system.type == Update) system.run();
            }
        }
    }
}