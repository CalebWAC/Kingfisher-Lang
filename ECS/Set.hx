class Set<T> {
    public var data : Array<T>;
    public var length : Int;

    public function new(data: Array<T>) {
        this.data = data;
        this.length = data.length;
    }

    public function contains(element: T) return data.contains(element);

    public function iter() return data.iterator;

    public function add(element: T) 
        if (!data.contains(element)) {
            data.push(element);
            length++;
        }


    public function remove(element: T) if (data.remove(element)) length--;
}