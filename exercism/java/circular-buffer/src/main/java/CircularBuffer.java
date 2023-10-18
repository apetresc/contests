class CircularBuffer<T> {
    private final int capacity;
    private final T[] buffer;
    private int head = 0;
    private int tail = 0;
    private int size = 0;


    @SuppressWarnings("unchecked")
    CircularBuffer(final int capacity) {
        this.capacity = capacity;
        buffer = (T[]) new Object[capacity];
    }

    T read() throws BufferIOException {
        if (size == 0) throw new BufferIOException("Tried to read from empty buffer");
        T data = buffer[tail];
        tail = ++tail % capacity;
        size--;
        return data;
    }

    void write(T data) throws BufferIOException {
        if (size == capacity) throw new BufferIOException("Tried to write to full buffer");
        buffer[head] = data;
        size++;
        head = ++head % capacity;
    }

    void overwrite(T data) {
        if (size == capacity) tail = ++tail % capacity;
        buffer[head] = data;
        head = ++head % capacity;
        size = Math.min(++size, capacity);
    }

    void clear() {
        head = tail = size = 0;
    }

}